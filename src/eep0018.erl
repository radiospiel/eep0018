-module(eep0018).

-export([start/0, start/1]).
-export([stop/0]).

-export([json_to_term/1, json_to_term/2]).
-export([term_to_json/1]).

-compile(export_all).

%% constants (see eep0018.h)

-define(PARSE_EI,       2).
-define(PARSE_VALUE, 1).

-define(PARSE_NUMBERS_AS_NUMBER, 2).
-define(PARSE_NUMBERS_AS_FLOAT, 4).
-define(PARSE_NUMBERS_AS_TUPLE, 0).

-define(PARSE_KEYS_AS_ATOM, 8).
-define(PARSE_KEYS_AS_BINARY, 0).

-define(EI,                 17).


%% start/stop port

start() ->
  start(".").
 
start(LibPath) ->
  case erl_ddll:load_driver(LibPath, "eep0018_drv") of
    ok -> ok;
    {error, already_loaded} -> ok;
    {error, X} -> exit({error, X});
    _ -> exit({error, could_not_load_driver})
  end,
  spawn(fun() -> init("eep0018_drv") end).

init(SharedLib) ->
  register(eep0018, self()),
  Port = open_port({spawn, SharedLib}, [binary]),
  loop(Port).

stop() ->
  eep0018 ! stop.

%% logging helpers. 

l(X) ->
  io:format("Log: ~p ~n", [ X ]), X.

l(M, X) ->
  io:format(M ++ ": ~p ~n", [ X ]), X.

%% Misc utils

identity(S) -> S.

%
% Options are as follows
%

-record(options, {
  % original values
  parse,
  objects,
  labels,
  number,

  % Options for driver
  options, 

  % implementations
  list_to_number
}).

fetch_option(Key, Dict, Default) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    _ -> Default
  end.

build_options(In) ->
  Dict = dict:from_list(In),

  Opt_number = fetch_option(number, Dict, exact),
  
  #options{
    parse   = fetch_option(parse, Dict, object),      % object or value
    objects = fetch_option(objects, Dict, compat),    % compat or eep0018
    labels  = fetch_option(labels, Dict, binary),     % binary or atom
    number  = Opt_number,                             % exact, number, float
    
    list_to_number = list_to_number_fun(Opt_number)
  }.

nop_options(O) -> O#options.number /= exact. 

%% Options for the driver

driver_option(parse, object)    -> 0;
driver_option(parse, value)     -> ?PARSE_VALUE;

driver_option(objects, compat)  -> 0;
driver_option(objects, eep0018) -> 0;

driver_option(number, exact)    -> ?PARSE_NUMBERS_AS_TUPLE;
driver_option(number, number)   -> ?PARSE_NUMBERS_AS_NUMBER;
driver_option(number, float)    -> ?PARSE_NUMBERS_AS_FLOAT;

driver_option(labels, binary)   -> ?PARSE_KEYS_AS_BINARY;
driver_option(labels, atom)     -> ?PARSE_KEYS_AS_ATOM;

driver_option(L, I) -> io:format("Unsupported option ~p ~p ~n", [L,I]), 0.

driver_option(O) ->
  driver_option(parse, O#options.parse) +
  driver_option(objects, O#options.objects) +
  driver_option(labels, O#options.labels) +
  driver_option(number, O#options.number).

%% Convert numbers

to_number(S) ->  
  try list_to_integer(S) 
  catch _:_ -> list_to_float(S)
  end.

list_to_number_fun(exact)   ->  fun to_number/1;
list_to_number_fun(float)   ->  fun identity/1;
list_to_number_fun(number)  ->  fun identity/1.

%% receive values from the driver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_value(InternOption) -> 
  receive
    { _, { _, [ ?EI | DATA ] } } -> 

      Adjusted = case erlang:binary_to_term(DATA) of
        {error, _} -> throw(badarg);  
        R          -> adjust(InternOption, R, InternOption#options.objects)
      end,

      case InternOption#options.parse of
        object -> Adjusted;
        value  -> [ VALUE ] = Adjusted, VALUE
      end
    % UNKNOWN -> io:format("UNKNOWN 1 ~p ~n", [UNKNOWN]), UNKNOWN
  end.

%% adjust returned term %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The return values from the driver might need some adjustment.
%
% - Numbers might be encoded as {number, <<"123">>, d}
% - In compat settings objects must be rebuild.

adjust_compat_object(_, {}) -> [];
adjust_compat_object(O, {K,V}) -> {K, adjust_compat(O, V)}.

adjust_compat(O, In) ->
  case In of
    {number,N,_}     -> (O#options.list_to_number)(N);
    {K,V}            -> {K,adjust_compat(O, V)};

    [{}]             -> {[]};
    [{number,_,_}|_] -> lists:map(fun(S) -> adjust_compat(O, S) end, In);             % This is a list starting with a number 
    [{_,_}|_]        -> { lists:map(fun(S) -> adjust_compat_object(O, S) end, In) };  % This is a list of {K,V} pairs 
      
    [_|_]            -> lists:map(fun(S) -> adjust_compat(O, S) end, In);
    X                -> X
  end.

adjust_eep0018(O, In) ->
  case In of
    {number,N,_}   -> (O#options.list_to_number)(N);
    {K,V}          -> {K,adjust_eep0018(O, V)};
    [_|_]          -> lists:map(fun(S) -> adjust_eep0018(O, S) end, In);
    X              -> X
  end.

% adjust(O, In) -> adjust_compat(O, In).

adjust(O, In, compat)  -> adjust_compat(O, In);
adjust(O, In, eep0018) -> adjust(O, In, nop_options(O));
adjust(_, In, true)    -> In;
adjust(O, In, _)       -> adjust_eep0018(O, In).

% 

loop(Port) ->
  receive
    {parse, Caller, X, O} ->
      InternOptions = build_options(O),
      DriverOpts = driver_option(InternOptions),
      
      Port ! {self(), {command, [ ?PARSE_EI | [ DriverOpts | X ] ]}},
      
      Result = receive_value(InternOptions),
      Caller ! {result, Result},
      loop(Port);

    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} -> exit(normal)
      end;

    {'EXIT', Port, Reason} ->
      io:format("exit ~p ~n", [Reason]),
      exit(port_terminated);
      
    X ->
      io:format("Unknown input! ~p ~n", [X]),
      loop(Port)
  end.

%% parse json

parse(X,O)  -> 
  eep0018 ! {parse, self(), X, O},
  receive
    {result, Result} -> Result
  end.

% The public, EEP0018-like interface.

%
%
json_to_term(X) -> parse(X, []).
json_to_term(X,O) -> parse(X, O).

% 
term_to_json(X) -> 
  rabbitmq:encode(X).
