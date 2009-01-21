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
-define(PARSE_NUMBERS_AS_TUPLE, 8).

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
  labels,
  float,

  % implementations
  binary_to_label, list_to_number
}).

fetch_option(Key, Dict, Default) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    _ -> Default
  end.

build_options(In) ->
  Dict = dict:from_list(In),
  
  Opt_labels = fetch_option(labels, Dict, binary),
  Opt_float = fetch_option(number, Dict, exact),
  
  #options{
    labels = Opt_labels,
    float = Opt_float,

    binary_to_label = binary_to_label_fun(Opt_labels),
    list_to_number = list_to_number_fun(Opt_float)
  }.

%% Convert labels

to_existing_atom(V) when is_list(V) -> try list_to_existing_atom(V) catch _:_ -> V end;
to_existing_atom(V)                 -> to_existing_atom(binary_to_list(V)).

to_atom(V) when is_list(V)  -> try list_to_atom(V) catch _:_ -> V end;
to_atom(V)                  -> to_atom(binary_to_list(V)).

binary_to_label_fun(binary)         -> fun identity/1; 
binary_to_label_fun(atom)           -> fun to_atom/1;
binary_to_label_fun(existing_atom)  -> fun to_existing_atom/1;
binary_to_label_fun(X) -> io:format("Unsupported labels value ~p ~n", [ X ]). 

%% Convert numbers

to_number(S) ->  
  try list_to_integer(S) 
  catch _:_ -> list_to_float(S)
  end.

to_float(S) ->  
  try list_to_integer(S) + 0.0 
  catch _:_ -> list_to_float(S)
  end.

list_to_number_fun(exact) ->  fun to_number/1;
list_to_number_fun(float) ->  fun to_float/1;

list_to_number_fun(X) -> io:format("Unsupported number parameter ~p ~n", [ X ]). 

%% receive values from the driver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_value(O) -> 
  receive
    { _, { _, [ ?EI | DATA ] } }     -> receive_ei_encoded(O, DATA);

    UNKNOWN                   -> io:format("UNKNOWN 1 ~p ~n", [UNKNOWN]), UNKNOWN
  end.

receive_value(object, O)  -> receive_value(O);
receive_value(value, O)   -> [ Value ] = receive_value(O), Value.

% for testing
%adjust(O, In) ->
%  adjust(build_options(O), In).

%  eep0018:adjust([], [{number,"-123"},<<"foo">>|{}]),

adjust_object_entries(O, {}) -> [];              %compat
adjust_object_entries(O, {K,V}) ->               %compat
  {(O#options.binary_to_label)(K),adjust(O, V)}.

adjust_compat(O, In) ->
  case In of
    {number,N}          -> (O#options.list_to_number)(N);
    {K,V}               -> {(O#options.binary_to_label)(K),adjust(O, V)};

    [{}]                -> {[]};                                  % compat

    [{number,V}|T]     ->                                         % compat
      lists:map(fun(S) -> adjust(O, S) end, In);

    [{K,V}|T]           ->                                        % compat
      { lists:map(fun(S) -> adjust_object_entries(O, S) end, In) };
      
    [H|T]               -> lists:map(fun(S) -> adjust(O, S) end, In);
    X                   -> X
  end.

adjust_new(O, In) ->
  case In of
    {number,N}          -> (O#options.list_to_number)(N);
    {K,V}               -> {(O#options.binary_to_label)(K),adjust(O, V)};
    [H|T]               -> lists:map(fun(S) -> adjust(O, S) end, In);
    X                   -> X
  end.

adjust(O, In) -> adjust_compat(O, In).

receive_ei_encoded(O, DATA) ->
  Raw = erlang:binary_to_term(DATA), 

  case Raw of
    {error, _} -> throw(badarg);  
    _ -> ok
  end,

  %
  % If the caller knows what he does we might not have to adjust 
  % the returned data:
  % - map keys are stored as binaries.
  % - numbers are accepted as {number, String} tuple
  %
  case {O#options.labels, O#options.float} of
    {binary, intern} -> Raw;
    _ -> adjust(O, Raw)
  end.

loop(Port) ->
  receive
    {parse, Caller, X, O} ->
      Parse = fetch_option(parse, dict:from_list(O), object),
      Opt = case Parse of
        object -> 0;
        value  -> ?PARSE_VALUE
      end,
      
      Port ! {self(), {command, [ ?PARSE_EI | [ Opt | X ] ]}},
      
      Result = receive_value(Parse, build_options(O)),
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
