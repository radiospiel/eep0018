-module(eep0018).

-export([start/0, start/1]).
-export([stop/0]).

-export([json_to_term/1, json_to_term/2]).
-export([term_to_json/1]).

%% constants (see eep0018.h)

-define(JSON_PARSE_EI,        2).
-define(JSON_PARSE_IN_VALUE, 1).
-define(JSON_PARSE_RAW_NUMBERS, 2).
-define(EI,               17).

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
  duplicate_labels,

  % implementations
  binary_to_label, number_to_number, tuple_to_number, check_labels
}).

fetch_option(Key, Dict, Default) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    _ -> Default
  end.

build_options(In) ->
  Dict = dict:from_list(In),
  
  Opt_labels = fetch_option(labels, Dict, binary),
  Opt_float = fetch_option(float, Dict, true),
  Opt_duplicate_labels = fetch_option(duplicate_labels, Dict, true),
  
  #options{
    labels = Opt_labels,
    float = Opt_float,
    duplicate_labels = Opt_duplicate_labels,

    binary_to_label   = binary_to_label_fun(Opt_labels),

    number_to_number= number_to_number_fun(Opt_float),
    tuple_to_number = tuple_to_number_fun(Opt_float),
    
    check_labels= check_labels_fun(Opt_duplicate_labels)
  }.

%% Convert labels

to_existing_atom(V) when is_list(V) -> try list_to_existing_atom(V) catch _:_ -> V end;
to_existing_atom(V)                 -> to_existing_atom(binary_to_list(V)).

to_atom(V) when is_list(V)  -> try list_to_atom(V) catch _:_ -> V end;
to_atom(V)                  -> to_atom(binary_to_list(V)).

binary_to_label_fun(binary)         -> fun identity/1; 
binary_to_label_fun(atom)           -> fun to_atom/1;
binary_to_label_fun(existing_atom)  -> fun to_existing_atom/1.

%% Convert numbers

to_number(S) ->  
  try list_to_integer(S) 
  catch _:_ -> list_to_float(S)
  end.

to_float(S) ->  
  try list_to_integer(S) + 0.0 
  catch _:_ -> list_to_float(S)
  end.

number_to_number_fun(intern) ->   fun identity/1; 
number_to_number_fun(false) ->    fun identity/1;
number_to_number_fun(true) ->     fun(N) -> 0.0 + N end.

tuple_to_number_fun(intern) ->   fun identity/1; 
tuple_to_number_fun(false) ->    fun({_,S}) -> to_number(S) end;
tuple_to_number_fun(true) ->     fun({_,S}) -> to_float(S) end.

%% Finish maps
%
% TODO: Add working implementations for check_labels_fun(false), 
% check_labels_fun(raise).
%

check_labels_fun(true)  -> fun identity/1;
check_labels_fun(false) -> fun identity/1;
check_labels_fun(raise) -> fun identity/1.

%% receive values from the driver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_value(O) -> 
  receive
    { _, { _, [ ?EI | DATA ] } }     -> receive_ei_encoded(O, DATA);

    UNKNOWN                   -> io:format("UNKNOWN 1 ~p ~n", [UNKNOWN]), UNKNOWN
  end.

receive_value(object, O)  -> receive_value(O);
receive_value(value, O)   -> [ Value ] = receive_value(O), Value.

adjust_ei_encoded(O, In) ->
  case In of
    {number,_}          -> (O#options.tuple_to_number)(In);
    {K,V}               -> {(O#options.binary_to_label)(K),V};
    [H|T]               -> lists:map(fun(S) -> adjust_ei_encoded(O, S) end, [H|T]);
    X when is_number(X) -> (O#options.number_to_number)(O, X);
    X                   -> X
  end.

receive_ei_encoded(O, DATA) ->
  Raw = erlang:binary_to_term(DATA),

  %
  % If the caller knows what he does we might not have to adjust 
  % the returned data:
  % - numbers are accepted as {number, String} tuple
  % - map keys are not to be stored as atoms.
  % - duplicate map keys are ok.
  %
  case {O#options.labels, O#options.float, O#options.duplicate_labels} of
    {binary, intern, true} -> Raw;
    _ -> adjust_ei_encoded(O, Raw)
  end.

loop(Port) ->
  receive
    {parse, Caller, X, O} ->
      Parse = fetch_option(parse, dict:from_list(O), object),
      Opt = case Parse of
        object -> 0;
        value  -> ?JSON_PARSE_IN_VALUE
      end,
      
      Port ! {self(), {command, [ ?JSON_PARSE_EI | [ Opt | X ] ]}},
      
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
