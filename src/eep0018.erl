-module(eep0018).

-export([start/0, start/1]).
-export([stop/0]).

-export([json_to_term/1, json_to_term/2]).
-export([term_to_json/1]).

%% constants (see eep0018.h)

% commands

-define(JSON_PARSE,           1).
-define(JSON_PARSE_EI,        2).

% options

-define(JSON_PARSE_IN_VALUE, 1).
-define(JSON_PARSE_RAW_NUMBERS, 2).

% sax parser types

-define(ATOM,             10).
-define(NUMBER,           11).
-define(STRING,           12).
-define(KEY,              13).
-define(MAP,              14).
-define(ARRAY,            15).
-define(END,              16).

% ei parser type

-define(EI,               17).

-define(DriverMode, ei).
%-define(DriverMode, sax).

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
  Port = open_port({spawn, SharedLib}, []),
  loop(Port).

stop() ->
  eep0018 ! stop.

%% logging helpers. 

l(X) ->
  io:format("Log: ~p ~n", [ X ]), X.

l(M, X) ->
  io:format(M ++ ": ~p ~n", [ X ]), X.

%
% Options are as follows
%

-record(options, {list_to_label, list_to_number, number_to_number, duplicate_labels}).

fetch_option(Key, Dict, Default) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    _ -> Default
  end.

build_options(In) ->
  Dict = dict:from_list(In),
  
  #options{
    list_to_label   = list_to_label_fun(fetch_option(labels, Dict, binary)),
    list_to_number  = list_to_number_fun(fetch_option(float, Dict, true)),
    number_to_number= number_to_number_fun(fetch_option(float, Dict, true)),
    duplicate_labels= duplicate_labels_fun(fetch_option(duplicate_labels, Dict, true))
  }.

%% Convert labels

list_to_label_fun(binary)         ->  fun(S) -> S end;
list_to_label_fun(atom)           ->  
  fun(S) -> 
    try list_to_atom(S) of A -> A 
    catch _:_ -> list_to_binary(S) end
  end;
list_to_label_fun(existing_atom)  ->  
  fun(S) -> 
    try list_to_existing_atom(S) of A -> A 
    catch _:_ -> list_to_binary(S) end
  end.

list_to_label(O, S) ->
  (O#options.list_to_label)(S).

%% Convert numbers

list_to_number_fun(false) ->  
  fun(S) ->
    try list_to_integer(S) of
      I -> I
    catch
      _:_ -> list_to_float(S)
    end
  end;

list_to_number_fun(true) ->  
  fun(S) ->
    try list_to_integer(S) of
      I -> 0.0 + I
  catch
    _:_ -> list_to_float(S)
  end
end.

list_to_number(O, S) ->
  (O#options.list_to_number)(S).


number_to_number_fun(true) ->
  fun(N) -> 0.0 + N end;
number_to_number_fun(false) ->
  fun(N) -> N end.

number_to_number(O, S) ->
  (O#options.number_to_number)(S).

%% Finish maps
%
% TODO: Add working implementations for duplicate_labels_fun(false), 
% duplicate_labels_fun(raise).
%

duplicate_labels_fun(true)  -> fun(S) -> S end;
duplicate_labels_fun(false) -> fun(S) -> S end;
duplicate_labels_fun(raise) -> fun(S) -> S end.

%% receive values from the Sax driver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_map(O, In) -> 
  case receive_value(O) of
    'end' -> 
      (O#options.duplicate_labels)(
        lists:reverse(In)
      );
    Key   -> receive_map(O, [ {Key, receive_value(O)} | In ])
  end.

receive_array(O, In) ->  
  case receive_value(O) of
    'end' -> lists:reverse(In);
    T     -> receive_array(O, [ T | In ])
  end.

receive_value(O) -> 
  receive
    { _, { _, [ ?MAP ] } }    -> receive_map(O, []);
    { _, { _, [ ?ARRAY ] } }  -> receive_array(O, []);
    { _, { _, [ ?END ] } }    -> 'end';

    { _, { _, [ ?ATOM | DATA ] } }   -> list_to_atom(DATA);
    { _, { _, [ ?NUMBER | DATA ] } } -> list_to_number(O, DATA);
    { _, { _, [ ?STRING | DATA ] } } -> list_to_binary(DATA);
    { _, { _, [ ?KEY | DATA ] } }    -> list_to_label(O, DATA);

    { _, { _, [ ?EI | DATA ] } }     -> receive_ei_encoded(O, DATA);

    UNKNOWN                   -> io:format("UNKNOWN 1 ~p ~n", [UNKNOWN]), UNKNOWN
  end.

receive_value(object, O)  -> receive_value(O);
receive_value(value, O)   -> [ Value ] = receive_value(O), Value.

%% receive values from the EI driver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


adjust_ei_encoded(O, {number,S})          -> (O#options.list_to_number)(S);
adjust_ei_encoded(O, {K,V})               -> {(O#options.list_to_label)(K),V};
adjust_ei_encoded(O, [H|T])               -> lists:map(fun(S) -> adjust_ei_encoded(O, S) end, [H|T]);
adjust_ei_encoded(O, X) when is_number(X) -> number_to_number(O, X);
adjust_ei_encoded(_, X)                   -> X.

receive_ei_encoded(O, DATA) ->
  Raw = erlang:binary_to_term(list_to_binary(DATA)),
  l("raw", Raw),
  adjust_ei_encoded(O, Raw).

loop(Port) ->
  receive
    {parse, Caller, X, O} ->
      Parse = fetch_option(parse, dict:from_list(O), object),
      Cmd = case ?DriverMode of
        ei  -> ?JSON_PARSE_EI;
        sax -> ?JSON_PARSE
      end,
      Opt = case Parse of
        object -> 0;
        value  -> ?JSON_PARSE_IN_VALUE
      end,
      
      Port ! {self(), {command, [ Cmd | [ Opt | X ] ]}},
      
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
