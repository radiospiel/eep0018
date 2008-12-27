-module(eep0018).

-export([start/0, stop/0]).
-export([parse/1, parse/2]).
-export([parse_file/1, parse_file/2]).

%% constants (see eep0018.h)

-define(JSON_PARSE,       1).
-define(ATOM,             10).
-define(NUMBER,           11).
-define(STRING,           12).
-define(KEY,              13).
-define(MAP,              14).
-define(ARRAY,            15).
-define(END,              16).

%% start/stop port

start() ->
  start("eep0018_drv").
 
start(SharedLib) ->
  case erl_ddll:load_driver(".", SharedLib) of
    ok -> ok;
    {error, already_loaded} -> ok;
    {error, X} -> exit({error, X});
    _ -> exit({error, could_not_load_driver})
  end,
  spawn(fun() -> init(SharedLib) end).

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
% {float,true}
% {label,binary|atom|existing_atom}
% {duplicate_labels,true/false/raise} % raise badarg
%

-record(options, {list_to_label, list_to_number, duplicate_labels, strict_order}).

fetch_option(Property, In, Default) ->
  Default.

build_options(In) ->
  #options{
    list_to_label   = list_to_label_fun(fetch_option(labels, In, binary)),
    list_to_number  = list_to_number_fun(fetch_option(float, In, true)),
    duplicate_labels= duplicate_labels_fun(fetch_option(duplicate_labels, In, true))
  }.

%% Convert labels

list_to_label_fun(binary)         ->  fun(S) -> S end;
list_to_label_fun(atom)           ->  
  fun(S) -> 
    try list_to_atom(S) of A -> A 
    catch _:_ -> S end
  end;
list_to_label_fun(existing_atom)  ->  
  fun(S) -> 
    try list_to_existing_atom(S) of A -> A 
    catch _:_ -> S end
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
  fun(S) -> list_to_float(S) end.

list_to_number(O, S) ->
  (O#options.list_to_number)(S).

%% Finish maps

duplicate_labels_fun(true)  -> fun(S) -> S end.

% Not yet implemented
% duplicate_labels_fun(false)  
% duplicate_labels_fun(raise)  

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

    UNKNOWN                   -> io:format("UNKNOWN ~p ~n", [UNKNOWN]), UNKNOWN
  end.
  
loop(Port) ->
  receive
    {parse, Caller, X, O} ->
      Port ! {self(), {command, [ ?JSON_PARSE | X ]}},
      
      Result = receive_value(build_options(O)),
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

%% control loop for driver

%

parse(X)      -> parse(X, []).

read_file(File) ->
  { ok, Data } = file:read_file(File),
  Data.

parse_file(X) -> parse(read_file(X)).
parse_file(X,O) -> parse(read_file(X), O).

parse(X,O)  -> 
  eep0018 ! {parse, self(), X, O},
  receive
    {result, Result} -> Result
  end.
