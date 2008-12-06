-module(eep0018).

-export([start/0, stop/0]).
-export([json_to_term/1, json_to_term/2]).
-export([term_to_json/1, term_to_json/2]).
-export([twice/1, sum/2]).                      % TODO: remove test code!

% Public interface

%%% TODO: set default parameters

json_to_term(X) -> json_to_term(X, 0).
term_to_json(X) -> term_to_json(X, 0).

% Implementation

start() ->
  start("eep0018_drv").

start(SharedLib) ->
  case erl_ddll:load_driver(".", SharedLib) of
    ok                      -> ok;
    {error, already_loaded} -> ok;
    {error, X}              -> exit({error, X});
    _                       -> exit({error, could_not_load_driver})
  end,
  spawn(fun() -> init(SharedLib) end).

init(SharedLib) ->
  register(eep0018, self()),
  Port = open_port({spawn, SharedLib}, []),
  loop(Port).

stop() ->
  eep0018 ! stop.

% event loop, etc.

call_port(Msg) -> 
  eep0018 ! {call, self(), Msg},
  receive 
    {eep0018, Result} -> Result
  end.

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      % io:format("~p ~n", [ "pre" ]),
      % io:format("~p ~n", [ encode(Msg) ]),		
      Port ! {self(), {command, encode(Msg)}},
      % io:format("~p ~n", [ "post" ]),
      receive
        {Port, {data, Data}} -> Caller ! {eep0018, decode(Data)}
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} -> exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      io:format("~p ~n", [Reason]),
      exit(port_terminated)
  end.

% 

twice(X) -> call_port({twice, X}).
sum(X,Y) -> call_port({sum, X, Y}).
json_to_term(X,Y) -> call_port({json_to_term, X, Y}).
term_to_json(X,Y) -> call_port({term_to_json, X, Y}).

encode({json_to_term, X, Y})  -> ["json_to_term", X, Y];
encode({term_to_json, X, Y}) -> [2, X, Y];
encode({twice, X})  -> [1, X];
encode({sum, X, Y}) -> [2, X, Y].

decode([Int]) -> Int.
