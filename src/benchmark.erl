-module(benchmark).
-export([run/1, run/2]).
-export([timed/1]).
-export([run3/1, run3/2]).

run(Fun) ->
  run("*", Fun).

timed(Fun) ->
  T1 = erlang:now(),
  R = Fun(),
  T2 = erlang:now(),
  {R,timer:now_diff(T2,T1) / 1000}.

run(Label, Fun) ->
  {R,MSecs} = timed(Fun),
  io:format(Label ++ " Elapsed time: ~w ms ~n" , [MSecs]),
  R.

run3(Fun) ->
  run("*", Fun).

run3(Label, Fun) ->
  Fun(),
  {R,MSecs} = timed(fun() -> Fun(), Fun() end),
  io:format(Label ++ " Elapsed time: ~w ms ~n" , [MSecs/2]),
  R.
