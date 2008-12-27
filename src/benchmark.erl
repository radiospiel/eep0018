-module(benchmark).
-export([run/1, run/2]).

run(Fun) ->
  run("", Fun).

run(Label, Fun) ->

  T1 = erlang:now(),

  R = Fun(),

  T2 = erlang:now(),
  Diff = timer:now_diff(T2,T1) / 1000,
  
  io:format(Label ++ "Elapsed time: ~w ms ~n" , [Diff]),
  R.
