-module(runner).
-export([run/1]).
-export([run_case/1]).
-export([parallel/1]).
-export([benchmark/1]).

-define(PROCS, 5).

%
% which modules to check?

-define(TEST_CONFIGURATIONS, [ 
  [ eep0018 ] 
% ,[ eep0018, [{float, false}, {labels, atom}] ]
% ,[ eep0018, [{float, true}, {labels, binary}] ]
% ,[ eep0018, [{float, false}, {labels, atom}] ]
% ,[ eep0018, [{float, false}, {labels, binary}] ]
% ,[ eep0018, [{float, intern}, {labels, atom}] ]
% ,[ eep0018, [{float, intern}, {labels, binary}] ]
 ,[ mochijson2 ]
 ,[ rabbitmq ]
]
).

run(A) ->
  eep0018:start("../bin"),
  lists:foreach(fun(Subdir) -> do_run(Subdir) end, A),
  init:stop().

run_case(X) ->
  eep0018:start("../bin"),
  lists:foreach(fun(C) -> testcase:run_case(X,C) end, ?TEST_CONFIGURATIONS),
  init:stop().

do_run(Subdir) -> 
  lists:foreach(fun(C) -> testcase:run(Subdir, C) end, ?TEST_CONFIGURATIONS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

benchmark(Subdir) ->
  eep0018:start("../bin"),

  lists:foreach(
    fun(C) -> 
      Time = benchmark:timed(
        fun() -> testcase:benchmark(Subdir, C) end
      ),
      io:format("*** ~p: overall time ~p ~n", [ C, Time ])
    end,
    ?TEST_CONFIGURATIONS),
  init:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parallel(A) ->
  eep0018:start("../bin"),
  do_parallel(A),
  init:stop().

do_parallel(A) ->
  RunFun = fun() -> 
    lists:foreach(fun(Subdir) -> do_run(Subdir) end, A)
  end,
  spawn_procs(?PROCS, RunFun),
  wait_for_procs(?PROCS).

spawn_procs(0, _) ->
  ok;
spawn_procs(N, Func) ->
  S = self(),
  spawn(fun() -> Func(), S ! finished end),
  spawn_procs(N-1, Func).

wait_for_procs(0) ->
  ok;
wait_for_procs(N) ->
  receive finished -> ok end,
  wait_for_procs(N-1).
