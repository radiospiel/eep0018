-module(runner).
-export([run/1]).
-export([run_case/1]).
-export([benchmark/1]).


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
% ,[ mochijson2 ]
% ,[ rabbitmq ]
]
).

run(A) ->
  eep0018:start("../bin"),
  lists:foreach(fun(Subdir) -> do_run(Subdir) end, A),
  init:stop().

do_run(Subdir) -> 
  lists:foreach(fun(C) -> testcase:run(Subdir, C) end, ?TEST_CONFIGURATIONS).

run_case(A) ->
  eep0018:start("../bin"),
  lists:foreach(fun(C) -> testcase:run_case(A, C) end, ?TEST_CONFIGURATIONS),
  init:stop().

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
