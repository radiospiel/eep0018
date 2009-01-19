-module(runner).
-export([run/1]).
-export([run_case/1]).


%
% which modules to check?

-define(TEST_CONFIGURATIONS, [ 
  [ eep0018 ],
  [ mochijson2 ],
  [ rabbitmq ]
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

