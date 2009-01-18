-module(runner).
-export([run/1]).

%
% which modules to check?

-define(TEST_CONFIGURATIONS, [ 
  [ eep0018 ]
  ]
).

run(A) ->
  eep0018:start("../bin"),
  lists:foreach(fun(Subdir) -> do_run(Subdir) end, A),
  init:stop().

do_run(Subdir) -> 
  lists:foreach(fun(C) -> testcase:run(Subdir, C) end, ?TEST_CONFIGURATIONS).
