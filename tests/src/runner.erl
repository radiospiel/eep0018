-module(runner).
-export([run/1]).

%
% which modules to check?

-define(TEST_CONFIGURATIONS, [ 
  [ eep0018 ]
  ]
).

run(Subdir) ->
  eep0018:start("../bin"),
  lists:foreach(fun(C) -> testcase:run(Subdir, C) end, ?TEST_CONFIGURATIONS),
  init:stop().
