-module(testcase).
-export([run/2]).
-export([parallel/2]).

-define(PROCS, 5).

% loads and runs all test cases from a given subdir. A testcase
% consists of a JSON input file, and some optional Erlang term
% gold-files. The file naming scheme is like this:
%
%  * <testcase>.json              - a JSON inout file
%  * <testcase>.<config>.gold.erl - the expected result, when parsing 
%                                   <testcase>.json with the given 
%                                   configuration
%  * <testcase>.<config>.out.erl  - the result of parsing 
%                                   <testcase>.json with the given 
%                                   configuration
%
run(Subdir, Config) ->
  do_run(test, Subdir, Config).

parallel(Subdir, Config) ->
  do_run(parallel, Subdir, Config).

% -- main entry -------------------------------------------------------

do_run(Mode, Subdir, Config) ->
% eep0018:start("../bin"),

  {ok, FileNames} = file:list_dir(Subdir),
  JsonFiles = [Fname || Fname <- lists:sort(FileNames), lists:suffix(".json", Fname)],
  lists:foreach(fun(Case) -> 
    [ Basename, _ ] = string:tokens(Case, "."),
    run_case(Mode, Subdir, Basename, Config)
  end, JsonFiles).

% -- some I/O helpers -------------------------------------------------

% read a file
read_file(File) ->
  {ok, Json} = file:read_file(File), Json.

% parse an erlang term from a file
read_term(File) ->
  case file:consult(File) of
    {ok, [Term]} -> Term;
    {error, {Line, Mod, Term}} ->
      Msg = apply(Mod, format_error, {Line, Mod, Term}),
      io:format("SYNTAX ERROR: ~p ~n", [ Msg ]);
    {error, _} -> nil
  end.

% write an erlang term to a file
write_term(File, Term) ->
  {ok, FileDev} = file:open(File, write),
  io:fwrite(FileDev, "~w", [ Term ]), %~p", [ Term ]),
  file:close(FileDev).


% -- run testcase in a specific config. -------------------------------

run_case(Mode, Subdir, Case, Config) ->
  CaseBase = Subdir ++ "/" ++ Case,
  JsonInput = read_file(CaseBase ++ ".json"),
  case Mode of
    test      -> test_case(JsonInput, CaseBase, Config);
    parallel  -> parallel_case(JsonInput, CaseBase, Config)
  end.

% -- run testcase in non-parallel
  
test_case(JsonInput, CaseBase, Config) ->
  delete_results(CaseBase, Config),
  Term = parse_json(JsonInput, Config), 
  Result = check_result(Term, CaseBase, Config), % should be pass, fail, out.
  write_term(result_file(CaseBase, Config, Result), Term),
  io:format("~p ~p -> ~p ~n", [Config, CaseBase, Result]),
  Result.

% check the results
check_result(Result, CaseBase, Config) ->
  Gold = read_term(CaseBase ++ "." ++ Config ++ ".gold.erl"),
  pass. % compare:equiv(Gold, Result)).

delete_results(CaseBase, Config) ->
  lists:foreach(fun(R) -> file:delete(result_file(CaseBase, Config, R)) end, [ pass, fail, out ]).

result_file(CaseBase, Config, R) -> CaseBase ++ "." ++ Config ++ "." ++ atom_to_list(R).

% -- run testcase in parallel

parallel_case_run(JsonInput, CaseBase, Config) ->
  test_case(JsonInput, CaseBase, Config).

parallel_case(JsonInput, CaseBase, Config) ->
  RunFun = fun() -> parallel_case_run(JsonInput, CaseBase, Config) end,
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

% -- different json parsers -------------------------------------------

parse_json(Input, [ eep0018 | T ])  -> eep0018:json_to_term(Input);
parse_json(Input, [ mochijson2 ])   -> mochijson2:decode(Input);
parse_json(Input, [ rabbitmq ])     -> rabbitmq:decode(Input);
parse_json(Input, X)                -> io:format("Unsupported configuration: ~p ~n", [ X ]).
