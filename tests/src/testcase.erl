-module(testcase).
-export([run/2]).
-export([parallel/2]).
-export([run_case/2]).

-define(PROCS, 5).

% loads and runs all test cases from a given subdir. A testcase
% consists of a JSON input file, and some optional Erlang term
% gold-files. The file naming scheme is like this:
%
%  * <case>.json                  ... a JSON inout file
%  * <case>.<config>.gold.erl     ... the expected result for parsing 
%                                     <testcase>.json with in the given
%                                     configuration
%  * <case>.<config>.<status>.erl ... the result of parsing <testcase>.json
%                                     with the given configuration; status 
%                                     is fail,ok, or pass.
%
run(Subdir, Config) ->
  do_run(test, Subdir, Config).

run_case(CaseBase, Config) ->
  run_case(test, CaseBase, Config).
  
parallel(Subdir, Config) ->
  do_run(parallel, Subdir, Config).

% -- main entry -------------------------------------------------------

do_run(Mode, Subdir, Config) ->
% eep0018:start("../bin"),

  {ok, FileNames} = file:list_dir(Subdir),
  JsonFiles = [Fname || Fname <- lists:sort(FileNames), lists:suffix(".json", Fname)],
  lists:foreach(fun(Case) -> 
    [ Basename, _ ] = string:tokens(Case, "."),
    CaseBase = Subdir ++ "/" ++ Basename,
    run_case(Mode, CaseBase, Config)
  end, JsonFiles).

% -- some I/O helpers -------------------------------------------------

% read a file
read_file(File) ->
  {ok, Json} = file:read_file(File), Json.

% parse an erlang term from a file
read_term(nil) -> nil;
read_term(File) ->
  % io:format("File: ~p ~n", [ File ]),
  case file:consult(File) of
    {ok, [Term]}        -> Term;
    {error, enoent}     -> nil;
    {error, {_,_,Msg}}  -> io:format("~s ~s ~n", [ File, Msg ]), nil;
    {error, Msg}        -> io:format("~s ~s ~n", [ File, Msg ]), nil
  end.

% write an erlang term to a file
write_term(File, Term) ->
  {ok, FileDev} = file:open(File, write),
  io:fwrite(FileDev, "~p.~n", [ Term ]), %~p", [ Term ]),
  file:close(FileDev).


% -- run testcase in a specific config. -------------------------------

run_case(Mode, CaseBase, Config) ->
  JsonInput = read_file(CaseBase ++ ".json"),
  case Mode of
    test      -> test_case(JsonInput, CaseBase, Config);
    parallel  -> parallel_case(JsonInput, CaseBase, Config)
  end.

% -- run testcase in non-parallel
  
result_name(pass) -> "pass (unverified)";
result_name(fail) -> "fail";
result_name(ok)   -> "ok".

test_case(JsonInput, CaseBase, Config) ->
  delete_results(CaseBase, Config),
  Term = parse_json(JsonInput, Config), 
  Result = check_result(Term, CaseBase, Config), % should be err, pass, fail, ok.
  write_term(result_file(CaseBase, Config, Result), Term),
  io:format("~p ~p -> ~p ~n", [Config, CaseBase, result_name(Result)]),
  Result.

% check the results

gold_name(CaseBase, [eep0018])    -> CaseBase ++ "." ++ "gold.erl";
gold_name(CaseBase, [eep0018|T])  -> CaseBase ++ "." ++ join(T, "-") ++ ".gold.erl";
gold_name(_, _)                   -> nil.

check_result(Result, CaseBase, Config) ->
  GoldName = gold_name(CaseBase, Config),
  Gold = read_term(GoldName),
  % io:format("\t\t\t\t\t\t~p -> ~p~n", [ GoldName, Gold ]),
  verify_result(Gold, Result).
  
verify_result(true) -> ok;
verify_result(false) -> fail.

verify_result(nil, _) -> pass;
verify_result(Gold, R) -> verify_result(compare:equiv(Gold, R)).

  % pass. % compare:equiv(Gold, Result)).

delete_results(CaseBase, Config) ->
  lists:foreach(fun(R) -> file:delete(result_file(CaseBase, Config, R)) end, [ pass, fail, ok ]).

result_file(CaseBase, Config, R) -> CaseBase ++ "." ++ Config ++ "." ++ to_s(R).

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

parse_json(Input, [ eep0018 | _ ])  -> eep0018:json_to_term(Input);
parse_json(Input, [ mochijson2 ])   -> mochijson2:decode(Input);
parse_json(Input, [ rabbitmq ])     -> rabbitmq:decode(Input);
parse_json(_, X)                    -> io:format("Unsupported configuration: ~p ~n", [ X ]).

% -- string join

to_s(X) when is_atom(X) -> atom_to_list(X);
to_s(X) -> X.

% -- not fast, but working :)
join([], _)     -> "";
join([H], _)    -> to_s(H);
join([H|T], S)  -> to_s(H) ++ S ++ join(T, S).
