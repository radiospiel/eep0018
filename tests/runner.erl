-module(runner).
-export([run/1]).
-export([interface_for/1]).

-define(PROCS, 5).

-define(RUN_SINGLE_PROC, true).
-define(RUN_MULTI_PROC, false).

%
% which modules to check?

%-define(CHECK_MODULES, [ eep0018 ]).
-define(CHECK_MODULES, [ eep0018, mochijson2, rabbitmq ]).

run("benchmark")  -> run("./benchmark", false, true);
run("incomplete") -> run("./incomplete", true, false);
run(Subdir)       -> run(Subdir, false, false).

run(Subdir, Incomplete, Benchmark) ->
   Cases = read_cases(Subdir),

   io:format("Loaded testcases ~n"),

   io:format("~p ~n", [ ?CHECK_MODULES ]),
   lists:foreach(
     fun(Module) -> run(Module, Cases, Incomplete, Benchmark) end,
     ?CHECK_MODULES
   ),
   init:stop().

% { encoder, decoder, init-fun }

%
% Deal w/different interfaces for different modules:
%
% get_encoder_and_decoder(Module, DecodeIncomplete)
%

interface_for(eep0018) -> { term_to_json, 
  json_to_term, json_value_to_term, 
  fun() -> eep0018:start("../bin") end };

interface_for(_) ->  { encode, decode }.

get_encoder_and_decoder(Module, false) ->
  case interface_for(Module) of
    {Encoder, Decoder, _, Init} -> Init(),  {Encoder, Decoder};
    {Encoder, Decoder}          ->          {Encoder, Decoder}
  end;
get_encoder_and_decoder(Module, true) ->
  case interface_for(Module) of
    {Encoder, _, Decoder, Init} -> Init(),  {Encoder, Decoder};
    {Encoder, Decoder}          ->          {Encoder, Decoder}
  end.
  
run(Module, Cases, Incomplete, IsBenchmark) ->
  % timer:sleep(1000),
  {Encoder, Decoder} = get_encoder_and_decoder(Module, Incomplete),
  Encode = fun(X) -> apply(Module, Encoder, [ X ]) end, 
  Decode = fun(X) -> apply(Module, Decoder, [ X ]) end,

  Name = atom_to_list(Module),
  io:format("Module ~p ~n", [ Module ]),
  
  run_module(?RUN_SINGLE_PROC, Name ++ "(single)", IsBenchmark, 
    fun() -> single(Cases, Encode, Decode) end),  
  run_module(?RUN_MULTI_PROC, Name ++ "(multi)", IsBenchmark, 
    fun() -> multiple(Cases, Encode, Decode) end)
  .

%
%  run_module(DoRun, Label, AsBenchmark, Func)
%
run_module(false, _, _, _) -> ok;
run_module(true, Label, false, Func) ->
  io:format("~p ...", [Label]),
  Func(),
  io:format(" ok ~n");
run_module(true, Label, true, Func) ->
  benchmark:run3(Label, Func).

single(Cases, Encode, Decode) ->
    lists:foreach(fun({Json, Term, Case}) ->
        case Term of
        nil ->
            Decode(Json);
        _ ->
            true = compare:equiv(Term, Decode(Json)),
            true = compare:equiv(Term, Decode(Encode(Term)))
        end
    end, Cases).

multiple(Cases, Encode, Decode) ->
    RunFun = fun() -> single(Cases, Encode, Decode) end,
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

read_cases(Subdir) ->
    {ok, FileNames} = file:list_dir(Subdir),
    JsonFiles = [Fname || Fname <- lists:sort(FileNames), lists:suffix(".json", Fname)],
    [
        begin
            {ok, Json} = file:read_file(Subdir ++ "/" ++ Case),
            {Pred, _} = lists:splitwith(fun(C) -> C == "." end, Case),
            ErlFname = "./cases/" ++ Pred ++ ".erl",
            Gold =
            case file:read_file_info(ErlFname) of
            {ok, _} ->
                {ok, [Term]} = file:consult(ErlFname),
                Term;
            {error, _} ->
                nil
            end,
            {Json, Gold, Case}
        end
        || Case <- JsonFiles
    ].
