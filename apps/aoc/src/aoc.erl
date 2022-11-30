-module(aoc).

-export([ read_file/2
        , read_file/3
        , read_file_fold/4
        ]).

-export([ single_pass/0
        , single_pass/1
        , multi_pass/0
        , multi_pass/1
        , multi_pass/2
        ]).

%% File readers ===============================================================
-spec read_file(string(), binary()) -> [any()].
read_file(File, Split) ->
  {ok, Bin} = read(File),
  [ S || S <- binary:split(Bin, Split, [trim, global]) ].

-spec read_file(string(), binary(), fun((any()) -> any())) -> [any()].
read_file(File, Split, CastFun) ->
  {ok, Bin} = read(File),
  [ CastFun(S) || S <- binary:split(Bin, Split, [trim, global]) ].

-spec read_file_fold(string(), binary(), fun((any(), any()) -> any()), any()) -> any().
read_file_fold(File, Split, FoldFun, Acc) ->
  {ok, Bin} = read(File),
  lists:foldl(FoldFun, Acc, binary:split(Bin, Split, [trim, global])).

-spec read(string()) -> {error, atom()} | {ok, binary()}.
read(File) ->
  file:read_file(code:priv_dir(aoc) ++ "/inputs/" ++ File).

%% Run all ====================================================================
single_pass() ->
  single_pass(days()).

single_pass(Days) ->
  io:format("Results for single iteration: ~n"),
  lists:foreach(fun(Day) ->
                  io:format("~p -> ", [Day]),
                  {T, _} = timer:tc(fun Day:solve/0),
                  io:format("Time: ~p us (~p ms)~n", [T, T div 1000])
                end, Days).

multi_pass() ->
  multi_pass(days()).

multi_pass(Days) ->
  multi_pass(Days, 10).

multi_pass(Days, N) ->
  io:format("~nResults for ~p iterations: ~n", [N]),
  lists:foreach(fun(Day) ->
                  io:format("~p -> ", [Day]),
                  util:time_avg(fun Day:solve/0, N)
                end, Days).

days() ->
  Files = os:cmd("ls " ++ filename:join([code:priv_dir(aoc), "..", "src"])),
  lists:filtermap(fun("day" ++ _ = Day) ->
                    {true, list_to_existing_atom(filename:basename(Day, ".erl"))};
                  (_) ->
                    false
               end, string:tokens(Files, "\n")).
