-module(day07).

-export([solve/0]).

solve() ->
  Input = aoc:read_file("day07.txt", <<"\n">>),
  {_, Tree} = build_dir_structure(tl(Input), new_tree(<<"/">>)),
  SummedDirs = summed_dirs(Tree),
  {part1(SummedDirs), part2(SummedDirs)}.

build_dir_structure([], Acc) ->
  Acc;
build_dir_structure([<<"$ cd ..">> | T], Acc) ->
  build_dir_structure(T, pop_dir(Acc));
build_dir_structure([<<"$ cd ", Target/binary>> | T], Acc) ->
  build_dir_structure(T, push_dir(Target, Acc));
build_dir_structure([<<"$ ls">> | T], Acc) ->
  {NewT, NewAcc} = push_contents(T, Acc),
  build_dir_structure(NewT, NewAcc).

push_contents([], Acc) ->
  {[], Acc};
push_contents([<<"$ ", _/binary>> | _] = T, Acc) ->
  {T, Acc};
push_contents([<<"dir ", Dir/binary>> | T], Acc) ->
  push_contents(T, new_dir(Dir, Acc));
push_contents([File | T], Acc) ->
  {match, [{_StartSize, EndSize}]} = re:run(File, <<"[0-9]+">>),
  {Size, Name} = split_binary(File, EndSize),
  push_contents(T, add_file_size(Name, Size, Acc)).

new_tree(Path) ->
  {Path, #{Path => 0}}.

new_dir(Dir, {Path, Contents}) ->
  {Path, maps:put(filename:join(Path, Dir), 0, Contents)}.

add_file_size(_Name, Size, {Path, Contents}) ->
  {Path, maps:update_with(Path, fun(V) -> V + binary_to_integer(Size) end, binary_to_integer(Size), Contents)}.

pop_dir({Path, Contents}) ->
  {filename:dirname(Path), Contents}.

push_dir(Target, {Path, Contents}) ->
  {filename:join(Path, Target), Contents}.

summed_dirs(Tree) ->
  maps:fold(fun(K, _, Acc) ->
              SumWithSubDirs = lists:sum(maps:values(maps:filter(fun(KI, _) -> binary:match(KI, K) =/= nomatch end, Tree))),
              Acc#{K => SumWithSubDirs}
            end, #{}, Tree).

part1(Tree) ->
  Filtered = maps:filter(fun(_, V) -> V =< 100_000 andalso V =/= 0 end, Tree),
  lists:sum(maps:values(Filtered)).

part2(Sums) ->
  Total =  maps:get(<<"/">>, Sums, 0),
  Unused = 70_000_000 - Total,
  Required = 3_000_0000,
  ToDelete = Required - Unused,
  Candidates = maps:to_list(maps:filter(fun(_, V) -> V > ToDelete end, Sums)),
  {_Dir, Size} = hd(lists:keysort(2, Candidates)),
  Size.
