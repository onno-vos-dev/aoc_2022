-module(day07).

-export([solve/0]).

solve() ->
  Input = aoc:read_file("day07.txt", <<"\n">>),
  Tree = build_dir_structure(tl(Input), new_tree(<<"/">>)),
  {part1(Tree), part2(Tree)}.

build_dir_structure([], Acc) ->
  sum_final_dirs(Acc);
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
push_contents([<<"dir ", _/binary>> | T], Acc) ->
  push_contents(T, Acc);
push_contents([File | T], Acc) ->
  [Size, Name] = binary:split(File, <<" ">>),
  push_contents(T, add_file_size(Name, Size, Acc)).

-compile({inline, [new_tree/1]}).
new_tree(Path) ->
  {Path, #{Path => 0}}.

-compile({inline, [add_file_size/3]}).
add_file_size(_Name, Size, {Path, Contents}) ->
  {Path, maps:update_with(Path, fun(V) -> V + binary_to_integer(Size) end, binary_to_integer(Size), Contents)}.

-compile({inline, [pop_dir/1]}).
pop_dir({Path, Contents}) ->
  NewPath = filename:dirname(Path),
  PathSize = maps:get(Path, Contents),
  {NewPath, maps:update_with(NewPath, fun(V) -> V + PathSize end, PathSize, Contents)}.

-compile({inline, [push_dir/2]}).
push_dir(Target, {Path, Contents}) ->
  {filename:join(Path, Target), Contents}.

sum_final_dirs({<<"/">>, Acc}) ->
  Acc;
sum_final_dirs(Acc) ->
  sum_final_dirs(pop_dir(Acc)).

part1(Tree) ->
  Filtered = maps:filter(fun(_, V) -> V =< 100_000 end, Tree),
  lists:sum(maps:values(Filtered)).

part2(Sums) ->
  Total =  maps:get(<<"/">>, Sums, 0),
  Unused = 70_000_000 - Total,
  Required = 3_000_0000,
  ToDelete = Required - Unused,
  Candidates = maps:filter(fun(_, V) -> V > ToDelete end, Sums),
  lists:min(maps:values(Candidates)).
