-module(day07).

-export([solve/0]).

-define(REGEX, {re_pattern,0,0,0, <<69,82,67,80,105,0,0,0,0,0,0,0,1,0,0,0,255,255,255,255,
                                    255,255,255,255,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,131,0,37,
                                    110,0,0,0,0,0,0,255,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                    0,0,0,0,0,0,0,0,107,120,0,37,0>>}).

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
push_contents([<<"dir ", Dir/binary>> | T], Acc) ->
  push_contents(T, new_dir(Dir, Acc));
push_contents([File | T], Acc) ->
  {match, [{_StartSize, EndSize}]} = re:run(File, ?REGEX),
  {Size, Name} = split_binary(File, EndSize),
  push_contents(T, add_file_size(Name, Size, Acc)).

-compile({inline, [new_tree/1]}).
new_tree(Path) ->
  {Path, #{Path => 0}}.

-compile({inline, [new_dir/2]}).
new_dir(Dir, {Path, Contents}) ->
  {Path, maps:put(filename:join(Path, Dir), 0, Contents)}.

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
  Filtered = maps:filter(fun(_, V) -> V =< 100_000 andalso V =/= 0 end, Tree),
  lists:sum(maps:values(Filtered)).

part2(Sums) ->
  Total =  maps:get(<<"/">>, Sums, 0),
  Unused = 70_000_000 - Total,
  Required = 3_000_0000,
  ToDelete = Required - Unused,
  Candidates = maps:filter(fun(_, V) -> V > ToDelete andalso V =/= 0 end, Sums),
  lists:min(maps:values(Candidates)).
