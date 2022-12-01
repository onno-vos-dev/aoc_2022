-module(day01).

-export([ solve/0 ]).

solve() ->
  TopThree = summed_calories(),
  Part1 = hd(TopThree),
  Part2 = lists:sum(TopThree),
  {Part1, Part2}.

summed_calories() ->
  {LastGroup, Acc} =
    aoc:read_file_fold("day01.txt",
                       <<"\n">>,
                       fun (<<>>, {Group, Acc}) ->
                             {0, lists:sublist(lists:reverse(lists:sort([Group | Acc])), 3)};
                           (B, {Group, Acc}) ->
                             {binary_to_integer(B) + Group, Acc}
                       end,
                       {0, [0, 0, 0]}),
  [LastGroup | Acc].
