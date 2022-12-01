-module(day01).

-export([ solve/0 ]).

solve() ->
  Input =
    aoc:read_file("day01.txt",
                  <<"\n">>,
                  fun (<<>>) ->
                        new_group;
                      (B) ->
                        binary_to_integer(B)
                  end),
  SummedCalories = sum_calories(Input),
  Part1 = lists:max(SummedCalories),
  Part2 = lists:sum(lists:sublist(lists:reverse(lists:sort(sum_calories(Input))), 3)),
  {Part1, Part2}.

sum_calories(Input) ->
  sum_calories(Input, {0, []}).

sum_calories([], {Group, Acc}) ->
  [Group | Acc];
sum_calories([new_group | T], {Group, Acc}) ->
  sum_calories(T, {0, [Group | Acc]});
sum_calories([H | T], {Group, Acc}) ->
  sum_calories(T, {H + Group, Acc}).

