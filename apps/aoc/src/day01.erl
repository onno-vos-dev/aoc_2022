-module(day01).

-export([ solve/0 ]).
-export([generate_large_puzzle_input/1]).

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
  lists:sublist(lists:reverse(lists:sort([LastGroup | Acc])), 3).

generate_large_puzzle_input(NumberOfElves) ->
  {ok, FD} = file:open("/tmp/aoc_2022_day01_large_input.txt", [write]),
  ok = do_generate_large_puzzle_input(FD, NumberOfElves),
  file:close(FD).

do_generate_large_puzzle_input(_FD, 0) ->
  ok;
do_generate_large_puzzle_input(FD, NumberOfElves) ->
  NumberOfInts = rand:uniform(25),
  lists:foreach(fun(_) -> file:write(FD, io_lib:format("~p~n", [rand:uniform(10_000_000)]))
                end,
                lists:seq(1, NumberOfInts)),
  file:write(FD, <<"\n">>),
  do_generate_large_puzzle_input(FD, NumberOfElves - 1).
