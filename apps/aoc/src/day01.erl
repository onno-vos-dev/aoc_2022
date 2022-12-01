-module(day01).

-export([ solve/0 ]).
-export([generate_large_puzzle_input/1]).

solve() ->
  TopThree = summed_calories(),
  Part1 = hd(TopThree),
  Part2 = lists:sum(TopThree),
  {Part1, Part2}.

summed_calories() ->
  aoc:read_file_fold("day01.txt",
                     <<"\n\n">>,
                     fun(B, Acc) when is_binary(B) ->
                        sort([lists:sum([binary_to_integer(X) || X <- binary:split(B, <<"\n">>, [global]), X =/= <<>> ]) | Acc])
                     end,
                     []).

sort([H1, H2, H3 |_]) when H1 > H2 ->
  [H1, H2, H3];
sort([H1, H2, H3, H4]) when H1 < H2 andalso H1 < H3 andalso H1 < H4 ->
  [H2, H3, H4];
sort([H1, H2, H3, H4]) when H1 < H2 andalso H1 < H3 andalso H1 > H4 ->
  [H2, H3, H1];
sort([H1, H2, H3 | _]) when H1 < H2 andalso H1 > H3 ->
  [H2, H1, H3];
sort([H1, H2, H3]) when H1 > H2 andalso H2 > H3 ->
  [H1, H2, H3];
sort([H1, H2, H3]) when H1 < H2 andalso H1 < H3 ->
  [H2, H3, H1];
sort([H]) ->
  [H];
sort([H1, H2]) when H1 > H2 ->
  [H1, H2];
sort([H1, H2]) when H1 < H2 ->
  [H2, H1].

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
