-module(day02).

-export([solve/0]).

solve() ->
  Input = aoc:read_file("day02.txt", <<"\n">>),
  lists:foldl(fun(X, {Part1, Part2}) ->
               {P1, P2} = play(X),
               {P1 + Part1, P2 + Part2}
            end,
            {0, 0},
            Input).

play(<<"A X">> = _RockB) -> {1 + 3, 3 + 0};
play(<<"A Y">> = _PaperB) -> {2 + 6, 1 + 3};
play(<<"A Z">> = _ScissorB) -> {3 + 0, 2 + 6};
play(<<"B X">> = _RockB) -> {1 + 0, 1 + 0};
play(<<"B Y">> = _PaperB) -> {2 + 3, 2 + 3};
play(<<"B Z">> = _ScisorB) -> {3 + 6, 3 + 6};
play(<<"C X">> = _RockB) -> {1 + 6, 2 + 0};
play(<<"C Y">> = _PaperB) -> {2 + 0, 3 + 3};
play(<<"C Z">> = _ScissorB) -> {3 + 3, 1 + 6}.
