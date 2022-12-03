-module(day03).

-export([solve/0]).

solve() ->
  Input = aoc:read_file("day03.txt", <<"\n">>, fun binary_to_list/1),
  {part1(Input, 0), part2(Input, 0)}.

part1([], Acc) -> Acc;
part1([Str | T], Acc) ->
  {SubStr1, SubStr2} = lists:split(length(Str) div 2, Str),
  part1(T, score(common_letters(SubStr1, SubStr2)) + Acc).

part2([], Acc) -> Acc;
part2([Str1, Str2, Str3 | T], Acc) ->
  part2(T, score(common_letters(Str3, common_letters(Str1, Str2))) + Acc).

common_letters(Str1, Str2) ->
  Str2 -- (Str2 -- Str1).

score([H | _]) when H >= 97 ->
  H - 96;
score([H | _]) ->
  (H - 64) + 26.
