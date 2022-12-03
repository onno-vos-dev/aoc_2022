-module(day03).

-export([solve/0]).

solve() ->
  Input = aoc:read_file("day03.txt", <<"\n">>),
  {part1(Input, 0), part2(Input, 0)}.

part1([], Acc) -> Acc;
part1([Str | T], Acc) ->
  {SubStr1, SubStr2} = erlang:split_binary(Str, byte_size(Str) div 2),
  part1(T, score(common_letters(SubStr1, SubStr2)) + Acc).

part2([], Acc) -> Acc;
part2([Str1, Str2, Str3 | T], Acc) ->
  part2(T, score(common_letters(Str3, common_letters(Str1, Str2))) + Acc).

common_letters(Str1, Str2) when is_binary(Str1) andalso is_binary(Str2) ->
  common_letters(binary_to_list(Str1), binary_to_list(Str2));
common_letters(Str1, Str2) when is_binary(Str1) ->
  common_letters(binary_to_list(Str1), Str2);
common_letters(Str1, Str2) ->
  Str2 -- (Str2 -- Str1).

score([H | _]) when H >= 97 ->
  H - 96;
score([H | _]) ->
  (H - 64) + 26.
