-module(day02).

-export([solve/0]).

solve() ->
  {ok, Bin} = aoc:read("day02.txt"),
  play(Bin, {0, 0}).

play(<<>>, {P1, P2}) ->
  {P1, P2};
play(<<$A:8, _:8, $X:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 4, P2 + 3});
play(<<$A:8, _:8, $Y:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 8, P2 + 4});
play(<<$A:8, _:8, $Z:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 3, P2 + 8});
play(<<$B:8, _:8, $X:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 1, P2 + 1});
play(<<$B:8, _:8, $Y:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 5, P2 + 5});
play(<<$B:8, _:8, $Z:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 9, P2 + 9});
play(<<$C:8, _:8, $X:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 7, P2 + 2});
play(<<$C:8, _:8, $Y:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 2, P2 + 6});
play(<<$C:8, _:8, $Z:8, _:8, Rest/binary>>, {P1, P2}) when is_binary(Rest) ->
  play(Rest, {P1 + 6, P2 + 7}).
