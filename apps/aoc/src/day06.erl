-module(day06).

-export([solve/0]).

solve() ->
  {ok, Input} = aoc:read("day06.txt"),
  find_markers(Input).

find_markers(Input) ->
  do_find_markers(Input, 4, {0, 0}).

do_find_markers(<<_:8, Rest/binary>> = Bin, Size, {Part1, Cnt}) ->
  case is_marker(Bin, {0, Size}, #{}) of
    true when Size =:= 4 ->
      do_find_markers(Rest, 14, {_Part1 = Cnt + Size, Cnt + 1});
    true ->
      {Part1, Cnt + Size};
    false ->
      do_find_markers(Rest, Size, {Part1, Cnt + 1})
  end.

is_marker(<<A:8, _/binary>>, _, Acc) when is_map_key(A, Acc) ->
  false;
is_marker(_, {Cnt, Size}, _) when Cnt =:= Size ->
  true;
is_marker(<<A:8, Rest/binary>>, {Cnt, Size}, Acc) ->
  is_marker(Rest, {Cnt + 1, Size}, Acc#{A => A}).