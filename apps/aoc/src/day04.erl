-module(day04).

-export([solve/0]).

solve() ->
  Input = aoc:read_file("day04.txt", <<"\n">>, fun split/1),
  solve(Input, {0, 0}).

solve([], Acc) ->
  Acc;
solve([[ElfAStart, ElfAEnd, ElfBStart, ElfBEnd] | T], {Part1, Part2}) ->
  OverlappingRange = is_overlapping_range([ElfAStart, ElfAEnd, ElfBStart, ElfBEnd]),
  OverlappingSection = is_overlapping_section([ElfAStart, ElfAEnd, ElfBStart, ElfBEnd]),
  case {OverlappingRange, OverlappingSection} of
    {true, true} ->
      solve(T, {Part1 + 1, Part2 + 1});
    {false, true} ->
      solve(T, {Part1, Part2 + 1});
    {true, false} ->
      solve(T, {Part1 + 1, Part2});
    {false, false} ->
      solve(T, {Part1, Part2})
  end.

is_overlapping_range([ElfAStart, ElfAEnd, ElfBStart, ElfBEnd]) ->
  ((ElfAStart >= ElfBStart) andalso (ElfAEnd =< ElfBEnd)) orelse
  ((ElfBStart >= ElfAStart) andalso (ElfBEnd =< ElfAEnd)).

is_overlapping_section([ElfAStart, ElfAEnd, ElfBStart, ElfBEnd]) ->
  (((ElfAStart >= ElfBStart) andalso (ElfAStart =< ElfBEnd)) orelse
   ((ElfAEnd >= ElfBEnd) andalso (ElfAEnd =< ElfBEnd))) orelse
  (((ElfBStart >= ElfAStart) andalso (ElfBStart =< ElfAEnd)) orelse
   ((ElfBEnd >= ElfAEnd) andalso (ElfBEnd =< ElfAEnd))).

split(Binary) when is_binary(Binary) ->
  CastF = fun binary_to_integer/1,
  [CastF(X) || X <- binary:split(Binary, [<<"-">>, <<",">>], [global])].
