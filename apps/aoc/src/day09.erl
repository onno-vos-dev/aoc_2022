-module(day09).

-export([solve/0]).

-define(space, $\ ).

solve() ->
  Input = aoc:read_file("day09.txt", <<"\n">>),
  Instructions = parse_input(Input, []),
  P1 = part1(Instructions),
  P2 = part2(Instructions),
  {P1, P2}.

parse_input([], Acc) ->
  lists:reverse(Acc);
parse_input([<<D:8, ?space:8, Num/binary>> | T], Acc) ->
  parse_input(T, [{direction(D), binary_to_integer(Num)} | Acc]).

direction($D) -> {0, -1};
direction($U) -> {0, 1};
direction($L) -> {-1, 0};
direction($R) -> {1, 0}.

part1(Instructions) ->
  maps:size(solve(Instructions, {knots(2), #{}})).

part2(Instructions) ->
  maps:size(solve(Instructions, {knots(10), #{}})).

knots(N) ->
  lists:duplicate(N, {0, 0}).

solve([], {_, Acc}) ->
  Acc;
solve([{Direction, Distance} | T], {Knots, Acc}) ->
  {NewKnots, NewAcc} = step(Direction, Distance, Knots, Acc),
  solve(T, {NewKnots, NewAcc}).

step(_Direction, 0, Knots, Acc) ->
  {Knots, Acc};
step({DeltaX, DeltaY}, Distance, [Head | Tails], Acc) ->
  NewHead = move(Head, {DeltaX, DeltaY}),
  NewKnots = [NewHead | move_tails(NewHead, Tails, [])],
  step({DeltaX, DeltaY}, Distance - 1, NewKnots, Acc#{lists:last(NewKnots) => 1}).

move_tails(_, [], NewTails) ->
  lists:reverse(NewTails);
move_tails(Head, [Tail | OtherTails], NewTails) ->
  NewTail = move(Tail, move_tail_by(Head, Tail)),
  move_tails(NewTail, OtherTails, [NewTail | NewTails]).

-compile({inline, [move/2]}).
move({X, Y}, {Xd, Yd}) ->
  {X + Xd, Y + Yd}.

-compile({inline, [move_tail_by/2]}).
move_tail_by({HX, HY}, {TX, TY}) when (abs(HX - TX) =< 1) andalso (abs(HY - TY) =< 1) ->
  {0, 0};
move_tail_by({HX, HY}, {TX, TY}) ->
  {sign(HX - TX), sign(HY - TY)}.

-compile({inline, [sign/1]}).
sign(Int) when Int < 0   -> -1;
sign(Int) when Int =:= 0 -> 0;
sign(Int) when Int > 0   -> 1.
