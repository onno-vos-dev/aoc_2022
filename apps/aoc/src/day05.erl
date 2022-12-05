-module(day05).

-export([solve/0]).

-define(space, $\ ).
-define(is_bracket(X), (X =:= $[) orelse (X =:= $])).

solve() ->
  Input = aoc:read_file("day05.txt", <<"\n">>),
  {ok, Instructions, Stacks} = build_stacks(Input, #{}),
  perform_instructions(Instructions, {Stacks, Stacks}).

build_stacks([<<>> | Instructions], Acc) ->
  {ok, Instructions, maps:map(fun(_, V) -> binary_reverse(V) end, Acc)};
build_stacks([Line | T], Acc) ->
  build_stacks(T, do_build_stacks(Line, {1, Acc})).

do_build_stacks(<<>>, {_X, Crates}) ->
  Crates;
do_build_stacks(<<?space:8, ?space:8, ?space:8, ?space:8, Rest/binary>>, {X, Crates}) when is_binary(Rest) ->
  do_build_stacks(Rest, {X + 1, Crates});
do_build_stacks(<<LeftBracket:8, Char:8, RightBracket:8, ?space:8, Rest/binary>>, {X, Crates})
  when ?is_bracket(LeftBracket) andalso ?is_bracket(RightBracket) andalso is_binary(Rest) ->
  do_build_stacks(Rest, {X + 1, maps:update_with(X, fun(V) -> <<V/binary, Char>> end, <<Char>>, Crates)});
do_build_stacks(<<LeftBracket:8, Char:8, RightBracket:8, Rest/binary>>, {X, Crates})
  when ?is_bracket(LeftBracket) andalso ?is_bracket(RightBracket) andalso is_binary(Rest)  ->
  do_build_stacks(Rest, {X + 1, maps:update_with(X, fun(V) -> <<V/binary, Char>> end, <<Char>>, Crates)});
do_build_stacks(_, {_, Crates}) ->
  %% Skip, this will be the line containing 1...X indicating the stack numbers
  Crates.

perform_instructions([], {Acc1, Acc2}) ->
  Part1 = maps:fold(fun(_, Stack, A) -> <<V:8, _/binary>> = binary_reverse(Stack), <<A/binary, V>> end, <<"">>, Acc1),
  Part2 = maps:fold(fun(_, Stack, A) -> <<V:8, _/binary>> = binary_reverse(Stack), <<A/binary, V>> end, <<"">>, Acc2),
  {Part1, Part2};
perform_instructions([Instruction | T], Stacks) ->
  [Nr, Pos, Goal] = split_instruction(Instruction),
  NewStacks = new_stacks(Nr, Pos, Goal, Stacks),
  perform_instructions(T, NewStacks).

split_instruction(Instruction) ->
  [_, NrB, _, PosB, _, GoalB] = binary:split(Instruction, <<" ">>, [global]),
  [binary_to_integer(B) || B <- [NrB, PosB, GoalB]].

new_stacks(Nr, Pos, Goal, {Part1, Part2}) ->
  {new_stack(Nr, Pos, Goal, Part1, true), new_stack(Nr, Pos, Goal, Part2, false)}.

new_stack(Nr, Pos, Goal, Stacks, true = _Reverse) ->
  {Keep, Move} = get_crates(Nr, Pos, Stacks),
  #{Goal := Remaining} = Stacks,
  NewVal = <<Remaining/binary, (binary_reverse(Move))/binary>>,
  Stacks#{Pos => Keep, Goal => NewVal};
new_stack(Nr, Pos, Goal, Stacks, false = _Reverse) ->
  {Keep, Move} = get_crates(Nr, Pos, Stacks),
  #{Goal := Remaining} = Stacks,
  NewVal = <<Remaining/binary, Move/binary>>,
  Stacks#{Pos => Keep, Goal => NewVal}.

get_crates(Nr, Pos, Stacks) ->
  #{Pos := Bin} = Stacks,
  split_binary(Bin, byte_size(Bin) - Nr).

binary_reverse(Binary) ->
  Size = erlang:size(Binary) * 8,
  <<X:Size/integer-little>> = Binary,
  <<X:Size/integer-big>>.
