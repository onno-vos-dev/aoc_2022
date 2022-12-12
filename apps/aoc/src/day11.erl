-module(day11).

-export([solve/0]).

-record(monkey, {num, operation, test, when_pass, when_fail}).

-record(state, {items = #{}, monkeys = [], inspected_items = #{}}).

solve() ->
  Input = aoc:read_file("day11.txt", <<"\n">>),
  State = parse(Input, {#monkey{num = 0}, #state{}}),
  {part1(State), part2(State)}.

parse([], {Monkey, State}) ->
  Num = Monkey#monkey.num,
  Monkeys = State#state.monkeys,
  InspectedItems = State#state.inspected_items,
  State#state{monkeys = lists:reverse([Monkey | Monkeys]),
              inspected_items = InspectedItems#{Num => 0}};
parse([<<>> | T], {Monkey, State}) ->
  Num = Monkey#monkey.num,
  Monkeys = State#state.monkeys,
  InspectedItems = State#state.inspected_items,
  NewInspectedItems = InspectedItems#{Num => 0},
  NewMonkeys = [Monkey | Monkeys],
  NewState = State#state{monkeys = NewMonkeys, inspected_items = NewInspectedItems},
  parse(T, {#monkey{num = Num + 1}, NewState});
parse([<<"Monkey ", _/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey, State});
parse([<<"  Starting items: ", ItemsB/binary>> | T], {Monkey, State}) ->
  Num = Monkey#monkey.num,
  Items = State#state.items,
  MonkeyItems = [binary_to_integer(X) || X <- binary:split(ItemsB, <<", ">>, [global, trim])],
  parse(T, {Monkey, State#state{items = Items#{Num => MonkeyItems}}});
parse([<<"  Operation: new = old ", OpValue/binary>> | T], {Monkey, State}) ->
  [OperandB, ValueB] = binary:split(OpValue, <<" ">>, [trim]),
  Fun =
    case {OperandB, ValueB} of
      {<<"*">>, <<"old">>} ->
        fun(Item) -> Item * Item end;
      {<<"+">>, <<"old">>} ->
        fun(Item) -> Item + Item end;
      {<<"*">>, _} ->
        Value = binary_to_integer(ValueB),
        fun(Item) -> Item * Value end;
      {<<"+">>, _} ->
        Value = binary_to_integer(ValueB),
        fun(Item) -> Item + Value end
    end,
  parse(T, {Monkey#monkey{operation = Fun}, State});
parse([<<"  Test: divisible by ", Value/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey#monkey{test = binary_to_integer(Value)}, State});
parse([<<"    If true: throw to monkey ", ToMonkey/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey#monkey{when_pass = binary_to_integer(ToMonkey)}, State});
parse([<<"    If false: throw to monkey ", ToMonkey/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey#monkey{when_fail = binary_to_integer(ToMonkey)}, State}).

part1(State) ->
  play(20, fun(Item) -> Item div 3 end, State).

part2(State) ->
  LCM = lcm(State#state.monkeys),
  play(10000, fun(Item) -> Item rem LCM end, State).

lcm(Monkeys) ->
  lcm(Monkeys, 1).

lcm([], B) ->
  B;
lcm([#monkey{test = A} | T], B) ->
  lcm(T, abs((A * B) div gcd(A, B))).

gcd(A, 0) ->
  A;

gcd(A, B) ->
  gcd(B, A rem B).

play(0, _, State) ->
  [Active1, Active2 | _] =
    lists:reverse(
      lists:sort(
        maps:values(State#state.inspected_items))),
  Active1 * Active2;
play(RoundsLeft, F, State) ->
  NewState =
    lists:foldl(fun(#monkey{num = Num} = Monkey, #state{items = Items} = S) ->
                   case Items of
                     #{Num := []} ->
                       S;
                     #{Num := MonkeyItems} ->
                       inspect_items(Monkey, MonkeyItems, F, S)
                   end
                end,
                State,
                State#state.monkeys),
  play(RoundsLeft - 1, F, NewState).

inspect_items(#monkey{num = Num,
                      operation = Fun,
                      test = Test,
                      when_pass = TestPass,
                      when_fail = TestFail},
              MonkeyItems,
              F,
              #state{items = Items, inspected_items = InspectedItems} = State) ->
  #{TestPass := TestPassItems, TestFail := TestFailItems} = Items,
  {Pass, Fail} =
    lists:foldl(fun(Item, {MonkeyPass, MonkeyFail}) ->
                   WorryLevel = F(Fun(Item)),
                   case WorryLevel rem Test of
                     0 ->
                       {[WorryLevel | MonkeyPass], MonkeyFail};
                     _ ->
                       {MonkeyPass, [WorryLevel | MonkeyFail]}
                   end
                end,
                {TestPassItems, TestFailItems},
                MonkeyItems),
  #{Num := CurrentN} = InspectedItems,
  State#state{items = Items#{Num := [],
                             TestPass := Pass,
                             TestFail := Fail},
              inspected_items = InspectedItems#{Num := CurrentN + length(MonkeyItems)}}.
