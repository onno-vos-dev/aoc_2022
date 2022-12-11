-module(day11).

-export([solve/0]).

-record(monkey, {num, operation, test, when_pass, when_fail}).

-record(state, {items = array:new(), monkeys = [], inspected_items = array:new()}).

solve() ->
  Input = aoc:read_file("day11.txt", <<"\n">>),
  State = parse(Input, {#monkey{num = 0}, #state{}}),
  Part1 = part1(State),
  Part2 = part2(State),
  {Part1, Part2}.

part1(State) ->
  play(20, fun(Item) -> Item div 3 end, State).

part2(State) ->
  Divisor = lists:foldl(fun(#monkey{test = Div}, Acc) ->
                        Acc * Div
                    end, 1, State#state.monkeys),
  play(10000, fun(Item) -> Item rem Divisor end, State).

parse([], {Monkey, State}) ->
  Num = Monkey#monkey.num,
  Monkeys = State#state.monkeys,
  InspectedItems = State#state.inspected_items,
  State#state{monkeys = lists:reverse([Monkey | Monkeys]),
              inspected_items = array:set(Num, 0, InspectedItems)};
parse([<<>> | T], {Monkey, State}) ->
  Num = Monkey#monkey.num,
  Monkeys = State#state.monkeys,
  InspectedItems = State#state.inspected_items,
  NewInspectedItems = array:set(Num, 0, InspectedItems),
  NewMonkeys = [Monkey | Monkeys],
  NewState = State#state{monkeys = NewMonkeys, inspected_items = NewInspectedItems},
  parse(T, {#monkey{num = Num + 1}, NewState});
parse([<<"Monkey ", _/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey, State});
parse([<<"  Starting items: ", ItemsB/binary>> | T], {Monkey, State}) ->
  Num = Monkey#monkey.num,
  Items = State#state.items,
  MonkeyItems = [binary_to_integer(X) || X <- binary:split(ItemsB, <<", ">>, [global, trim])],
  parse(T, {Monkey, State#state{items = array:set(Num, MonkeyItems, Items)}});
parse([<<"  Operation: new = old ", OpValue/binary>> | T], {Monkey, State}) ->
  [OperandB, ValueB] = binary:split(OpValue, <<" ">>, [trim]),
  Operand = binary_to_atom(OperandB, utf8),
  Fun =
    case ValueB of
      <<"old">> ->
        fun(Item) -> erlang:apply(erlang, Operand, [Item, Item]) end;
      _ ->
        fun(Item) -> erlang:apply(erlang, Operand, [Item, binary_to_integer(ValueB)]) end
    end,
  parse(T, {Monkey#monkey{operation = Fun}, State});
parse([<<"  Test: divisible by ", Value/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey#monkey{test = binary_to_integer(Value)}, State});
parse([<<"    If true: throw to monkey ", ToMonkey/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey#monkey{when_pass = binary_to_integer(ToMonkey)}, State});
parse([<<"    If false: throw to monkey ", ToMonkey/binary>> | T], {Monkey, State}) ->
  parse(T, {Monkey#monkey{when_fail = binary_to_integer(ToMonkey)}, State}).

play(0, _, State) ->
  [Active1, Active2 | _] =
    lists:reverse(
      lists:sort(
        array:to_list(State#state.inspected_items))),
  Active1 * Active2;
play(RoundsLeft, F, State) ->
  NewState =
    lists:foldl(fun(Monkey, S) -> inspect_items(Monkey, F, S) end,
                State,
                State#state.monkeys),
  play(RoundsLeft - 1, F, NewState).

inspect_items(#monkey{num = Num,
                      operation = Fun,
                      test = Test,
                      when_pass = TestPass,
                      when_fail = TestFail},
              F,
              #state{items = Items, inspected_items = InspectedItems} = State) ->
  MonkeyItems = array:get(Num, Items),
  NewItems = array:set(Num, [], Items),
  NewInspectedItems = array:set(Num, array:get(Num, InspectedItems) + length(MonkeyItems), InspectedItems),
  NewState = State#state{ items = NewItems, inspected_items = NewInspectedItems},
  lists:foldl(fun(Item, #state{items = SI} = S) ->
                 WorryLevel = F(Fun(Item)),
                 case WorryLevel rem Test of
                   0 ->
                     S#state{items = array:set(TestPass, [WorryLevel | array:get(TestPass, SI)], SI)};
                   _ ->
                     S#state{items = array:set(TestFail, [WorryLevel | array:get(TestFail, SI)], SI)}
                 end
              end,
              NewState,
              MonkeyItems).
