-module(store).

-export([ new/3
        , store/4
        , lookup/3
        , destroy/2
        ]).

-type store_type() :: ets | map | process_dictionary | counters | list | dict.
-type store() :: any().

-spec new(store_type(), Name :: atom(), Size :: integer()) -> {store_type(), store()}.
new(list, _, _) -> {list, []};
new(map, _, _) -> {map, #{}};
new(process_dictionary, _, _) -> {process_dictionary, ignore};
new(counters, _, Size) -> {counters, counters:new(Size, [])};
new(ets, Name, _) -> {ets, ets:new(Name, [set])};
new(dict, _, _) -> {dict, dict:new()}.

-spec store(store_type(), pos_integer(), pos_integer(), store()) -> store().
store(list, Num, Turns, Store) ->
  lists:keystore(Num, 1, Store, {Num, Turns});
store(map, Num, Turns, Store) ->
  maps:put(Num, Turns, Store);
store(ets, Num, Turns, Store) ->
  true = ets:insert(Store, {Num, Turns}),
  Store;
store(process_dictionary, Num, Turns, _Store) ->
  put(Num, Turns);
store(counters, Num, Turns, Store) ->
  ok = counters:put(Store, Num + 1, Turns),
  Store;
store(dict, Num, Turns, Store) ->
  dict:store(Num, Turns, Store).

-spec lookup(store_type(), pos_integer(), store()) -> pos_integer().
lookup(list, Num, Store) ->
  case lists:keyfind(Num, 1, Store) of
    false -> [];
    {_Key, Value} -> Value
  end;
lookup(map, Num, Store) ->
  maps:get(Num, Store, []);
lookup(ets, Num, Store) ->
  case ets:lookup(Store, Num) of
    [] -> [];
    [{_Key, Value}] -> Value
  end;
lookup(process_dictionary, Num, _Store) ->
  case get(Num) of
    undefined -> [];
    Value -> Value
  end;
lookup(counters, Num, Store) ->
  case counters:get(Store, Num + 1) of
    0 -> [];
    Value -> Value
  end;
lookup(dict, Num, Store) ->
  case dict:find(Num, Store) of
    error -> [];
    {ok, Value} -> Value
  end.

-spec destroy(store_type(), store()) -> ok.
destroy(list, _Store) -> ok;
destroy(map, _Store) -> ok;
destroy(process_dictionary, _Store) ->
  erase(),
  ok;
destroy(ets, Store) ->
  ets:delete(Store),
  ok;
destroy(counters, _Store) -> ok;
destroy(dict, _Store) -> ok.
