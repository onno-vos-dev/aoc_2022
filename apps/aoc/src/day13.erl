-module(day13).

-export([solve/0]).

solve() ->
  Input0 =
    aoc:read_file_fold("day13.txt", <<"\n">>,
                       fun (<<>>, Acc) ->
                             Acc;
                           (Line, Acc) ->
                             [bin_to_list(Line) | Acc]
                       end,
                       []),
  Input = lists:reverse(Input0),
  Part1 = indexes_with_sorted_pairs(Input, {1, []}),
  SortedPairs = lists:sort(fun is_sorted/2, Input ++ [[[2]], [[6]]]),
  {lists:sum(Part1), locate_dividers(SortedPairs, {1, 1})}.

bin_to_list(Bin) ->
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin) ++ "."),
  {ok, List} = erl_parse:parse_term(Tokens),
  List.

indexes_with_sorted_pairs([], {_, Acc}) -> Acc;
indexes_with_sorted_pairs([L1, L2 | T], {N, Acc}) ->
  case is_sorted(L1, L2) of
    true ->
      indexes_with_sorted_pairs(T, {N + 1, [N | Acc]});
    false ->
      indexes_with_sorted_pairs(T, {N + 1, Acc})
  end.

is_sorted(L, R) when is_integer(L) andalso is_list(R) ->
  is_sorted([L], R);
is_sorted(L, R) when is_list(L) andalso is_integer(R) ->
  is_sorted(L, [R]);
is_sorted(L, R) when L =:= R -> %% Irrelevant whether or not they're equal integers or equal lists
  equal;
is_sorted(L, R) when is_integer(L) andalso is_integer(R) andalso L =:= R ->
  equal;
is_sorted(L, R) when is_integer(L) andalso is_integer(R) andalso L < R ->
  true;
is_sorted(L, R) when is_integer(L) andalso is_integer(R) ->
  false;
is_sorted(L, []) when is_list(L) ->
  false;
is_sorted([], R) when is_list(R) ->
  true;
is_sorted([L | LT], [R | RT]) ->
  case is_sorted(L, R) of
    true ->
      true;
    equal ->
      is_sorted(LT, RT);
    false ->
      false
  end.

locate_dividers([H | T], {N, _}) when H =:= [[2]] ->
  locate_dividers(T, {N + 1, N});
locate_dividers([H | _], {N, I}) when H =:= [[6]] ->
  I * N;
locate_dividers([_ | T], {N, I}) ->
  locate_dividers(T, {N + 1, I}).
