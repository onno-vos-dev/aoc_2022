-module(day08).

-export([solve/0]).
-compile([export_all]).

solve() ->
  Input = aoc:read_file("day08.txt", <<"\n">>),
  {XMap, YMap, Grid} = grid(Input, {1, {#{}, #{}, #{}}}),
  %% io:format("XMap: ~p~n", [XMap]),
  %% io:format("YMap: ~p~n", [YMap]),
  {Tpart1, Part1} = timer:tc(fun() -> part1(XMap, YMap, Grid) end),
  {Tpart2, Part2} = timer:tc(fun() -> part2(XMap, YMap, Grid) end),
  io:format("Tpart1: ~p (~p ms)~n", [Tpart1, Tpart1 div 1000]),
  io:format("Tpart2: ~p (~p ms)~n", [Tpart2, Tpart2 div 1000]),
  {Part1, Part2}.

grid([], {_, Acc}) -> Acc;
grid([Line | T], {Y, Acc}) ->
  NewAcc = parse_line(Line, {{_X = 1, Y}, Acc}),
  grid(T, {Y + 1, NewAcc}).

parse_line(<<>>, {_, {XMap, YMap, Grid}}) ->
  {XMap, YMap, Grid};
parse_line(<<TreeB:8, OtherTrees/binary>>, {{X, Y}, {XMap, YMap, Grid}}) ->
  Tree = TreeB - $0,
  NewXMap = maps:update_with(X, fun(V) -> [Tree | V] end, [Tree], XMap),
  NewYMap = maps:update_with(Y, fun(V) -> [Tree | V] end, [Tree], YMap),
  NewGrid = Grid#{{X, Y} => Tree},
  parse_line(OtherTrees, {{X + 1, Y}, {NewXMap, NewYMap, NewGrid}}).

part1(XMap, YMap, Grid) ->
  maps:size(maps:filter(fun(K, V) -> is_visible(XMap, YMap, {K, V}) end, Grid)).

is_visible(_, _, {{X,Y}, _}) when X =:= 1 orelse Y =:= 1 orelse X =:= 99 orelse Y =:= 99 ->
  true;
is_visible(XMap, YMap, {{X, Y}, Tree}) ->
  {Up, Down} = split_directions(Y, maps:get(X, XMap)),
  {Left, Right} = split_directions(X, maps:get(Y, YMap)),
  do_is_visible([Left, Right, Up, Down], Tree).

split_directions(Skip, List) ->
  split_directions(Skip, List, {99, {[], []}}).

split_directions(_, [], {_, {L1, L2}}) -> {L1, L2};
split_directions(Skip, [Tree | T], {N, {L1, L2}}) ->
  case {N > Skip, N < Skip} of
    {true, false} ->
      split_directions(Skip, T, {N - 1, {L1, [Tree | L2]}});
    {false, true} ->
      split_directions(Skip, T, {N - 1, {[Tree | L1], L2}});
    _ ->
      split_directions(Skip, T, {N - 1, {L1, L2}})
  end.

do_is_visible([], _Tree) ->
  false;
do_is_visible([[] | _], _Tree) ->
  true;
do_is_visible([Trees | T], Tree) ->
  AllSmaller = lists:max(Trees) < Tree, %% lists:all(fun(T__) -> T__ < Tree end, Trees),
  case AllSmaller of
    true ->
      true;
    false ->
      do_is_visible(T, Tree)
  end.

part2(XMap, YMap, Grid) ->
  Scores = maps:fold(fun(K, V, A) -> [scenic_score(XMap, YMap, {K, V})|A] end, [], Grid),
  lists:max(Scores).

scenic_score(XMap, YMap, {{X,Y}, Tree}) ->
  {Up, Down} = split_directions(Y, maps:get(X, XMap)),
  {Left, Right} = split_directions(X, maps:get(Y, YMap)),
  do_scenic_score([lists:reverse(Left), Right, lists:reverse(Up), Down], Tree, []).

do_scenic_score([], _Tree, [A, B, C, D]) ->
  lists:foldl(fun (X, Acc) when X =/= 0 ->
                    Acc * X;
                  (_, Acc) ->
                    Acc
              end,
              1,
              [A, B, C, D]),
  ((A * B) * C) * D;
do_scenic_score([Trees | T], Tree, ScenicScore) ->
  TallerTrees = foldwhile(Trees, fun(T__) -> T__ < Tree end, 0),
  do_scenic_score(T, Tree, [TallerTrees | ScenicScore]).

foldwhile([], _, Acc) ->
  Acc;
foldwhile([H | T], F, Acc) ->
  case F(H) of
    true ->
      foldwhile(T, F, Acc + 1);
    false ->
      Acc + 1
  end.
