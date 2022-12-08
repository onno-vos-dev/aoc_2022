-module(day08).

-export([solve/0]).

solve() ->
  Input = aoc:read_file("day08.txt", <<"\n">>),
  MaxX = MaxY = byte_size(hd(Input)),
  XRange = lists:seq(1, MaxX),
  YRange = lists:seq(1, MaxY),
  {ParseT, {GridL, GridM}} = timer:tc(fun() -> grid(XRange, YRange, MaxX, MaxY, Input, {1, []}) end),
  io:format("ParseT: ~p (~p ms)~n", [ParseT, ParseT div 1000]),
  {T1, Part1} = timer:tc(fun() -> part1({MaxX, MaxY}, GridM, GridL) end),
  {T2, Part2} = timer:tc(fun() -> part2(GridM, GridL) end),
  io:format("T1: ~p (~p ms)~n", [T1, T1 div 1000]),
  io:format("T2: ~p (~p ms)~n", [T2, T2 div 1000]),
  {1713,268464} = {Part1, Part2}.

grid(_XRange, _YRange, _MaxX, _MaxY, [], {_, Acc}) -> {Acc, maps:from_list(Acc)};
grid(XRange, YRange, MaxX, MaxY, [H | T], {Y, Acc}) ->
  {_, NewAcc} = parse_line(XRange, YRange, MaxX, MaxY, H, {{1, Y}, Acc}),
  grid(XRange, YRange, MaxX, MaxY, T, {Y + 1, NewAcc}).

parse_line(_XRange, _YRange, _MaxX, _MaxY, <<>>, Acc) -> Acc;
parse_line(XRange, YRange, MaxX, MaxY, <<H:8, Rest/binary>>, {{X, Y}, Acc}) ->
  parse_line(XRange, YRange, MaxX, MaxY, Rest, {{X + 1, Y}, [make_coordinate(XRange, YRange, MaxX, MaxY, X, Y, H) | Acc]}).

part1({MaxX, MaxY}, GridM, GridL) ->
  length(lists:filter(fun(Tree) -> is_visible({MaxX, MaxY}, Tree, GridM) end, GridL)).

is_visible(_, {_, {Tree, Left, Right, Up, Down}}, GridMap) ->
  do_is_visible([Left, Right, Up, Down], Tree, GridMap).

do_is_visible([], _Tree, _GridMap) ->
  false;
do_is_visible([[] | _], _Tree, _GridMap) ->
  true;
do_is_visible([Coordinates | T], Tree, GridMap) ->
  AllSmaller =
    lists:all(fun(Coordinate) -> element(1, maps:get(Coordinate, GridMap)) < Tree end, Coordinates),
  case AllSmaller of
    true ->
      true;
    false ->
      do_is_visible(T, Tree, GridMap)
  end.

part2(GridM, GridL) ->
  Scores = lists:map(fun(Tree) -> scenic_score(Tree, GridM) end, GridL),
  lists:max(Scores).

scenic_score({_, {Tree, Left, Right, Up, Down}}, GridMap) ->
  do_scenic_score([lists:reverse(Left), Right, lists:reverse(Up), Down], Tree, GridMap, []).

do_scenic_score([], _Tree, _GridMap, [A, B, C, D]) ->
  lists:foldl(fun(X, Acc) when X =/= 0 -> Acc * X; (_, Acc) -> Acc end, 1, [A,B,C,D]),
  A * B * C * D;
do_scenic_score([Coordinates | T], Tree, GridMap, ScenicScore) ->
  TallerTrees = foldwhile(Coordinates, fun(Coordinate) -> element(1, maps:get(Coordinate, GridMap)) < Tree end, 0),
  do_scenic_score(T, Tree, GridMap, [TallerTrees | ScenicScore]).

foldwhile([], _, Acc) -> Acc;
foldwhile([H | T], F, Acc) ->
  case F(H) of
    true -> foldwhile(T, F, Acc + 1);
    false -> Acc + 1
  end.

make_coordinate(XRange, YRange, MaxX, MaxY, X, Y, H) ->
  Xs = [X_ || X_ <- XRange, (X =/= 1) orelse (X =/= MaxX)],
  Ys = [Y_ || Y_ <- YRange, (Y =/= 1) orelse (Y =/= MaxY)],
  Left = [{X__, Y} || X__ <- Xs, X__ < X],
  Right = [{X__, Y} || X__ <- Xs, X__ > X],
  Up = [{X, Y__} || Y__ <- Ys, Y__ < Y],
  Down = [{X, Y__} || Y__ <- Ys, Y__ > Y],
  {{X, Y}, {H - $0, Left, Right, Up, Down}}.
