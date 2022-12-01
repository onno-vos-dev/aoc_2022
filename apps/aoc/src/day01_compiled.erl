-module(day01_compiled).

-compile({parse_transform, ct_expand}).

-export([ solve/0 ]).

%% Exported as otherwise the function is marked as unused
%% by the compiler since it's actual use is compiled away...
-export([ do_solve/0 ]).

solve() ->
  ct_expand:term(do_solve()).

do_solve() ->
  TopThree = summed_calories(),
  Part1 = hd(TopThree),
  Part2 = lists:sum(TopThree),
  {Part1, Part2}.

summed_calories() ->
  aoc:read_file_fold("day01.txt",
                     <<"\n\n">>,
                     fun(B, Acc) when is_binary(B) ->
                        sort([lists:sum([binary_to_integer(X)
                                         || X <- binary:split(B, <<"\n">>, [global]), X =/= <<>>])
                              | Acc])
                     end,
                     [0, 0, 0]).

sort([Curr, H1, H2, H3]) ->
  if Curr > H1 ->
       [Curr, H1, H2];
     Curr > H2 ->
       [H1, Curr, H2];
     Curr > H3 ->
       [H1, H2, Curr];
     true ->
       [H1, H2, H3]
  end.
