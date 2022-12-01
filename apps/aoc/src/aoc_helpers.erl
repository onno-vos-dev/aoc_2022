-module(aoc_helpers).

-export([ count_increases/1
        , count_decreases/1
        , sum_sliding/2
        , median/1
        , mean/1
        , median_nif/1
        , mean_nif/1
        , binary_to_decimal/1
        , split_to_chunks/2
        , sort_ints/1
        , pmap/2
        ]).

-on_load(init/0).

-define(APPNAME, aoc).
-define(LIBNAME, "libaoc").

-dialyzer({nowarn_function, [ mean_nif/1
                            , median_nif/1
                            , sort_ints/1
                            , load_mean_nif/1
                            , load_median_nif/1
                            , load_sort_ints_nif/1
                            ]}).

%% API ========================================================================
%% Credit where credit is due: https://gist.github.com/nicklasos/c177478b972e74872b3b
pmap(F, L) ->
  S = self(),
  Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
  pmap_gather(Pids).

pmap_gather([H|T]) ->
  receive
    {H, Ret} -> [Ret|pmap_gather(T)]
  end;
pmap_gather([]) ->
  [].

pmap_f(Parent, F, I) ->
  Parent ! {self(), (catch F(I))}.

median(Unsorted) ->
  Sorted = lists:sort(Unsorted),
  Length = length(Sorted),
  Mid = Length div 2,
  Rem = Length rem 2,
  (lists:nth(Mid + Rem, Sorted) + lists:nth(Mid + 1, Sorted)) / 2.

mean(L) -> lists:sum(L) / length(L).

-spec binary_to_decimal(binary()) -> integer().
binary_to_decimal(Binary) ->
  binary_to_integer(Binary, 2).

-spec split_to_chunks([any()], pos_integer()) -> [[any()], ...].
split_to_chunks(L, N) when is_integer(N), N > 0 ->
  split_to_chunks(N, 0, L, []).

split_to_chunks(_, _, [], Acc) ->
  [Acc];
split_to_chunks(N, N, L, Acc) ->
  [Acc | split_to_chunks(N, 0, L, [])];
split_to_chunks(N, X, [H|T], Acc) ->
  split_to_chunks(N, X + 1, T, [H|Acc]).

%% @doc
%% Given a list a of integers in random order
%% count the amount of times a subsequent integer is an increase of the previous.
%% @end
-spec count_increases([integer()]) -> integer().
count_increases(Input) ->
  count_change(tl(Input), fun(A, B) -> A > B end, {hd(Input), 0}).

%% @doc
%% Given a list a of integers in random order
%% count the amount of times a subsequent integer is an increase of the previous.
%% @end
-spec count_decreases([integer()]) -> integer().
count_decreases(Input) ->
  count_change(tl(Input), fun(A, B) -> A < B end, {hd(Input), 0}).

count_change([], _F, {_Last, Acc}) -> Acc;
count_change([H | T], F, {Last, Acc}) ->
  case F(H, Last) of
    true ->
      count_change(T, F, {H, Acc + 1});
    false ->
      count_change(T, F, {H, Acc})
  end.

%% @doc
%% Given a list of integers in random order
%% sum in sets of N.
%% @end
-spec sum_sliding([integer()], integer()) -> [integer()].
sum_sliding(Input, N) ->
  sum_sliding(Input, N, []).

sum_sliding(L, N, Acc) ->
  {L1, L2} = lists:split(N, L),
  NewL = tl(L1) ++ L2,
  case length(NewL) >= N of
    true ->
      sum_sliding(NewL, N, [lists:sum(L1) | Acc]);
    false ->
      lists:reverse([lists:sum(L1) | Acc])
  end.

sort_ints(A) -> load_sort_ints_nif(A).

mean_nif(A) -> load_mean_nif(A).

median_nif(A) -> load_median_nif(A).

%%%_* NIFS ====================================================================
load_sort_ints_nif(_) ->
  not_loaded(?LINE).

load_mean_nif(_) ->
  not_loaded(?LINE).

load_median_nif(_) ->
  not_loaded(?LINE).

init() ->
  SoName = case code:priv_dir(?APPNAME) of
              {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                  true ->
                    filename:join(["..", priv, ?LIBNAME]);
                  _ ->
                    filename:join([priv, ?LIBNAME])
                end;
              Dir ->
                filename:join(Dir, ?LIBNAME)
            end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
