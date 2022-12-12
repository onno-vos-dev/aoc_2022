-module(util).

-export([ time_avg/2
        , time_avg/4
        ]).

%% API ========================================================================
-spec time_avg(fun(() -> any()), non_neg_integer()) -> ok.
time_avg(Fun, N) when is_function(Fun, 0) andalso is_integer(N) ->
  do_time_avg(Fun, N).

-spec time_avg(fun(() -> any()), non_neg_integer(), [any()], fun((any()) -> ok)) -> ok.
time_avg(Fun, N, Configs, ConfigFun) ->
  lists:foreach(fun(Config) ->
                  io:format("Timing config: ~p~n", [Config]),
                  ConfigFun(Config),
                  do_time_avg(Fun, N)
                end, Configs).

-spec do_time_avg(fun(() -> any()), non_neg_integer()) -> ok.
do_time_avg(Fun, X) ->
  TimesMicro =
    lists:map(fun(_) ->
                 {Avg, _} = timer:tc(fun() -> Fun() end),
                 Avg
              end,
              lists:seq(1, X)),
  Median = aoc_helpers:median(TimesMicro),
  Max = lists:max(TimesMicro),
  Min = lists:min(TimesMicro),
  io:format("Num loops: ~p~n", [X]),
  io:format("Time Min: ~p us ~.5f ms ~n", [Min, Min / 1000]),
  io:format("Time Max: ~p us ~.5f ms ~n", [Max, Max / 1000]),
  io:format("Time Median: ~p us ~.5f ms ~n", [Median, Median / 1000]).

