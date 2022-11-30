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
  AvgTimeMicro = lists:sum(
                    lists:map(fun(_) ->
                                  {Avg, _} = timer:tc(fun() -> Fun() end),
                                  Avg
                              end, lists:seq(1, X))) / X,
  io:format("Time: ~p us ~.5f ms ~n", [AvgTimeMicro, AvgTimeMicro / 1000]).
