-module(day06).

-export([solve/0]).

solve() ->
  {ok, Input} = aoc:read("day06.txt"),
  find_markers(Input).

find_markers(Input) ->
  do_find_markers(Input, 4, {0, 0}).

do_find_markers(<<_:8, Rest/binary>> = Bin, Size, {Part1, Pos}) ->
  case is_marker(Bin, Size) of
    true when Size =:= 4 ->
      do_find_markers(Rest, 14, {_Part1 = Pos + Size, Pos + 1});
    true ->
      {Part1, _Part2 = Pos + Size};
    false ->
      do_find_markers(Rest, Size, {Part1, Pos + 1})
  end.

%% Optimized function clauses for a size of 4 and 14.
%% Yes this is ugly and a different approach exists in the git history
%% but this is simply to prove the absolute fastest way we can chew through this in Erlang.
%% Use of ; is done here since ; compiles differently than orelse and ; is slightly faster.
-compile({inline, [is_marker/2]}).
is_marker(<<A:8/integer, B:8/integer, C:8/integer, D:8/integer, _/binary>>, 4 = _Size)
  when (A =:= B); (A =:= C); (A =:= D);
       (B =:= C); (B =:= D);
       (C =:= D) ->
  false;
is_marker(<<A:8/integer, B:8/integer, C:8/integer, D:8/integer, E:8/integer, F:8/integer, G:8/integer,
            H:8/integer, I:8/integer, J:8/integer, K:8/integer, L:8/integer, M:8/integer, N:8/integer, _/binary>>,
          14 = _Size)
  when (A =:= B); (A =:= C); (A =:= D); (A =:= E); (A =:= F);
       (A =:= G); (A =:= H); (A =:= I); (A =:= I); (A =:= J);
       (A =:= K); (A =:= L); (A =:= M); (A =:= N)
      ;
       (B =:= C); (B =:= D); (B =:= E); (B =:= F); (B =:= G);
       (B =:= H); (B =:= I); (B =:= I); (B =:= J); (B =:= K);
       (B =:= L); (B =:= M); (B =:= N)
      ;
       (C =:= D); (C =:= E); (C =:= F); (C =:= G); (C =:= H);
       (C =:= I); (C =:= J); (C =:= K); (C =:= L); (C =:= M);
       (C =:= N)
      ;
       (D =:= E); (D =:= F); (D =:= G); (D =:= H); (D =:= I);
       (D =:= J); (D =:= K); (D =:= L); (D =:= M); (D =:= N)
      ;
       (E =:= F); (E =:= G); (E =:= H); (E =:= I); (E =:= J);
       (E =:= K); (E =:= L); (E =:= M); (E =:= N)
      ;
       (F =:= G); (F =:= H); (F =:= I); (F =:= J); (F =:= K);
       (F =:= L); (F =:= M); (F =:= N)
      ;
       (G =:= H); (G =:= I); (G =:= J); (G =:= K); (G =:= L);
       (G =:= M); (G =:= N)
      ;
       (H =:= I); (H =:= J); (H =:= K); (H =:= L); (H =:= M);
       (H =:= N)
      ;
       (I =:= J); (I =:= K); (I =:= L); (I =:= M); (I =:= N)
      ;
       (J =:= K); (J =:= L); (J =:= M); (J =:= N)
      ;
       (K =:= L); (K =:= M); (K =:= N)
      ;
       (L =:= M); (L =:= N)
      ;
       (M =:= N) ->
  false;
is_marker(_, _) ->
  true.
