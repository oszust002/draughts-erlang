%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2017 03:05
%%%-------------------------------------------------------------------
-module(utils).
-author("oszust").

%% API
-export([isEmpty/2, getField/2, isInBounds/1, isKing/1, isPawn/1, replaceNth/3, getColor/1, getFigure/1, signum/1, reverseColor/1]).
-include("consts.hrl").

isInBounds({X, Y}) when is_integer(X), is_integer(Y) ->
  X =< 8 andalso X > 0 andalso Y > 0 andalso Y =< 8.

replaceNth(N, NewValue, List) when N > 0, is_list(List) ->
  lists:sublist(List, 1, N - 1) ++ [NewValue] ++ lists:sublist(List, N + 1, length(List) + 1).

isEmpty(Board, Position = {X, Y}) when is_integer(X), is_integer(Y) ->
  Field = getField(Board, Position),
  Field == nothing.

getField(Board, {X, Y}) ->
  lists:nth(Y, lists:nth(X, Board)).

getColor({Color, _}) ->
  Color.

getFigure({_, Figure}) ->
  Figure.

isKing({_, Figure}) ->
  Figure == king.

isPawn({_, Figure}) ->
  Figure == pawn.

signum(X) when X > 0 ->
  1;

signum(X) when X == 0 ->
  0;

signum(X) when X < 0 ->
  -1.

reverseColor(white) ->
  black;

reverseColor(black) ->
  white.

%% just for time tests
test_avg(M, F, A, N) when N > 0 ->
  L = test_loop(M, F, A, N, []),
  Length = length(L),
  Min = lists:min(L),
  Max = lists:max(L),
  Med = lists:nth(round((Length / 2)), lists:sort(L)),
  Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
  io:format("Range: ~b - ~b mics~n"
  "Median: ~b mics~n"
  "Average: ~b mics~n",
    [Min, Max, Med, Avg]),
  Med.

test_loop(_M, _F, _A, 0, List) ->
  List;
test_loop(M, F, A, N, List) ->
  {T, _Result} = timer:tc(M, F, A),
  test_loop(M, F, A, N - 1, [T|List]).