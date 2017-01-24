%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2017 03:29
%%%-------------------------------------------------------------------
-module(evaluation).
-author("oszust").
-define(EVALUATION_TABLE,
  [[7 * 4, 7 * 4, 7 * 4, 7 * 4, 7 * 4, 7 * 4, 7 * 4, 7 * 4],
    [8 * 4, 8 * 3, 8 * 3, 8 * 3, 8 * 3, 8 * 3, 8 * 3, 8 * 4],
    [9 * 4, 9 * 3, 9 * 2, 9 * 2, 9 * 2, 9 * 2, 9 * 3, 9 * 4],
    [10 * 4, 10 * 3, 10 * 2, 10 * 1, 10 * 1, 10 * 2, 10 * 3, 10 * 4],
    [11 * 4, 11 * 3, 11 * 2, 11 * 1, 11 * 1, 11 * 2, 11 * 3, 11 * 4],
    [12 * 4, 12 * 3, 12 * 2, 12 * 2, 12 * 2, 12 * 2, 12 * 3, 12 * 4],
    [13 * 4, 13 * 3, 13 * 3, 13 * 3, 13 * 3, 13 * 3, 13 * 3, 13 * 4],
    [14 * 4, 14 * 4, 14 * 4, 14 * 4, 14 * 4, 14 * 4, 14 * 4, 14 * 4]]).
%% API
-import(utils, [getField/2]).
-export([evaluateBoard/2]).


evaluatePosition(black, {X, Y}) when X > 0, Y > 0 ->
  getField(?EVALUATION_TABLE, {X, Y});
evaluatePosition(white, {X, Y}) when X > 0, Y > 0 ->
  getField(lists:reverse(?EVALUATION_TABLE), {X, Y}).


evaluateFigure(king) ->
  2;
evaluateFigure(pawn) ->
  1.

evaluateColor(X, X) ->
  1;
evaluateColor(_, _) ->
  -1.

evaluateField(Color, Board, Position = {X, Y}) when is_integer(X), is_integer(Y) ->
  Field = getField(Board, Position),
  case Field of
    nothing ->
      0;
    {Col, Figure} ->
      evaluateColor(Col, Color) * evaluateFigure(Figure) * evaluatePosition(Color, Position)
  end.

evaluateBoard(Color, Board) ->
  lists:foldl(fun(X, Sum) -> X + Sum end, 0,
    [evaluateField(Color, Board, {X, Y}) || X <- lists:seq(1, 8), Y <- lists:seq(1, 8)]).