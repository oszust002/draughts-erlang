%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2017 18:59
%%%-------------------------------------------------------------------
-module(board).
-author("oszust").
%% API
-include("consts.hrl").

-export([printBoard/1, getInitBoard/0, getTestBoard/0]).

printFigure(?WHITE_PAWN) ->
  "w";
printFigure(?BLACK_PAWN) ->
  "b";
printFigure({Color, king}) ->
  string:to_upper(printFigure({Color, pawn}));
printFigure(nothing) ->
  ".".

printRow({RowNumber, FigureList}) when is_integer(RowNumber), RowNumber > 0, is_list(FigureList) ->
  FiguresStringList = [printFigure(X) || X <- FigureList],
  RowNumberString = integer_to_list(RowNumber),
  string:join([RowNumberString | FiguresStringList], " ").

printBoard(Board) when is_list(Board) ->
  BoardString = string:join([printRow(X) || X <- lists:zip(lists:seq(1, 8), Board)], "~n"),
  io:format(lists:concat(["  1 2 3 4 5 6 7 8 ~n", BoardString, "~n"])).

getInitBoard() ->
  ?INITIAL_BOARD.

getTestBoard() ->
  [[nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing],
    [nothing, nothing, ?WHITE_KING, nothing, nothing, nothing, nothing, nothing],
    [nothing, ?BLACK_PAWN, nothing, nothing, nothing, ?WHITE_PAWN, nothing, nothing],
    [nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing],
    [nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing],
    [nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing],
    [nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing],
    [nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing]].




