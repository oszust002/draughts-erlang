%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jan 2017 18:55
%%%-------------------------------------------------------------------
-module(generation).
-author("oszust").
-import(moves, [makeMove/2, getPermittedMoves/2]).
-import(evaluation,[evaluateBoard/2]).
-import(utils, [reverseColor/1]).
%% API
-compile(export_all).
%%-export([]).
getTreeValue(Board, Move, 0, Color, _, _) ->
  {evaluateBoard(Color, makeMove(Board, Move)), Move};

getTreeValue(Board, Move, Levels, Color, MoveColor, min) ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  case getPermittedMoves(NewBoard, ReverseColor) of
    [] -> {evaluateBoard(Color, NewBoard), Move};
    List -> {Val,_} = lists:min([getTreeValue(NewBoard, X, Levels-1, Color, ReverseColor, max) || X <- List]),
            {Val, Move}
  end;

getTreeValue(Board, Move, Levels, Color, MoveColor, max) ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  case getPermittedMoves(NewBoard, ReverseColor) of
    []  -> {evaluateBoard(Color, NewBoard), Move};
    List -> {Val,_} = lists:max([getTreeValue(NewBoard, X, Levels-1, Color, ReverseColor, min) || X <- List]),
            {Val, Move}
  end.

calculateGameValue(Pid, Board, Pos, Levels, Color) ->
  Pid ! {self(), getTreeValue(Board,Pos,Levels, Color, Color, min)}.

getTheBestMove(Board, Color, Levels) ->
  PermittedMoves = getPermittedMoves(Board, Color),
  S = self(),
  PidMoveMap = lists:map(fun(Element) ->
    {spawn_link(fun() -> calculateGameValue(S,Board,Element,Levels,Color) end), Element} end, PermittedMoves),
  {_,Move} = lists:max(getMoveScores(PidMoveMap)),
  Move.



getMoveScores([]) ->
  [];
getMoveScores(PidMoveMap) ->
  receive
    {Pid, {Value,Move}} ->
      [{Value, Move}| getMoveScores(lists:keydelete(Pid,1,PidMoveMap))]
  end.