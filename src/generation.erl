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
getTreeValue(Move, Board, 0, _, Color, _, _) ->
  {evaluateBoard(Color, makeMove(Board, Move)), Move};

getTreeValue(Move, Board, Levels, 0, Color, MoveColor, max) ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  case getPermittedMoves(NewBoard, ReverseColor) of
    []  -> {evaluateBoard(Color, NewBoard), Move};
    List -> {Val,_} = lists:max([getTreeValue(X, Board, Levels-1, 0, Color, ReverseColor, min) || X <- List]),
      {Val, Move}
  end;

getTreeValue(Move,Board, Levels, 0, Color, MoveColor, min) ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  case getPermittedMoves(NewBoard, ReverseColor) of
    [] -> {evaluateBoard(Color, NewBoard), Move};
    List -> {Val,_} = lists:min([getTreeValue(X, Board, Levels-1,0, Color, ReverseColor, max) || X <- List]),
      {Val, Move}
  end;

getTreeValue(Move, Board, Levels, CLevels, Color, MoveColor, min) when Levels >= CLevels ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  NextLevelArgs = [NewBoard, Levels-1, CLevels-1, Color, ReverseColor, max],
  case getPermittedMoves(NewBoard, ReverseColor) of
    [] -> {evaluateBoard(Color, NewBoard), Move};
    List ->
      S = self(),
      PidMoveMap = lists:map(fun(Element) ->
        {spawn_link(fun() ->
          execute(S, getTreeValue, [Element|NextLevelArgs]) end), Element} end, List),
      {Val, _} = lists:min(getMoveScores(PidMoveMap, S, getTreeValue, NextLevelArgs)),
      {Val, Move}
  end;

getTreeValue(Move, Board, Levels, CLevels, Color, MoveColor, max)  when Levels >= CLevels ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  NextLevelArgs = [NewBoard, Levels-1, CLevels-1, Color, ReverseColor, min],
  case getPermittedMoves(NewBoard, ReverseColor) of
    []  -> {evaluateBoard(Color, NewBoard), Move};
    List ->
      S = self(),
      PidMoveMap = lists:map(fun(Element) ->
        {spawn_link(fun() ->
          execute(S, getTreeValue, [Element|NextLevelArgs]) end), Element} end, List),
      {Val, _} = lists:max(getMoveScores(PidMoveMap, S, getTreeValue, NextLevelArgs)),
      {Val, Move}
  end.

execute(Pid, Function, Args) ->
  Pid ! {self(), erlang:apply(?MODULE, Function, Args)}.

%% non-concurrent version of getting the best move
getTheBestMove(Board, Color, Levels, 0) ->
  PermittedMoves = getPermittedMoves(Board, Color),
  {_,Move} = lists:max([getTreeValue(X,Board,Levels,0, Color,Color, min) || X <- PermittedMoves]),
  Move;

%% concurrent version of getting the best move with CLevels down of concurrency
getTheBestMove(Board, Color, Levels, CLevels)  when Levels >= CLevels ->
  PermittedMoves = getPermittedMoves(Board, Color),
  S = self(),
  PidMoveMap = lists:map(fun(Element) ->
    {spawn_link(fun() ->
      execute(S,getTreeValue,[Element,Board,Levels,CLevels,Color,Color, min]) end), Element} end, PermittedMoves),
  {_,Move} = lists:max(getMoveScores(PidMoveMap,S, getTreeValue, [Board, Levels, Color, Color, min])),
  Move.

getMoveScores([],_,_,_) ->
  [];

getMoveScores(PidMoveMap, ParentPid, RestartFunc, Args) ->
  receive
    {Pid, {Value,Move}} ->
      [{Value, Move}| getMoveScores(lists:keydelete(Pid,1,PidMoveMap), ParentPid, RestartFunc, Args)];
    {'EXIT', ExitPid, _} ->
      {MoveValue,_,NewList} = lists:keytake(ExitPid, 1, PidMoveMap),
      getMoveScores([spawn_link(fun() ->
        execute(ParentPid, RestartFunc, [MoveValue|Args]) end) | NewList], ParentPid, RestartFunc, Args)
  end.