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
-import(evaluation, [evaluateBoard/2]).
-import(utils, [reverseColor/1, splitList/2]).
%% API
-export([getTheBestMove/4,getTreeValue/7,getTheBestMove/5, execute/3]).
getTreeValue(Move, Board, 0, _, Color, _, _) ->
  {evaluateBoard(Color, makeMove(Board, Move)), Move};

getTreeValue(Move, Board, Levels, 0, Color, MoveColor, max) ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  case getPermittedMoves(NewBoard, ReverseColor) of
    [] -> {evaluateBoard(Color, NewBoard), Move};
    List -> {Val, _} = lists:max([getTreeValue(X, NewBoard, Levels - 1, 0, Color, ReverseColor, min) || X <- List]),
      {Val, Move}
  end;

getTreeValue(Move, Board, Levels, 0, Color, MoveColor, min) ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  case getPermittedMoves(NewBoard, ReverseColor) of
    [] -> {evaluateBoard(Color, NewBoard), Move};
    List -> {Val, _} = lists:min([getTreeValue(X, NewBoard, Levels - 1, 0, Color, ReverseColor, max) || X <- List]),
      {Val, Move}
  end;

getTreeValue(Move, Board, Levels, CLevels, Color, MoveColor, min) when Levels >= CLevels ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  NextLevelArgs = [NewBoard, Levels - 1, CLevels - 1, Color, ReverseColor, max],
  case getPermittedMoves(NewBoard, ReverseColor) of
    [] -> {evaluateBoard(Color, NewBoard), Move};
    List ->
      S = self(),
      PidMoveMap = lists:map(fun(Element) ->
        {spawn_link(fun() ->
          execute(S, getTreeValue, [Element | NextLevelArgs]) end), Element} end, List),
      {Val, _} = lists:min(getMoveScores(PidMoveMap, S, getTreeValue, NextLevelArgs)),
      {Val, Move}
  end;

getTreeValue(Move, Board, Levels, CLevels, Color, MoveColor, max) when Levels >= CLevels ->
  NewBoard = makeMove(Board, Move),
  ReverseColor = reverseColor(MoveColor),
  NextLevelArgs = [NewBoard, Levels - 1, CLevels - 1, Color, ReverseColor, min],
  case getPermittedMoves(NewBoard, ReverseColor) of
    [] -> {evaluateBoard(Color, NewBoard), Move};
    List ->
      S = self(),
      PidMoveMap = lists:map(fun(Element) ->
        {spawn_link(fun() ->
          execute(S, getTreeValue, [Element | NextLevelArgs]) end), Element} end, List),
      {Val, _} = lists:max(getMoveScores(PidMoveMap, S, getTreeValue, NextLevelArgs)),
      {Val, Move}
  end.

execute(Pid, Function, Args) ->
  Pid ! {self(), erlang:apply(?MODULE, Function, Args)}.


%% non-concurrent version of getting the best move
getTheBestMove(Moves, Board, Color, Levels, 0) ->
  lists:max([getTreeValue(X, Board, Levels, 0, Color, Color, min) || X <- Moves]);

%% concurrent version of getting the best move with CLevels down of concurrency
getTheBestMove(Moves, Board, Color, Levels, CLevels) when Levels >= CLevels ->
  S = self(),
  PidMoveMap = lists:map(fun(Element) ->
    {spawn_link(fun() ->
      execute(S, getTreeValue, [Element, Board, Levels, CLevels, Color, Color, min]) end), Element} end, Moves),
  lists:max(getMoveScores(PidMoveMap, S, getTreeValue, [Board, Levels, Color, Color, min])).

getTheBestMove(Board, Color, Levels, CLevels) ->
  process_flag(trap_exit, true),
  S = self(),
  PermittedMoves = getPermittedMoves(Board, Color),
  BaseArgs = [Board, Color, Levels, CLevels],
  Nodes = nodes(),
  [MainNode | Rest]= splitList(PermittedMoves, length(Nodes)+1),
  Zipped = lists:zip(lists:sublist(Nodes, min(length(Rest), length(Nodes))), Rest),
  PidMovesMap = lists:map(fun({Node, Moves}) ->
    {spawn_link(Node, ?MODULE, execute, [S, getTheBestMove, [Moves | BaseArgs]]), Moves} end, Zipped),
  {_, Move} = lists:max([getTheBestMove(MainNode, Board, Color, Levels, CLevels)
    | getMoveScores(PidMovesMap, S, execute, BaseArgs)]),
  Move.

getMoveScores([], _, _, _) ->
  [];

getMoveScores(PidMoveMap, ParentPid, RestartFunc, Args) ->
  receive
    {Pid, {Value, Move}} ->
      NewList = lists:keydelete(Pid, 1, PidMoveMap),
      if
        length(NewList) == length(PidMoveMap) -> self() ! {Pid, {Value, Move}};
        true -> ok
      end,
      [{Value, Move} | getMoveScores(NewList, ParentPid, RestartFunc, Args)];
    {'EXIT', _, normal} ->
      getMoveScores(PidMoveMap, ParentPid, RestartFunc, Args);
    {'EXIT', ExitPid, R} ->
      io:format("~p", [R]),
      {MoveValue, _, NewList} = lists:keytake(ExitPid, 1, PidMoveMap),
      getMoveScores([spawn_link(fun() ->
        execute(ParentPid, RestartFunc, [MoveValue | Args]) end) | NewList], ParentPid, RestartFunc, Args)
    after 20000 ->
      c:flush()
  end.