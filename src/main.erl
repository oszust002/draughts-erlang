%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2017 22:13
%%%-------------------------------------------------------------------
-module(main).
-author("oszust").
-import(board,[getInitBoard/0, printBoard/1]).
-import(parser, [parseAllMoves/1,parseStringToMove/1]).
-import(moves, [getPermittedMoves/2, makeMove/2]).
-import(generation, [getTheBestMove/4]).
-import(utils, [reverseColor/1, loadAllModules/0]).

-include("consts.hrl").
%% API
-export([start/1, init/0, fullAutoPlay/0]).

init() ->
  lists:foreach(fun(X) -> compile:file(X) end, [board, utils, moves, generation, parser, evaluation]),
  loadAllModules().

start(white) ->
  init(),
  gameLoop(getInitBoard(), white);

start(black) ->
  init(),
  makeAutoPlay(getInitBoard(), white);

start(_) ->
  only_black_or_white.

fullAutoPlay() ->
  init(),
  autoPlay(board:getInitBoard(), white).

autoPlay(Board, Color) ->
  case isWinner(Board, reverseColor(Color)) of
    true -> io:format("~p wins!!~n", [reverseColor(Color)]);
    false ->
      printBoard(Board),
      BestMove = getTheBestMove(Board, Color, ?LEVELS, ?CLEVELS),
      autoPlay(makeMove(Board, BestMove), reverseColor(Color))
  end.
gameLoop(Board, Color) ->
  case isWinner(Board, reverseColor(Color)) of
    true -> io:format("~p wins!!", [reverseColor(Color)]);
    false -> makePlay(Board, Color)
  end.

makePlay(Board, Color) ->
  printBoard(Board),
  PermittedMoves = getPermittedMoves(Board, Color),
  {_, [X]} = io:fread("Your move: ", "~s"),
  case parseStringToMove(X) of
    invalid ->
      io:format("Wrong value(correct is from 1 to 32) or invalid format of move, try this ~nMoves: 1-2~nJumps: 1x2x3"),
      gameLoop(Board, Color);
    Move -> case lists:member(Move, PermittedMoves) of
              true ->
                makeAutoPlay(makeMove(Board, Move), reverseColor(Color));
              false ->
                io:format(string:join(["Move not permitted, permitted moves:", parseAllMoves(PermittedMoves)],"~n")),
                gameLoop(Board, Color)
            end
  end.

makeAutoPlay(Board, Color) ->
  case isWinner(Board, reverseColor(Color)) of
    true -> io:format("~p wins!!", [reverseColor(Color)]);
    false ->
      io:format("~n~p", [parseAllMoves(getPermittedMoves(Board, Color))]),
      printBoard(Board),
      BestMove = getTheBestMove(Board, Color, ?LEVELS, ?CLEVELS),
      gameLoop(makeMove(Board, BestMove), reverseColor(Color))
  end.

isWinner(Board, Color) ->
  PermittedMoves = getPermittedMoves(Board, reverseColor(Color)),
  ColorFigs = countFigs(Board, Color),
  ReverseColorFigs = countFigs(Board, reverseColor(Color)),
  if
    (length(PermittedMoves) == 0 orelse (ColorFigs == 0 andalso ReverseColorFigs =/= 0)) -> true;
    true -> false
  end.

countFigs(Board, Color) ->
  AllFigs = [X || X <- lists:flatten(Board), X =/= nothing],
  length(lists:filter(fun({_,Col}) -> Col == Color end, AllFigs)).
