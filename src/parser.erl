%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2017 19:29
%%%-------------------------------------------------------------------
-module(parser).
-author("oszust").

%% API
-export([parseStringToMove/1, parseAllMoves/1]).

parseStringToMove(String) ->
  parseStringToMove(String, string:rstr(String, "-"), string:rstr(String, "x")).

parseStringToMove(_, 0, 0) ->
  invalid;

parseStringToMove(String, _, 0) ->
  Moves = [element(1, string:to_integer(X)) || X <- string:tokens(String, "-")],
  parseToMove(Moves);

parseStringToMove(String, 0, _) ->
  Jumps = [element(1, string:to_integer(X)) || X <- string:tokens(String, "x")],
  parseToJump(Jumps);

parseStringToMove(_, _, _) ->
  invalid.

parseToMove([From, To]) when is_integer(From), is_integer(To), From >= 1, From =< 32, To >= 1, To =< 32 ->
  {intToPos(From), intToPos(To)};

parseToMove(_) ->
  invalid.

intToPos(N) ->
  X = ((N - 1) div 4) + 1,
  Y = if
        ((X - 1) rem 2 == 0) -> 2 * (N - 4 * (X - 1));
        true -> 2 * (N - 4 * (X - 1) - 1) + 1
      end,
  {X, Y}.

parseToJump(Jumps) when length(Jumps) < 2 ->
  invalid;

parseToJump(Jumps) ->
  parseToJump(Jumps, []).

parseToJump([], List) ->
  List;

parseToJump([H | T], List) when is_integer(H) ->
  parseToJump(T, List ++ [intToPos(H)]);

parseToJump(_, _) ->
  invalid.

parseMoveToString([]) ->
  "";

parseMoveToString([H | T]) ->
  TailString = lists:concat([lists:concat(["x", integer_to_list(posToInt(X))]) || X <- T]),
  lists:concat([integer_to_list(posToInt(H)), TailString]);

parseMoveToString({From, To}) ->
  lists:concat([integer_to_list(posToInt(From)), "-", integer_to_list(posToInt(To))]).

posToInt({X, Y}) ->
  4 * (X - 1) + ((Y + 1) div 2).

parseAllMoves([]) ->
  "";

parseAllMoves([H | T]) ->
  TailString = lists:concat(lists:map(fun(X) -> lists:concat([",", parseMoveToString(X)]) end, T)),
  lists:concat([parseMoveToString(H), TailString]).