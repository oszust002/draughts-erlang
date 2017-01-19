%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2017 19:47
%%%-------------------------------------------------------------------
-define(WHITE_PAWN, {white, pawn}).
-define(BLACK_PAWN, {black, pawn}).
-define(WHITE_KING, {white, king}).
-define(BLACK_KING, {black, king}).

-define(INITIAL_BOARD, [
  [nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN],
  [?BLACK_PAWN, nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN, nothing],
  [nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN, nothing, ?BLACK_PAWN],
  [nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing],
  [nothing, nothing, nothing, nothing, nothing, nothing, nothing, nothing],
  [?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing],
  [nothing, ?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing, ?WHITE_PAWN],
  [?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing, ?WHITE_PAWN, nothing]
]).