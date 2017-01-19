
%%%-------------------------------------------------------------------
%%% @author oszust
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Jan 2017 08:53
%%%-------------------------------------------------------------------
-module(moves).
-author("oszust").

%% API
-compile(export_all).
-import(utils, [isInBounds/1, replaceNth/3, getColor/1, getField/2, signum/1, isEmpty/2]).

createLine({X, Y}, Length, ne) when Length > 0 ->
  [{X + A, Y + B} || A <- lists:seq(-Length, -1), B <- lists:seq(1, Length), isInBounds({X + A, Y + B}), (X + A) + (Y + B) == X + Y];

createLine({X, Y}, Length, nw) when Length > 0 ->
  [{X + A, Y + B} || A <- lists:seq(-Length, -1), B <- lists:seq(-Length, -1), isInBounds({X + A, Y + B}), (X + A) - (Y + B) == X - Y];

createLine({X, Y}, Length, se) when Length > 0 ->
  [{X + A, Y + B} || A <- lists:seq(1, Length), B <- lists:seq(1, Length), isInBounds({X + A, Y + B}), (X + A) - (Y + B) == X - Y];

createLine({X, Y}, Length, sw) when Length > 0 ->
  [{X + A, Y + B} || A <- lists:seq(1, Length), B <- lists:seq(-Length, -1), isInBounds({X + A, Y + B}), (X + A) + (Y + B) == X + Y].

placeFigure(Board, {X, Y}, NewField) ->
  Row = lists:nth(X, Board),
  replaceNth(X, replaceNth(Y, NewField, Row), Board).

deleteFigure(Board, Pos) ->
  placeFigure(Board, Pos, nothing).

checkPromotion({X, _}, white) ->
  X == 1;

checkPromotion({X, _}, black) ->
  X == 8.

moveFigure(Board, OldPos, NewPos, ShouldPromote) when is_boolean(ShouldPromote) ->
  Color = getColor(getField(Board, OldPos)),
  Field = case ShouldPromote of
            true -> case checkPromotion(NewPos, Color) of
                      true -> {Color, king};
                      false -> getField(Board, OldPos)
                    end;
            false -> getField(Board, OldPos)
          end,
  placeFigure(deleteFigure(Board, OldPos), NewPos, Field).

makeMove(Board, {From, To}) ->
  moveFigure(Board, From, To, true);

makeMove(Board, [_]) ->
  Board;
%% When move is list, it's a jump
makeMove(Board, [From = {X1, Y1}, To = {X2, Y2}]) ->
  CapturedPosition = {X2 + signum(X1 - X2), Y2 + signum(Y1 - Y2)},
  deleteFigure(moveFigure(Board, From, To, true), CapturedPosition);

makeMove(Board, [From = {X1, Y1}, To = {X2, Y2} | T]) ->
  CapturedPosition = {X2 + signum(X1 - X2), Y2 + signum(Y1 - Y2)},
  makeMove(deleteFigure(moveFigure(Board, From, To, false), CapturedPosition), [To | T]).

processJump(Board, [From = {X1, Y1}, To = {X2, Y2}]) ->
  CapturedPosition = {X2 + signum(X1 - X2), Y2 + signum(Y1 - Y2)},
  deleteFigure(moveFigure(Board, From, To, false), CapturedPosition).

getNeighbour({X, Y}, Dir) ->
  case Dir of
    ne -> {X - 1, Y + 1};
    nw -> {X - 1, Y - 1};
    se -> {X + 1, Y + 1};
    sw -> {X + 1, Y - 1}
  end.

getPawnMove(white, Pos) ->
  [{Pos, getNeighbour(Pos, nw)}, {Pos, getNeighbour(Pos, ne)}];

getPawnMove(black, Pos) ->
  [{Pos, getNeighbour(Pos, sw)}, {Pos, getNeighbour(Pos, se)}].

getKingMoves(Pos) ->
  lists:map(fun(L) -> {Pos, L} end, lists:merge(lists:map(fun(L) -> createLine(Pos, 7, L) end, [ne, nw, se, sw]))).

getMoves(Board, Pos, Color) ->
  Field = getField(Board, Pos),
  case Field of
    {Color, pawn} -> getPawnMove(Color, Pos);
    {Color, king} -> getKingMoves(Pos);
    _Else -> []
  end.

getAllMoves(Board, Color) ->
  lists:merge([getMoves(Board, {A, B}, Color) || A <- lists:seq(1, 8), B <- lists:seq(1, 8)]).

trackIsEmpty(_, {{X1, _}, {X1, _}}) ->
  true;

trackIsEmpty(Board, {From = {X1, Y1}, {X2, Y2}}) ->
  CurrState = isEmpty(Board, {X2, Y2}),
  Next = {From, {X2 + signum(X1 - X2), Y2 + signum(Y1 - Y2)}},
  CurrState andalso trackIsEmpty(Board, Next).

permittedMove(Board, Move = {_, To}) ->
  isInBounds(To) andalso trackIsEmpty(Board, Move).

getPossibleMoves(Board, Color) ->
  lists:filter(fun(X) -> permittedMove(Board, X) end, getAllMoves(Board, Color)).

getEnemyOnDir(Board, Pos, Color, Direction, Step) ->
  Next = getNeighbour(Pos, Direction),
  case not(isInBounds(Next)) of
    true -> {Pos, nothing};
    false -> makeStep(Board, Pos, Color, Direction, Step, Next)
  end.

makeStep(Board, Pos, Color, Direction, Step, Next) ->
  case {isEmpty(Board, Next), Step} of
    {true, false} -> {Pos, nothing};
    {true, _} -> getEnemyOnDir(Board, Next, Color, Direction, Step - 1);
    _Else -> checkOtherCases(Board, Pos, Color, Direction, Next)
  end.

checkOtherCases(Board, Pos, Color, Direction, Next) ->
  NextNext = getNeighbour(Next, Direction),
  NextField = getField(Board, Next),
  NextColor = getColor(NextField),
  case not(isInBounds(NextNext)) orelse Color == NextColor orelse not(isEmpty(Board, NextNext)) of
    true -> {Pos, nothing};
    false -> {Next, NextField}
  end.

addToEveryList(Element, []) ->
  [[Element]];

addToEveryList(Element, L) when is_list(L) ->
  [[Element | X] || X <- L].

getAllMaxLists([]) ->
  [];
getAllMaxLists(Lists) ->
  MaxLength = lists:max(lists:map(fun erlang:length/1, Lists)),
  lists:filter(fun(X) -> MaxLength == length(X) end, Lists).

getJump(Board, Pos, Color) ->
  CurField = getField(Board, Pos),
  FieldColor = getColor(CurField),
  case Color of
    FieldColor ->
      AllJumps = lists:merge(lists:map(fun(X) -> getJumpList(Board, CurField, Pos, Color, X) end, [ne, nw, se, sw])),
      getAllMaxLists(AllJumps);
    _Else -> []
  end.

getJumpList(Board, CurField, Pos, Color, Dir) ->
  {EnemyPos, Field} = getEnemyOnDir(Board, Pos, Color, Dir, getMaxStepLength(CurField)),
  case Field of
    nothing -> [];
    _Else ->
      NextPos = getNeighbour(EnemyPos, Dir),
      addToEveryList(NextPos, getJump(makeMove(Board, [Pos, NextPos]), NextPos, Color))
  end.

getMaxStepLength({_, king}) ->
  7;
getMaxStepLength({_, _}) ->
  0.


getAllJumps(Board, Color) ->
  lists:merge([addToEveryList({X, Y}, getJump(Board, {X, Y}, Color)) ||
    X <- lists:seq(1, 8), Y <- lists:seq(1, 8), getField(Board, {X, Y}) =/= nothing]).

notEmptyJump([]) ->
  false;
notEmptyJump([_]) ->
  false;
notEmptyJump(_) ->
  true.

getPossibleJumps(Board, Color) ->
  lists:filter(fun ?MODULE:notEmptyJump/1, getAllJumps(Board, Color)).

getPermittedMoves(Board, Color) ->
  Jumps = getPossibleJumps(Board, Color),
  case length(Jumps) of
    0 -> getPossibleMoves(Board, Color);
    _Else -> Jumps
  end.


