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
-include("consts.hrl").

-export([isInBounds/1, replaceNth/3, isEmpty/2, getColor/1, signum/1,
  reverseColor/1, splitList/2, test_avg/4, loadAllModules/0]).

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


splitList(List, N) when length(List) < N ->
  splitList(List, length(List));

splitList(List, N) ->
  Size = length(List) div N,
  lists:filter(fun(X) -> length(X) > 0 end, splitList(List, Size, [], N)).

splitList([], _, Splitted, _) ->
  Splitted;

splitList(List, _, Splitted = [H | T], PartSize) when length(Splitted) == PartSize ->
  [lists:merge([H, List]) | T];

splitList(List, N, Splitted, _) when length(List) < N ->
  [List | Splitted];

splitList(List, N, Splitted, PartSize) when length(List) >= N ->
  {Part, Rest} = lists:split(N, List),
  splitList(Rest, N, [Part | Splitted], PartSize).


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
  test_loop(M, F, A, N - 1, [T | List]).

loadAllModules() ->
  lists:foreach(fun(Mod) -> loadModule(Mod, nodes()) end, [board, utils, moves, generation, parser, evaluation, main]).

loadModule(_, []) ->
  ok;
loadModule(Mod, Nodes) ->
  {Mod, Bin, File} = code:get_object_code(Mod),
  {_Replies, _} = rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]).