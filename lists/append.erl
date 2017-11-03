%% @author: Michael Bausano

-module(append).

%% Start the module with getlast:start().
-export([start/0]).

start() ->
  A = [5, 4, 3],
  B = [2, 1],
  List = append(A, B),
  io:format("~p\n", [List]).

%% Our breaking condition. Once the first list is
%% dismantled, we join it again with a tail of B,
%% which is our second list.
append ([], B ) -> B;

%% We dismantle the first list into elements,
%% put second list B into a tail, and recreate
%% the first list again with B appended.
append (_A = [X | T], B ) ->
  [ X | append (T , B) ].
