%% @author: Michael Bausano

-module(delete_all).

%% Start the module with delete_all:start().
-export([start/0]).

start() ->
  List = [2, 3, 5, 3],
  X = 3,
  Deleted = delete_all(X, List),
  io:format("~p\n", [Deleted]).

%% If we looped though all elements in the array,
%% we want to stop our cycle by appending one last element.
%% That element is an empty list [ ]. Now run your append
%% function from previous example on A = [2, 5], B = [].
%% You get [2, 5]. That means appending an empty list
%% to another list doesn't change it at all!
delete_all(X, []) -> [];

%% By using same variable name for first argument, which is
%% the number we want to remove, and first element of the list,
%% We are basically saying if (X == first(List)).
delete_all(X, [X | T]) ->
  %% If it is, we just proceed in rebuilding the list without it.
  delete_all(X, T);

%% We know that X doesn't equal Y.
delete_all(X, [Y | T]) ->
  %% Les check the rest of the list. Remember, if T
  %% is empty, we go to our breaking condition.
  [Y | delete_all(X, T)].
