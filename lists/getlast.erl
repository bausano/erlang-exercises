%% @author: Michael Bausano

-module(getlast).

%% Start the module with getlast:start().
-export([start/0]).

start() ->
  List = ["Special one", "Second", "Third", "Last!"],
  Last = last(List),
  io:format("~p\n", [Last]).

%% Our breaking condition. We split a List into a
%% member and his next phone numbers to call - the tail.
%% If the tail is empty (there's no next phone
%% number to call) return that member.
last([Last | []]) -> Last;
%% Else go to our next definition of last().

%% This definition says: I don't care about _Member, just
%% give me next phone number and check if that one is last.
last([_Member | Rest]) ->
  %% Do that until you match our breaking condition.
  last(Rest).
