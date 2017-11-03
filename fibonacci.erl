%% @author: Michael Bausano

-module(fibonacci).

%% Start the module with fibonacci:start().
-export([start/0]).

start() ->
  N = 5,
  io:format("Quick: ~p\n\n", [fib2(N)]),
  io:format("Slow: ~p\n\n", [fib(N)]).

%% ------ Slow vesion ------ %%

%% Our first state is the first number of the sequence.
%% We want to keep the conventions so this state has number 0.
%% However it could be fib(1) -> 0; and fib(2) -> 1; and
%% our program would still be completely valid.
fib(0) -> 0;

%% Our second state would be the second number of the sequence.
fib(1) -> 1;

%% Recursion ala while loop. As long as N is not 0 or 1, compute previous N.
%% Also we only want to work with positive numbers, so by specifying
%% when N > 1 we eliminate infinit loop cases.
fib(N) when N > 1 ->
  fib(N - 1) + fib(N - 2).

%% ------ Quick vesion ------ %%

%% We don't care about the value of third parameter, so the convension
%% is to omit it with underscore.
fib2(1, A, _) -> A;

%% We want to loop with these 3 arguments and break once N - 1 equals 1.
fib2(N, A, B) ->
	fib2(N - 1, A + B, A).

%% Some code sugar. We start the sequence with 1, 0, the first two
%% numbers of FS. God bless the pattern matching.
fib2(N) when N > 0 ->
	fib2(N, 1, 0).
