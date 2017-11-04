%% @author: Michael Bausano

-module(main).

%% Start the program with main:start().
-export([start/0]).

%% Importing module methods.
%% TODO: http://www.erlang.se/doc/programming_rules.shtml#HDR26
-import(set, [new/1, add/2, del/2,
			  contains/2, empty/1,
			  union/2, intersection/2]).

%% Boot point.
start() ->
	Set = set:new([1, 2, 3, 2, 5, 6]),
	
	Set1 = set:new([egg, bread, milk, apple]),
	
	Set2 = set:new([bread, milk, sugar, coffee]),
	
	io:format("\nNew set\n~p\n", [ Set ]),
	
	io:format("\nAdding 2, 4, 8\n~p\n", [ set:add(Set, [2, 4, 8]) ]),
	
	io:format("\nDeleting 5\n~p\n", [ set:del(Set, 5) ]),
	
	io:format("\nContains 10?\n~p\n", [ set:contains(Set, 10) ]),
	
	io:format("\nContains 3?\n~p\n", [ set:contains(Set, 3) ]),
	
	io:format("\nIs empty?\n~p\n", [ set:empty(Set) ]),

	io:format("\nUnion of sets\n~p\n", [ set:union(Set1, Set2) ]),
	
	io:format("\nIntersection of sets\n~p\n", [ set:intersection(Set1, Set2) ]).

