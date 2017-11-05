%% Provides basic operations with sets.
%%
%% @author: Michael Bausano

-module(set).

-export([new/0, new/1, add/2, del/2, append/2,
		 contains/2, empty/1,
		 union/2, intersection/2]).

%% Set is a list with no repeating values.
-type set() :: list().

%% Element that can be placed into a set.
-type el() :: any().

%% @param set	Set we want to append to
%% @param any	Element we want to append. List is treated as element.
- spec append(set(), any()) -> set().

%% @param el 	Set will be created from given argument.
%%						If list is given, a union of that list with set is returned.
-spec new(el()) -> set().

%% Returns empty set.
-spec new() -> set().

%% @param set	Set given elements should be put into.
%% @param el 	Element to be put into a set.
%%						If list is given, a union of that list with set is returned.
-spec add(set(), el()) -> set().

%% @param set	Set given element should be removed from.
%% @param el 	Element to delete.
-spec del(set(), el()) -> set().

%% @param set 	Set to search in.
%% @param el 	Element to look for.
-spec contains(set(), el()) -> 'true' | 'false'.

%% @param set 	Set to check on.
-spec empty(set()) -> 'true' | 'false'.

%% Joins two sets.
%% @param set
%% @param set
-spec union(set(), set()) -> set().

%% Returns mutual elements for both sets.
%% @param set
%% @param set
- spec intersection(set(), set()) -> set().

%% <function append/2>
%% If element is not found, add it to the tail.
append([], El) -> [El];

%% If set already contains this element, break the loop.
append(Set = [El | _], El) -> Set;

append([First | Tail], El) ->
	[ First | append(Tail, El) ].
%% </endfunction>

%% <function add/2>
%% Breaking point is hit if it loops though all elements to add.
add(Set, []) -> Set;

add(Set, [First | Tail]) ->
	New = append(Set, First),
	add(New, Tail);

%% If given element is not list, make it one and go to second statement.
add(Set, El) ->
	add(Set, [El]).
%% </endfunction>

%% <function new/1, new/0>
new(El) ->
	add([], El).

new() -> [].
%% </end function>

%% <function empty/1>
empty([]) -> true;

empty(_Set) -> false.
%% </endfunction>

%% <function union/2>
union(Set1, Set2) ->
	add(Set1, Set2).
%% </endfunction>

%% <function contains/2>
contains([El | _], El) -> true;

contains([], _El) -> false;

contains([_Any | Tail], El) ->
	contains(Tail, El).
%% </endfunction>

%% <function del/2>
del([], _El) -> [];

del([El | Tail], El) -> Tail;

del([First | Tail], El) ->
	[ First | del(Tail, El) ].
%% </endfunction>

%% <function intersection/2>
intersection(_, []) -> [];

intersection(Set, [El | Tail]) ->
	case contains(Set, El) of
		true -> [El | intersection(Set, Tail)];
		%% If first set doen't contain an element from second set, proceed to next one.
		false -> intersection(Set, Tail)
	end.
%% </endfunction>
