-module(forth_test).
-include_lib("eunit\\include\\eunit.hrl").

code_definition_test() ->
    T = ets:new(table, [bag]),
    ?assertMatch(
        {T, _, [5]},
        forth:cr([':', hello, "hello,world", emit, ';', 1, 2, 3, '+', '*'],
                 {T, []})
    ),
    ?assertMatch([{hello,_}|_], ets:lookup(T, hello)).

empty_definition_test() ->
    T = ets:new(table, [bag]),
    forth:cr([':', 'star', '[char]', $*, emit], {T, []}),
    ?assertMatch([], ets:lookup(T, star)).

tag_definition_test() ->
    T = ets:new(table, [bag]),
    forth:cr([':', 'star', '[char]', $*, emit, ';'], {T, []}),
    ?assertMatch([{star,_}|_], ets:lookup(T, star)).

expression_test() ->
    io:fwrite("print test - 5 would be printed:~n"),
    ?assertMatch({undefined, _, [5]},
                 forth:cr([1, 2, 3, '+', '*'], {undefined, []})).

file_load_test() ->
    ?assertMatch(
        [<<":">>,<<"star">>,<<"[char]">>,<<"*">>,<<"emit">>,<<";">>],
	forth:load("..\\ebin\\in.txt")).
