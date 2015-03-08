-module(forth_test).
-include_lib("eunit\\include\\eunit.hrl").

code_definition_test() ->
    T = ets:new(table, [bag]),
    io:fwrite("print test - 5 would be printed:~n"),
    ?assertMatch(
        {T, _, []},
        forth:cr([1, 2, 3, '+', '*', emit], {T, []})
    ),
    ?assertMatch(
        {T, _, [5]},
        forth:cr([':', hello, "hello,world", emit, ';', 1, 2, 3, '+', '*'],
                 {T, []})
    ),
    ?assertMatch([{hello,_}|_], ets:lookup(T, hello)).

