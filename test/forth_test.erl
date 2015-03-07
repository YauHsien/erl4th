-module(forth_test).
-include_lib("eunit\\include\\eunit.hrl").

code_definition_test() ->
    T = ets:new(table, [bag]),
    forth:cr([':', hello, "hello,world", emit, ';', 1, 2, 3, '+', '*'],
             {T, []}),
    ?assertMatch([{hello,_}|_], ets:lookup(T, hello)).

