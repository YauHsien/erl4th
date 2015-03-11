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
        [":","star","[char]","*","emit",";"],
	forth:load("..\\ebin\\in.txt")).

file_load1_test() ->
    io:fwrite("~tp~n", [forth:load("..\\ebin\\test.f")]),
    ?assertMatch(
        ["fload","FileID1.f","create","cyn","1024","allot",":","test","(","--",")",
         "s\"","4743_20130503.txt\"","fid1",
         [27284,21517],
         "+","lplace",
         [38283,27284,49],
         "\\",
         [35712,27284,49],
         "\\","fid1",
         [27284,26696,36039,26009],
         "+","fid1",
         [27284,26696,35712,21462,38263,24230],
         "+","@","dump","cr","100","0","do","cyn","1024","fid1",
         [27284,38957,24207,34399],
         "+","@","read-line","drop","drop","cyn","swap","type","cr","loop",";","test"],
	forth:load("..\\ebin\\test.f")).

