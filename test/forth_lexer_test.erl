-module(forth_lexer_test).
-include_lib("eunit\\include\\eunit.hrl").

word_scan_test() ->
    ?assertMatch(
	{word, "hello,world", <<>>},
	forth_lexical:scan(<<"hello,world">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {word, "hello", <<";world">>},
	forth_lexical:scan(<<"hello;world">>, {undefined, <<>>})
    ).

int_scan_test() ->
    ?assertMatch(
        {integer, 10, <<" emit . ">>},
	forth_lexical:scan(<<"10 emit . ">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {integer, 5, <<>>},
	forth_lexical:scan(<<"5">>, {undefined, <<>>})
    ).

float_scan_test() ->
    {float, F1, <<" emit . ">>} =
	forth_lexical:scan(<<"10.1 emit . ">>, {undefined, <<>>}),
    {float, F2, <<>>} =
	forth_lexical:scan(<<"5.9">>, {undefined, <<>>}),
	io:fwrite("~p~n", [{F1, F2}]),
    ?assert(((F1 - 10.1 < 0.00000001) and (F1 - 10.1 > -0.000000001))),
    ?assert(((F2 - 5.9 < 0.00000001) and (F2 - 5.9 > -0.0000000001))).

delim_scan_test() ->
    ?assertMatch(
        {delimiter, $\s, <<>>},
	forth_lexical:scan(<<" ">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, $\s, <<"emit">>},
	forth_lexical:scan(<<" emit">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, $;, <<>>},
	forth_lexical:scan(<<";">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, $;, <<"emit">>},
	forth_lexical:scan(<<";emit">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, $:, <<>>},
	forth_lexical:scan(<<":">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, $:, <<"emit">>},
	forth_lexical:scan(<<":emit">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, $", <<>>},
	forth_lexical:scan(<<"\"">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, $", <<"emit">>},
	forth_lexical:scan(<<"\"emit">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, <<".\"">>, <<>>},
	forth_lexical:scan(<<".\"">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {delimiter, <<".\"">>, <<"emit">>},
	forth_lexical:scan(<<".\"emit">>, {undefined, <<>>})
    ).
