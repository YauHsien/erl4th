-module(forth_lexer_test).
-include_lib("eunit\\include\\eunit.hrl").

word_scan_test() ->
    ?assertMatch(
	{{word, <<"hello,world">>}, <<>>},
	forth_lexical:scan(<<"hello,world">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{word, <<"hello;world">>}, <<>>},
	forth_lexical:scan(<<"hello;world">>, {undefined, <<>>})
    ).

int_scan_test() ->
    ?assertMatch(
        {{integer, <<"10">>}, <<" emit . ">>},
	forth_lexical:scan(<<"10 emit . ">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{integer, <<"5">>}, <<>>},
	forth_lexical:scan(<<"5">>, {undefined, <<>>})
    ).

float_scan_test() ->
    ?assertMatch(
        {{integer, <<"10.1">>}, <<" emit . ">>},
	forth_lexical:scan(<<"10.1 emit . ">>, {undefined, <<>>})
    ).

delim_scan_test() ->
    ?assertMatch(
        {{word, <<$\s>>}, <<>>},
	forth_lexical:scan(<<" ">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{delimiter, <<$;>>}, <<>>},
	forth_lexical:scan(<<";">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{word, <<";emit">>}, <<>>},
	forth_lexical:scan(<<";emit">>, {undefined, <<>>})
    ).

delim_colon_test() ->
    ?assertMatch(
        {{delimiter, <<$:>>}, <<>>},
	forth_lexical:scan(<<":">>, {undefined, <<>>})
    ).

delim_colon_ajacent_test() ->
    ?assertMatch(
        {{word, <<":emit">>}, <<>>},
	forth_lexical:scan(<<":emit">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{delimiter, <<$">>}, <<>>},
	forth_lexical:scan(<<"\"">>, {undefined, <<>>})
    ).

delim_dquote_ajacent_test() ->
    ?assertMatch(
        {{word, <<"\"emit">>}, <<>>},
	forth_lexical:scan(<<"\"emit">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{delimiter, <<".\"">>}, <<>>},
	forth_lexical:scan(<<".\"">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{word, <<".\"emit">>}, <<>>},
	forth_lexical:scan(<<".\"emit">>, {undefined, <<>>})
    ),
    ?assertMatch(
        {{word, <<".\"emit">>}, <<" .">>},
	forth_lexical:scan(<<".\"emit .">>, {undefined, <<>>})
    ).

scan_test() ->
    ?assertMatch(
        [{integer, 42},
	 {word, "emit"}
	],
	forth_lexical:scan(<<"42    emit">>)
    ).

define_scan_test() ->
    ?assertMatch(
        [{delimiter, ':'},
	 {word, "star"},
	 {word, "[char]"},
	 {integer, 42},
	 {word, "emit"},
	 {delimiter, ';'}],
        forth_lexical:scan(<<": star  [char] 42 emit ;">>)
    ),
    ?assertMatch(
        [{word, ".\"emit"}],
	forth_lexical:scan(<<".\"emit">>)
    ).

define_float_scan_test() ->
    ?assertMatch(
        [{delimiter, '."'},
         {word, "3.14h16"},
         {delimiter, '"'}],
	forth_lexical:scan(<<".\" 3.14h16 \"">>)
    ).

integer_test() ->
    ?assertMatch(
        [{integer, 1},
	 {integer, 1},
	 {integer, 10},
	 {integer, -1},
	 {integer, -1},
	 {integer, -10}
	],
	forth_lexical:scan(<<"1 1. 1.0 -1 -1. -1.0">>)
    ).

