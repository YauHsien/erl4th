-module(forth_syntactical_test).
-include_lib("eunit\\include\\eunit.hrl").
-include("..\\include\\forth_syntactical.hrl").

string_test() ->
    ?assertMatch(
        {_, [{string, "hello, world"}]},
        forth_syntactical:scan(forth_lexical:scan(<<".\" hello, world \"">>))
    ).

analysis_test() ->
    {#state{}, Code, Tab} =
	forth_syntactical:scan(forth_lexical:scan(<<": star 42 emit ;">>),
	                       ets:new(table, [set])),
    ?assertMatch(
        [{"star", {0, [{_,_}|_]}}],
	ets:lookup(Tab, "star")
    ).
