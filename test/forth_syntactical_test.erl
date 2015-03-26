-module(forth_syntactical_test).
-include_lib("eunit\\include\\eunit.hrl").
-include("..\\include\\forth_syntactical.hrl").

string_test() ->
    ?assertMatch(
        {_, [{string, "hello, world", -1}]},
        forth_syntactical:scan(forth_lexical:scan(<<".\" hello, world \"">>))
    ).

analysis_test() ->
    forth_symtable:start(),
    Tab = ets:new(table, [set]),
    {#state{}, _Code} =
	forth_syntactical:scan(forth_lexical:scan(<<": star 42 emit ;">>),
	                       Tab),
    %% match {name::string(), {arity(), [lexeme()]}}
    ?assertMatch(
        [{"star", {0, [{_,_}|_]}}],
	ets:lookup(Tab, "star")
    ).
