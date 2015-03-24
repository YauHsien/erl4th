-module(forth_syntactical_test).
-include_lib("eunit\\include\\eunit.hrl").

string_test() ->
    ?assertMatch(
        [{string, "hello, world"}],
        forth_syntactical:scan(forth_lexical:scan(<<".\" hello, world \"">>))
    ).
