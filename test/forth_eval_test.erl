-module(forth_eval_test).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    ?assertEqual("hello,world", forth_eval:run([], [], [], "hello,world")),
    ?assertEqual("hello,world", forth_eval:run([42], [], [], "hello,world")).
