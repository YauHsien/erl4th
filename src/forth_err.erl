-module(forth_err).
-export([print/3]).

-spec(print(ErrNum::integer(), Arg::any(),
            {Stack1::list(), Word::any(), Stack2::list()})
           -> ok).
print(-4, Thing, {Stack1, Word, Stack2}) ->
    io:fwrite("~p ~p ~p~nError(~B): ~w stack underflow~n",
              [Stack1, Word, Stack2, -4, Thing]).

