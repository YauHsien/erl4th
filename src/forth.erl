-module(forth).
-export([cr/2]).

%% cr/2: to run a list of input over a stack.
-spec cr(Input::list(), {Dict::tuple(), Stack::list()}) -> {Dict1::tuple(), Stack1::list()}.
cr([], {Dict, Stack}) ->
    {Dict, Stack};
cr([Num|Input], {Dict, Stack}) when is_number(Num) ->
    cr(Input, {Dict, [Num|Stack]});
cr([Word|Input], {Dict, Stack}) ->
    case find_op(Word) of
        undefined ->
            case ets:lookup(Dict, Word) of
                [Block|_] ->
                    cr(Input, cr(Block, {Dict, Stack}));
                [] ->
                    cr(Input, {Dict, [Word|Stack]})
            end;
        Op ->
            cr(Input, {Dict, apply(forth_op, Op, [Stack])})
    end.

find_op($+) -> plus;
find_op($-) -> minus;
find_op($*) -> multiply;
find_op($/) -> divide;
find_op('and') -> bit_and;
find_op('or') -> bit_or;
find_op(_) -> undefined.
