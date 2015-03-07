-module(forth).
-export([cr/2]).
-export([find_code/1]).

%% cr/2: to run a list of input over a stack.
-spec cr(Input::list(), {Dict::tuple(), Stack::list()}) -> {Dict1::tuple(), Stack1::list()}.
cr([], {Dict, Stack}) ->
    {Dict, Stack};
cr([Num|Input], {Dict, Stack}) when is_number(Num) ->
    cr(Input, {Dict, [Num|Stack]});
cr([':'|Input], {Dict, Stack}) ->
    {Name, Code, Input1} = find_code(Input),
    case Code of
        undefined ->
            io:fwrite("~p would not be read as a code definition,"
                     ++ " because of lack of definition part ~n",
                     [[':', Name, '...'|Input1]]);
        _ ->
            ets:insert(Dict, {Name, Code})
    end,
    cr(Input1, {Dict, Stack});
cr([Word|Input], {Dict, Stack}) when is_atom(Word) ->
    case find_op(Word) of
        undefined ->
            case ets:lookup(Dict, Word) of
                [Code|_] ->
                    cr(Input, cr(Code, {Dict, Stack}));
                [] ->
                    cr(Input, {Dict, [Word|Stack]})
            end;
        Op ->
            cr(Input, {Dict, apply(forth_op, Op, [Stack])})
    end;
cr([Str|Input], {Dict, Stack}) ->
    cr(Input, {Dict, [Str|Stack]}).

find_op('+') -> plus;
find_op('-') -> minus;
find_op('*') -> multiply;
find_op('/') -> divide;
find_op('and') -> bit_and;
find_op('or') -> bit_or1;
find_op(_) -> undefined.

find_code([':'|Input]) ->
    find_code(Input);
find_code(Input) ->
    find_code(Input, {undefined, []}).

find_code([], {Name, Code}) ->
    {Name, undefined, lists:reverse(Code)};
find_code([Name|Input], {undefined, Code}) ->
    find_code(Input, {Name, Code});
find_code([';'|Input], {Name, []}) ->
    {Name, undefined, Input};
find_code([';'|Input], {Name, Code}) ->
    {Name, lists:reverse(Code), Input};
find_code([Code|Input], {Name, Code1}) ->
    find_code(Input, {Name, [Code|Code1]}).
