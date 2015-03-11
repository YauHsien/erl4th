-module(forth).
-export([load/1, cr/2]).
-export([find_code/1]).

load(File) ->
    lexer(file:read_file(File)).

lexer({ok, Bin}) when is_binary(Bin) ->
    lists:map(fun(X) -> unicode:characters_to_list(X, unicode) end,
        lists:filter(fun(X) -> X /= <<>> end,
            binary:split(
                binary:replace(Bin, <<"\r\n">>, <<" ">>, [global]),
                <<" ">>, [global]))).

%% cr/2: to run a list of input over a stack.
-spec cr(Input::list(), {Dict::tuple(), StackAcc::list(), Stack::list()})
        -> { Dict1::tuple(),
             StackAcc1::list(),
             Stack1::list()
           };
        (Input::list(), {Dict::tuple(), Stack::list()})
        -> { Dict1::tuple(),
             StackAcc1::list(),
             Stack1::list()
           }.
%% interface for the initial
cr(Input, {Dict, Stack}) ->
    cr(Input, {Dict, [], Stack});
%% to get result
cr([], {Dict, Acc, Stack}) ->
    {Dict, Acc, Stack};
%% to execute number
cr([Num|Input], {Dict, Acc, Stack}) when is_number(Num) ->
    cr(Input, {Dict, [Num|Acc], [Num|Stack]});
%% to do definition
cr([':'|Input], {Dict, Acc, Stack}) ->
    case find_code(Input) of
        {Name, Code, Input1} ->
            ets:insert(Dict, {Name, Code}),
            cr(Input1, {Dict, Acc, Stack});
        undefined ->
            {Dict, Acc, Stack}
    end;
%% to execute built-in or defined word
cr([Word|Input], {Dict, Acc, Stack}) when is_atom(Word) ->
    case find_op(Word) of
        undefined ->
            case ets:lookup(Dict, Word) of
                [{_Name, Code}|_] ->
                    cr(Input, cr(Code, {Dict, Acc, Stack}));
                [] ->
                    cr(Input, {Dict, [Word|Acc], [Word|Stack]})
            end;
        Op ->
            case apply(forth_op, Op, [Stack, Acc]) of
                {ok, Stack1, Acc1} ->
                    cr(Input, {Dict, Acc1, Stack1});
                {error, ErrNum} ->
                    forth_err:print(ErrNum, Word,
                                    { lists:reverse(Acc),
                                      Op,
                                      Input
                                    }
                                   )
            end
    end;
%% to execute string
cr([Str|Input], {Dict, Acc, Stack}) ->
    cr(Input, {Dict, Acc, [Str|Stack]}).

find_op('+') -> plus;
find_op('-') -> minus;
find_op('*') -> multiply;
find_op('/') -> divide;
find_op('and') -> bit_and;
find_op('or') -> bit_or1;
find_op(emit) -> emit;
find_op(_) -> undefined.

find_code([':'|Input]) ->
    find_code(Input);
find_code(Input) ->
    find_code(Input, {undefined, []}).

find_code([], {_Name, _Code}) ->
    undefined;
find_code(['[char]',Char|Input], {Name, Code}) ->
    find_code(Input, {Name, [Char|Code]});
find_code([Name|Input], {undefined, Code}) ->
    find_code(Input, {Name, Code});
find_code([';'|Input], {Name, Code}) ->
    {Name, lists:reverse(Code), Input};
find_code([Code|Input], {Name, Code1}) ->
    find_code(Input, {Name, [Code|Code1]}).
