-module(forth_syntactical).
-export([scan/1]).

-record(state, {type, status, src, trg}).

scan(List) ->
    {#state{}, Result} = scan(List, {#state{}, []}),
    Result.

%% <Code> ::= <String> || <If>

%% <String> ::= ." <In-string><*> "

%% <In-string> ::= <word> || <Integer> || <Float> || <delimiter>

%% <If> ::= <Condition> if <Code> then
%%       || <Condition> if <Code> else <Code> then

%% <word> ::= <alphabet><+> <

%% <Intger> ::= <Not-plus> <Unsigned>

%% <Unsigned> ::= <Digits>
%%             || <Digits> .
%%             || <Digits> . <Digits>

%% <Float> ::= <Sign> <Unsigned> <Exp> <Digits>

%% <delimiter> ::= ." || " || ;

%% <definition> ::= :

%% <alphabet> ::= a -- z || A -- Z

%% <Letter> ::= <alphabet> || <digits> || <delimiter> || <definition>
%%           || <Sign> || <misc-letter>

%% <Digits> ::= <Digit> <Digit><+>

%% <Sign> ::= + || <Not-plus>

%% <Not-plus> ::= - || <Nothing>

%% <Digit> ::= 0 || 1 || 2 || 3 || 4 || 5 || 6 || 7 || 8 || 9

%% <misc-letter> ::= ~!@#$%^&*()_+{}|:"<>?`-=[]\;',./

%% <Nothing> ::= 

scan([], {State, Acc}) ->
    {State, lists:reverse(Acc)};
scan([{delimiter, '."'}|List], {State, Acc}) ->
    case find_string(List) of
        {no_end, String}  ->
	    scan([], {State#state{type=string, status=incomplete, src=List,
	              trg=String}, Acc});
        {String, [{delimiter, '"'}|Rest]} ->
	    scan(Rest, {State, [{string, String}|Acc]})
    end.

find_string(List) ->
    find_string(List, []).

find_string([], Acc) ->
    {no_end, string:join(lists:reverse(Acc), " ")};
find_string([{delimiter, '"'}|_] = List, Acc) ->
    {string:join(lists:reverse(Acc), " "), List};
find_string([{word, Word}|Rest], Acc) ->
    find_string(Rest, [Word|Acc]).

