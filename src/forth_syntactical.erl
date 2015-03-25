-module(forth_syntactical).
-export([scan/1, scan/2]).
-include("..\\include\\forth_syntactical.hrl").

-spec(scan([lexeme()]) -> {#state{}, [lexeme()]}).
scan(List) ->
    scan1(List, {#state{}, []}).

-spec(scan([lexeme()], tab()) -> {#state{}, [lexeme()], tab()}).
scan(List, Tab) ->
    {State, Result} = scan1(List, {#state{}, []}),
    {Tab1, Result1} = analyze(Tab, Result),
    {State, Result1, Tab1}.

analyze(Tab, Result) ->
    {Tab, Result}.

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

scan1([], {State, Acc}) ->
    {State, lists:reverse(Acc)};
scan1(List, {#state{status=incomplete}=State, Acc}) ->
    {State, lists:reverse(Acc)};
scan1([{delimiter, '."'}|List], {State, Acc}) ->
    case find_string(List) of
        {no_end, String}  ->
	    scan1(List, {State#state{type=string, status=incomplete, src=List,
	                 trg=String}, Acc});
        {String, [{delimiter, '"'}|Rest]} ->
	    scan1(Rest, {State, [{string, String}|Acc]})
    end;
scan1([Lexeme|Rest], {State, Acc}) ->
    scan1(Rest, {State, [Lexeme|Acc]}).

find_string(List) ->
    find_string(List, []).

find_string([], Acc) ->
    {no_end, string:join(lists:reverse(Acc), " ")};
find_string([{delimiter, '"'}|_] = List, Acc) ->
    {string:join(lists:reverse(Acc), " "), List};
find_string([{word, Word}|Rest], Acc) ->
    find_string(Rest, [Word|Acc]).

