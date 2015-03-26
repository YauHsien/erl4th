-module(forth_syntactical).
-export([scan/1, scan/2]).
-include("..\\include\\forth_syntactical.hrl").

-spec(scan([lexeme()]) -> {#state{}, [lexeme()]}).
scan(List) ->
    List1 = lists:map(fun({Type, Value}) -> {Type, Value, -1} end, List),
    scan1(List1, {#state{}, []}).

-spec(scan([lexeme()], tab()) -> {#state{}, [lexeme()]}).
scan(List, Tab) ->
    List1 = lists:map(fun({Type, Value}) -> {Type, Value, -1} end, List),
    {State, Result} = scan1(List1, {#state{}, []}),
    Result1 = analyze(Tab, Result),
    {State, Result1}.

-spec(analyze(tab(), [lexeme()]) -> [lexeme()]).
analyze(Tab, [{delimiter, ':', _Arity}, {word, Name, _Arity1}|Rest] = List) ->
    List1 = lists:takewhile(fun({delimiter, ';', _A}) -> false;
			       ({_Type, _Value, _A}) -> true
			    end, Rest),
    List2 = lists:dropwhile(fun({delimiter, ';', _A}) -> false;
			       ({_Type, _Value, _A}) -> true
			    end, Rest),
    case {List1, List2} of
	{_, []} ->
	    List;
	{_, [_|List3]} ->
	    {Arity2, List4} = find_arity(List1, Tab),
	    push_dict(Tab, {Name, {Arity2, List4}}),
	    List3
    end.

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
scan1([{delimiter, '."', _Arity}|List], {State, Acc}) ->
    case find_string(List) of
        {no_end, String}  ->
	    scan1(List, {State#state{type=string, status=incomplete, src=List,
	                 trg=String}, Acc});
        {String, [{delimiter, '"', _Arity1}|Rest]} ->
	    scan1(Rest, {State, [{string, String, -1}|Acc]})
    end;
scan1([Lexeme|Rest], {State, Acc}) ->
    scan1(Rest, {State, [Lexeme|Acc]}).

find_string(List) ->
    find_string(List, []).

-spec(find_string([lexeme()], list()) -> {no_end, string()};
                 ([lexeme()], list()) -> {string, [lexeme()]}).
find_string([], Acc) ->
    {no_end, string:join(lists:reverse(Acc), " ")};
find_string([{delimiter, '"', _}|_] = List, Acc) ->
    {string:join(lists:reverse(Acc), " "), List};
find_string([{word, Word, _Arity}|Rest], Acc) ->
    find_string(Rest, [Word|Acc]).

-spec(find_arity([lexeme()], tab()) -> {arity(), [lexeme()]}).
find_arity(List, Tab) ->
    find_arity(List, {0, []}, Tab).

-spec(find_arity([lexeme()], {integer(), [lexeme()]}, tab()) -> {arity(), [lexeme()]}).
find_arity([], {N, Acc}, _Tab) ->
    {-N, lists:reverse(Acc)};
find_arity([{word, Word, -1}|List], {N, Acc}, Tab) ->
    M = forth_symtable:find_arity(Word),
    M1 = case ets:lookup(Tab, Word) of
	     [] -> -1;
	     [{_Name, {Arity, _Value}}|_] -> Arity
	 end,
    if M > 0 ->
	    find_arity(List, {N + M, [{word, Word, M}|Acc]}, Tab);
       M1 > 0 ->
	    find_arity(List, {N + M1, [{word, Word, M1}|Acc]}, Tab);
       true ->
	    find_arity(List, {N - 1, [{word, Word, -1}|Acc]}, Tab)
    end;
find_arity([{Type, Value, -1}|List], {N, Acc}, Tab) ->
    find_arity(List, {N - 1, [{Type, Value, -1}|Acc]}, Tab).

-spec(push_dict(tab(), {string(), any()}) -> ok | {error, dup}).
push_dict(Tab, {Key, Value}) ->
    case ets:lookup(Tab, Key) of
	[] ->
	    ets:insert(Tab, {Key, Value}),
	    ok;
	_ ->
	    {error, dup}
    end.


