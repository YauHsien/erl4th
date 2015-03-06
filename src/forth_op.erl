-module(forth_op).

%% Forth built-in pperators
-export([ plus/1,
	  minus/1,
	  multiply/1,
	  divide/1,
	  bit_and/1,
	  bit_or/1
	  ]).

plus([B,A|Stack]) ->
    [A+B|Stack];
plus(Stack) ->
    io:fwrite("~p would not be accepted by +~n",
              [lists:reverse(Stack)]).

minus([B,A|Stack]) ->
    [A-B|Stack];
minus(Stack) ->
    io:fwrite("~p would not be accepted by -~n",
              [lists:reverse(Stack)]).

multiply([B,A|Stack]) ->
    [A*B|Stack];
multiply(Stack) ->
    io:fwrite("~p would not be accepted by *~n",
              [lists:reverse(Stack)]).

divide([B,A|Stack]) ->
    [A/B|Stack];
divide(Stack) ->
    io:fwrite("~p would not be accepted by /~n",
              [lists:reverse(Stack)]).

bit_and([B,A|Stack]) ->
    [A and B|Stack];
bit_and(Stack) ->
    io:fwrite("~p would not be accepted by AND~n",
              [lists:reverse(Stack)]).

bit_or([B,A|Stack]) ->
    [A or B|Stack];
bit_or(Stack) ->
    io:fwrite("~p would not be accepted by OR~n",
              [lists:reverse(Stack)]).
