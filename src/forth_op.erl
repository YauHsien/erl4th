-module(forth_op).

%% Forth built-in pperators
-export([ plus/2,
	  minus/2,
	  multiply/2,
	  divide/2,
	  bit_and/2,
	  bit_or/2,
	  emit/2
	  ]).

plus([B,A|Stack], Acc) ->
    {ok, [A+B|Stack], [A,B|Acc]};
plus(_, _) ->
    {error, -4}.

minus([B,A|Stack], Acc) ->
    {ok, [A-B|Stack], [A,B|Acc]};
minus(_, _) ->
    {error, -4}.

multiply([B,A|Stack], Acc) ->
    {ok, [A*B|Stack], [A,B|Acc]};
multiply(_, _) ->
    {error, -4}.

divide([B,A|Stack], Acc) ->
    {ok, [A/B|Stack], [A,B|Acc]};
divide(_, _) ->
    {error, -4}.

bit_and([B,A|Stack], Acc) ->
    {ok, [A and B|Stack], [A,B|Acc]};
bit_and(_, _) ->
    {error, -4}.

bit_or([B,A|Stack], Acc) ->
    {ok, [A or B|Stack], [A,B|Acc]};
bit_or(_, _) ->
    {error, -4}.

-spec(emit(Stack::list(), StackAcc::list())
          -> {ok, Stack1::list(), Stack2::list()};
          (Stack::list(), StackAcc::list())
          -> {error, ErrorNum::integer()}).
emit([A|Stack], Acc) ->
    try
        io:fwrite(" ~c", [A]),
        {ok, Stack, [A|Acc]}
    catch _:_ ->
        {error, -4}
    end;
emit(_, _) ->
    {error, -4}.
