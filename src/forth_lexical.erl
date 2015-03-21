-module(forth_lexical).
-export([scan/1]).
-export([scan/2]).

%% type annotation
-define(initial, {undefined, <<>>}).
-define(word(X), {word, X}).
-define(integer(X), {integer, X}).
-define(float(X), {float, X}).
-define(delimiter(X), {delimiter, X}).

%% specific type
                              % +/-
-define(float_1, {float, 1}). % w-- {+ -}; only + is implemented
-define(float_2, {float, 2}). % w-- digits
-define(float_3, {float, 3}). % w-- .
-define(float_4, {float, 4}). % w-- digits
-define(float_5, {float, 5}). % fff {e E}
-define(float_6, {float, 6}). % fff {+ -} optional
-define(float_7, {float, 7}). % fff digits optional

                                  % /-
-define(integer_1, {integer, 1}). % -w -
-define(integer_2, {integer, 2}). % ii digits
-define(integer_3, {integer, 3}). % ii . optional
-define(integer_4, {integer, 4}). % ii digits optional

%% pattern
-define(all_beginning, {undefined, _}).
-define(some_float_type(X), {{float, _}, X}).
-define(some_integer_type(X), {{integer, _}, X}).
-define(some_word_type(X), {word, X}).
-define(some_delimiter_type(X), {delimiter, X}).
-define(some_malform_type(X), {malform, X}).
-define(some_type(X), {_, X}).

%% assertion
-define(is_digit(X), (X >= $0 andalso X =< $9)).
-define(not_ditit(X), (X < $0 orelse X > $9)).
-define(is_sign(X), (X == $+ orelse X == $-)).
-define(is_letter_e(X), (X == $e orelse X == $E)).

scan(Bin) ->
    scan_1(Bin, []).

scan_1(<<>>, Acc) ->
    lists:reverse(Acc);
scan_1(Bin, Acc) ->
    {{Type, Parsed}, Rest} = scan(eat(Bin), ?initial),
    Term = case Type of
               word -> binary:bin_to_list(Parsed);
	       integer -> to_integer(Parsed);
	       float -> to_float(Parsed);
	       delimiter -> to_delimiter(Parsed)
           end,
    scan_1(Rest, [{Type, Term}|Acc]).

-spec(scan(Bin::binary(), {word, Parsed::any()}) ->
              {{word, Word::term()}, Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {{integer, Int::integer()}, Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {{float, Float::float()}, Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {{delmiter, Delim::atom()}, Rest1::binary()}
      ).
%% base case met...
scan(<<>>, ?all_beginning) ->
    {?delimiter(eof), <<>>};
scan(<<>>, ?some_word_type(Parsed)) ->
    {?word(Parsed), <<>>};
scan(<<>>, {?integer_1, Parsed}) ->
    {?word(Parsed), <<>>};
scan(<<>>, ?some_integer_type(Parsed)) ->
    {?integer(Parsed), <<>>};
scan(<<>>, {TypeFloat, Parsed})
when TypeFloat == ?float_1 orelse TypeFloat == ?float_2
orelse TypeFloat == ?float_3 orelse TypeFloat == ?float_4 ->
    {?word(Parsed), <<>>};
scan(<<>>, ?some_float_type(Parsed)) ->
    {?float(Parsed), <<>>};
scan(<<>>, ?some_delimiter_type(Parsed)) ->
    {?delimiter(Parsed), <<>>};
%% space met...
scan(<<$\s, _/binary>> = Bin, ?some_word_type(Parsed)) ->
    case binary:last(Parsed) of
        $" ->
	    Len = binary:byte_size(Parsed),
	    Char = binary:part(Parsed, {Len -1}),
	    {?word(binary:part(Parsed, 0, Len - 1)), <<Char:8/integer, Bin/binary>>};
	_ ->
            {?word(Parsed), Bin}
    end;
scan(<<$\s, _/binary>> = Bin, {?integer_1, Parsed}) ->
    {?word(Parsed), Bin};
scan(<<$\s, _/binary>> = Bin, ?some_integer_type(Parsed)) ->
    {?integer(Parsed), Bin};
scan(<<$\s, _/binary>> = Bin, {TypeFloat, Parsed})
when TypeFloat == ?float_1 orelse TypeFloat == ?float_2
orelse TypeFloat == ?float_3 orelse TypeFloat == ?float_4 ->
    {?word(Parsed), Bin};
scan(<<$\s, _/binary>> = Bin, ?some_float_type(Parsed)) ->
    {?float(Parsed), Bin};
scan(<<$\s, _/binary>> = Bin, ?some_delimiter_type(Parsed)) ->
    {?delimiter(Parsed), Bin};
%% initial symbol...
scan(<<$., $", Rest/binary>>, ?all_beginning) ->
    scan(Rest, ?delimiter(<<$., $">>));
scan(<<$:, Rest/binary>>, ?all_beginning) ->
    scan(Rest, ?delimiter(<<$:>>));
scan(<<$", Rest/binary>>, ?all_beginning) ->
    scan(Rest, ?delimiter(<<$">>));
scan(<<$;, Rest/binary>>, ?all_beginning) ->
    scan(Rest, ?delimiter(<<$;>>));
scan(<<$+, Rest/binary>>, ?all_beginning) ->
    scan(Rest, {?float_1, <<$+>>});
scan(<<$-, Rest/binary>>, ?all_beginning) ->
    scan(Rest, {?integer_1, <<$->>});
scan(<<Char, Rest/binary>>, ?all_beginning)
when ?is_digit(Char) ->
    scan(Rest, {?integer_2, <<Char:8/integer>>});
%% midterm met...
scan(<<Char, Rest/binary>>, {?integer_1, Parsed})
when ?is_digit(Char) ->
    scan(Rest, {?integer_2, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {?integer_2, Parsed})
when ?is_digit(Char) ->
    scan(Rest, {?integer_2, <<Parsed/binary, Char:8/integer>>});
scan(<<$., Rest/binary>>, {?integer_2, Parsed}) ->
    scan(Rest, {?integer_3, <<Parsed/binary, $.>>});
scan(<<Char, Rest/binary>>, {?integer_3, Parsed})
when ?is_digit(Char) ->
    scan(Rest, {?integer_4, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {?integer_4, Parsed})
when Char == $e orelse Char == $E ->
    scan(Rest, {?float_5, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {?float_1, Parsed})
when ?is_digit(Char) ->
    scan(Rest, {?float_2, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {?float_2, Parsed})
when ?is_digit(Char) ->
    scan(Rest, {?float_3, <<Parsed/binary, Char:8/integer>>});
scan(<<$., Rest/binary>>, {?float_2, Parsed}) ->
    scan(Rest, {?float_3, <<Parsed/binary, $.>>});
scan(<<Char, Rest/binary>>, {?float_3, Parsed})
when ?is_digit(Char) ->
    scan(Rest, {?float_4, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {?float_4, Parsed})
when Char == $e orelse Char == $E ->
    scan(Rest, {?float_5, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {?float_5, Parsed})
when ?is_sign(Char) ->
    scan(Rest, {?float_6, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {?float_6, Parsed})
when ?is_digit(Char) ->
    scan(Rest, {?float_7, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {_Type, Parsed}) ->
    scan(Rest, {word, <<Parsed/binary, Char:8/integer>>}).

eat(<<$\s, Rest/binary>>) ->
    eat(Rest);
eat(Bin) ->
    Bin.

to_integer(Bin) ->
    {Int, _} = string:to_integer(
                   lists:filter(fun(X)-> X /= $. end,
                       binary:bin_to_list(Bin)
               )),
    Int.

to_float(Bin) ->
    to_float(binary:bin_to_list(Bin), undefined, {0, 0}).

to_float([], _, {Base, Exp}) ->
    Base * math:pow(10, Exp);
to_float([Char|List], _, {Base, Exp}) when ?is_sign(Char) ->
    to_float(List, base, {Base, Exp});
to_float([Char|List], _, {Base, Exp}) when ?is_letter_e(Char) ->
    to_float(List, exp, {Base, Exp});
to_float([$.|List], Dir, {Base, Exp}) ->
    to_float(List, Dir, {Base, Exp});
to_float([Char|List], base, {Base, Exp}) ->
    to_float(List, base, {Base * 10 + Char - $0, Exp});
to_float([Char|List], exp, {Base, Exp}) ->
    to_float(List, exp, {Base, Exp * 10 + Char - $0}).

to_delimiter(Bin) ->
    list_to_atom(binary:bin_to_list(Bin)).
