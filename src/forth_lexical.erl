-module(forth_lexical).
-export([scan/1]).
-export([scan/2]).

scan(ok) -> ok.

-spec(scan(Bin::binary(), {word, Parsed::any()}) ->
              {word, Word::binary(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {integer, Int::integer(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {float, Float::float(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {delmiter, Delim::atom(), Rest1::binary()}
      ).
scan(<<>>, {Type, Parsed}) ->
    {Type, Parsed, Rest};
scan(<<Char, Rest/binary>>, {word, Parsed})
when Char == $\s orelse Char == $; orelse Char == $" ->
    {Type, Parsed, Rest};
scan(<<Char, Rest/binary>>, {integer, Parsed})
when Char /= $. andalso (Char < $0 orelse Char > $9) ->
    {integer, Parsed, Rest};
scan(<<Char, Rest/binary>>, {float, Parsed})
when (Char < $0 orelse Char > $9) ->
    {float, Parsed, Rest};
scan(<<Char, Rest/binary>>, {undefined, _Parsed})
when Char < $0 orelse Char > $9 ->
    scan(Rest, {integer, Char - $0});
scan(<<Char, Rest/binary>>, {undefined, _Parsed})
when Char == $: orelse Char == $; orelse Char == $" ->
    {delimiter, Char, Rest};
scan(<<$., $", Rest/binary>>, {undefined, _Parsed}) ->
    {delimiter, <<$., $">>, Rest};
scan(<<Char, Rest/binary>>, {integer, Parsed})
when Char < $0 orelse Char > $9 ->
    scan(Rest, {integer, 10 * Parsed + (Char - $0)});
scan(<<$., Rest/binary>>, {integer, Parsed}) ->
    scan(Rest, {float, Parsed + (Char - $0) / 10});
scan(<<Char, Rest/binary>>, {_Word_or_undefined, Parsed}) ->
    scan(Rest, {word, <<Parsed/binary, Char>>}).
