-module(forth_lexical).
-export([scan/1]).
-export([scan/2]).

scan(ok) -> ok.

base() -> {undefined, <<>>}.

-spec(scan(Bin::binary(), {word, Parsed::any()}) ->
              {word, Word::binary(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {integer, Int::integer(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {float, Float::float(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {delmiter, Delim::atom(), Rest1::binary()}
      ).
scan(<<>>, {word, Parsed}) ->
    {word, binary:bin_to_list(Parsed), <<>>};
scan(<<>>, {integer, Parsed}) ->
    {integer, Parsed, <<>>};
scan(<<>>, {float, {Parsed, Magnitude}}) ->
    {float, Parsed * math:pow(10, -Magnitude), <<>>};
scan(<<Char, _/binary>> = Bin, {word, Parsed})
when Char == $\s orelse Char == $; orelse Char == $" ->
    {word, binary:bin_to_list(Parsed), Bin};
scan(<<Char, _/binary>> = Bin, {integer, Parsed})
when Char == $\s orelse Char == $: orelse Char == $; orelse Char == $" orelse
     (Char /= $. andalso (Char < $0 orelse Char > $9)) ->
    {integer, Parsed, Bin};
scan(<<Char, _/binary>> = Bin, {float, {Parsed, Magnitude}})
when (Char < $0 orelse Char > $9) ->
    {float, Parsed * math:pow(10, -Magnitude), Bin};
scan(<<Char, Rest/binary>>, {undefined, _Parsed})
when Char >= $0 andalso Char =< $9 ->
    scan(Rest, {integer, Char - $0});
scan(<<Char, Rest/binary>>, {undefined, _Parsed})
when Char == $\s orelse Char == $: orelse Char == $; orelse Char == $" ->
    {delimiter, Char, Rest};
scan(<<$., $", Rest/binary>>, {undefined, _Parsed}) ->
    {delimiter, <<$., $">>, Rest};
scan(<<$., Rest/binary>>, {integer, Parsed}) ->
    scan(Rest, {float, {Parsed, 0}});
scan(<<Char, Rest/binary>>, {integer, Parsed})
when Char < $0 orelse Char > $9 ->
    scan(Rest, {integer, 10 * Parsed + (Char - $0)});
scan(<<Char, Rest/binary>>, {integer, Parsed})
when Char >= $0 andalso Char =< $9 ->
    scan(Rest, {integer, Parsed * 10 + (Char - $0)});
scan(<<Char, Rest/binary>>, {float, {Parsed, Magnitude}}) ->
    scan(Rest, {float, {Parsed * 10 + (Char -$0), Magnitude + 1}});
scan(<<Char, Rest/binary>>, {Type, Parsed})
when Type == word orelse Type == undefined ->
    scan(Rest, {word, <<Parsed/binary, Char:8/integer>>}).
