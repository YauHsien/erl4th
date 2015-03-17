-module(forth_lexical).
-export([scan/1]).
-export([scan/2]).

scan(Bin) ->
    scan_1(Bin, []).

scan_1(<<>>, Acc) ->
    lists:reverse(Acc);
scan_1(Bin, Acc) ->
    {Type, Parsed, Rest} = scan(eat(Bin), base()),
    scan_1(Rest, [{Type, Parsed}|Acc]).

base() -> {undefined, <<>>}.

-spec(scan(Bin::binary(), {word, Parsed::any()}) ->
              {word, Word::binary(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {integer, Int::integer(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {float, Float::float(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {delmiter, Delim::atom(), Rest1::binary()};
	  (Bin::binary(), {Type::atom(), Parsed::any()}) ->
	      {syntex_error, any(), Rest1::binary()}
      ).
scan(<<>>, {undefined, _Parsed}) ->
    {delimiter, eof, <<>>};
scan(<<>>, {word, Parsed}) ->
    {word, binary:bin_to_list(Parsed), <<>>};
scan(<<>>, {integer, Parsed}) ->
    {integer, Parsed, <<>>};
scan(<<>>, {float, {Parsed, Magnitude}}) ->
    {float, Parsed * math:pow(10, -Magnitude), <<>>};
scan(<<>>, {delimiter, Parsed}) ->
    {delimiter, Parsed, <<>>};
scan(<<>>, {syntax_error, Parsed}) ->
    {syntax_error, binary:bin_to_list(Parsed), <<>>};
scan(<<$., $", _/binary>> = Bin, {word, Parsed}) ->
    {word, binary:bin_to_list(Parsed), Bin};
scan(<<Char, _/binary>> = Bin, {word, Parsed})
when Char == $\s orelse Char == $: orelse Char == $; orelse Char == $" ->
    {word, binary:bin_to_list(Parsed), Bin};
scan(<<Char, _/binary>> = Bin, {integer, Parsed})
when Char == $\s orelse Char == $: orelse Char == $; orelse Char == $" orelse
     (Char /= $. andalso (Char < $0 orelse Char > $9)) ->
    {integer, Parsed, Bin};
scan(<<Char, _/binary>> = Bin, {float, {Parsed, Magnitude}})
when (Char < $0 orelse Char > $9) ->
    {float, Parsed * math:pow(10, -Magnitude), Bin};
scan(<<Char, _/binary>> = Bin, {delimiter, Parsed})
when Char /= $\s ->
    {delimiter, Parsed, Bin};
scan(<<Char, Rest/binary>>, {syntax_error, Parsed})
when Char == $\s ->
    {syntax_error, binary:bin_to_list(Parsed), Rest};
scan(<<Char, Rest/binary>>, {undefined, _Parsed})
when Char >= $0 andalso Char =< $9 ->
    scan(Rest, {integer, Char - $0});
scan(<<Char, Rest/binary>>, {undefined, _Parsed})
when Char == $\s orelse Char == $: orelse Char == $; orelse Char == $" ->
    {delimiter, Char, Rest};
scan(<<$., $">> = Bin, {undefined, _Parsed}) ->
    {delimiter, Bin, <<>>};
scan(<<$., $", $\s, Rest/binary>>, {undefined, _Parsed}) ->
    {delimiter, <<$., $">>, Rest};
scan(<<Char, _/binary>> = Bin, {delimiter, Parsed})
when Char == $\s ->
    {delimiter, Parsed, Bin};
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
when (Type == undefined orelse Type == word) andalso Char /= $\s andalso
     Char /= $: andalso Char /= $" andalso Char /= $; andalso Char /= $. ->
    scan(Rest, {word, <<Parsed/binary, Char:8/integer>>});
scan(<<Char, Rest/binary>>, {Type, Parsed})
when Type == undefined orelse Type == syntax_error ->
    scan(Rest, {syntax_error, <<Parsed/binary, Char:8/integer>>}).

eat(<<$\s, Rest/binary>>) ->
    eat(Rest);
eat(Bin) ->
    Bin.
