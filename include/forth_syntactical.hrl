-type type() :: atom().
-type value() :: atom() | list() | integer() | float().
-type lexeme() :: {type(), value()}.
-type tab() :: atom().

-record(state, {type, status, src, trg}).
