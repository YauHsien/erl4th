.phony: all unit

all:
	rebar compile

unit:
	rebar eunit
