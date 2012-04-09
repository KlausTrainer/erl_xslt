all:
	@rebar compile

check: all
	@rebar eunit 2> /dev/null

doc:
	@rebar doc

clean:
	@rebar clean
