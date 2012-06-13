REBAR = ./rebar
ERL ?= erl
APP := snowflake

.PHONY: deps

all: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

test: all
	@$(REBAR) skip_deps=true eunit

dialyzer: all
	dialyzer ebin -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs

typer: 
	typer src

full: all test dialyzer typer
