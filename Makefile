.PHONY: compile test format check-format clean

compile:
	@rebar3 compile

test:
	@rebar3 eunit

format:
	@rebar3 fmt

check-format:
	@rebar3 fmt --check

clean:
	@rebar3 clean
