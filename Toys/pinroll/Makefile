.PHONY: deps

all: compile
	rebar skip_deps=true escriptize

deps:
	rebar get-deps

compile: deps
	@(rebar compile)

# From http://blog.erlware.org/2013/06/04/universal-makefile-for-erlang-projects-that-use-rebar/
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib ssl

# Necessary (in place of -r deps) because merl breaks the PLT.
REBAR_DEPS=deps/erlydtl deps/ibrowse deps/getopt deps/jsx

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS) -r $(REBAR_DEPS)
 
dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

test: compile
	@(rebar eunit)

clean:
	@rebar clean

distclean: clean
	@rm -rf pinroll ebin deps .eunit .rebar $(DEPS_PLT)
