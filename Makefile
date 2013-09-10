APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	public_key mnesia syntax_tools compiler
COMBO_PLT = $(HOME)/.cuttlefish_combo_dialyzer_plt

.PHONY: deps

all: deps compile
	./rebar skip_deps=true escriptize

deps:
	./rebar get-deps

docs:
	./rebar skip_deps=true doc

docsclean:
	@rm -rf doc/*.png doc/*.html doc/*.css edoc-info

compile: deps
	./rebar compile

clean:
	@./rebar clean

distclean: clean
	@rm -rf cuttlefish deps

test: all	
	./rebar skip_deps=true eunit

docs: deps
	./rebar skip_deps=true doc

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	dialyzer --plt $(COMBO_PLT) ebin
