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
