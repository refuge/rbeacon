ERL          ?= erl
ERLC		     ?= erlc
APP          := rbeacon 
REBAR?= rebar

.PHONY: deps doc

all: deps compile

dev: devbuild

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

doc: dev
	$(REBAR) -C rebar_dev.config doc skip_deps=true


clean:
	@$(REBAR) clean
	@rm -f t/*.beam
	@rm -f doc/*.html doc/*.css doc/edoc-info doc/*.png

distclean: clean
	@$(REBAR) delete-deps
	@rm -rf deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin


# development
#
devclean:
	$(REBAR) -C rebar_dev.config clean

devbuild: devdeps
	$(REBAR) -C rebar_dev.config compile

devdeps:
	$(REBAR) -C rebar_dev.config get-deps
