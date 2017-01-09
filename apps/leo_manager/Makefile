.PHONY: all deps compile xref eunit release check_plt build_plt dialyzer doc callgraph graphviz clean distclean

REBAR := ./rebar
APPS = erts kernel stdlib sasl crypto compiler inets mnesia public_key runtime_tools snmp syntax_tools tools xmerl webtool ssl
LIBS = deps/leo_commons/ebin deps/leo_logger/ebin deps/leo_object_storage/ebin deps/leo_mq/ebin \
       deps/leo_redundant_manager/ebin deps/leo_rpc/ebin deps/leo_statistics/ebin \
       deps/leo_s3_libs deps/savanna_commons
PLT_FILE = .leo_manager_dialyzer_plt
DOT_FILE = leo_manager.dot
CALL_GRAPH_FILE = leo_manager.png

all: deps compile xref eunit
deps:
	@$(REBAR) get-deps
compile:
	find . -name rebar.config|xargs sed -i 's/require_otp_vsn,\s\+"\(.\+\)"/require_otp_vsn, "R16B*|17|18|19"/g'
	@$(REBAR) compile
xref:
	@$(REBAR) xref skip_deps=true
eunit:
	@$(REBAR) eunit skip_deps=true
release:
	rm -rf rel/leo_manager
	@$(REBAR) compile
	(cd rel/ && ../rebar generate)
check_plt:
	@$(REBAR) compile
	dialyzer --check_plt --plt $(PLT_FILE) --apps $(APPS)
build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(PLT_FILE) --apps $(APPS) $(LIBS)
dialyzer:
	@$(REBAR) compile
	dialyzer -Wno_return --plt $(PLT_FILE) -r ebin/ --dump_callgraph $(DOT_FILE) -Wrace_conditions | fgrep -v -f ./dialyzer.ignore-warnings
doc: compile
	@$(REBAR) doc
callgraph: graphviz
	dot -Tpng -o$(CALL_GRAPH_FILE) $(DOT_FILE)
graphviz:
	$(if $(shell which dot),,$(error "To make the depgraph, you need graphviz installed"))
clean:
	@$(REBAR) clean
distclean:
	@$(REBAR) delete-deps
	@$(REBAR) clean
