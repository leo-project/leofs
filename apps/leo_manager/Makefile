.PHONY: all compile xref eunit release check_plt build_plt dialyzer doc clean distclean

REBAR := rebar3
APPS = erts kernel stdlib sasl crypto compiler inets mnesia public_key runtime_tools snmp syntax_tools tools xmerl ssl
PLT_FILE = .leo_manager_dialyzer_plt
DOT_FILE = leo_manager.dot
CALL_GRAPH_FILE = leo_manager.png

all: compile xref eunit

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

eunit:
	@$(REBAR) eunit

check_plt:
	@$(REBAR) compile
	dialyzer --check_plt --plt $(PLT_FILE) --apps $(APPS)

build_plt:
	@$(REBAR) compile
	dialyzer --build_plt --output_plt $(PLT_FILE) --apps $(APPS)

dialyzer:
	@$(REBAR) dialyzer

doc:
	@$(REBAR) edoc

clean:
	@$(REBAR) clean

distclean:
	@$(REBAR) clean --all
	rm -rf _build
