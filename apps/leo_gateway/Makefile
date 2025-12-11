.PHONY: all compile xref eunit dialyzer clean distclean doc gen_rpc gen_nfs get-deps patch-deps

REBAR := rebar3

all: compile xref eunit

get-deps:
	@$(REBAR) get-deps

patch-deps:
	@./scripts/patch_deps.sh

compile: get-deps patch-deps
	@$(REBAR) compile

xref:
	@$(REBAR) xref

eunit:
	@$(REBAR) eunit

dialyzer:
	@$(REBAR) dialyzer

doc:
	@$(REBAR) ex_doc

clean:
	@$(REBAR) clean

distclean:
	@$(REBAR) clean -a
	@rm -rf _build

gen_rpc:
	@$(REBAR) compile
	@cd _build/default/lib/erpcgen && $(MAKE)

gen_nfs: gen_rpc
	./_build/default/lib/erpcgen/priv/erpcgen -a [svc_callback,xdr,hrl] src/leo_nfs_proto3.x
	./_build/default/lib/erpcgen/priv/erpcgen -a [svc_callback,xdr,hrl] src/leo_nfs_mount3.x
	./_build/default/lib/erpcgen/priv/erpcgen -a [svc_callback,xdr,hrl] src/leo_nlm_proto4.x

shell:
	@$(REBAR) shell

test:
	@$(REBAR) ct

fmt:
	@$(REBAR) fmt

check-fmt:
	@$(REBAR) fmt --check
