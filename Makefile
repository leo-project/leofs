.PHONY: deps test

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps
clean:
	@./rebar clean
xref:
	@./rebar xref
eunit:
	@./rebar eunit

generate:
	rm -rf rel/leo_manager/leo_manager/
	rm -rf rel/leo_storage/leo_storage/
	rm -rf rel/leo_gateway/leo_gateway/
	@./rebar compile
	(cd rel/leo_manager && ../../rebar generate)
	(cd rel/leo_storage && ../../rebar generate)
	(cd rel/leo_gateway && ../../rebar generate)

release:
	@./rebar compile
	rm -rf package
	##
	## manager-master
	##
	rm -rf rel/leo_manager/leo_manager/
	mkdir -p package/leo_manager_0
	cp rel/leo_manager/vars/manager_master_vars.config rel/leo_manager/vars.config
	(cd rel/leo_manager && ../../rebar generate)
	cp -r rel/leo_manager/leo_manager/* package/leo_manager_0/
	##
	## manager-slave
	##
	rm -rf rel/leo_manager/leo_manager/
	mkdir -p package/leo_manager_1
	cp rel/leo_manager/vars/manager_slave_vars.config rel/leo_manager/vars.config
	(cd rel/leo_manager && ../../rebar generate)
	cp -r rel/leo_manager/leo_manager/* package/leo_manager_1/
	##
	## storage
	##
	rm -rf rel/leo_storage/leo_storage/
	mkdir -p package/leo_storage
	(cd rel/leo_storage && ../../rebar generate)
	cp -r rel/leo_storage/leo_storage/* package/leo_storage/
	##
	## gateway
	##
	rm -rf rel/leo_gateway/leo_gateway/
	mkdir -p package/leo_gateway
	(cd rel/leo_gateway && ../../rebar generate)
	cp -r rel/leo_gateway/leo_gateway/* package/leo_gateway/

