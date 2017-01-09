#======================================================================
#
# LeoFS
#
# Copyright (c) 2012-2015 Rakuten, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
#======================================================================
.PHONY: all compile deps clean xref eunit generate release pkgsrc

all: deps compile
compile:
	@./rebar3 compile
deps:
	@./rebar3 deps
clean:
	@./rebar3 clean
	make -C pkg clean
xref:
	@./rebar3 xref
eunit:
	@./rebar3 eunit
generate:
	rm -rf rel/leo_manager/leo_manager/
	rm -rf rel/leo_storage/leo_storage/
	rm -rf rel/leo_gateway/leo_gateway/
	@./rebar3 compile
	(cd rel/leo_manager && ../../rebar generate)
	(cd rel/leo_storage && ../../rebar generate)
	(cd rel/leo_gateway && ../../rebar generate)
release:
	@./rebar3 compile
	rm -rf package/leo_*
	##
	## manager-master
	##
	rm -rf rel/leo_manager/leo_manager/
	mkdir -p package/leo_manager_0
	cp deps/leo_manager/priv/leo_manager_0.conf rel/leo_manager/files/leo_manager.conf
	cp deps/leo_manager/priv/leo_manager_0.schema rel/leo_manager/files/leo_manager.schema
	(cd rel/leo_manager && ../../rebar generate)
	cp -r rel/leo_manager/leo_manager/* package/leo_manager_0/
	##
	## manager-slave
	##
	rm -rf rel/leo_manager/leo_manager/
	mkdir -p package/leo_manager_1
	cp deps/leo_manager/priv/leo_manager_1.conf rel/leo_manager/files/leo_manager.conf
	cp deps/leo_manager/priv/leo_manager_1.schema rel/leo_manager/files/leo_manager.schema
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
	cp README.md package/
	cp leofs-adm package/

pkgsrc: release
	make -C pkg
