#======================================================================
#
# LeoFS
#
# Copyright (c) 2012-2018 Rakuten, Inc.
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
.PHONY: all compile deps clean distclean test generate release replace_launch_env release_for_test pkgsrc

ifneq ($(with_sd_notify),yes)
with_sd_notify := no
endif

all: deps compile
compile:
	@./rebar compile
deps:
	@./rebar get-deps
clean:
	@./rebar clean
	make -C pkg clean
distclean:
	@./rebar delete-deps
	@./rebar clean
	rm -f rel/leo_*/reltool.config
test:
	(cd apps/leo_manager && make)
	(cd apps/leo_gateway && make)
	(cd apps/leo_storage && make)
generate:
	rm -rf rel/leo_manager/leo_manager/
	rm -rf rel/leo_storage/leo_storage/
	rm -rf rel/leo_gateway/leo_gateway/
	@./rebar compile
	(cd rel/leo_manager && ../../rebar generate)
	(cd rel/leo_storage && ../../rebar generate)
	(cd rel/leo_gateway && ../../rebar generate)
sd_notify:
	make -C deps/sd_notify
reltool:
	for reltool_config in rel/leo_*/reltool.config.in; do \
		./make_reltool.sh $(with_sd_notify) $$reltool_config > $$(echo $$reltool_config | sed s/.in$$//); \
	done
release: reltool
	@./rebar compile
	rm -rf package/leo_*
	#
	# manager-master
	#
	rm -rf rel/leo_manager/leo_manager/
	mkdir -p package/leo_manager_0
	cp apps/leo_manager/priv/leo_manager_0.conf rel/leo_manager/files/leo_manager.conf
	cp apps/leo_manager/priv/leo_manager_0.schema rel/leo_manager/files/leo_manager.schema
	(cd rel/leo_manager && ../../rebar generate)
	cp -r rel/leo_manager/leo_manager/* package/leo_manager_0/
	#
	# manager-slave
	#
	rm -rf rel/leo_manager/leo_manager/
	mkdir -p package/leo_manager_1
	cp apps/leo_manager/priv/leo_manager_1.conf rel/leo_manager/files/leo_manager.conf
	cp apps/leo_manager/priv/leo_manager_1.schema rel/leo_manager/files/leo_manager.schema
	(cd rel/leo_manager && ../../rebar generate)
	cp -r rel/leo_manager/leo_manager/* package/leo_manager_1/
	#
	# storage
	#
	rm -rf rel/leo_storage/leo_storage/
	mkdir -p package/leo_storage
	(cd rel/leo_storage && ../../rebar generate)
	cp -r rel/leo_storage/leo_storage/* package/leo_storage/
	#
	# gateway
	#
	rm -rf rel/leo_gateway/leo_gateway/
	mkdir -p package/leo_gateway
	(cd rel/leo_gateway && ../../rebar generate)
	cp -r rel/leo_gateway/leo_gateway/* package/leo_gateway/
	cp README.md package/
	cp leofs-adm package/
replace_launch_env:
	( echo '# Directories used by launch script can be re-defined here, if needed'; \
	  echo '# Default values will be picked for commented or empty parameters'; \
	  echo ''; \
	  echo '# Directory with main .conf file. It must be writable by $$RUNNER_USER'; \
	  echo '# RUNNER_ETC_DIR='; \
	  echo ''; \
	  echo '# Directory for .schema file.'; \
	  echo '# RUNNER_SCHEMA_DIR='; \
	  echo ''; \
	  echo '# Directory for erlang log files (erlang.log.* and run_erl.log)'; \
	  echo '# RUNNER_LOG_DIR='; \
	  echo ''; \
	  echo '# Defaults to "leofs"'; \
	  echo 'RUNNER_USER=${USER}'; \
	) > rel/common/launch.environment
	sudo rm -rf /tmp/home
release_for_test: replace_launch_env release

pkgsrc: release
	make -C pkg
build_doc:
	rm -rf site/ && mkdocs build
serve_doc: build_doc
	mkdocs serve --dev-addr 0.0.0.0:8000
