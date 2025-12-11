#======================================================================
#
# LeoFS
#
# Copyright (c) 2012-2018 Rakuten, Inc.
# Copyright (c) 2019-2025 Lions Data, Ltd.
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
.PHONY: all compile clean distclean test release replace_launch_env release_for_test pkgsrc

all: compile

compile:
	(cd apps/leo_manager && rebar3 compile)
	(cd apps/leo_storage && rebar3 compile)
	(cd apps/leo_gateway && rebar3 compile)

clean:
	(cd apps/leo_manager && rebar3 clean)
	(cd apps/leo_storage && rebar3 clean)
	(cd apps/leo_gateway && rebar3 clean)
	make -C pkg clean

distclean:
	(cd apps/leo_manager && rm -rf _build)
	(cd apps/leo_storage && rm -rf _build)
	(cd apps/leo_gateway && rm -rf _build)
	rm -rf _build

test:
	(cd apps/leo_manager && rebar3 eunit)
	(cd apps/leo_storage && rebar3 eunit)
	(cd apps/leo_gateway && rebar3 eunit)

dialyzer:
	(cd apps/leo_manager && rebar3 dialyzer)
	(cd apps/leo_storage && rebar3 dialyzer)
	(cd apps/leo_gateway && rebar3 dialyzer)

xref:
	(cd apps/leo_manager && rebar3 xref)
	(cd apps/leo_storage && rebar3 xref)
	(cd apps/leo_gateway && rebar3 xref)

release:
	rm -rf package/leo_*
	#
	# Fetch dependencies and patch for CMake compatibility
	# (Makefile is created during compile, so we try compile first then patch)
	#
	-(cd apps/leo_manager && rebar3 compile 2>/dev/null || true) && \
		(cd apps/leo_manager && ./scripts/patch_deps.sh 2>/dev/null || true)
	-(cd apps/leo_storage && rebar3 compile 2>/dev/null || true) && \
		(cd apps/leo_storage && ./scripts/patch_deps.sh 2>/dev/null || true)
	-(cd apps/leo_gateway && rebar3 compile 2>/dev/null || true) && \
		(cd apps/leo_gateway && ./scripts/patch_deps.sh 2>/dev/null || true)
	#
	# manager-master
	#
	(cd apps/leo_manager && \
		cp priv/leo_manager_0.conf priv/leo_manager.conf && \
		cp priv/leo_manager_0.schema priv/leo_manager.schema && \
		cp config/vm.args.0 config/vm.args && \
		cp config/sys.config.0 config/sys.config && \
		rebar3 release -n leo_manager)
	mkdir -p package/leo_manager_0
	cp -r apps/leo_manager/_build/default/rel/leo_manager/* package/leo_manager_0/
	#
	# manager-slave
	#
	(cd apps/leo_manager && \
		rm -rf _build/default/rel/leo_manager && \
		cp priv/leo_manager_1.conf priv/leo_manager.conf && \
		cp priv/leo_manager_1.schema priv/leo_manager.schema && \
		cp config/vm.args.1 config/vm.args && \
		cp config/sys.config.1 config/sys.config && \
		rebar3 release -n leo_manager)
	mkdir -p package/leo_manager_1
	cp -r apps/leo_manager/_build/default/rel/leo_manager/* package/leo_manager_1/
	#
	# storage
	#
	(cd apps/leo_storage && rebar3 release -n leo_storage)
	mkdir -p package/leo_storage
	cp -r apps/leo_storage/_build/default/rel/leo_storage/* package/leo_storage/
	#
	# gateway
	#
	(cd apps/leo_gateway && rebar3 release -n leo_gateway)
	mkdir -p package/leo_gateway
	cp -r apps/leo_gateway/_build/default/rel/leo_gateway/* package/leo_gateway/
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
