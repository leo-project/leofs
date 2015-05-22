include ../../leofs.mk
VERSION=$(LEOFS_VERSION)
COMPONENT_INTERNAL=leo_manager
COMPONENT=leo_manager
COMPONENT_PATH=../../package/$(COMPONENT_INTERNAL)_0
BASE=../bootstrap/$(COMPONENT)
DIR=../bootstrap/$(COMPONENT)/$(COMPONENT)
STAGE_DIR=$(BASE)
PKG_CATEGORY=leofs
PKG_HOMEPAGE=http://www.leofs.org/
DEPS="erlang>=16.1.2"
TARGET_DIR=/opt/local

include ../../deps/fifo_utils/priv/pkg.mk

.PHONY: build_info package package_list all bootstrap

all: package

package: prepare $(FILE).tgz

prepare:
	-rm -r $(DIR)
	mkdir -p $(BASE)
	cp -r $(COMPONENT_PATH) $(DIR)
	mkdir -p $(DIR)/share
	mkdir -p $(BASE)/sbin
	cp ../../leofs-adm $(BASE)/sbin
	cp files/$(COMPONENT) $(DIR)/bin/
	cp files/$(COMPONENT).xml $(DIR)/share
	cp files/start $(DIR)/erts-*/bin/start
	cp files/leo_manager.conf* $(DIR)/etc/
	cp files/leo_manager_snmp.config $(DIR)/snmp/snmpa_manager_0/
	cp files/leo_manager_snmp.config $(DIR)/snmp/snmpa_manager_1/
	rm $(DIR)/etc/leo_manager.conf
	cp files/leo_manager.conf.master $(DIR)/etc/leo_manager.conf.example

clean: clean-pkg
	-rm -r $(DIR)
	-rm *.tgz
