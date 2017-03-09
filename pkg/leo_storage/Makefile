include ../../leofs.mk
VERSION=$(LEOFS_VERSION)
COMPONENT_INTERNAL=leo_storage
COMPONENT=leo_storage
COMPONENT_PATH=../../package/$(COMPONENT_INTERNAL)
DIR=$(STAGE_DIR)/$(COMPONENT)
PKG_CATEGORY=leofs
PKG_HOMEPAGE=https://leo-project.net/leofs/
DEPS="erlang>=16.1.2"

include ../../deps/fifo_utils/priv/pkg.mk

.PHONY: build_info package package_list all bootstrap

all: package

package: prepare $(FILE).tgz

prepare:
	-rm -r $(DIR)
	cp -r $(COMPONENT_PATH) $(DIR)
	mkdir $(DIR)/share
	cp files/$(COMPONENT).xml $(DIR)/share
	cp files/start $(DIR)/erts-*/bin/start
	rm $(DIR)/etc/leo_storage.conf
	cp files/leo_storage.conf $(DIR)/etc/leo_storage.conf.example
	cp files/$(COMPONENT)_snmp.config.0 $(DIR)/snmp/snmpa_storage_0/$(COMPONENT)_snmp.config
	cp files/$(COMPONENT)_snmp.config.1 $(DIR)/snmp/snmpa_storage_1/$(COMPONENT)_snmp.config
	cp files/$(COMPONENT)_snmp.config.2 $(DIR)/snmp/snmpa_storage_2/$(COMPONENT)_snmp.config
	cp files/$(COMPONENT)_snmp.config.3 $(DIR)/snmp/snmpa_storage_3/$(COMPONENT)_snmp.config

clean: clean-pkg
	-rm -r $(DIR)
	-rm *.tgz
