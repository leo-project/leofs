include ../../leofs.mk
VERSION=$(LEOFS_VERSION)
COMPONENT_INTERNAL=leo_gateway
COMPONENT=leo_gateway
COMPONENT_PATH=../../package/$(COMPONENT_INTERNAL)
DIR=$(STAGE_DIR)/$(COMPONENT)
PKG_CATEGORY=leofs
PKG_HOMEPAGE=http://www.leofs.org/
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
	rm $(DIR)/etc/leo_gateway.conf
	cp files/leo_gateway.conf $(DIR)/etc/leo_gateway.conf.example
	cp files/leo_gateway_snmp.config.0 $(DIR)/snmp/snmpa_gateway_0/leo_gateway_snmp.config
	cp files/leo_gateway_snmp.config.1 $(DIR)/snmp/snmpa_gateway_1/leo_gateway_snmp.config

clean: clean-pkg
	-rm $(STAGE_DIR)/sbin/leofs-adm
	-rm -r $(DIR)
	-rm *.tgz
