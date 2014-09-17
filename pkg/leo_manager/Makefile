include ../../leofs.mk
VERSION=$(LEOFS_VERSION)
COMPONENT_INTERNAL=leo_manager
COMPONENT=leo_manager
COMPONENT_PATH=../../package/$(COMPONENT_INTERNAL)_0
BASE=../bootstrap/$(COMPONENT)
DIR=../bootstrap/$(COMPONENT)/$(COMPONENT)

.PHONY: build_info package package_list all bootstrap

all: package

bootstrap:
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

package_list: bootstrap
	cat pre_pkg > packlist
	(cd $(BASE); find * -type f | sort) >> packlist
	cat post_pkg >> packlist

build_info:
	pkg_info -X pkg_install | egrep '^(MACHINE_ARCH|OPSYS|OS_VERSION|PKGTOOLS_VERSION)' >build-info
	echo "CATEGORIES=leofs" >>build-info
	echo "HOMEPAGE=http://www.leofs.org/" >>build-info

clean:
	-rm -r $(DIR)
	-rm *.tgz build-info packlist

package: package_list build_info
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I /opt/local/ -p $(BASE) -U $(COMPONENT)-$(VERSION)$(SUFFIX).tgz
