include ../../leofs.mk
VERSION=$(LEOFS_VERSION)
COMPONENT_INTERNAL=leo_manager
COMPONENT=leo_manager
COMPONENT_PATH=../../package/$(COMPONENT_INTERNAL)_0
DIR=../bootstrap/$(COMPONENT)

.PHONY: build_info package package_list all bootstrap

all: package

bootstrap:
	-rm -r $(DIR)
	cp -r $(COMPONENT_PATH) $(DIR)
	mkdir $(DIR)/share
	cp files/$(COMPONENT) $(DIR)/bin/
	cp files/$(COMPONENT).xml $(DIR)/share
	cp files/start $(DIR)/erts-*/bin/start
	cp files/app.config* $(DIR)/etc/
	cp files/leo_manager_snmp.config $(DIR)/snmp/snmpa_manager_0/
	cp files/leo_manager_snmp.config $(DIR)/snmp/snmpa_manager_1/
	rm $(DIR)/etc/app.config
	cp files/app.config.master $(DIR)/etc/app.config.example
	mv $(DIR)/etc/vm.args $(DIR)/etc/vm.args.example

package_list: bootstrap
	cat pre_pkg > packlist
	(cd $(DIR); find * -type f | sort) >> packlist
	cat post_pkg >> packlist

build_info:
	pkg_info -X pkg_install | egrep '^(MACHINE_ARCH|OPSYS|OS_VERSION|PKGTOOLS_VERSION)' >build-info
	echo "CATEGORIES=leofs" >>build-info
	echo "HOMEPAGE=http://www.leofs.org/" >>build-info

clean:
	-rm -r $(DIR)
	-rm *.tgz build-info packlist

package: package_list build_info
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I /opt/local/$(COMPONENT) -p $(DIR) -U $(COMPONENT)-$(VERSION)$(SUFFIX).tgz
