VERSION=0.14.9
COMPONENT_INTERNAL=leo_storage
COMPONENT=leo_storage
COMPONENT_PATH=../../package/$(COMPONENT_INTERNAL)

.PHONY: build_info package package_list all clean 

all: package

package_list:
	cat pre_pkg > packlist
	(cd $(COMPONENT_PATH); find * -type f | sort) >> packlist
	cat post_pkg >> packlist

build_info:
	pkg_info -X pkg_install | egrep '^(MACHINE_ARCH|OPSYS|OS_VERSION|PKGTOOLS_VERSION)' >build-info
	echo "CATEGORIES=leofs" >>build-info
	echo "HOMEPAGE=http://www.leofs.org/" >>build-info

clean:
	-rm *.tgz build-info packlist

package: package_list build_info
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I /opt/local/$(COMPONENT) -p $(COMPONENT_PATH) -U $(COMPONENT)-$(VERSION)$(SUFFIX).tgz
