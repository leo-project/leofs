.PHONY: all leo_manager leo_gateway leo_storage

all: leo_manager leo_gateway leo_storage

clean: clean_manager clean_gateway clean_storage

clean_manager:
	make -C leo_manager clean

clean_gateway:
	make -C leo_gateway clean

clean_storage:
	make -C leo_storage clean

leo_manager:
	make -C leo_manager package

leo_gateway:
	make -C leo_gateway package

leo_storage:
	make -C leo_storage package
