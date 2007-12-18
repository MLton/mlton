TARGET_ARCH := $(shell ../../bin/host-arch)
TARGET_OS := $(shell ../../bin/host-os)
ifeq ($(origin VERSION), undefined)
	VERSION := $(shell date +%Y%m%d)
endif
ifeq ($(origin RELEASE), undefined)
	RELEASE := 1
endif

mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS).dmg: mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS).pkg
	rm -rf $@
	hdiutil create -srcfolder $< $@
	rm -rf $<

mlton-$(VERSION)-$(RELEASE).$(TARGET_ARCH)-$(TARGET_OS).pkg: Info.plist install/usr/local/bin/mlton Resources/*
	rm -rf $@
	/Developer/Tools/packagemaker -build -ds -i Info.plist -f install/usr/local -r Resources -p $@
	sudo rm -rf install

install/usr/local/bin/mlton: ../../install/usr/local/bin/mlton
	sudo rm -rf install
	cp -prf ../../install install
	sudo chown -R root:wheel install/usr

Info.plist: Info.plist.in
	rm -f Info.plist
	cp Info.plist.in Info.plist
	sed "s/\(.*\)VERSION\(.*\)/\1$(VERSION)\2/" <Info.plist >z && \
	mv z Info.plist;
	sed "s/\(.*\)RELEASE\(.*\)/\1$(RELEASE)\2/" <Info.plist >z && \
	mv z Info.plist;
	sed "s/\(.*\)TARGET_ARCH\(.*\)/\1$(TARGET_ARCH)\2/" <Info.plist >z && \
	mv z Info.plist;
	sed "s/\(.*\)TARGET_OS\(.*\)/\1$(TARGET_OS)\2/" <Info.plist >z && \
	mv z Info.plist;

.PHONY: clean
clean:
	../../bin/clean
	rm -f *.dmg
