MLTON=../../build/bin/mlton
PKG=self32 self64 mingw32 mingw64 gmp32 gmp64 dlfcn32 dlfcn64 msys wget 7zip

PKG_LST=$(patsubst %,%.lst,$(PKG))
PKG_WXS=$(patsubst %,%.wxs,$(PKG))
PKG_WIXOBJ=$(patsubst %,%.wixobj,$(PKG))

ifeq (,$(VERSION))
VERSION := $(shell date +%Y%m%d)-1
endif

# Windows versions have to be (0-255).(0-255).(0-65536)
# So lets just use YY.MM.DD for the version (leading 0s stripped)
WINVERSION := $(shell echo $(VERSION) | sed -r 's/20([0-9][0-9])([0-9][0-9])([0-9][0-9])-([0-9]+)/\1.\2.\3.\4/;s/0*([1-9]+)/\1/g')

all:	MLton-$(VERSION).exe MLton-$(VERSION).msi

MLton-$(VERSION).exe:	7zS.sfx MLton.conf data.7z
	cat $^ > $@

data.7z:	7z/MLton-$(VERSION).msi
	rm -f $@
	cd 7z; 7z a ../$@ -mx9 -mmt $(<F)

.INTERMEDIATE: MLton.msi
7z/MLton-$(VERSION).msi:	mlton.wixobj dirs.wixobj files.wixobj $(PKG_WIXOBJ)
	mkdir -p $(@D)
	light -dcl:none -cultures:en-us -ext WixUIExtension -out $@ $^
MLton-$(VERSION).msi:	mlton.wixobj dirs.wixobj files.wixobj $(PKG_WIXOBJ)
	light -dcl:high -cultures:en-us -ext WixUIExtension -out $@ $^

.PHONY: clean distclean
clean:
	../../bin/clean
	rm -rf staging 7z MLton.conf data.7z *.wixpdb *.wixobj *.wxs
	rm -f guid.exe files2cmp.exe dirs2wix.exe files2wix.exe
	rm -f $(PKG_LST)
distclean:	clean
	rm -f MLton-*.exe MLton-*.msi dl/*.7z

%.wixobj:	%.wxs
	candle -ext WixUIExtension $<

%.exe:		%.sml
	$(MLTON) $<

# Cannot use a pattern rule b/c %.wxs already exists.
mlton.wxs:	mlton.wxs.in guid.exe
	sed "s/@VERSION@/$(VERSION)/g;s/@WINVERSION@/$(WINVERSION)/g" < $< | \
	sed "s/@GUID1@/`./guid $(VERSION).1`/g;s/@GUID2@/`./guid $(VERSION).2`/g" > $@
MLton.conf:	MLton.conf.in guid.exe
	sed "s/@VERSION@/$(VERSION)/g;s/@WINVERSION@/$(WINVERSION)/g" < $< | \
	sed "s/@GUID1@/`./guid $(VERSION).1`/g;s/@GUID2@/`./guid $(VERSION).2`/g" > $@

self32.lst:
	rm -rf staging
	$(MAKE) -C ../.. PREFIX= install
	mv ../../install staging
	rm staging/lib/mlton/targets/self/*-gdb.a
	cd staging; find * -type f > ../$@.tmp
	mv $@.tmp $@

self64.lst:	self32.lst
	rm -rf ../../runtime.x64
	cp -a ../../runtime ../../runtime.x64
	$(MAKE) -C ../../runtime.x64 TARGET=x86_64-w64-mingw32 TARGET_ARCH=amd64 TARGET_OS=mingw clean all
	mkdir -p staging/lib/mlton/targets/x86_64-w64-mingw32/sml
	cp ../../runtime.x64/gen/c-types.sml staging/lib/mlton/targets/x86_64-w64-mingw32/sml
	mkdir -p staging/lib/mlton/targets/x86_64-w64-mingw32/include
	cp ../../runtime.x64/gen/c-types.h staging/lib/mlton/targets/x86_64-w64-mingw32/include
	cp ../../runtime.x64/gen/sizes ../../runtime.x64/*.a staging/lib/mlton/targets/x86_64-w64-mingw32
	echo "amd64" >staging/lib/mlton/targets/x86_64-w64-mingw32/arch
	echo "mingw" >staging/lib/mlton/targets/x86_64-w64-mingw32/os
	rm staging/lib/mlton/targets/x86_64-w64-mingw32/*-gdb.a
	rm -rf ../../runtime.x64
	sed "s@'/lib/mlton'@\`cd \"\$$dir/lib/mlton\" \&\& pwd\`@" < staging/bin/mlton > staging/mlton
	./staging/mlton -target x86_64-w64-mingw32 -build-constants true > staging/w64-constants.c
	./staging/mlton -target x86_64-w64-mingw32 -verbose 1 staging/w64-constants.c
	./staging/w64-constants > staging/lib/mlton/targets/x86_64-w64-mingw32/constants
	cd staging; find * -type f | grep -v self > ../$@.tmp
	mv $@.tmp $@

.PRECIOUS:	dl/%.7z
dl/%.7z:	dl/%.url
	wget -c -O $@.tmp `cat $<`
	mv $@.tmp $@

%.lst:	dl/%.7z self64.lst
	rm -rf staging.tmp
	mkdir staging.tmp
	cd staging.tmp; 7z x ../$<
	cd staging.tmp; find * -type f | sed 's@^\./@@' > ../$@.tmp
	rm -rf staging.tmp
	cd staging; 7z x ../$<
	mv $@.tmp $@

%.wxs:	%.lst files2cmp.exe
	./files2cmp $(*F) < $< > $@

# This has to happen after everything else is unpacked; depend on PKG_LST
dirs.wxs:	dirs2wix.exe $(PKG_LST)
	cd staging; find * -type d | ../dirs2wix > ../$@.tmp
	mv $@.tmp $@
files.wxs:	files2wix.exe guid.exe $(PKG_LST)
	cat $(PKG_LST) | sort | uniq | ./files2wix > ./$@.tmp
	mv $@.tmp $@
