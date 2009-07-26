MLTON=../../build/bin/mlton
PKG=self runtime w32api binutils gcc gmp msys gdb dlfcn wget self64 gmp64 mingw64

PKG_LST=$(patsubst %,%.lst,$(PKG))
PKG_WXS=$(patsubst %,%.wxs,$(PKG))
PKG_WIXOBJ=$(patsubst %,%.wixobj,$(PKG))

MLton.msi:	mlton.wixobj dirs.wixobj files.wixobj $(PKG_WIXOBJ)
	light -cultures:en-us -ext WixUIExtension -out $@ $^

clean:
	rm -rf staging MLton.msi *.wixpdb *.wixobj *.exe $(PKG_WXS) $(PKG_LST) files.wxs dirs.wxs

%.wixobj:	%.wxs
	candle -ext WixUIExtension $<

%.exe:		%.sml
	$(MLTON) $<

self.lst:	files2cmp.exe
	rm -rf staging
	$(MAKE) -C ../.. PREFIX= install
	mv ../../install staging
	rm staging/lib/mlton/self/*-gdb.a
	cd staging; find * -type f > ../$@.tmp
	mv $@.tmp $@

self64.lst:	self.lst
	rm -rf ../../runtime.x64
	cp -a ../../runtime ../../runtime.x64
	$(MAKE) -C ../../runtime.x64 TARGET=x86_64-w64-mingw32 TARGET_ARCH=amd64 TARGET_OS=mingw clean all
	mkdir -p staging/lib/mlton/sml/basis/config/c/amd64-mingw
	cp ../../runtime.x64/gen/c-types.sml staging/lib/mlton/sml/basis/config/c/amd64-mingw
	mkdir -p staging/lib/mlton/x86_64-w64-mingw32/include
	cp ../../runtime.x64/gen/c-types.h staging/lib/mlton/x86_64-w64-mingw32/include
	cp ../../runtime.x64/gen/sizes ../../runtime.x64/*.a staging/lib/mlton/x86_64-w64-mingw32
	rm staging/lib/mlton/x86_64-w64-mingw32/*-gdb.a
	rm -rf ../../runtime.x64
	$(MLTON) -target x86_64-w64-mingw32 -build-constants true > staging/w64-constants.c
	$(MLTON) -target x86_64-w64-mingw32 -verbose 1 staging/w64-constants.c
	./staging/w64-constants > staging/lib/mlton/x86_64-w64-mingw32/constants
	echo "x86_64-w64-mingw32 amd64 mingw" >> staging/lib/mlton/target-map
	cd staging; find lib/mlton/x86_64-w64-mingw32 lib/mlton/sml/basis/config/c/amd64-mingw -type f > ../$@.tmp
	mv $@.tmp $@

%.lst:	dl/%.get self.lst
	cd staging; . ../$< | tar --exclude postinstall -t -k | sed 's@^\./@@' > ../$@.tmp
	cd staging; . ../$< | tar -x
	mv $@.tmp $@

%.wxs:	%.lst
	./files2cmp $(*F) < $< > $@

# This has to happen after everything else is unpacked; depend on PKG_LST
dirs.wxs:	dirs2wix.exe $(PKG_LST)
	cd staging; find * -type d | ../dirs2wix > ../$@.tmp
	mv $@.tmp $@
files.wxs:	files2wix.exe $(PKG_LST)
	cat $(PKG_LST) | sort | uniq | ./files2wix > ./$@.tmp
	mv $@.tmp $@
