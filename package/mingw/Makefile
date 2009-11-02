MLTON=../../build/bin/mlton
PKG=self32 self64 mingw32 mingw64 gmp32 gmp64 dlfcn32 dlfcn64 msys wget 7zip

PKG_LST=$(patsubst %,%.lst,$(PKG))
PKG_WXS=$(patsubst %,%.wxs,$(PKG))
PKG_WIXOBJ=$(patsubst %,%.wixobj,$(PKG))

MLton.exe:	data.7z
	cat 7zS.sfx MLton.conf data.7z > $@

.PRECIOUS: data.7z
data.7z:	MLton.msi
	7z a data.7z -mx=9 $<

MLton.msi:	mlton.wixobj dirs.wixobj files.wixobj $(PKG_WIXOBJ)
	light -cultures:en-us -ext WixUIExtension -out $@ $^

clean:
	rm -rf staging MLton.msi *.wixpdb *.wixobj *.exe $(PKG_WXS) $(PKG_LST) files.wxs dirs.wxs

%.wixobj:	%.wxs
	candle -ext WixUIExtension $<

%.exe:		%.sml
	$(MLTON) $<

self32.lst:
	rm -rf staging
	$(MAKE) -C ../.. PREFIX= install
	mv ../../install staging
	rm staging/lib/mlton/self/*-gdb.a
	cd staging; find * -type f > ../$@.tmp
	mv $@.tmp $@

self64.lst:	self32.lst
	rm -rf ../../runtime.x64
	cp -a ../../runtime ../../runtime.x64
	$(MAKE) -C ../../runtime.x64 TARGET=x86_64-w64-mingw32 TARGET_ARCH=amd64 TARGET_OS=mingw clean all
	mkdir -p staging/lib/mlton/sml/basis/config/c/amd64-mingw
	cp ../../runtime.x64/gen/c-types.sml staging/lib/mlton/sml/basis/config/c/amd64-mingw
	mkdir -p staging/lib/mlton/x86_64-w64-mingw32/include
	cp ../../runtime.x64/gen/c-types.h staging/lib/mlton/x86_64-w64-mingw32/include
	cp ../../runtime.x64/gen/sizes ../../runtime.x64/*.a staging/lib/mlton/x86_64-w64-mingw32
	echo "x86_64-w64-mingw32 amd64 mingw" >> staging/lib/mlton/target-map
	rm staging/lib/mlton/x86_64-w64-mingw32/*-gdb.a
	rm -rf ../../runtime.x64
	sed "s@'/lib/mlton'@\`cd \"\$$dir/lib/mlton\" \&\& pwd\`@" < staging/bin/mlton > staging/mlton
	./staging/mlton -target x86_64-w64-mingw32 -build-constants true > staging/w64-constants.c
	./staging/mlton -target x86_64-w64-mingw32 -verbose 1 staging/w64-constants.c
	./staging/w64-constants > staging/lib/mlton/x86_64-w64-mingw32/constants
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
files.wxs:	files2wix.exe $(PKG_LST)
	cat $(PKG_LST) | sort | uniq | ./files2wix > ./$@.tmp
	mv $@.tmp $@
