WIX="/c/Program Files/WiX"
TARDIR=/c/DOCUME~1/terpstra/Desktop/downloads/mingw
MLTON=mlton

MINGW_RUNTIME=$(TARDIR)/mingw-runtime-3.14.tar.gz
MINGW_W32API=$(TARDIR)/w32api-3.11.tar.gz
MINGW_BINUTILS=$(TARDIR)/binutils-2.17.50-20060824-1.tar.gz
MINGW_GCC=$(TARDIR)/gcc-core-3.4.5-20060117-1.tar.gz
MINGW_MSYS=$(TARDIR)/msysCORE-1.0.11-2007.01.19-1.tar.bz2
MINGW_GMP=$(TARDIR)/gmp-static-mingw-4.1.tar.gz

MINGW_WXS=mingw-runtime.wxs w32api.wxs binutils.wxs gcc.wxs gmp.wxs msys.wxs
MINGW_WIXOBJ=$(patsubst %.wxs,%.wixobj,$(MINGW_WXS))

mlton.msi:	mlton.wixobj self.wixobj dbg.wixobj filesys.wixobj $(MINGW_WIXOBJ)
	$(WIX)/light -out $@ $^ $(WIX)/wixui.wixlib -loc $(WIX)/WixUI_en-us.wxl

clean:
	rm -rf staging mlton.msi *.wixobj *.exe $(MINGW_WXS) self.wxs dbg.wxs filesys.wxs

%.wixobj:	%.wxs
	$(WIX)/candle $<

%.exe:		%.sml
	$(MLTON) $<

# This has to happen after everything else is unpacked
filesys.wxs:	dirs2wix-filesys.exe $(MINGW_WIXOBJ) self.wixobj
	cd staging; find * -type d | ../dirs2wix-filesys > ../$@.tmp 
	mv $@.tmp $@

mingw-runtime.wxs:	$(MINGW_RUNTIME) self.wxs
	tar tzf $< | ./files2wix-component Runtime > $@.tmp
	cd staging; tar xzf $(MINGW_RUNTIME)
	mv $@.tmp $@

w32api.wxs:		$(MINGW_W32API) self.wxs
	tar tzf $< | ./files2wix-component W32API > $@.tmp
	cd staging; tar xzf $(MINGW_W32API)
	mv $@.tmp $@

binutils.wxs:		$(MINGW_BINUTILS) self.wxs
	tar tzf $< | grep -v info/dir | ./files2wix-component Binutils > $@.tmp
	cd staging; tar xzf $(MINGW_BINUTILS)
	mv $@.tmp $@

gcc.wxs:		$(MINGW_GCC) self.wxs
	tar tzf $< | ./files2wix-component GCC > $@.tmp
	cd staging; tar xzf $(MINGW_GCC)
	mv $@.tmp $@

msys.wxs:		$(MINGW_MSYS) self.wxs
	(tar --exclude postinstall -tjf $<; echo bin/bash.exe) | ./files2wix-component MSYS > $@.tmp
	cd staging; tar xjf $(MINGW_MSYS)
	cp staging/bin/sh.exe staging/bin/bash.exe
	mv $@.tmp $@

gmp.wxs:		$(MINGW_GMP) self.wxs
	tar tzf $< | sed 's@^gmp/@@' | ./files2wix-component GMP > $@.tmp
	tar xzf $(MINGW_GMP)
	cp -a gmp/* staging
	rm -rf gmp
	mv $@.tmp $@

self.wxs:	files2wix-component.exe
	rm -rf staging
	make -C ../.. PREFIX= install
	mv ../../install staging
	cp sml.ico mlb.ico staging/lib/mlton
	cd staging; find * -type f ! -name *-gdb.a | ../files2wix-component MLton > ../$@.tmp
	mv $@.tmp $@

dbg.wxs:	self.wxs
	cd staging; find * -type f -name *-gdb.a | ../files2wix-component Debug > ../$@.tmp
	mv $@.tmp $@
