WIX="/c/Program Files/WiX"
TARDIR=/c/DOCUME~1/terpstra/Desktop/downloads/mingw
MLTON=mlton

MINGW_RUNTIME=$(TARDIR)/mingw-runtime-3.14.tar.gz
MINGW_W32API=$(TARDIR)/w32api-3.11.tar.gz
MINGW_BINUTILS=$(TARDIR)/binutils-2.17.50-20060824-1.tar.gz
MINGW_GCC=$(TARDIR)/gcc-core-3.4.5-20060117-1.tar.gz
MINGW_MSYS=$(TARDIR)/msysCORE-1.0.11-2007.01.19-1.tar.bz2

MINGW_WXS=mingw-runtime.wxs w32api.wxs binutils.wxs gcc.wxs msys.wxs
MINGW_WIXOBJ=$(patsubst %.wxs,%.wixobj,$(MINGW_WXS))

mlton.msi:	mlton.wixobj self.wixobj filesys.wixobj $(MINGW_WIXOBJ)
	$(WIX)/light -out $@ $^ $(WIX)/wixui.wixlib -loc $(WIX)/WixUI_en-us.wxl

clean:
	rm -rf staging mlton.msi *.wixobj *.exe $(MINGW_WXS) self.wxs filesys.wxs

%.wixobj:	%.wxs
	$(WIX)/candle $<

%.exe:		%.sml
	$(MLTON) $<

# This has to happen after everything else is unpacked
filesys.wxs:	dirs2wix-filesys.exe $(MINGW_WIXOBJ) self.wixobj
	cd staging; find * -type d | ../dirs2wix-filesys > ../$@.tmp 
	mv $@.tmp $@

mingw-runtime.wxs:	$(MINGW_RUNTIME) self.wxs
	tar tzf $< | ./files2wix-feature "MinGW" "Runtime" "Runtime" "An assortment of thin wrappers that provide POSIX-like functionality on a windows platform." > $@.tmp
	cd staging; tar xzf $(MINGW_RUNTIME)
	mv $@.tmp $@

w32api.wxs:		$(MINGW_W32API) self.wxs
	tar tzf $< | ./files2wix-feature "MinGW" "W32API" "W32API" "Headers and link stub libraries needed to access windows system DLLs." > $@.tmp
	cd staging; tar xzf $(MINGW_W32API)
	mv $@.tmp $@

binutils.wxs:		$(MINGW_BINUTILS) self.wxs
	tar tzf $< | grep -v info/dir | ./files2wix-feature "MinGW" "Binutils" "Binutils" "Utilities for manipulating object files, such as the linker and assembler." > $@.tmp
	cd staging; tar xzf $(MINGW_BINUTILS)
	mv $@.tmp $@

gcc.wxs:		$(MINGW_GCC) self.wxs
	tar tzf $< | ./files2wix-feature "MinGW" "GCC" "GCC" "The GNU C Compiler. Compiles C files to assembly." > $@.tmp
	cd staging; tar xzf $(MINGW_GCC)
	mv $@.tmp $@

msys.wxs:		$(MINGW_MSYS) self.wxs
	(tar tjf $<; echo bin/bash.exe) | ./files2wix-feature "MinGW" "MSYS" "MSYS" "A system shell along with UNIX-style command-line utilities." > $@.tmp
	cd staging; tar xjf $(MINGW_MSYS)
	cp staging/bin/sh.exe staging/bin/bash.exe
	cp msys.bat staging
	mv $@.tmp $@

self.wxs:	files2wix-feature.exe
	rm -rf staging
	make -C ../.. PREFIX= install
	mv ../../install staging
	cd staging; find * -type f | ../files2wix-feature "Complete" "MLton" "MLton" "A whole-program optimizing Standard ML compiler." > ../$@.tmp
	mv $@.tmp $@
