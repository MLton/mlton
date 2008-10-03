WIX="/c/Programme/Windows Installer XML v3/bin"
TARDIR=/c/DOKUME~1/terpstra/Desktop/MinGW
MLTON=../../build/bin/mlton

MINGW_WXS=runtime.wxs w32api.wxs binutils.wxs gcc.wxs gmp.wxs msys.wxs gdb.wxs
MINGW_WIXOBJ=$(patsubst %.wxs,%.wixobj,$(MINGW_WXS))

MLton.msi:	mlton.wixobj self.wixobj dbg.wixobj filesys.wixobj $(MINGW_WIXOBJ)
	$(WIX)/light -cultures:en-us -ext WixUIExtension -out $@ $^

clean:
	rm -rf staging MLton.msi *.wixobj *.exe $(MINGW_WXS) self.wxs dbg.wxs filesys.wxs

%.wixobj:	%.wxs
	$(WIX)/candle -ext WixUIExtension $<

%.exe:		%.sml
	$(MLTON) $<

self.wxs:	files2wix-component.exe
	rm -rf staging
	make -C ../.. PREFIX= install
	mv ../../install staging
	cd staging; find * -type f ! -name *-gdb.a | ../files2wix-component MLton > ../$@.tmp
	mv $@.tmp $@

dbg.wxs:	self.wxs
	cd staging; find * -type f -name *-gdb.a | ../files2wix-component Debug > ../$@.tmp
	mv $@.tmp $@

# This has to happen after everything else is unpacked
filesys.wxs:	dirs2wix-filesys.exe $(MINGW_WIXOBJ) self.wixobj
	cd staging; find * -type d | ../dirs2wix-filesys > ../$@.tmp 
	mv $@.tmp $@

runtime.wxs:	$(TARDIR)/mingwrt-3.15-mingw32-dev.tar.gz self.wxs
w32api.wxs:		$(TARDIR)/w32api-3.12-mingw32-dev.tar.gz self.wxs
binutils.wxs:	$(TARDIR)/binutils-2.18.50-20080109-2-coffgen.tar.gz self.wxs
gcc.wxs:		$(TARDIR)/gcc-core-3.4.5-20060117-3.tar.gz self.wxs
msys.wxs:		$(TARDIR)/msysCORE-1.0.11-20080826.tar.gz self.wxs
gdb.wxs:		$(TARDIR)/gdb-6.8-mingw-3.tar.gz self.wxs
gmp.wxs:		$(TARDIR)/mingw-gmp-4.2.3-1.tar.gz self.wxs

%.wxs:
	tar --exclude postinstall -tzf $< | ./files2wix-component $(*F) > $@.tmp
	cd staging; tar -xzf $<
	mv $@.tmp $@
