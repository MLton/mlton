MLTON=../../build/bin/mlton
PKG=self dbg runtime w32api binutils gcc gmp msys gdb dlfcn

PKG_LST=$(patsubst %,%.lst,$(PKG))
PKG_WXS=$(patsubst %,%.wxs,$(PKG))
PKG_WIXOBJ=$(patsubst %,%.wixobj,$(PKG))

MLton.msi:	mlton.wixobj dirs.wixobj files.wixobj $(PKG_WIXOBJ)
	light -cultures:en-us -ext WixUIExtension -out $@ $^

clean:
	rm -rf staging MLton.msi *.wixobj *.exe $(PKG_WXS) $(PKG_LST) files.wxs dirs.wxs

%.wixobj:	%.wxs
	candle -ext WixUIExtension $<

%.exe:		%.sml
	$(MLTON) $<

self.lst:	files2cmp.exe
	rm -rf staging
	$(MAKE) -C ../.. PREFIX= install
	mv ../../install staging
	cd staging; find * -type f ! -name *-gdb.a > ../$@.tmp
	mv $@.tmp $@

dbg.lst:	self.lst
	cd staging; find * -type f -name *-gdb.a > ../$@.tmp
	mv $@.tmp $@

%.lst:	dl/%.get dbg.lst
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
