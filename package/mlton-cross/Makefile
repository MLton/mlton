MIRROR=http://ftp.de.debian.org/debian
TARGETS=alpha amd64 armel hppa i386 ia64 mips mipsel powerpc s390 sparc

FULL_VERSION := $(shell dpkg-parsechangelog | grep ^Version | cut -d" " -f2)
VERSION      := $(shell echo "$(FULL_VERSION)" | cut -d- -f1)
REVISION     := $(shell echo "$(FULL_VERSION)" | cut -d- -f2)

all:	debian/control $(patsubst %,cross-%,$(TARGETS)) mingw32 mingw64

clean:
	rm -rf gmp mlton mingw* cross-* constants.c

distclean:	clean
	rm -rf gmp* *.deb mlton_*.orig.tar.gz *.tmp *~

release:	distclean
	tar cvzf ../mlton-cross_$(VERSION).orig.tar.gz \
		--exclude .svn --exclude debian \
		--transform "s@^@mlton-cross-$(VERSION)/@" \
                                      *

install:
	for i in $(TARGETS); do \
		cp -a cross-$$i/* $(DESTDIR)/mlton-target-`dpkg-architecture -a$$i -f -qDEB_HOST_GNU_TYPE 2>/dev/null | sed 's@_@-@g'`/; \
	done
	cp -a mingw32/* $(DESTDIR)/mlton-target-i586-mingw32msvc/
	cp -a mingw64/* $(DESTDIR)/mlton-target-amd64-mingw32msvc/

debian/control:	control.in Makefile
	sed 's/VERSION/$(VERSION)-$(REVISION)/g' < $< > $@.tmp
	mv $@.tmp $@

.PRECIOUS: mlton_$(VERSION).orig.tar.gz
mlton_$(VERSION).orig.tar.gz:
	rm -f $@.tmp $@
	wget -O $@.tmp "$(MIRROR)/pool/main/m/mlton/$@"
	mv $@.tmp $@

mlton:	mlton_$(VERSION).orig.tar.gz
	rm -rf $@.tmp $@
	mkdir $@.tmp
	tar xvzfC $< $@.tmp mlton-$(VERSION)/runtime mlton-$(VERSION)/include mlton-$(VERSION)/bin
	mv $@.tmp/mlton-$(VERSION) $@
	rm -rf $@.tmp
	touch $@

gmp:
	apt-get source libgmp3-dev
	mv gmp-* gmp
	cd gmp; patch -p1 < ../fix-gmp-win64.patch
	cd gmp; autoconf

%.staging/stamp:	mlton
	mkdir -p $*.staging.tmp
	cp -a mlton/* $*.staging.tmp
	touch $*.staging.tmp/stamp
	mv $*.staging.tmp $*.staging

constants.c:
	mlton -build-constants true > $@

mingw32.staging/runtime/gmp.h:	mingw32.staging/stamp gmp
	mkdir -p mingw32.staging/gmp.obj
	cd mingw32.staging/gmp.obj; ../../gmp/configure ABI=32 --host=i586-mingw32msvc --enable-static --disable-shared
	make -C mingw32.staging/gmp.obj DESTDIR=`pwd`/mingw32.staging/gmp.dest all install
	cp mingw32.staging/gmp.dest/usr/local/include/gmp.h $@

mingw32:	mingw32.staging/runtime/gmp.h constants.c
	rm -rf $@.tmp $@
	mkdir -p $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/include
	mkdir -p $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/sml
	make -C mingw32.staging/runtime TARGET=i586-mingw32msvc TARGET_ARCH=x86 TARGET_OS=mingw
	cp mingw32.staging/runtime/gen/sizes mingw32.staging/runtime/*.a mingw32.staging/gmp.dest/usr/local/lib/libgmp.a $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc
	rm $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/*-gdb.a
	cp mingw32.staging/runtime/gen/c-types.h mingw32.staging/runtime/gmp.h $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/include
	cp mingw32.staging/runtime/gen/c-types.sml $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/sml
	echo mingw > $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/os
	echo x86 > $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/arch
	i586-mingw32msvc-gcc -std=gnu99 -m32 -o mingw32.staging/constants -O1 -w -I /usr/lib/mlton/include -I $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/include -L $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc constants.c -lmlton -lgmp -lgdtoa -lgmp -lm
	./mingw32.staging/constants > $@.tmp/usr/lib/mlton/targets/i586-mingw32msvc/constants
	mv $@.tmp $@
	
mingw64.staging/runtime/gmp.h:	mingw64.staging/stamp gmp
	mkdir -p mingw64.staging/gmp.obj
	cd mingw64.staging/gmp.obj; ../../gmp/configure ABI=64 --host=amd64-mingw32msvc --enable-static --disable-shared
	make -C mingw64.staging/gmp.obj DESTDIR=`pwd`/mingw64.staging/gmp.dest all install
	cp mingw64.staging/gmp.dest/usr/local/include/gmp.h $@

mingw64:	mingw64.staging/runtime/gmp.h constants.c
	rm -rf $@.tmp $@
	mkdir -p $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/include
	mkdir -p $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/sml
	# work-around bug #569914:
	cp win64/getopt.h mingw64.staging/runtime/
	# work-around wine not yet supporting 64-bit
	make -C mingw64.staging/runtime TARGET=amd64-mingw32msvc TARGET_ARCH=amd64 TARGET_OS=mingw gdtoa/arithchk.out util.o
	cp win64/*types* mingw64.staging/runtime/gen
	cp win64/arith.h mingw64.staging/runtime/gdtoa
	touch mingw64.staging/runtime/gen/gen-types.stamp
	make -C mingw64.staging/runtime TARGET=amd64-mingw32msvc TARGET_ARCH=amd64 TARGET_OS=mingw libmlton.a
	cp win64/sizes mingw64.staging/runtime/gen
	# end of work-around
	make -C mingw64.staging/runtime TARGET=amd64-mingw32msvc TARGET_ARCH=amd64 TARGET_OS=mingw OMIT_BYTECODE=yes
	cp mingw64.staging/runtime/gen/sizes mingw64.staging/runtime/*.a mingw64.staging/gmp.dest/usr/local/lib/libgmp.a $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc
	rm $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/*-gdb.a
	cp mingw64.staging/runtime/gen/c-types.h mingw64.staging/runtime/gmp.h $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/include
	cp mingw64.staging/runtime/gen/c-types.sml $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/sml
	echo mingw > $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/os
	echo amd64 > $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/arch
	amd64-mingw32msvc-gcc -std=gnu99 -m64 -o mingw64.staging/constants -O1 -w -I /usr/lib/mlton/include -I $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/include -L $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc constants.c -lmlton -lgmp -lgdtoa -lgmp -lm
	# another no win64 work-around
	cp win64/constants $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/constants
	#./mingw64.staging/constants > $@.tmp/usr/lib/mlton/targets/amd64-mingw32msvc/constants
	mv $@.tmp $@

.PRECIOUS: mlton_$(VERSION)-$(REVISION)_%.deb
mlton_$(VERSION)-$(REVISION)_%.deb:
	rm -f $@.tmp $@
	wget -O $@.tmp "$(MIRROR)/pool/main/m/mlton/$@"
	mv $@.tmp $@

cross-%:	mlton_$(VERSION)-$(REVISION)_%.deb
	rm -rf $@.tmp $@
	mkdir $@.tmp
	dpkg-deb --fsys-tarfile $< | tar xv -C $@.tmp ./usr/lib/mlton/targets/self
	mv $@.tmp/usr/lib/mlton/targets/self $@.tmp/usr/lib/mlton/targets/$(shell dpkg-architecture -a$* -f -qDEB_HOST_GNU_TYPE 2>/dev/null)
	mv $@.tmp $@
