## Copyright (C) 2011,2016 Matthew Fluet.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: ml-lpt/README

ml-lpt/README: ml-lpt.tgz ml-lpt.patch
	rm -rf ml-lpt
	tar -x -z --exclude '*/._*' -f ml-lpt.tgz
	chmod -R a+r ml-lpt
	chmod -R g-s ml-lpt
	patch -s -d ml-lpt -p1 < ml-lpt.patch

.PHONY: clean
clean:
	../../bin/clean

.PHONY: patch
patch:
	mv ml-lpt ml-lpt-mlton
	tar -x -z --exclude '*/._*' -f ml-lpt.tgz
	diff -Naur ml-lpt ml-lpt-mlton >ml-lpt.patch || exit 0
	rm -rf ml-lpt
	mv ml-lpt-mlton ml-lpt
