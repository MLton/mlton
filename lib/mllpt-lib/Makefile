## Copyright (C) 2011,2016,2018 Matthew Fluet.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

all: ml-lpt/README

ml-lpt/README: ml-lpt.tgz ml-lpt.patch
	rm -rf ml-lpt
	tar -x -z -f ml-lpt.tgz
	find ml-lpt -name '._*' -exec rm -f '{}' ';'
	chmod -R a+r ml-lpt
	chmod -R g-s ml-lpt
	patch -s -d ml-lpt -p1 < ml-lpt.patch
	touch $@  # ml-lpt/README not created by patch

.PHONY: clean
clean:
	../../bin/clean
