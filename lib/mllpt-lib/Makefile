## Copyright (C) 2011,2016,2018 Matthew Fluet.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

FIND := find
GZIP := gzip
PATCH := patch
TAR := tar

######################################################################
######################################################################

all: ml-lpt/README

ml-lpt/README: ml-lpt.tgz ml-lpt.patch
	rm -rf ml-lpt
	$(GZIP) -dc ml-lpt.tgz | $(TAR) xf -
	$(FIND) ml-lpt -name '._*' -prune -exec rm -f '{}' ';'
	chmod -R a+r ml-lpt
	chmod -R g-s ml-lpt
	$(PATCH) -s -d ml-lpt -p1 < ml-lpt.patch
	touch $@  # ml-lpt/README not created/updated by patch

.PHONY: clean
clean:
	../../bin/clean
