## Copyright (C) 2010 Matthew Fluet.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

#
# SML/NJ tools and heap suffix
#
ML_BUILD             := ml-build
ML_BUILD_FLAGS       := 
ML_HEAP_SUFFIX       := $(shell echo 'TextIO.output (TextIO.stdErr, SMLofNJ.SysInfo.getHeapSuffix ());' | sml 2>&1 1> /dev/null)

#
# cm2mlb root .cm 
#
CM2MLB_CM := cm2mlb.cm
#
# cm2mlb heap image
#
CM2MLB_HEAP := cm2mlb.$(ML_HEAP_SUFFIX)

all: $(CM2MLB_HEAP)

$(CM2MLB_HEAP): cm2mlb.cm cm2mlb.sml gen-mlb.sml cm2mlb-map
	rm -f $(CM2MLB_HEAP)
# 	$(ML_BUILD) $(ML_BUILD_FLAGS) $(CM2MLB_CM) CM2MLB.main $(CM2MLB_HEAP)
	(									\
		echo 'if (CM.make "cm2mlb.cm") handle _ => false';		\
		echo '   then ()';						\
		echo '   else OS.Process.exit OS.Process.failure;'; 		\
		echo 'CM2MLB.export();'						\
	) | sml

.PHONY: clean
clean:
	../../bin/clean

SMLDIR := $(dir $(shell which sml))
.PHONY: install
install: $(CM2MLB_HEAP)
	rm -f $(SMLDIR).heap/$(CM2MLB_HEAP) $(SMLDIR)cm2mlb
	cp $(CM2MLB_HEAP) $(SMLDIR).heap
	(cd $(SMLDIR) ; ln -s .run-sml cm2mlb)