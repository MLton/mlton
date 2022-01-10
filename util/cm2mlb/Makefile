## Copyright (C) 2010,2021-2022 Matthew Fluet.
 #
 # MLton is released under a HPND-style license.
 # See the file MLton-LICENSE for details.
 ##

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

#
# cm2mlb root .cm
#
CM2MLB_CM := cm2mlb.cm
#
# cm2mlb heap image
#
CM2MLB_HEAP := cm2mlb.$(SMLNJ_HEAP_SUFFIX)

all: $(CM2MLB_HEAP)

$(CM2MLB_HEAP):
	(								\
		echo 'if (CM.make "$(CM2MLB_CM)") handle _ => false';	\
		echo '   then ()';					\
		echo '   else OS.Process.exit OS.Process.failure;'; 	\
		echo 'CM2MLB.export();'				\
	) | CM_VERBOSE=false CONTROL_POLY_EQ_WARN=false $(SMLNJ)

$(CM2MLB_HEAP).d:
	@touch $@
	CM_VERBOSE=false $(SMLNJ_MAKEDEPEND) -n -f $@ $(CM2MLB_CM) $(CM2MLB_HEAP) || ($(RM) $@ ; exit 1)
	CM_VERBOSE=false $(SMLNJ_MAKEDEPEND) -n -f $@ $(CM2MLB_CM) $@ || ($(RM) $@ ; exit 1)
ifneq ($(MAKECMDGOALS),clean)
-include $(CM2MLB_HEAP).d
endif

SMLNJ_DIR := $(dir $(shell echo "$$(command -v $(SMLNJ))"))
.PHONY: install
install: $(CM2MLB_HEAP)
	$(RM) $(SMLNJ_DIR).heap/$(CM2MLB_HEAP) $(SMLNJ_DIR)cm2mlb
	$(CP) $(CM2MLB_HEAP) $(SMLNJ_DIR).heap
	(cd $(SMLNJ_DIR) ; ln -s .run-sml cm2mlb)

$(eval $(MK_COMMON_GOALS))
