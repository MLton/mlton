# Specify C compiler and binutils.
# Can be used for alternative tools (e.g., `CC=clang` or `CC=gcc-7`).
CC := cc

# Specify GMP include and library paths, if not on default search paths.
WITH_GMP_DIR :=
ifneq ($(WITH_GMP_DIR),)
WITH_GMP_INC_DIR := $(WITH_GMP_DIR)/include
WITH_GMP_LIB_DIR := $(WITH_GMP_DIR)/lib
endif

# Specify installation prefix and staged install destination.
PREFIX := /usr/local

SED := sed

######################################################################
######################################################################

ROOT := $(shell pwd)

CP := cp -fpR
MKDIR := mkdir -p
RM := rm -rf

######################################################################

TGT_REL_SRC = ref="$(1)" pos="$(2)" down=; ref="$${ref%%/}" pos="$${pos%%/}"; while :; do test "$$pos" = '/' && break ; case "$$ref" in "$$pos"/*) break;; esac; down="../$$down"; pos="$${pos%/*}"; done; echo "$$down$${ref\#\#$$pos/}"

SBIN := $(ROOT)/bin
SLIB := $(ROOT)/lib/mlton
SMAN := $(ROOT)/share/man/man1
SDOC := $(ROOT)/share/doc/mlton

SLIB_REL_SBIN := $(shell $(call TGT_REL_SRC,$(SLIB),$(SBIN)))

prefix := $(PREFIX)
exec_prefix := $(prefix)
bindir := $(exec_prefix)/bin
datarootdir := $(prefix)/share
docdir := $(datarootdir)/doc/mlton
libdir := $(exec_prefix)/lib
mandir := $(datarootdir)/man
man1dir := $(mandir)/man1

TBIN := $(DESTDIR)$(bindir)
TLIB := $(DESTDIR)$(libdir)/mlton
TMAN := $(DESTDIR)$(man1dir)
TDOC := $(DESTDIR)$(docdir)
TEXM := $(TDOC)/examples

TLIB_REL_TBIN := $(shell $(call TGT_REL_SRC,$(TLIB),$(TBIN)))

.PHONY: install
install:
	$(MKDIR) "$(TBIN)" "$(TLIB)" "$(TMAN)" "$(TDOC)"
	$(CP) "$(SBIN)/." "$(TBIN)/"
	$(SED) \
		-e "s;^LIB_REL_BIN=.*;LIB_REL_BIN=\"$(TLIB_REL_TBIN)\";" \
		< "$(SBIN)/mlton" > "$(TBIN)/mlton"
	chmod a+x "$(TBIN)/mlton"
	$(CP) "$(SLIB)/." "$(TLIB)/"
	$(CP) "$(SMAN)/." "$(TMAN)/"
	if [ -d "$(SDOC)" ]; then $(CP) "$(SDOC)/." "$(TDOC)/"; fi

.PHONY: update
update:
	$(CP) "$(SLIB)/targets/self/vars" "$(SLIB)/targets/self/vars.bak"
	$(SED) \
		-e "s;^CC=.*;CC=\"$(CC)\";" \
		-e "s;^GMP_INC_DIR=.*;GMP_INC_DIR=$(if $(WITH_GMP_INC_DIR),\"$(WITH_GMP_INC_DIR)\");" \
		-e "s;^GMP_LIB_DIR=.*;GMP_LIB_DIR=$(if $(WITH_GMP_LIB_DIR),\"$(WITH_GMP_LIB_DIR)\");" \
		< "$(SLIB)/targets/self/vars.bak" > "$(SLIB)/targets/self/vars"
	$(RM) "$(SLIB)/targets/self/vars.bak"
	$(CP) "$(SLIB)/targets/self/constants" "$(SLIB)/targets/self/constants.bak"
	$(SED) \
		-e "s;^default::pie=.*;default::pie=$(subst __pie__,0,$(shell echo "__pie__" | $(CC) -P -E -));" \
		-e "s;^default::pic=.*;default::pic=$(subst __pic__,0,$(shell echo "__pic__" | $(CC) -P -E -));" \
		< "$(SLIB)/targets/self/constants.bak" > "$(SLIB)/targets/self/constants"
	$(RM) "$(SLIB)/targets/self/constants.bak"
	if [ $$(echo "__pie__" | $(CC) -P -E -) != "__pie__" ]; then \
		pi="-pie"; \
	elif [ $$(echo "__pic__" | $(CC) -P -E -) != "__pic__" ]; then \
		pi="-pic"; \
	else \
		pi="-npi"; \
	fi ; \
	for lib in mlton gdtoa; do \
		for md in "" -dbg; do \
			$(CP) "$(SLIB)/targets/self/lib$${lib}$${md}$${pi}.a" "$(SLIB)/targets/self/lib$${lib}$${md}.a"; \
		done; \
	done
