#

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

.DELETE_ON_ERROR:
all: memory/platform/rtld-flags.$(TARGET_OS).sml

ifeq (mingw, $(TARGET_OS))
  CFLAGS := -Wall -I.
else
  CFLAGS := -Wall
endif

memory/platform/rtld-flags.$(TARGET_OS).sml: gen-rtld-flags.c
ifeq (mingw, $(TARGET_OS))
	$(CP) "$(ROOT)/package/msys2/dlfcn.h" .
endif
	$(CC) $(CFLAGS) -o gen-rtld-flags gen-rtld-flags.c
	./gen-rtld-flags > memory/platform/rtld-flags.$(TARGET_OS).sml
	$(RM) gen-rtld-flags$(EXE)

.PHONY: clean
clean:
	../../bin/clean
