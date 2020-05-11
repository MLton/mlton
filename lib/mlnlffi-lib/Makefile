#

ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

.DELETE_ON_ERROR:
all: memory/platform/rtld-flags.$(TARGET_OS).sml

CFLAGS := -Wall
memory/platform/rtld-flags.$(TARGET_OS).sml: gen-rtld-flags.c
	$(CC) $(CFLAGS) -o gen-rtld-flags gen-rtld-flags.c
	./gen-rtld-flags > memory/platform/rtld-flags.$(TARGET_OS).sml
	$(RM) gen-rtld-flags$(EXE)

.PHONY: clean
clean:
	../../bin/clean
