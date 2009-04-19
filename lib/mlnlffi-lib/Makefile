PATH := ../bin:$(shell echo $$PATH)

CC := gcc -std=gnu99
CFLAGS := -Wall

EXE :=

TARGET_ARCH := $(shell ../../bin/host-arch)
TARGET_OS := $(shell ../../bin/host-os)

ifeq ($(TARGET_OS), cygwin)
EXE := .exe
endif

ifeq ($(TARGET_OS), mingw)
EXE := .exe
endif

.DELETE_ON_ERROR:
all: memory/platform/rtld-flags.$(TARGET_OS).sml

memory/platform/rtld-flags.$(TARGET_OS).sml: gen-rtld-flags.c
	$(CC) $(CFLAGS) -o gen-rtld-flags gen-rtld-flags.c
	./gen-rtld-flags > memory/platform/rtld-flags.$(TARGET_OS).sml
	rm -f gen-rtld-flags$(EXE)

.PHONY: clean
clean:
	../../bin/clean
