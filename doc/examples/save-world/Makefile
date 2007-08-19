## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

PATH := ../../../build/bin:$(shell echo $$PATH)

mlton := mlton

all: test

.PHONY: test
test: world
	save-world @MLton load-world world --

world: save-world
	save-world

save-world: save-world.sml
	$(mlton) save-world.sml

.PHONY: clean
clean:
	../../../bin/clean
