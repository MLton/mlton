## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-1999 NEC Research Institute.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

PATH = ../../../build/bin:$(shell echo $$PATH)

mlton = mlton

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
