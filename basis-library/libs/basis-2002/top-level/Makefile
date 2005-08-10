## Copyright (C) 2004-2004 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

GEN = generate-overloads

overloads.sml: $(GEN).sml
	mlton $(GEN).sml
	$(GEN) >overloads.sml

.PHONY: clean
clean:
	../../../../bin/clean
