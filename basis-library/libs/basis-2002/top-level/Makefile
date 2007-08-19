## Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

GEN := generate-overloads

overloads.sml: $(GEN).sml
	mlton $(GEN).sml
	$(GEN) >overloads.sml

.PHONY: clean
clean:
	../../../../bin/clean
