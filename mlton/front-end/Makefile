## Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-1999 NEC Research Institute.
 #
 # MLton is released under the GNU General Public License (GPL).
 # Please see the file MLton-LICENSE for license information.
 ##

.PHONY: all
all: ml.lex.sml ml.grm.sig ml.grm.sml mlb.lex.sml mlb.grm.sig mlb.grm.sml

.PHONY: clean
clean:
	../../bin/clean

ml.lex.sml: ml.lex
	rm -f ml.lex.sml
	mllex ml.lex
	chmod -w ml.lex.sml

ml.grm.sig ml.grm.sml: ml.grm
	rm -f ml.grm.*
	mlyacc ml.grm
	chmod -w ml.grm.*

mlb.lex.sml: mlb.lex
	rm -f mlb.lex.sml
	mllex mlb.lex
	chmod -w mlb.lex.sml

mlb.grm.sig mlb.grm.sml: mlb.grm
	rm -f mlb.grm.*
	mlyacc mlb.grm
	chmod -w mlb.grm.*
