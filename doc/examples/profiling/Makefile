## Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 #    Jagannathan, and Stephen Weeks.
 # Copyright (C) 1997-2000 NEC Research Institute.
 #
 # MLton is released under a BSD-style license.
 # See the file MLton-LICENSE for details.
 ##

mlton := mlton
mlprof := mlprof
ALLOC_EX := list-rev
COUNT_EX := tak
MULT_EX := fib-tak
TIME_EX := tak

PATH := ../../../build/bin:$(shell echo $$PATH)

.PHONY: all
all: profile-time profile-alloc profile-count profile-stack profile-multiple

.PHONY: clean
clean:
	../../../bin/clean

.PHONY: profile-alloc
profile-alloc:
	$(mlton) -profile alloc $(ALLOC_EX).sml
	./$(ALLOC_EX)
	$(mlprof) -show-line true $(ALLOC_EX) mlmon.out

.PHONY: profile-count
profile-count:
	$(mlton) -profile count $(COUNT_EX).sml
	./$(COUNT_EX)
	$(mlprof) -raw true -show-line true $(COUNT_EX) mlmon.out

.PHONE: profile-multiple
profile-multiple:
	$(mlton) -profile time $(MULT_EX).sml
	./$(MULT_EX)
	$(mlprof) $(MULT_EX) mlmon.fib.out
	$(mlprof) $(MULT_EX) mlmon.tak.out
	$(mlprof) $(MULT_EX) mlmon.fib.out mlmon.tak.out mlmon.out

.PHONY: profile-stack
profile-stack:
	$(mlton) -profile alloc -profile-stack true $(ALLOC_EX).sml
	./$(ALLOC_EX)
	$(mlprof) -call-graph $(ALLOC_EX).dot -show-line true \
		$(ALLOC_EX) mlmon.out
	dot -Tps $(ALLOC_EX).dot >$(ALLOC_EX).ps || true

.PHONY: profile-time
profile-time:
	$(mlton) -profile time $(TIME_EX).sml
	./$(TIME_EX)
	$(mlprof) $(TIME_EX) mlmon.out
	$(mlprof) -raw true $(TIME_EX) mlmon.out
	$(mlprof) -show-line true $(TIME_EX) mlmon.out
