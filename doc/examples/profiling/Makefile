mlton = mlton
mlprof = mlprof
ALLOC_EX = list-rev
MULT_EX = fib-tak
TIME_EX = tak

.PHONY: all
all: profile-time profile-alloc profile-stack profile-multiple

.PHONY: clean
clean:
	../../../bin/clean


.PHONY: profile-alloc
profile-alloc:
	$(mlton) -profile alloc $(ALLOC_EX).sml
	./$(ALLOC_EX)
	$(mlprof) -show-line true -thresh 1 $(ALLOC_EX) mlmon.out

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
	$(mlprof) -show-line true -thresh 1 $(ALLOC_EX) mlmon.out
	dot $(ALLOC_EX).dot >$(ALLOC_EX).ps

.PHONY: profile-time
profile-time:
	$(mlton) -profile time $(TIME_EX).sml
	./$(TIME_EX)
	$(mlprof) $(TIME_EX) mlmon.out
	$(mlprof) -raw true $(TIME_EX) mlmon.out
	$(mlprof) -show-line true $(TIME_EX) mlmon.out
