mlton = mlton
mlprof = mlprof
mlton = $(HOME)/mlton/bin/mlton
mlprof = $(HOME)/mlton/src/mlprof/mlprof

.PHONY: all
all: profile-time profile-stack profile-multiple profile-alloc

.PHONY: clean
clean:
	../../../bin/clean

.PHONY: profile-alloc
profile-alloc:
	$(mlton) -profile alloc -profile-stack true list-rev.sml
	./list-rev
	$(mlprof) -show-line true -thresh 1 list-rev mlmon.out
	dot list-rev.dot >list-rev.ps

.PHONE: profile-multiple
profile-multiple:
	$(mlton) -profile time fib-tak.sml
	./fib-tak
	$(mlprof) fib-tak mlmon.fib.out
	$(mlprof) fib-tak mlmon.tak.out
	$(mlprof) fib-tak mlmon.fib.out mlmon.tak.out mlmon.out

.PHONY: profile-stack
profile-stack:
	$(mlton) -profile time -profile-stack true tak.sml
	./tak
	$(mlprof) tak mlmon.out
	dot tak.dot >tak.ps

.PHONY: profile-time
profile-time:
	$(mlton) -profile time tak.sml
	./tak
	$(mlprof) tak mlmon.out
