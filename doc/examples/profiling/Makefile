mlton = mlton
mlprof = mlprof
mlton = $(HOME)/mlton/bin/mlton
mlprof = $(HOME)/mlton/src/mlprof/mlprof

.PHONY: all
all: profile profile2 alloc

.PHONY: alloc
alloc:
	$(mlton) -profile alloc profiling-alloc.sml
	./profiling-alloc
	$(mlprof) profiling-alloc mlmon.out

.PHONY: profile
profile:
	$(mlton) -profile time -keep g profiling.sml
	./profiling
	$(mlprof) profiling mlmon.out

.PHONE: profile2
profile2:
	$(mlton) -profile time -keep g profiling2.sml
	./profiling2
	$(mlprof) profiling2 mlmon.fib.out
	$(mlprof) profiling2 mlmon.tak.out
	$(mlprof) profiling2 mlmon.fib.out mlmon.tak.out mlmon.out

.PHONY: clean
clean:
	../../../bin/clean
