mlton = mlton
mlprof = mlprof

.PHONY: all
all: profile profile2

.PHONY: profile
profile: profiling mlmon.out
	$(mlprof) profiling mlmon.out

mlmon.out: profiling
	./profiling

profiling: profiling.sml
	$(mlton) -profile true -keep g profiling.sml

.PHONE: profile2
profile2: profiling2 mlmon.fib.out mlmon.tak.out
	$(mlprof) profiling2 mlmon.fib.out
	$(mlprof) profiling2 mlmon.tak.out
	$(mlprof) profiling2 mlmon.fib.out mlmon.tak.out mlmon.out

mlmon.fib.out:
	./profiling2
mlmon.tak.out:
	./profiling2

profiling2: profiling2.sml
	$(mlton) -profile true -keep g profiling2.sml

.PHONY: clean
clean:
	../../../bin/clean
