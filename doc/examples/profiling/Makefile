mlton = mlton
mlprof = mlprof

all: profile

.PHONY: profile
profile: profiling mlmon.out
	$(mlprof) profiling mlmon.out

mlmon.out: profiling
	profiling

profiling: profiling.sml
	$(mlton) -p -keep g profiling.sml

.PHONY: clean
clean:
	../../../bin/clean
