mlton = mlton

main: ffi.o main.sml ffi.h
	$(mlton) -I. -include ffi.h main.sml ffi.o

ffi.o: ffi.c ffi.h
	$(mlton) -stop o ffi.c

clean:
	../../../bin/clean
