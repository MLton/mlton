mlton = mlton

main: ffi.o main.sml ffi.h
	$(mlton) main.sml ffi.o

ffi.o: ffi.c ffi.h
	$(mlton) -stop o ffi.c

clean:
	../../../bin/clean
