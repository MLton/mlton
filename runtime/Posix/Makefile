CC =		gcc -Wall -I..
CFLAGS =	-O2 -DNODEBUG
AR =		ar rc

OBJS =						\

%-gdb.o: %.c
	$(CC) -g -c -o $@ $<

all: mlton-posix.a

mlton-posix.a: $(OBJS)
	$(AR) $@ $(OBJS)
