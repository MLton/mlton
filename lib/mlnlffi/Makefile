CC := gcc -std=gnu99
CFLAGS := -Wall

.DELETE_ON_ERROR:
all: memory/rtld-flags.sml

memory/rtld-flags.sml: gen-rtld-flags.c
	$(CC) $(CFLAGS) -o gen-rtld-flags gen-rtld-flags.c
	./gen-rtld-flags > memory/rtld-flags.sml
