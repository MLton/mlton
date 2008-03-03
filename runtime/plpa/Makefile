
SRC = plpa_api_probe.c plpa_dispatch.c
OBJ = ${SRC:%.c=%.o}

%.o : %.c
	gcc -DHAVE_CONFIG_H -I. -g -c $<  -fPIC -DPIC -o $@;

libplpa_included.a: ${OBJ}
	ar cru $@ $<;
	ranlib $@;
