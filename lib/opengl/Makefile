mlton = mlton
MLTONFLAGS = -verbose 1

HOST_OS = $(shell ../../bin/host-os)

ifeq (cygwin, $(HOST_OS))
GL_LINK_OPTS = -link-opt '-L/lib/w32api -lglut32 -lglu32 -lopengl32'
else
ifeq (linux, $(HOST_OS))
GL_LINK_OPTS = -link-opt '-lglut -lGLU -lGL'
else
$(error Don't know how to set GL_LINK_OPTS for $(HOST_OS))
endif
endif

GL_OBJS  = GL_c.o GLUT_c.o

%_h.h: %.cm %.sig %.sml
	$(mlton) -export-header $@ -stop tc $<

%.o: %.c

%_c.o: %_c.c %_h.h
	gcc -c $<

%: %.cm $(mlton -stop f %.cm) $(GL_OBJS)
	$(mlton) $(MLTON_FLAGS) $(GL_LINK_OPTS) $< $(GL_OBJS)

.PHONY: all
all: test

.PHONY: clean
clean:
	../../bin/clean

.PHONY: test
test: shortest hello solar atom triangle spin_cube blender molehill bits points
