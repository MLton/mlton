mlton := mlton
MLTON_FLAGS := \
	-default-ann 'allowFFI true' \
	-target-link-opt cygwin '-L/lib/w32api -lglut32 -lglu32 -lopengl32' \
        -target-link-opt darwin '-framework GLUT -framework OpenGL -framework Foundation' \
	-target-link-opt linux '-lglut -lGLU -lGL'

GL_OBJS  := GL_c.o GLUT_c.o

%_h.h: %.cm %.sig %.sml
	$(mlton) $(MLTON_FLAGS) -export-header $@ -stop tc $<

%.o: %.c

%_c.o: %_c.c %_h.h
	gcc -c $<

%: %.cm $(mlton -stop f %.cm) $(GL_OBJS)
	$(mlton) $(MLTON_FLAGS) $< $(GL_OBJS)

.PHONY: all
all: test

.PHONY: clean
clean:
	../../bin/clean

.PHONY: test
test: shortest hello solar atom triangle spin_cube blender molehill bits points menus
