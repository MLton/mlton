mlton = mlton
MLTONFLAGS = -verbose 0

# On Cygwin Windows
GL_LINK_OPTS = -link-opt '-L/lib/w32api -lglut32 -lglu32 -lopengl32'

# On Linux
# GL_LINK_OPTS = -link-opt '-lglut -lGLU -lGL'

CPPFLAGS =  -P -C -x c -E

GL_CPP_STRUCT = ../gl/gl.sml
GL_CPP_SIG = ../gl/gl.sig
GL_CPP = $(GL_CPP_STRUCT) $(GL_CPP_SIG)
GL_SIG = GL.sig
GL_STRUCT = GL.sml
GL_SML = $(GL_STRUCT) $(GL_SIG)
GL_C   = GL_c.c GL_h.h

GLU_CPP_STRUCT = ../glu/glu.sml
GLU_CPP_SIG = ../glu/glu.sig
GLU_CPP = $(GLU_CPP_STRUCT) $(GLU_CPP_SIG)
GLU_SIG = GLU.sig
GLU_STRUCT = GLU.sml
GLU_SML = $(GLU_STRUCT) $(GLU_SIG)
GLU_C   = GLU_c.c GLU_h.h

GLUT_CPP_STRUCT = ../glut/glut.sml
GLUT_CPP_SIG = ../glut/glut.sig
GLUT_CPP = $(GLUT_CPP_STRUCT) $(GLUT_CPP_SIG)
GLUT_SIG = GLUT.sig
GLUT_STRUCT = GLUT.sml
GLUT_SML = $(GLUT_STRUCT) $(GLUT_SIG)
GLUT_C   = GLUT_c.c GLUT_h.h

.PHONY: all
all:  gl glu glut test

smlgllib: gl glut
	ar rv libmltongl.a Gl.*.o GL_c.o Glut.*.o GLUT_c.o

gl: $(GL_SML) $(GL_C) GL_c.o

GL.sml: $(GL_CPP)
	gcc -DMLTON_SOURCE $(CPPFLAGS) $(GL_CPP_STRUCT) > $(GL_STRUCT)

GL.sig: $(GL_CPP)
	gcc -DMLTON_SOURCE $(CPPFLAGS) $(GL_CPP_SIG) > $(GL_SIG)

GL_h.h: $(GL_SML)
	$(mlton) -export-header true GL.cm > GL_h.h

GL_c.o: $(GL_C)
	gcc -c GL_c.c

glu: $(GLU_SML) $(GLU_C) GLU_c.o

GLU.sml:  $(GL_CPP) $(GLU_CPP)
	gcc -DMLTON_SOURCE $(CPPFLAGS) $(GLU_CPP_STRUCT) > $(GLU_STRUCT)

GLU.sig:  $(GLU_CPP) $(GL_CPP)
	gcc -DMLTON_SOURCE $(CPPFLAGS) $(GLU_CPP_SIG) > $(GLU_SIG)

GLU_h.h: $(GLU_SML)
	$(mlton) -export-header true GLU.cm > GLU_h.h

GLU_c.o: $(GLU_C)
	gcc -c GLU_c.c

glut: $(GLUT_SML) $(GLUT_C) GLUT_c.o

GLUT.sml: $(GLUT_CPP) $(GL_CPP)
	gcc -DMLTON_SOURCE $(CPPFLAGS) $(GLUT_CPP_STRUCT) > $(GLUT_STRUCT)

GLUT.sig: $(GLUT_CPP) $(GL_CPP)
	gcc -DMLTON_SOURCE $(CPPFLAGS)  $(GLUT_CPP_SIG) > $(GLUT_SIG)

GLUT_h.h: $(GLUT_SML)
	$(mlton) $(MLTONFLAGS) -export-header true GLUT.cm > GLUT_h.h

GLUT_c.o: $(GLUT_C)
	gcc -c GLUT_c.c

test: shortest hello solar atom triangle spin_cube blender molehill bits

shortest: $(GLUT_SML) $(GL_SML) shortest.sml
	mlton $(GL_LINK_OPTS) shortest.cm GL_c.c GLUT_c.c

hello: $(GLUT_SML) $(GLU_SML) $(GL_SML) hello.sml
	mlton $(GL_LINK_OPTS) hello.cm GL_c.c GLUT_c.c

solar: $(GLUT_SML) $(GLU_SML) $(GL_SML) solar.sml
	mlton $(GL_LINK_OPTS) solar.cm GL_c.c GLUT_c.c

atom: $(GLUT_SML) $(GLU_SML) $(GL_SML) atom.sml
	mlton $(GL_LINK_OPTS) atom.cm GL_c.c GLUT_c.c

bits: $(GLUT_SML) $(GLU_SML) $(GL_SML) bits.sml
	mlton $(GL_LINK_OPTS) bits.cm Gl_c.c Glut_c.c

triangle: $(GLUT_SML) $(GLU_SML) $(GL_SML) triangle.sml
	mlton $(GL_LINK_OPTS) triangle.cm GL_c.c GLUT_c.c

spin_cube: $(GLUT_SML) $(GLU_SML) $(GL_SML) spin_cube.sml
	mlton $(GL_LINK_OPTS) spin_cube.cm GL_c.c GLUT_c.c

blender: $(GLUT_SML) $(GLU_SML) $(GL_SML) blender.sml
	mlton $(GL_LINK_OPTS) blender.cm GL_c.c GLUT_c.c

molehill: $(GLUT_SML) $(GLU_SML) $(GL_SML) molehill.sml
	mlton $(GL_LINK_OPTS) molehill.cm GL_c.c GLUT_c.c

clean:
	rm -f *_h.h *.stackdump *~ *.o *.exe Glut *.a hello shortest atom solar triangle spin_cube blender molehill bits

