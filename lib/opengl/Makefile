mlton = mlton
MLTONFLAGS = -verbose 0
CPPFLAGS =  -P -C -x c -E
GL_LINK_OPTS = -link-opt -L/lib/w32api -link-opt -lglut32 -link-opt -lglu32 -link-opt -lopengl32

.PHONY: all
all:  gl glu glut test

smlgllib: gl glut
	ar rv libmltongl.a Gl.*.o Gl_c.o Glut.*.o Glut_c.o

gl: GL.sml GL.sig GL_h.h GL_c.o

GL.sml: ../gl/gl.sml ../gl/gl.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../gl/gl.sml > GL.sml

GL.sig: ../gl/gl.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../gl/gl.sig > GL.sig

GL_h.h: GL.sml
	$(mlton) -export-header true GL.sml > GL_h.h

GL_c.o: GL_c.c
	gcc -c GL_c.c

glu: GLU.sml GLU.sig GLU_h.h GLU_c.o

GLU.sml: ../glu/glu.sml ../gl/gl.sml ../gl/gl.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glu/glu.sml > GLU.sml

GLU.sig: ../glu/glu.sig ../gl/gl.sml ../gl/gl.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glu/glu.sig > GLU.sig

GLU_h.h: GLU.sml
	$(mlton) -export-header true GLU.sml > GLU_h.h

GLU_c.o: GLU_c.c
	gcc -c GLU_c.c

glut: GLUT.sml GLUT.sig GLUT_h.h GLUT_c.o

GLUT.sml: ../glut/glut.sml ../glut/glut.sig ../gl/gl.sml ../gl/gl.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glut/glut.sml > GLUT.sml

GLUT.sig: ../glut/glut.sig ../gl/gl.sml ../gl/gl.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glut/glut.sig > GLUT.sig

GLUT_h.h: GLUT.sml
	$(mlton) $(MLTONFLAGS) -export-header true GLUT.sml > GLUT_h.h

GLUT_c.o: GLUT_c.c
	gcc -c GLUT_c.c

test: shortest hello solar atom triangle

shortest: gl glut shortest.sml
	mlton $(GL_LINK_OPTS) shortest.cm Gl_c.c Glut_c.c

hello: gl glu glut hello.sml
	mlton $(GL_LINK_OPTS) hello.cm Gl_c.c Glut_c.c

solar: gl glu glut solar.sml
	mlton $(GL_LINK_OPTS) solar.cm Gl_c.c Glut_c.c

atom: gl glu glut atom.sml
	mlton $(GL_LINK_OPTS) atom.cm Gl_c.c Glut_c.c

triangle: gl glu glut triangle.sml
	mlton $(GL_LINK_OPTS) triangle.cm Gl_c.c Glut_c.c

clean:
	rm -f *_h.h *.stackdump *~ *.o *.exe Glut *.a hello shortest solar