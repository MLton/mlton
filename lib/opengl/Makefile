mlton = mlton
MLTONFLAGS = -verbose 0
CPPFLAGS =  -P -C -x c -E
GL_LINK_OPTS = -link-opt -L/lib/w32api -link-opt -lglut32 -link-opt -lopengl32

.PHONY: all
all:  gl glu glut test

smlgllib: gl glut
	ar rv libmltongl.a Gl.*.o Gl_c.o Glut.*.o Glut_c.o

gl: gl_sml gl_sig gl_h gl_c_o

gl_sml: ../gl/gl.sml
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../gl/gl.sml > GL.sml
	touch GL.sml

gl_sig: ../gl/gl.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../gl/gl.sig > GL.sig
	touch GL.sig

gl_h: GL.sml
	$(mlton) -export-header true GL.sml > GL_h.h

gl_c_o: GL_c.c
	gcc -c GL_c.c

glu: glu_sml glu_sig glu_h glu_c_o

glu_sml: ../glu/glu.sml
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glu/glu.sml > GLU.sml
	touch GLU.sml

glu_sig: ../glu/glu.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glu/glu.sig > GLU.sig
	touch GLU.sig

glu_h: GLU.sml
	$(mlton) -export-header true GLU.sml > GLU_h.h

glu_c_o: GLU_c.c
	gcc -c GLU_c.c

glut: glut_sml glut_sig glut_h glut_c_o

glut_sml: ../glut/glut.sml
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glut/glut.sml > GLUT.sml
	touch GLUT.sml

glut_sig: ../glut/glut.sig
	gcc -DMLTON_SOURCE $(CPPFLAGS) ../glut/glut.sig > GLUT.sig
	touch GLUT.sig

glut_h: GLUT.sml
	$(mlton) $(MLTONFLAGS) -export-header true GLUT.sml > GLUT_h.h

glut_c_o: GLUT_c.c
	gcc -c GLUT_c.c

nj: *.sml
	cat Glut.sml | sed -e 's/_ffi/Unsafe.cast/' -e 's/_export/Unsafe.cast/' > Glut_nj.sml

typecheck: nj
	echo "CM.make();" | sml.bat

test: shortest hello

shortest: gl glut shortest.sml
	mlton $(GL_LINK_OPTS) shortest.cm Gl_c.c Glut_c.c

hello: gl glut hello.sml
	mlton $(GL_LINK_OPTS) hello.cm Gl_c.c Glut_c.c

clean:
	rm -f *_h.h *.stackdump *~ *.o *.exe Glut *.a