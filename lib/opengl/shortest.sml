open GL;
open GLUT;

fun display1 () = (
     glClearColor 0.0 0.0 1.0 1.0;
     glClear GL.GL_COLOR_BUFFER_BIT;
     glFlush()
)

fun main () = 
(
	 GLUT.glutInit;
	 GLUT.glutCreateWindow "Short Test";
	 GLUT.glutDisplayFunc display1;
	 print("Click the close icon to close the window.");
	 GLUT.glutMainLoop()
)

val _ = main();

