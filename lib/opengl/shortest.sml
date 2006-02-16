open GL;
open GLUT;

fun display () = (
     glClearColor 0.0 0.0 1.0 1.0;
     glClear GL_COLOR_BUFFER_BIT;
     glFlush()
)

fun main () = 
(
         glutInit();
         glutCreateWindow "Short Test";
         glutDisplayFunc display;
         print("Click the close icon to close the window.");
         glutMainLoop()
)

val _ = main();

