open GL;
open GLUT;
open GLU;

fun bmstring (x : GLdouble) (y : GLdouble) (f : char -> unit) (s : string) =
    let 
        val chars = String.explode s;
    in
        glColor3d 1.0 1.0 1.0;
        glRasterPos2d x y;
        (*glutBitmapCharacter GLUT_BITMAP_HELVETICA_10 65*)
        map f chars
    end

fun ststring (x : GLdouble) (y : GLdouble) (f : char -> unit) (s : string) =
    let 
        val chars = String.explode s;
    in
        glColor3d 1.0 0.0 0.0;
        glPushMatrix();
        glTranslated x y 0.0;
        map f chars;
        glPopMatrix()
    end

fun hello (x : GLdouble) (y : GLdouble) =
     let
         val f1 = 
             glutBitmapCharacter GLUT_BITMAP_HELVETICA_10
         val f2 = 
             glutBitmapCharacter GLUT_BITMAP_HELVETICA_12
         val f3 = 
             glutBitmapCharacter GLUT_BITMAP_HELVETICA_18
         val f4 = 
             glutBitmapCharacter GLUT_BITMAP_9_BY_15
         val f5 = 
             glutBitmapCharacter GLUT_BITMAP_8_BY_13
         val f6 = 
             glutBitmapCharacter GLUT_BITMAP_TIMES_ROMAN_10
         val f7 = 
             glutBitmapCharacter GLUT_BITMAP_TIMES_ROMAN_24
         val f8 = 
             glutStrokeCharacter GLUT_STROKE_ROMAN
         val f9 = 
             glutStrokeCharacter GLUT_STROKE_MONO_ROMAN
     in
         bmstring x y f1 "Hello";
         bmstring (x-20.0) y f2 "Mike";
         bmstring (x-20.0) (y-30.0) f3 "Hello1";
         bmstring x (y-20.0) f4 "Mike1";
         bmstring (x-40.0) (y+10.0) f5 "Hello2";
         bmstring (x-40.0) (y-40.0) f6 "Mike2";
         bmstring (x-20.0) (y+20.0) f7 "Hello3";
         ststring (x-50.0) (y-50.0) f8 "Mike3";
         ststring (x-40.0) (y+40.0) f9 "Hello4"
     end

fun display () =
    (
     glClear GL_COLOR_BUFFER_BIT;
     hello 0.0 0.0;
     glFlush()
     )

fun main () =
( 
         glutInit();
         glutInitDisplayMode (GLUT_SINGLE + GLUT_RGB);
         glutInitWindowSize 200 200;
         glutCreateWindow "Font Test";
         glMatrixMode (GL_PROJECTION);
         glLoadIdentity();
         gluOrtho2D (~50.0) 50.0 (~50.0) 50.0;
         glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
         glEnable GL_BLEND;
         glEnable GL_LINE_SMOOTH;
         glLineWidth 2.0;
         glClearColor 0.0 0.0 0.0 1.0;
         glutDisplayFunc display;
         print("Click the close icon to close the window.");
         glutMainLoop()
)

val _ = main();


