open GL;
open GLU;
open GLUT;

val light0_ambient = [0.2, 0.2, 0.2, 1.0]
val light0_diffuse = [0.0, 0.0, 0.0, 1.0]
val light1_diffuse = [1.0, 0.0, 0.0, 1.0]
val light1_position = [1.0, 1.0, 1.0, 0.0]
val light2_diffuse = [0.0, 1.0, 0.0, 1.0]
val light2_position = [~1.0, ~1.0, 1.0, 0.0]
val s = ref 0.0
val angle1 = ref 0.0
val angle2 = ref 0.0

val amb = [0.4, 0.4, 0.4:GLreal]
val dif = [1.0, 1.0, 1.0:GLreal]



fun output (x : GLreal) (y : GLreal)  (text : string) : unit =
(
  glPushMatrix ();
  glTranslatef x y 0.0;
  map ( glutStrokeCharacter GLUT_STROKE_ROMAN ) ( String.explode text );
  glPopMatrix()
)

fun display () :unit =
    (
     let 
         val a = (Real32.fromLarge IEEEReal.TO_NEAREST (Math.cos ( !s ) / 2.0 + 0.5))
         val b = (Real32.fromLarge IEEEReal.TO_NEAREST (0.5 - Math.cos ( !s * 0.95 ) / 2.0))
     in
         glClear ( GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT );
         glEnable GL_LIGHT1;
         glDisable GL_LIGHT2;
         glMaterialfv GL_FRONT GL_AMBIENT (Array.fromList (amb@[a]));
         glMaterialfv GL_FRONT GL_DIFFUSE (Array.fromList (dif@[a]));

         glPushMatrix ();
         glTranslatef (~0.3) (~0.3) 0.0;
         glRotatef (!angle1) 1.0 5.0 0.0;
         glCallList 1;        (* render ico display list *)
         glPopMatrix();

         glClear GL_DEPTH_BUFFER_BIT;
         glEnable GL_LIGHT2 ;
         glDisable GL_LIGHT1 ;
         glMaterialfv GL_FRONT GL_AMBIENT (Array.fromList (amb@[b]));
         glMaterialfv GL_FRONT GL_DIFFUSE (Array.fromList (dif@[b]));

         glPushMatrix();
         glTranslatef 0.3 0.3 0.0;
         glRotatef (!angle2) 1.0 0.0 5.0;
         glCallList 1;        (* render ico display list *)
         glPopMatrix();

         glPushAttrib GL_ENABLE_BIT;
         glDisable GL_DEPTH_TEST;
         glDisable GL_LIGHTING;
         glMatrixMode GL_PROJECTION;
         glPushMatrix();
         glLoadIdentity();
         gluOrtho2D 0.0 1500.0 0.0 1500.0;
         glMatrixMode GL_MODELVIEW;
         glPushMatrix();
         glLoadIdentity();
         (* Rotate text slightly to help show jaggies. *)
         glRotatef 4.0 0.0 0.0 1.0;
         output 200.0 225.0 "This is antialiased.";
         glDisable GL_LINE_SMOOTH;
         glDisable GL_BLEND;
         output 160.0 100.0 "This text is not.";
         glPopMatrix ();
         glMatrixMode GL_PROJECTION;
         glPopMatrix();
         glPopAttrib();
         glMatrixMode GL_MODELVIEW;

         glutSwapBuffers()
     end
)

fun idle() =
(
 angle1 := Real32.rem (((!angle1) + 0.8), 360.0);
 angle2 := Real32.rem (((!angle2) + 1.1), 360.0);
 s := !s + 0.05;
 glutPostRedisplay()
 )

fun idle_nuthin() =
( )

fun visible ( vis : Word32.word ) : unit =
(
  if vis = GLUT_VISIBLE then
    glutIdleFunc(idle)
  else
    glutIdleFunc(idle_nuthin)
)

fun main () =
(
  glutInit ();
  glutInitDisplayMode (GLUT_DOUBLE + GLUT_RGB + GLUT_DEPTH);
  glutCreateWindow "blender";
  glutDisplayFunc display;
  glutVisibilityFunc visible;

  glNewList 1 GL_COMPILE;  (* create ico display list *)
  glutSolidIcosahedron();
  glEndList();

  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;
  glLightfv GL_LIGHT0 GL_AMBIENT light0_ambient;
  glLightfv GL_LIGHT0 GL_DIFFUSE light0_diffuse;
  glLightfv GL_LIGHT1 GL_DIFFUSE light1_diffuse;
  glLightfv GL_LIGHT1 GL_POSITION light1_position;
  glLightfv GL_LIGHT2 GL_DIFFUSE light2_diffuse;
  glLightfv GL_LIGHT2 GL_POSITION light2_position;
  glEnable GL_DEPTH_TEST;
  glEnable GL_CULL_FACE;
  glEnable GL_BLEND;
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
  glEnable GL_LINE_SMOOTH;
  glLineWidth 2.0;

  glMatrixMode GL_PROJECTION;
  gluPerspective (* field of view in degree *) 40.0
                 (* aspect ratio *) 1.0
                 (* Z near *) 1.0
                 (* Z far *) 10.0;
  glMatrixMode GL_MODELVIEW;
  gluLookAt 0.0 0.0 5.0  (* eye is at (0,0,5) *)
            0.0 0.0 0.0      (* center is at (0,0,0) *)
            0.0 1.0 0.0;      (* up is in positive Y direction *)
  glTranslatef 0.0 0.6 (~1.0);

  glutMainLoop()
)

val _ = main ()