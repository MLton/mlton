open GL;
open GLU;
open GLUT;

val whiteLight =  [0.2, 0.2, 0.2, 1.0]
val sourceLight =  [0.8, 0.8, 0.8, 1.0]
val lightPos =  [0.0, 0.0, 0.0, 1.0]
val fMoonRot = ref 0.0
val fEarthRot = ref 0.0

fun changeSize ((width : int), (height : int)) : unit = 
    let 
        val nRange = ref 100.0
        val h =
            Real32.fromInt (if height = 0 then 
                              1
                          else 
                              height)
        val w = Real32.fromInt width
        val fAspect = Real32./ (w,h) 
    in 
        glViewport 0 0 (Real32.trunc w) (Real32.trunc h);
        glMatrixMode GL_PROJECTION;
        glLoadIdentity();
        gluPerspective 45.0 (Real32.toLarge fAspect) 1.0 425.0;
        glMatrixMode GL_MODELVIEW;
        glLoadIdentity()
    end

fun initialise () =
    (
     glutInit();
     glutInitDisplayMode (GLUT_DOUBLE + GLUT_RGB);
     glutInitWindowSize 200 200;
     glutCreateWindow "Solar";
     glEnable GL_DEPTH_TEST;
     glFrontFace GL_CCW;        
     glEnable GL_CULL_FACE;
     glEnable GL_LIGHTING;
     glLightModelfv GL_LIGHT_MODEL_AMBIENT whiteLight;
     glLightfv GL_LIGHT0 GL_DIFFUSE sourceLight;
     glLightfv GL_LIGHT0 GL_POSITION lightPos;
     glEnable GL_LIGHT0;
     glEnable GL_COLOR_MATERIAL;
     glColorMaterial GL_FRONT GL_AMBIENT_AND_DIFFUSE;
     glClearColor 0.0 0.0 0.0 1.0
     )

fun renderScene () =
    (
        glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
        glMatrixMode GL_MODELVIEW;
        glPushMatrix();
        glLightfv GL_LIGHT0 GL_POSITION lightPos;
        glTranslatef 0.0 0.0 ~300.0;
        glColor3f 1.0 1.0 0.0;
        glutSolidSphere 15.0 50 50;
        glLightfv GL_LIGHT0 GL_POSITION lightPos;
        glRotatef (!fEarthRot) 0.0 1.0 0.0;
        glColor3f 0.0 0.0 1.0;
        glTranslatef 105.0 0.0 0.0;
        glutSolidSphere 15.0 50 50;
        glColor3f 0.8 0.8 0.8;
        glRotatef(!fMoonRot) 0.0 1.0 0.0;
        glTranslatef 30.0 0.0 0.0;

        fMoonRot := !fMoonRot + 15.0;
        if (!fMoonRot > 360.0) then
            fMoonRot := 0.0
        else
            ();
        glutSolidSphere 6.0 20 20;
        glPopMatrix();
        fEarthRot := !fEarthRot + 5.0;
        if (!fEarthRot > 360.0) then
            fEarthRot := 0.0
        else
            ();
        glFlush();
        glutSwapBuffers()
    )

fun main () = 
    (
     initialise();
     glutReshapeFunc changeSize;
     glutIdleFunc renderScene;
     glutDisplayFunc renderScene;
     glutMainLoop ()
     )

val _ = main();
