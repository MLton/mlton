open GL;
open GLU;
open GLUT;

val whiteLight =  [0.2, 0.2, 0.2, 1.0]
val sourceLight =  [0.8, 0.8, 0.8, 1.0]
val lightPos =  [0.0, 0.0, 0.0, 1.0]
val xRot         = ref 0.0
val yRot         = ref 0.0
val fElect1      = ref 0.0

fun changeSize ((width : int), (height : int)) : unit = 
    let 
        val nRange    = ref 100.0
        val h =
            Real.fromInt (if height = 0 then 
                              1
                          else 
                              height)
        val w = Real.fromInt (width)
    in 
        glViewport 0 0 (Real.trunc w) (Real.trunc h);
        glMatrixMode GL_PROJECTION;
        glLoadIdentity();

        if w <= h then
             glOrtho (~(!nRange)) 
                     (!nRange) 
                     (~(!nRange) * h / w)
                     (!nRange * h / w)
                     (~(!nRange))
                     (!nRange)
        else
             glOrtho (~(!nRange) * w / h)
                     (!nRange * w / h) 
                     (~(!nRange)) 
                     (!nRange) 
                     (~(!nRange))
                     (!nRange);
             glMatrixMode(GL_MODELVIEW);
             glLoadIdentity()
    end

fun initialise () =
    (
     glutInit();
     glutInitDisplayMode (GLUT_DOUBLE + GLUT_RGBA);
     glutInitWindowPosition 100 100;
     glutInitWindowSize 250 250;
     glutCreateWindow "Atom";
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
        glLoadIdentity();
        glTranslatef 0.0 0.0 ~100.0;
        glColor3d 1.0 0.0 0.0;
        glutSolidSphere 10.0 20 20;
        glColor3d 1.0 1.0 0.0 ;

        glPushMatrix();
        glRotatef (!fElect1) 0.0 1.0 0.0;
        glTranslatef 90.0 0.0 0.0;
        glutSolidSphere 6.0 20 20;
        glPopMatrix();

        glPushMatrix();
        glRotatef 45.0 0.0 0.0 1.0;
        glRotatef (!fElect1) 0.0 1.0 0.0;
        glTranslatef ~70.0 0.0 0.0;
        glutSolidSphere 6.0 20 20;
        glPopMatrix();
        
        glPushMatrix();
        glRotatef 315.0 0.0 0.0 1.0;
        glRotatef (!fElect1) 0.0 1.0 0.0;
        glTranslatef 0.0 0.0 60.0;
        glutSolidSphere 6.0 20 20;
        glPopMatrix();

        fElect1 := !fElect1 + 10.0;
        if (!fElect1 > 360.0) then
            fElect1 := 0.0
        else
            ();
        glFlush();
        glutSwapBuffers()
    )

fun limitXRot() : unit =
    (
     if !xRot > 356.0 then
         xRot := 0.0
     else
          if !xRot < ~1.0 then
              xRot := 355.0
          else
              ()
     )

fun limitYRot() : unit =
    (
     if !yRot > 356.0 then
         yRot := 0.0
     else
          if !yRot < ~1.0 then
              yRot := 355.0
          else
              ()
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


