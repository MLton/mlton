open GL;
open GLU;
open GLUT;

val xRot         = ref 0.0
val yRot         = ref 0.0
val GL_PI        = 3.1415

fun changeSize ((width : int), (height : int)) : unit = 
    let 
        val nRange    = ref 100.0
        val h =
            if height = 0 then 
                1
            else 
                height
        val w = width
    in 
        glViewport 0 0 w h;
        glMatrixMode GL_PROJECTION;
        glLoadIdentity();

        if w <= h then
             glOrtho (~(!nRange)) 
                     (!nRange) 
                     (~(!nRange) * Real.fromInt h / Real.fromInt w)
                     (!nRange * Real.fromInt h / Real.fromInt w) 
                     (~(!nRange))
                     (!nRange)
        else
             glOrtho (~(!nRange) * Real.fromInt w / Real.fromInt h)
                     (!nRange * Real.fromInt w / Real.fromInt h)
                     (~(!nRange)) 
                     (!nRange) 
                     (~(!nRange))
                     (!nRange);
        glMatrixMode GL_MODELVIEW;
        glLoadIdentity()
    end

fun renderScene () : unit =
    let
        local
            fun doPoint angle z =
                (
                 glVertex3d (50.0 * (Math.sin angle)) 
                            (50.0 * (Math.cos angle)) 
                            z;
                 z + 0.02
                 )
        in
            fun spiral angle z =
                if angle <= (6.0 * GL_PI) then
                    spiral (angle + 0.02) (doPoint angle z)
                else 
                    ()
        end
    in
        glClear(GL_COLOR_BUFFER_BIT);
        glPushMatrix();
        glRotated (!xRot) 1.0 0.0 0.0;
        glRotated (!yRot) 0.0 1.0 0.0;

        if !xRot >= 356.0 then
            xRot := 0.0
        else
            xRot := (!xRot) + 5.0;
        if !yRot >= 356.0 then
            yRot := 0.0
        else
            yRot := (!yRot) + 5.0;

        glBegin GL_POINTS ;
        spiral 0.0 ~50.0;
        glEnd();
        glPopMatrix();
        glFlush();
        glutSwapBuffers()
    end

fun idleFunction () : unit =
    (
     renderScene()
     )
         

fun main () = 
    (
     glutInit();
     glutInitDisplayMode (GLUT_DOUBLE + GLUT_RGBA);
     glutInitWindowSize 400 400;
     glutCreateWindow "Animating rectangle";
     glClearColor 0.0 0.0 0.0 0.0;
     glColor3d 0.0 1.0 0.0;

     glutReshapeFunc changeSize;
     glutIdleFunc renderScene;
     glutDisplayFunc renderScene;
     glutMainLoop ()
     )

val _ = main();


    
