open GL;
open GLU;
open GLUT;

val xRot         = ref 0.0
val yRot         = ref 0.0
val bCull        = ref false
val bOutline     = ref false
val bDepth       = ref false
val GL_PI        = 3.1415

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
	glMatrixMode(GL_PROJECTION);
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
        glMatrixMode GL_MODELVIEW;
	glLoadIdentity()
    end

fun initialise () =
    (
     glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGBA);
     glutInitWindowPosition 100 100;
     glutInitWindowSize 250 250;
     glutCreateWindow "Triangles";
     glClearColor 0.0 0.0 0.0 1.0;
     glColor3d 0.0 1.0 0.0;
     glShadeModel GL_FLAT;
     glFrontFace GL_CW
     )

fun renderScene () =
    let
	local
	    fun doPart angle bPivot =
		(
		 if bPivot then
		     glColor3d 0.0 1.0 0.0
		 else
		     glColor3d 1.0 0.0 0.0;
		 glVertex2d (50.0 * Math.sin angle) (50.0 * Math.cos angle);
		 not bPivot
		 )
	in
	    fun fan angle bPivot =
		if angle > (2.0 * GL_PI) then
		    ()
		else 
		    fan (angle + GL_PI/8.0) (doPart angle bPivot)
	end
    in
	glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
	if !bCull then
	    glEnable(GL_CULL_FACE)
	else
	    glDisable(GL_CULL_FACE);

	if !bDepth then
	    glEnable(GL_DEPTH_TEST)
	else
	    glDisable(GL_DEPTH_TEST);

        if !bOutline then
	    glPolygonMode GL_BACK GL_LINE
	else
	    glPolygonMode GL_BACK GL_FILL;

	glPushMatrix();
	glRotated (!xRot) 1.0 0.0 0.0;
	glRotated (!yRot) 0.0 1.0 0.0;
	glBegin GL_TRIANGLE_FAN;
	glVertex3d 0.0 0.0 75.0;
	fan 0.0 false;
	glEnd();
	glBegin GL_TRIANGLE_FAN;
	glVertex2d 0.0 0.0;
	fan 0.0 false;
	glEnd();
	glPopMatrix();
	glFlush();
	glutSwapBuffers()
    end

fun cKeyCallback() : unit =
    (
     bCull := not (!bCull);
     print ("Toggled Cull " ^ (Bool.toString (!bCull)) ^ "\n")
     )

fun oKeyCallback() : unit =
    (
     bOutline := not (!bOutline);
     print ("Toggled outline rendering " ^ (Bool.toString (!bOutline)) ^ "\n")
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

fun dKeyCallback() : unit =
    (
     bDepth := not (!bDepth);
     print ("Toggled depth " ^ (Bool.toString (!bDepth)) ^ "\n")
     )

fun keyCallback ((c:char), (x:int), (y:int)) : unit
    if c == 'c' then cKeyCallback()
    else if c == 'o' then oKeyCallback()
    else if c == 'd' then dKeyCallback()
	  
fun upKeyCallback() : unit =
    (
     xRot := !xRot - 5.0;
     limitXRot()
     )

fun downKeyCallback() : unit =
    (
     xRot := !xRot + 5.0;
     limitXRot()
     )

fun leftKeyCallback() : unit =
    (
     yRot := !yRot - 5.0;
     limitYRot()
     )

fun rightKeyCallback() : unit =
    (
     yRot := !yRot + 5.0;
     limitYRot()
     )

fun main () = 
    (
     initialise();
     print ("Press c - Toggle culling, o - Toggle outline, d - Toggle depth,\n Arrow keys rotate\n");
     glutReshapeFunc changeSize;
     glutKeyFunc AUX_c cKeyCallback;
     auxKeyFunc AUX_o oKeyCallback;
     auxKeyFunc AUX_d dKeyCallback;
     auxKeyFunc AUX_UP upKeyCallback;
     auxKeyFunc AUX_DOWN downKeyCallback;
     auxKeyFunc AUX_LEFT leftKeyCallback;
     auxKeyFunc AUX_RIGHT rightKeyCallback;
     glutIdleFunc renderScene;
     glutMainLoop renderScene
     )

val _ = main();


    