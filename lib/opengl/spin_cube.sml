open GL;
open GLU;
open GLUT;

val whiteLight =  [0.2, 0.2, 0.2, 1.0]
val sourceLight =  [0.8, 0.8, 0.8, 1.0]
val lightPos =  [0.0, 0.0, 0.0, 1.0]

datatype spec = RGB of GLreal * GLreal * GLreal;

fun initialise () =
    (
     glutInit;
     glutInitDisplayMode (GLUT_DOUBLE+GLUT_RGB);
     glutInitWindowSize 400 400;
     glutCreateWindow "Spinning Cube";
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

(* Draw a primitive *)
fun DrawPrim (_,[]) = glFlush ()
  | DrawPrim (obj,l) =
    let
	fun draw_vertices [] = ()
	  | draw_vertices ((x,y,z)::t) =
		    ((glVertex3f x y z); draw_vertices t)
	  
	fun draw_all [] = ()
	  | draw_all ((RGB(r,g,b), v)::t) =
	    ((glColor3f r g b) ; draw_vertices(v);
	     draw_all t)
    in
	(glBegin(obj);
	 draw_all l;
	 glEnd();
	 glFlush())
    end

fun loop () : unit = 
  (
   glClear(GL_COLOR_BUFFER_BIT);
   DrawPrim (GL_QUADS,
	     [
	      (RGB(0.9, 1.0, 0.0),
	       [(~1.0, 1.0, 1.0), (1.0,1.0,1.0)]),
	      (RGB(0.0,0.7,0.1),
	       [(1.0,~1.0,1.0),(~1.0,~1.0,1.0)]),
	      
	      (* (RGB(0.9,1.0,0.0),
	       [(~1.0,1.0,~1.0), (1.0,1.0,~1.0)]),
	       (RGB(0.2,0.2,1.0),
	       [(1.0,~1.0,~1.0), (~1.0,~1.0,~1.0)]), *)
	      
	      (RGB(0.2,0.2,1.0),
	       [(~1.0,1.0,~1.0), (1.0,1.0,~1.0)]),
	      (RGB(0.7,0.0,0.1),
	       [(1.0,~1.0,~1.0), (~1.0,~1.0,~1.0)]),
	      
	      (* (RGB(0.2,0.2,1.0),
	       [(~1.0,1.0,1.0), (1.0,1.0,1.0)]),
	       (RGB(0.7,0.0,0.1),
	       [(1.0,1.0,~1.0), (~1.0,1.0,~1.0)]), *)
	      
	      (RGB(0.9,1.0,0.0),
	       [(~1.0,1.0,1.0), (1.0,1.0,1.0)]),
	      (RGB(0.2,0.2,1.0),
	       [(1.0,1.0,~1.0), (~1.0,1.0,~1.0)]),
	      
	      (RGB(0.0,0.7,0.1),
	       [(~1.0,~1.0,1.0), (1.0,~1.0,1.0)]),
	      (RGB(0.7,0.0,0.1),
	       [(1.0,~1.0,~1.0), (~1.0,~1.0,~1.0)])
	      ]);
   
   glRotated 5.0 1.0 0.6 (~0.5);
   glutSwapBuffers()
)
  
fun main () = 
    (
     initialise();
     glutIdleFunc loop;
     glutDisplayFunc loop;
     glutMainLoop ()
     )

val _ = main();
