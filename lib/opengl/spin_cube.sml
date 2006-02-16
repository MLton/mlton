open GL;
open GLU;
open GLUT;

val whiteLight : GLreal list =  [0.5, 0.5, 0.5, 1.0]
val sourceLight : GLreal list =  [0.8, 0.8, 0.8, 1.0]
val lightPos : GLreal list =  [0.0, 0.0, 0.0, 1.0]

datatype spec = RGB of GLreal * GLreal * GLreal;

fun changeSize ((width : int), (height : int)) : unit = 
    let 
        val nRange    = ref 2.0
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
     glutInit();
     glutInitDisplayMode (GLUT_DOUBLE+GLUT_RGB);
     glutInitWindowSize 400 400;
     glutCreateWindow "Spinning Cube";
     glEnable GL_DEPTH_TEST;
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
   glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
   DrawPrim (GL_QUADS,
             [
              (RGB(0.9, 1.0, 0.0),
               [(~1.0, 1.0, 1.0), (1.0,1.0,1.0)]),
              (RGB(0.0,0.7,0.1),
               [(1.0,~1.0,1.0),(~1.0,~1.0,1.0)]),
              
              (RGB(0.9,1.0,0.0),
               [(~1.0,1.0,1.0), (~1.0,1.0,~1.0)]),
               (RGB(0.2,0.2,1.0),
               [(~1.0,~1.0,~1.0), (~1.0,~1.0,1.0)]),
              
              (RGB(0.2,0.2,1.0),
               [(~1.0,1.0,~1.0), (1.0,1.0,~1.0)]),
              (RGB(0.7,0.0,0.1),
               [(1.0,~1.0,~1.0), (~1.0,~1.0,~1.0)]),
              
              (RGB(0.2,0.2,1.0),
               [(1.0,1.0,1.0), (1.0,1.0,~1.0)]),
               (RGB(0.7,0.0,0.1),
               [(1.0,~1.0,~1.0), (1.0,~1.0,1.0)]),
              
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
     glutReshapeFunc changeSize;
     glutIdleFunc loop;
     glutDisplayFunc loop;
     glutMainLoop ()
     )

val _ = main();
