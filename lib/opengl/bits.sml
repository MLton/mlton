open GL;
open GLU;
open GLUT;

fun randrange in1 =
( 
 let 
     val n = (Real.fromInt in1)
     val r1 =  (MLton.Random.rand())
 in
     (
      (*print ("randrange: n = " ^ (Real.toString(n)) ^ "; r1 = " ^ (Word.toString (r1)) ^ "\n");*)
      let val r = Real.toInt IEEEReal.TO_NEAREST 
                             ((Real.fromLargeInt (Word.toLargeInt r1)) * n / 4294976295.0)
      in 
          (
           (*print ("randrand: n = " ^ (Int.toString(in1)) ^ "; r = " ^ (Int.toString (r)) ^ "\n");*)
           r
           )
      end
          )
 end
)
fun redraw w h =
    let 
        val smiley = [
                      0wx03, 0wxc0, 0wx0, 0wx0, (*       ****       *)
                      0wx0f, 0wxf0, 0wx0, 0wx0, (*     ********     *)
                      0wx1e, 0wx78, 0wx0, 0wx0, (*    ****  ****    *)
                      0wx39, 0wx9c, 0wx0, 0wx0, (*   ***  **  ***   *)
                      0wx77, 0wxee, 0wx0, 0wx0, (*  *** ****** ***  *)
                      0wx6f, 0wxf6, 0wx0, 0wx0, (*  ** ******** **  *)
                      0wxff, 0wxff, 0wx0, 0wx0, (* **************** *)
                      0wxff, 0wxff, 0wx0, 0wx0, (* **************** *)
                      0wxff, 0wxff, 0wx0, 0wx0, (* **************** *)
                      0wxff, 0wxff, 0wx0, 0wx0, (* **************** *)
                      0wx73, 0wxce, 0wx0, 0wx0, (*  ***  ****  ***  *)
                      0wx73, 0wxce, 0wx0, 0wx0, (*  ***  ****  ***  *)
                      0wx3f, 0wxfc, 0wx0, 0wx0, (*   ************   *)
                      0wx1f, 0wxf8, 0wx0, 0wx0, (*    **********    *)
                      0wx0f, 0wxf0, 0wx0, 0wx0, (*     ********     *)
                      0wx03, 0wxc0, 0wx0, 0wx0  (*       ****       *)
                      ]
        fun doSome 0 w h = ()
          | doSome n w h =
            let
                val ranx = randrange (w-1)
                val rany = randrange (h-1)
                val r = Real32.fromInt ( randrange 255 ) / 255.0
                val g = Real32.fromInt ( randrange 255 ) / 255.0
                val b = Real32.fromInt ( randrange 255 ) / 255.0
            in  
                glColor3f r g b;
                glRasterPos2i ranx rany;
                glBitmap 16 16 8.0 8.0 0.0 0.0 
                         (Word8Vector.fromList smiley);
                doSome (n-1) w h
            end
    in
        glViewport 0 0 w h;
        glClearColor 0.0 0.0 0.0 1.0;
        glClear GL_COLOR_BUFFER_BIT;
        glMatrixMode GL_PROJECTION;
        glLoadIdentity();
        glOrtho 0.0 
                ((Real.fromInt w) - 1.0) 
                0.0 
                ((Real.fromInt h) - 1.0) 
                ~1.0 
                1.0;

        (*
         * This bitmap is aligned to 4-byte boundaries...
         *)

        glPixelTransferi GL_UNPACK_ALIGNMENT 4;
        doSome 200 w h;
        glFinish()
    end

fun display () =
    (
     redraw 350 350
     )

fun init () =
    (
     )

fun reshape ( (x : int), (y : int) ) : unit =
    (
     redraw x y
     )

fun mouseLMB (state : Word.word) : unit = 
    if (state = GLUT_DOWN) then
        (
         )
    else 
        ()

fun mouseRMB (state : Word.word) : unit = 
    if state = GLUT_DOWN then
        (
         )
    else
        ()

fun mouse ((button:GLenum), (state:GLenum), (x:int), (y:int)):unit =
    if button = GLUT_LEFT_BUTTON then
        mouseLMB state
    else
        if button = GLUT_MIDDLE_BUTTON orelse button = GLUT_RIGHT_BUTTON then
            mouseRMB state
        else
            ()

fun keyboard ((key:char), (x:int), (y:int)) =
    case key 
        of #"\u001b" => (OS.Process.exit OS.Process.success) 
      | _ => print (Char.toString key)


fun main () = 
(
 (*randrange (349);
 randrange (349);
 randrange 255;
 randrange 255;
 randrange 255*)
    glutInit ();
    glutInitDisplayMode (GLUT_SINGLE + GLUT_RGB);
    glutInitWindowSize 350 350;
    glutInitWindowPosition 100 100;
    glutCreateWindow "Short Test";
    init();
    glutDisplayFunc display;
    glutReshapeFunc reshape;
    glutMouseFunc mouse;
    glutKeyboardFunc keyboard;
    print("Click the close icon to close the window.\n");
    glutMainLoop()
)

val _ = main();


    