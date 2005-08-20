open GL;
open GLU;
open GLUT;

val mat_red_diffuse = Array.fromList [ 0.7, 0.0, 0.1, 1.0 ]
val mat_green_diffuse = Array.fromList [ 0.0, 0.7, 0.1, 1.0 ]
val mat_blue_diffuse = Array.fromList [ 0.0, 0.1, 0.7, 1.0 ]
val mat_yellow_diffuse = Array.fromList [ 0.7, 0.8, 0.1, 1.0 ]
val mat_specular = Array.fromList [ 1.0, 1.0, 1.0, 1.0 ]
val mat_shininess = Array.fromList [ 100.0 ]
val knots = Array.fromList [ 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0 ]

fun map3d f sizex sizey sizez from =
(
 let val size2d = sizey * sizez
     val x = from div size2d
     val y = (Int.rem (from, size2d)) div sizez
     val z = (Int.rem (from, sizez))
 in f x y z
 end
)

fun map3dToArray f sizex sizey sizez =
    Array.tabulate ((sizex * sizey * sizez), (map3d f sizex sizey sizez))

fun to1dIndex sizex sizey sizez x y z =
    x * sizey * sizez + y * sizez + z

fun initRedControlPoints u v w =
(
    if w = 0 then ( 2.0 * (Real32.fromInt u) ) else
    if w = 1 then ( 2.0 * (Real32.fromInt v) ) else
    if ( u = 1 orelse u = 2 ) andalso ( v = 1 orelse v = 2 ) then
        6.0
    else 0.0
)
    
fun initGreenControlPoints u v w =
(
    if w = 0 then ( 2.0 * ((Real32.fromInt u) - 3.0 )) else
    if w = 1 then ( 2.0 * ((Real32.fromInt v) - 3.0 )) else
    if ( u = 1 orelse u = 2 ) andalso ( v = 1 orelse v = 2 ) then
        if u = 1 andalso v = 1 then 15.0
        else (~2.0)
    else 0.0
)
    
fun initBlueControlPoints u v w =
(
    if w = 0 then ( 2.0 * ((Real32.fromInt u) - 3.0 )) else
    if w = 1 then ( 2.0 * (Real32.fromInt v) ) else
    if ( u = 1 orelse u = 2 ) andalso ( v = 1 orelse v = 2 ) then
        if u = 1 andalso v = 2 then 11.0
        else 2.0
    else 0.0
)
    
fun initYellowControlPoints u v w =
(
    if w = 0 then ( 2.0 * (Real32.fromInt u) ) else
    if w = 1 then ( 2.0 * ((Real32.fromInt v) - 3.0) ) else
    if ( u = 1 orelse u = 2 orelse u = 3 ) andalso ( v = 1 orelse v = 2 ) then
        if v = 1 then (~2.0)
        else 5.0
    else 0.0
)

fun renderHill nurb knots diffuse controlpts =
(
 glMaterialfv GL_FRONT GL_DIFFUSE diffuse;
 gluBeginSurface nurb;
 gluNurbsSurface nurb 8 knots 8 knots
                 (4 * 3) 3 controlpts
                 4 4 GL_MAP2_VERTEX_3;
 gluEndSurface nurb
)

fun display() =
(
  glClear (GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
  glCallList 1;
  glFlush()
)

fun main() =
(
  glutInit ();
  glutCreateWindow "molehill";
  glMaterialfv GL_FRONT GL_SPECULAR mat_specular;
  glMaterialfv GL_FRONT GL_SHININESS mat_shininess;
  glEnable GL_LIGHTING;
  glEnable GL_LIGHT0;
  glEnable GL_DEPTH_TEST;
  glEnable GL_AUTO_NORMAL;
  glEnable GL_NORMALIZE;
  let val nurb = gluNewNurbsRenderer()
      val redctlpts = map3dToArray initRedControlPoints 4 4 3
      val greenctlpts = map3dToArray initGreenControlPoints 4 4 3
      val bluectlpts = map3dToArray initBlueControlPoints 4 4 3
      val yellowctlpts = map3dToArray initYellowControlPoints 4 4 3
      val toControlArray = to1dIndex 4 4 3
  in
      gluNurbsProperty nurb GLU_SAMPLING_TOLERANCE 25.0;
      gluNurbsProperty nurb GLU_DISPLAY_MODE (Real32.fromInt (Word.toInt GLU_FILL));

      (* Stretch up red's far right corner. *)
      Array.update (redctlpts, (toControlArray 3 3 2), 6.0);

      (* Pull down green's near left corner a little. *)
      Array.update (greenctlpts, (toControlArray 0 0 2), ~2.0);

      (* Turn up meeting of four corners. *)
      Array.update (redctlpts, (toControlArray 0 0 2), 1.0);
      Array.update (greenctlpts, (toControlArray 3 3 2), 1.0);
      Array.update (bluectlpts, (toControlArray 3 0 2), 1.0);
      Array.update (yellowctlpts, (toControlArray 0 3 2), 1.0);
      
      glMatrixMode GL_PROJECTION;
      gluPerspective 55.0  1.0  2.0  24.0;
      glMatrixMode GL_MODELVIEW;
      glTranslatef 0.0 0.0 ~15.0;
      glRotatef 330.0  1.0 0.0 0.0;

      glNewList 1 GL_COMPILE;
      (* Render hills. *)
      renderHill nurb knots mat_red_diffuse redctlpts;
      renderHill nurb knots mat_green_diffuse greenctlpts;
      renderHill nurb knots mat_blue_diffuse bluectlpts;
      renderHill nurb knots mat_yellow_diffuse yellowctlpts;
      glEndList();

      glutDisplayFunc display;
      glutMainLoop()
  end
)

val _ = main();