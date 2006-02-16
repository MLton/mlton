open GL;
open GLUT;

val win = ref 0
val subwin = ref 0
val mainmenu = ref 0
val submenu = ref 0
val item = ref 666

fun display () =
    (
     glClear GL_COLOR_BUFFER_BIT;
     glFlush()
     )

fun gokey (key : char) (x : int) (y : int) =
     let 
         val mods = glutGetModifiers()
         val altactive = Word.andb(mods, GLUT_ACTIVE_ALT)
     in
         print("key = " ^ Char.toString(key) ^ "  mods = 0x" ^
               (Word.fmt StringCvt.HEX mods) ^ "\n");
         if GLUT_ACTIVE_ALT = altactive then
             case key of
             #"1" =>
                 (
                  print "Change to sub menu 1\n";
                  glutChangeToSubMenu 1 "sub 1" (!submenu)
                  )
           | #"2" =>
                 (
                  print "Change to sub menu 2\n";
                  glutChangeToSubMenu 2 "sub 2" (!submenu)
                  )
           | #"3" =>
                 (
                  print "Change to sub menu 3\n";
                  glutChangeToSubMenu 3 "sub 3" (!submenu)
                  )
           | #"4" =>
                 (
                  print "Change to sub menu 4\n";
                  glutChangeToSubMenu 4 "sub 4" (!submenu)
                  )
           | #"5" =>
                 (
                  print "Change to sub menu 5\n";
                  glutChangeToSubMenu 5 "sub 5" (!submenu)
                  )
           | _ =>
                 (
                  raise Fail "gokey 1"
                      )
         else 
             case key of
             #"1" =>
                 (
                  print "Change to menu entry 1\n";
                  glutChangeToMenuEntry 1 "entry 1" 1
                  )
           | #"2" =>
                 (
                  print "Change to menu entry 2\n";
                  glutChangeToMenuEntry 2 "entry 2" 2
                  )
           | #"3" =>
                 (
                  print "Change to menu entry 3\n";
                  glutChangeToMenuEntry 3 "entry 3" 3
                  )
           | #"4" =>
                 (
                  print "Change to menu entry 4\n";
                  glutChangeToMenuEntry 4 "entry 4" 4
                  )
           | #"5" =>
                 (
                  print "Change to menu entry 5\n";
                  glutChangeToMenuEntry 5 "entry 5" 5
                  )
           | #"a" =>
                 (
                  print ("Adding menu entry " ^ (Int.toString (!item)) ^ "\n");
                  glutAddMenuEntry ("added entry" ^ (Int.toString (!item))) (!item);
                  item := !item + 1
                  )
           | #"A" =>
                 (
                  print ("Adding menu entry " ^ (Int.toString (!item)) ^ "\n");
                  glutAddMenuEntry ("added entry" ^ (Int.toString (!item))) (!item);
                  item := !item + 1
                  )
           | #"s" =>
                 (
                  print ("Adding submenu " ^ (Int.toString (!item)) ^ "\n");
                  glutAddMenuEntry ("added submenu " ^ (Int.toString (!item)))
                                   (!submenu);
                  item := !item + 1
                  )
           | #"S" =>
                 (
                  print ("Adding submenu " ^ (Int.toString (!item)) ^ "\n");
                  glutAddMenuEntry ("added submenu " ^ 
                                    (Int.toString (!item)))
                                   (!submenu);
                  item := !item + 1
                  )
           | #"q" =>
                 (
                  print "Remove 1\n";
                  glutRemoveMenuItem 1
                  )
           | #"w" =>
                 (
                  print "Remove 2\n";
                  glutRemoveMenuItem 2
                  )
           | #"e" =>
                 (
                  print "Remove 3\n";
                  glutRemoveMenuItem 3
                  )
           | #"r" =>
                 (
                  print "Remove 4\n";
                  glutRemoveMenuItem 4
                  )
           | #"t" =>
                 (
                  print "Remove 5\n";
                  glutRemoveMenuItem 5
                  )
           | _ =>
                 (
                  raise Fail "gokey 2"
                      )
     end


fun keyboard ((key : char), (x : int), (y : int)) : unit =
    (
     glutSetMenu (!mainmenu);
     gokey key x y;
     ()
     )

fun keyboard2 ((key : char), (x : int), (y : int)) =
    (
     glutSetMenu (!submenu);
     gokey key x y
     )

fun menu (value : int)  =
    (
     print ("menu: entry = " ^ Int.toString(value) ^ "\n")
     )

fun menu2 (value : int) =
    (
     print ("menu2: entry = " ^ Int.toString(value) ^ "\n")
     )

fun main () =
    (
    glutInit ();
    glutInitDisplayMode (GLUT_SINGLE + GLUT_RGB);
    win := glutCreateWindow "Menu Test";
    glClearColor 0.3 0.3 0.3 0.0;

    glutDisplayFunc display;
    print("Click the close icon to close the window.");
    glutKeyboardFunc keyboard;

    submenu := glutCreateMenu menu2;
    glutAddMenuEntry "Sub menu 1" 1001;
    glutAddMenuEntry "Sub menu 2" 1002;
    glutAddMenuEntry "Sub menu 3" 1003;

    mainmenu := glutCreateMenu menu;
    glutAddMenuEntry "First" ~1;
    glutAddMenuEntry "Second" ~2;
    glutAddMenuEntry "Third" ~3;
    glutAddSubMenu "Submenu init" (!submenu);
    glutAttachMenu GLUT_RIGHT_BUTTON;

    subwin := glutCreateSubWindow (!win) 50 50 50 50;

    glClearColor 0.7 0.7 0.7 0.0;
    glutDisplayFunc display;
    glutKeyboardFunc keyboard2;

    glutMainLoop()
    )

val _ = main();