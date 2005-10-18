open GL

structure GLUT :> GLUT =
    struct
        type glutfont = MLton.Pointer.t

        (* Display mode bit masks. *)
        val GLUT_RGB = 0w0
        val GLUT_RGBA = GLUT_RGB
        val GLUT_INDEX = 0w1
        val GLUT_SINGLE = 0w0
        val GLUT_DOUBLE = 0w2
        val GLUT_ACCUM = 0w4
        val GLUT_ALPHA = 0w8
        val GLUT_DEPTH = 0w16
        val GLUT_STENCIL = 0w32
        (* #if (GLUT_API_VERSION >= 0w2) *)
        val GLUT_MULTISAMPLE = 0w128
        val GLUT_STEREO = 0w256
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 0w3) *)
        val GLUT_LUMINANCE = 0w512
        (* #endif *)

        (* Mouse buttons. *)
        val GLUT_LEFT_BUTTON = 0w0
        val GLUT_MIDDLE_BUTTON = 0w1
        val GLUT_RIGHT_BUTTON = 0w2

        (* Mouse button state. *)
        val GLUT_DOWN = 0w0
        val GLUT_UP = 0w1

        (* #if (GLUT_API_VERSION >= 0w2) *)
        (* function keys *)
        val GLUT_KEY_F1 = 0w1
        val GLUT_KEY_F2 = 0w2
        val GLUT_KEY_F3 = 0w3
        val GLUT_KEY_F4 = 0w4
        val GLUT_KEY_F5 = 0w5
        val GLUT_KEY_F6 = 0w6
        val GLUT_KEY_F7 = 0w7
        val GLUT_KEY_F8 = 0w8
        val GLUT_KEY_F9 = 0w9
        val GLUT_KEY_F10 = 0w10
        val GLUT_KEY_F11 = 0w11
        val GLUT_KEY_F12 = 0w12
        (* directional keys *)
        val GLUT_KEY_LEFT = 0w100
        val GLUT_KEY_UP = 0w101
        val GLUT_KEY_RIGHT = 0w102
        val GLUT_KEY_DOWN = 0w103
        val GLUT_KEY_PAGE_UP = 0w104
        val GLUT_KEY_PAGE_DOWN = 0w105
        val GLUT_KEY_HOME = 0w106
        val GLUT_KEY_END = 0w107
        val GLUT_KEY_INSERT = 0w108
        (* #endif *)

        (* Entry/exit state. *)
        val GLUT_LEFT = 0w0
        val GLUT_ENTERED = 0w1

        (* Menu usage state. *)
        val GLUT_MENU_NOT_IN_USE = 0w0
        val GLUT_MENU_IN_USE = 0w1

        (* Visibility state. *)
        val GLUT_NOT_VISIBLE = 0w0
        val GLUT_VISIBLE = 0w1

        (* Window status state. *)
        val GLUT_HIDDEN = 0w0
        val GLUT_FULLY_RETAINED = 0w1
        val GLUT_PARTIALLY_RETAINED = 0w2
        val GLUT_FULLY_COVERED = 0w3

        (* Color index component selection values. *)
        val GLUT_RED = 0w0
        val GLUT_GREEN = 0w1
        val GLUT_BLUE = 0w2

        (* Layers for use. *)
        val GLUT_NORMAL = 0w0
        val GLUT_OVERLAY = 0w1

        (* glutGet parameters. *)
        val GLUT_WINDOW_X = 0w100
        val GLUT_WINDOW_Y = 0w101
        val GLUT_WINDOW_WIDTH = 0w102
        val GLUT_WINDOW_HEIGHT = 0w103
        val GLUT_WINDOW_BUFFER_SIZE = 0w104
        val GLUT_WINDOW_STENCIL_SIZE = 0w105
        val GLUT_WINDOW_DEPTH_SIZE = 0w106
        val GLUT_WINDOW_RED_SIZE = 0w107
        val GLUT_WINDOW_GREEN_SIZE = 0w108
        val GLUT_WINDOW_BLUE_SIZE = 0w109
        val GLUT_WINDOW_ALPHA_SIZE = 0w110
        val GLUT_WINDOW_ACCUM_RED_SIZE = 0w111
        val GLUT_WINDOW_ACCUM_GREEN_SIZE = 0w112
        val GLUT_WINDOW_ACCUM_BLUE_SIZE = 0w113
        val GLUT_WINDOW_ACCUM_ALPHA_SIZE = 0w114
        val GLUT_WINDOW_DOUBLEBUFFER = 0w115
        val GLUT_WINDOW_RGBA = 0w116
        val GLUT_WINDOW_PARENT = 0w117
        val GLUT_WINDOW_NUM_CHILDREN = 0w118
        val GLUT_WINDOW_COLORMAP_SIZE = 0w119
        (* #if (GLUT_API_VERSION >= 0w2) *)
        val GLUT_WINDOW_NUM_SAMPLES = 0w120
        val GLUT_WINDOW_STEREO = 0w121
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 0w3) *)
        val GLUT_WINDOW_CURSOR = 0w122
        (* #endif *)
        val GLUT_SCREEN_WIDTH = 0w200
        val GLUT_SCREEN_HEIGHT = 0w201
        val GLUT_SCREEN_WIDTH_MM = 0w202
        val GLUT_SCREEN_HEIGHT_MM = 0w203
        val GLUT_MENU_NUM_ITEMS = 0w300
        val GLUT_DISPLAY_MODE_POSSIBLE = 0w400
        val GLUT_INIT_WINDOW_X = 0w500
        val GLUT_INIT_WINDOW_Y = 0w501
        val GLUT_INIT_WINDOW_WIDTH = 0w502
        val GLUT_INIT_WINDOW_HEIGHT = 0w503
        val GLUT_INIT_DISPLAY_MODE = 0w504
        (* #if (GLUT_API_VERSION >= 0w2) *)
        val GLUT_ELAPSED_TIME = 0w700
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 0w13) *)
        val GLUT_WINDOW_FORMAT_ID = 0w123
        (* #endif *)

        (* #if (GLUT_API_VERSION >= 0w2) *)
        (* glutDeviceGet parameters. *)
        val GLUT_HAS_KEYBOARD = 0w600
        val GLUT_HAS_MOUSE = 0w601
        val GLUT_HAS_SPACEBALL = 0w602
        val GLUT_HAS_DIAL_AND_BUTTON_BOX = 0w603
        val GLUT_HAS_TABLET = 0w604
        val GLUT_NUM_MOUSE_BUTTONS = 0w605
        val GLUT_NUM_SPACEBALL_BUTTONS = 0w606
        val GLUT_NUM_BUTTON_BOX_BUTTONS = 0w607
        val GLUT_NUM_DIALS = 0w608
        val GLUT_NUM_TABLET_BUTTONS = 0w609
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 0w13) *)
        val GLUT_DEVICE_IGNORE_KEY_REPEAT = 0w610
        val GLUT_DEVICE_KEY_REPEAT = 0w611
        val GLUT_HAS_JOYSTICK = 0w612
        val GLUT_OWNS_JOYSTICK = 0w613
        val GLUT_JOYSTICK_BUTTONS = 0w614
        val GLUT_JOYSTICK_AXES = 0w615
        val GLUT_JOYSTICK_POLL_RATE = 0w616
        (* #endif *)

        (* #if (GLUT_API_VERSION >= 0w3) *)
        (* glutLayerGet parameters. *)
        val GLUT_OVERLAY_POSSIBLE = 0w800
        val GLUT_LAYER_IN_USE = 0w801
        val GLUT_HAS_OVERLAY = 0w802
        val GLUT_TRANSPARENT_INDEX = 0w803
        val GLUT_NORMAL_DAMAGED = 0w804
        val GLUT_OVERLAY_DAMAGED = 0w805

        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 0w9) *)
        (* glutVideoResizeGet parameters. *)
        val GLUT_VIDEO_RESIZE_POSSIBLE = 0w900
        val GLUT_VIDEO_RESIZE_IN_USE = 0w901
        val GLUT_VIDEO_RESIZE_X_DELTA = 0w902
        val GLUT_VIDEO_RESIZE_Y_DELTA = 0w903
        val GLUT_VIDEO_RESIZE_WIDTH_DELTA = 0w904
        val GLUT_VIDEO_RESIZE_HEIGHT_DELTA = 0w905
        val GLUT_VIDEO_RESIZE_X = 0w906
        val GLUT_VIDEO_RESIZE_Y = 0w907
        val GLUT_VIDEO_RESIZE_WIDTH = 0w908
        val GLUT_VIDEO_RESIZE_HEIGHT = 0w909
        (* #endif *)

        (* glutGetModifiers return mask. *)
        val GLUT_ACTIVE_SHIFT = 0w1
        val GLUT_ACTIVE_CTRL = 0w2
        val GLUT_ACTIVE_ALT = 0w4

        (* glutSetCursor parameters. *)
        (* Basic arrows. *)
        val GLUT_CURSOR_RIGHT_ARROW = 0w0
        val GLUT_CURSOR_LEFT_ARROW = 0w1
        (* Symbolic cursor shapes. *)
        val GLUT_CURSOR_INFO = 0w2
        val GLUT_CURSOR_DESTROY = 0w3
        val GLUT_CURSOR_HELP = 0w4
        val GLUT_CURSOR_CYCLE = 0w5
        val GLUT_CURSOR_SPRAY = 0w6
        val GLUT_CURSOR_WAIT = 0w7
        val GLUT_CURSOR_TEXT = 0w8
        val GLUT_CURSOR_CROSSHAIR = 0w9
        (* Directional cursors. *)
        val GLUT_CURSOR_UP_DOWN = 0w10
        val GLUT_CURSOR_LEFT_RIGHT = 0w11
        (* Sizing cursors. *)
        val GLUT_CURSOR_TOP_SIDE = 0w12
        val GLUT_CURSOR_BOTTOM_SIDE = 0w13
        val GLUT_CURSOR_LEFT_SIDE = 0w14
        val GLUT_CURSOR_RIGHT_SIDE = 0w15
        val GLUT_CURSOR_TOP_LEFT_CORNER = 0w16
        val GLUT_CURSOR_TOP_RIGHT_CORNER =0w17
        val GLUT_CURSOR_BOTTOM_RIGHT_CORNER =0w18
        val GLUT_CURSOR_BOTTOM_LEFT_CORNER =0w19
        (* Inherit from parent window. *)
        val GLUT_CURSOR_INHERIT = 0w100
        (* Blank cursor. *)
        val GLUT_CURSOR_NONE = 0w101
        (* Fullscreen crosshair (if available). *)
        val GLUT_CURSOR_FULL_CROSSHAIR =0w102
        (* #endif *)

        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 0w13) *)
        (* GLUT device control sub-API. *)
        (* glutSetKeyRepeat modes. *)
        val GLUT_KEY_REPEAT_OFF = 0w0
        val GLUT_KEY_REPEAT_ON = 0w1
        val GLUT_KEY_REPEAT_DEFAULT = 0w2

        (* Joystick button masks. *)
        val GLUT_JOYSTICK_BUTTON_A = 0w1
        val GLUT_JOYSTICK_BUTTON_B = 0w2
        val GLUT_JOYSTICK_BUTTON_C = 0w4
        val GLUT_JOYSTICK_BUTTON_D = 0w8

        (* GLUT game mode sub-API. *)
        (* glutGameModeGet. *)
        val GLUT_GAME_MODE_ACTIVE = 0w0
        val GLUT_GAME_MODE_POSSIBLE = 0w1
        val GLUT_GAME_MODE_WIDTH = 0w2
        val GLUT_GAME_MODE_HEIGHT = 0w3
        val GLUT_GAME_MODE_PIXEL_DEPTH = 0w4
        val GLUT_GAME_MODE_REFRESH_RATE = 0w5
        val GLUT_GAME_MODE_DISPLAY_CHANGED = 0w6

        local




            (* Create Menu callback *)
            val gCreateMenuFA = _export "glutCreateMenuArgument": (int -> unit) -> unit;
            val callGCreateMenuF = _import "callGlutCreateMenu": unit -> int;

            (* Display function callback *)
            val gDisplayFA = _export "glutDisplayFuncArgument": (unit -> unit) -> unit;
            val callGDisplayF = _import "callGlutDisplayFunc": unit -> unit;

            (* Idle function callback *)
            val gIdleFA = _export "glutIdleFuncArgument": (unit -> unit) -> unit;
            val callGIdleF = _import "callGlutIdleFunc": unit -> unit;

            (* Reshape function callback *)
            val gReshapeFA = _export "glutReshapeFuncArgument": (int * int -> unit) -> unit;
            val callGReshapeF = _import "callGlutReshapeFunc": unit -> unit;

            (* Keyboard function callback *)
            val gKbdFA = _export "glutKeyboardFuncArgument": (char * int * int -> unit) -> unit;
            val callGKbdF = _import "callGlutKeyboardFunc": unit -> unit;

            (* Mouse function callback *)
            val gMouseFA = _export "glutMouseFuncArgument": (GLenum * GLenum * int * int -> unit) -> unit;
            val callGMouseF = _import "callGlutMouseFunc": unit -> unit;

            (* Special function callback *)
            val gSpecFA = _export "glutSpecialFuncArgument": (int * int * int -> unit) -> unit;
            val callGSpecF = _import "callGlutSpecialFunc": unit -> unit;

            (* Visibility function callback *)
            val gVisibilityFA = _export "glutVisibilityFuncArgument": (Word32.word -> unit) -> unit;
            val callGVisibilityF = _import "callGlutVisibilityFunc": unit -> unit;


            (* GLUT initialisation *)
            val cGI = _import "callGlutInit": unit -> unit;

        in
            (* Stroke font constants (use these in GLUT program). *)
            val c_GLUT_STROKE_ROMAN = _import "mlton_glut_stroke_roman" : unit -> glutfont;
            val GLUT_STROKE_ROMAN = c_GLUT_STROKE_ROMAN()

            val c_GLUT_STROKE_MONO_ROMAN = _import "mlton_glut_stroke_mono_roman" : unit -> glutfont;
            val GLUT_STROKE_MONO_ROMAN = c_GLUT_STROKE_MONO_ROMAN()

            (* Bitmap font constants (use these in GLUT program). *)
            val c_GLUT_BITMAP_9_BY_15 = _import "mlton_glut_bitmap_9_by_15" : unit -> glutfont;
            val GLUT_BITMAP_9_BY_15 = c_GLUT_BITMAP_9_BY_15()

            val c_GLUT_BITMAP_8_BY_13 = _import "mlton_glut_bitmap_8_by_13" : unit -> glutfont;
            val GLUT_BITMAP_8_BY_13 = c_GLUT_BITMAP_8_BY_13()

            val c_GLUT_BITMAP_TIMES_ROMAN_10 = _import "mlton_glut_bitmap_times_roman_10" : unit -> glutfont;
            val GLUT_BITMAP_TIMES_ROMAN_10 = c_GLUT_BITMAP_TIMES_ROMAN_10()

            val c_GLUT_BITMAP_TIMES_ROMAN_24 = _import "mlton_glut_bitmap_times_roman_24" : unit -> glutfont;
            val GLUT_BITMAP_TIMES_ROMAN_24 = c_GLUT_BITMAP_TIMES_ROMAN_24()

            val c_GLUT_BITMAP_HELVETICA_10 = _import "mlton_glut_bitmap_helvetica_10" : unit -> glutfont;
            val GLUT_BITMAP_HELVETICA_10 = c_GLUT_BITMAP_HELVETICA_10()

            val c_GLUT_BITMAP_HELVETICA_12 = _import "mlton_glut_bitmap_helvetica_12" : unit -> glutfont;
            val GLUT_BITMAP_HELVETICA_12 = c_GLUT_BITMAP_HELVETICA_12()

            val c_GLUT_BITMAP_HELVETICA_18 = _import "mlton_glut_bitmap_helvetica_18" : unit -> glutfont;
            val GLUT_BITMAP_HELVETICA_18 = c_GLUT_BITMAP_HELVETICA_18()

            fun glutCreateMenu (cm : int -> unit) = ( gCreateMenuFA cm; callGCreateMenuF ()) : int;
            fun glutDisplayFunc (display: unit -> unit) = (gDisplayFA display; callGDisplayF ())
            fun glutIdleFunc (idle: unit -> unit) = (gIdleFA idle; callGIdleF ())
            fun glutReshapeFunc (reshape: int * int -> unit) = ( gReshapeFA reshape; callGReshapeF ())
            fun glutKeyboardFunc (kbd: char * int * int -> unit) = ( gKbdFA kbd; callGKbdF ())
            fun glutMouseFunc (mouse: GL.GLenum * GL.GLenum * int * int -> unit) =
                ( gMouseFA mouse; callGMouseF ())
            fun glutSpecialFunc (kbd: int * int * int -> unit) = ( gSpecFA kbd; callGSpecF ())
            fun glutVisibilityFunc (vis: Word32.word -> unit) = ( gVisibilityFA vis; callGVisibilityF ())

            val c_glutGetModifiers = _import "glutGetModifiers" stdcall: unit -> GL.GLenum;
            fun glutGetModifiers () = c_glutGetModifiers () :  GL.GLenum;

            val c_glutDestroyMenu = _import "glutDestroyMenu" stdcall: int -> unit;
            fun glutDestroyMenu (a:int) = c_glutDestroyMenu (a): unit;

            val c_glutGetMenu = _import "glutGetMenu" stdcall: unit -> int;
            fun glutGetMenu () = c_glutGetMenu () : int;

            val c_glutSetMenu = _import "glutSetMenu" stdcall: int -> unit ;
            fun glutSetMenu (a:int) = c_glutSetMenu (a) : unit;

            val c_glutAddMenuEntry = _import "glutAddMenuEntry" stdcall: string * int -> unit ;
            fun glutAddMenuEntry (a:string) (b:int) = c_glutAddMenuEntry (a,b) : unit;

            val c_glutAddSubMenu = _import "glutAddSubMenu" stdcall: string * int -> unit ;
            fun glutAddSubMenu (a:string) (b:int) = c_glutAddSubMenu (a,b) : unit;

            val c_glutChangeToMenuEntry = _import "glutChangeToMenuEntry" stdcall: int * string * int -> unit ;
            fun glutChangeToMenuEntry (c:int) (a:string) (b:int) = c_glutChangeToMenuEntry (c,a,b) : unit;

            val c_glutChangeToSubMenu = _import "glutChangeToSubMenu" stdcall: int * string * int -> unit ;
            fun glutChangeToSubMenu (c:int) (a:string) (b:int) = c_glutChangeToSubMenu (c,a,b) : unit;

            val c_glutRemoveMenuItem = _import "glutRemoveMenuItem" stdcall: int -> unit ;
            fun glutRemoveMenuItem (a:int) = c_glutRemoveMenuItem (a) : unit;

            val c_glutAttachMenu = _import "glutAttachMenu" stdcall: GL.GLenum -> unit ;
            fun glutAttachMenu (a:GLenum) = c_glutAttachMenu (a): unit;

            val c_glutDetachMenu = _import "glutDetachMenu" stdcall: GL.GLenum -> unit ;
            fun glutDetachMenu (a:GLenum) = c_glutDetachMenu (a) : unit;

            fun glutInit () = cGI ()

            (*val init = _import "glutInit" : int -> string list -> unit;*)
            val c_glutInitDisplayMode = _import "glutInitDisplayMode" stdcall: GL.GLenum -> unit;
            fun glutInitDisplayMode (a:GL.GLenum) = c_glutInitDisplayMode (a) : unit

            (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)*)
            val c_glutInitDisplayString = _import "glutInitDisplayString" stdcall: string -> unit;
            fun glutInitDisplayString (a:string) = c_glutInitDisplayString (a) : unit

            val c_glutInitWindowPosition = _import "glutInitWindowPosition" stdcall: int * int -> unit ;
            fun glutInitWindowPosition (a:int) (b:int) = c_glutInitWindowPosition (a, b) :unit

            val c_glutInitWindowSize = _import "glutInitWindowSize" stdcall: int * int -> unit;
            fun glutInitWindowSize (a:int) (b:int) = c_glutInitWindowSize (a, b) :unit

            val glutCreateWindow = _import "glutCreateWindow" stdcall: string -> int;

            val c_glutCreateSubWindow = _import "glutCreateSubWindow" stdcall: int * int * int * int * int -> int;
            fun glutCreateSubWindow (a:int) (b:int) (c:int) (d:int) (e:int) = c_glutCreateSubWindow (a, b, c, d, e) :int

            val glutDestroyWindow = _import "glutDestroyWindow" stdcall: int -> unit;

            val glutMainLoop = _import "glutMainLoop" stdcall: unit -> unit;

            val glutPostRedisplay = _import "glutPostRedisplay" stdcall: unit -> unit;

            val c_glutBitmapCharacter = _import "glutBitmapCharacter" stdcall: glutfont * int -> unit;
            fun glutBitmapCharacter (a:glutfont) (b:char) =
                let val c = ord (b)
                in c_glutBitmapCharacter (a,c) end

            (*val c_glutBitmapWidth : glutfont -> int -> int =
                Dynlib.app2 (Dynlib.dlsym dlh "mosml_glutBitmapWidth")*)

            val c_glutStrokeCharacter = _import "glutStrokeCharacter" stdcall: glutfont * int -> unit;
            fun glutStrokeCharacter (a:glutfont) (b:char) =
                let val c = ord (b)
                in c_glutStrokeCharacter (a,c) end

            val c_glutSolidSphere = _import "glutSolidSphere" stdcall: GLdouble * int * int -> unit;
            fun glutSolidSphere (a:GLdouble) (b:int) (c:int) = c_glutSolidSphere (a,b,c)

            val c_glutSolidIcosahedron = _import "glutSolidIcosahedron" stdcall: unit -> unit;
            fun glutSolidIcosahedron () = c_glutSolidIcosahedron ()

            val glutSwapBuffers = _import "glutSwapBuffers" stdcall: unit -> unit;
        end


    end
