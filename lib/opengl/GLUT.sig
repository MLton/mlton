open GL
signature GLUT =
    sig

        type glutfont = MLton.Pointer.t
        (* Display mode bit masks. *)
        val GLUT_RGB : GL.GLenum
        val GLUT_RGBA : GL.GLenum
        val GLUT_INDEX : GL.GLenum
        val GLUT_SINGLE : GL.GLenum
        val GLUT_DOUBLE : GL.GLenum
        val GLUT_ACCUM : GL.GLenum
        val GLUT_ALPHA : GL.GLenum
        val GLUT_DEPTH : GL.GLenum
        val GLUT_STENCIL : GL.GLenum
        (* #if (GLUT_API_VERSION >= 2) *)
        val GLUT_MULTISAMPLE : GL.GLenum
        val GLUT_STEREO : GL.GLenum
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 3) *)
        val GLUT_LUMINANCE : GL.GLenum
        (* #endif *)

        (* Mouse buttons. *)
        val GLUT_LEFT_BUTTON : GL.GLenum
        val GLUT_MIDDLE_BUTTON : GL.GLenum
        val GLUT_RIGHT_BUTTON : GL.GLenum

        (* Mouse button state. *)
        val GLUT_DOWN : GL.GLenum
        val GLUT_UP : GL.GLenum

        (* #if (GLUT_API_VERSION >= 2) *)
        (* function keys *)
        val GLUT_KEY_F1 : GL.GLenum
        val GLUT_KEY_F2 : GL.GLenum
        val GLUT_KEY_F3 : GL.GLenum
        val GLUT_KEY_F4 : GL.GLenum
        val GLUT_KEY_F5 : GL.GLenum
        val GLUT_KEY_F6 : GL.GLenum
        val GLUT_KEY_F7 : GL.GLenum
        val GLUT_KEY_F8 : GL.GLenum
        val GLUT_KEY_F9 : GL.GLenum
        val GLUT_KEY_F10 : GL.GLenum
        val GLUT_KEY_F11 : GL.GLenum
        val GLUT_KEY_F12 : GL.GLenum
        (* directional keys *)
        val GLUT_KEY_LEFT : GL.GLenum
        val GLUT_KEY_UP : GL.GLenum
        val GLUT_KEY_RIGHT : GL.GLenum
        val GLUT_KEY_DOWN : GL.GLenum
        val GLUT_KEY_PAGE_UP : GL.GLenum
        val GLUT_KEY_PAGE_DOWN : GL.GLenum
        val GLUT_KEY_HOME : GL.GLenum
        val GLUT_KEY_END : GL.GLenum
        val GLUT_KEY_INSERT : GL.GLenum
        (* #endif *)

        (* Entry/exit state. *)
        val GLUT_LEFT : GL.GLenum
        val GLUT_ENTERED : GL.GLenum

        (* Menu usage state. *)
        val GLUT_MENU_NOT_IN_USE : GL.GLenum
        val GLUT_MENU_IN_USE : GL.GLenum

        (* Visibility state. *)
        val GLUT_NOT_VISIBLE : GL.GLenum
        val GLUT_VISIBLE : GL.GLenum

        (* Window status state. *)
        val GLUT_HIDDEN : GL.GLenum
        val GLUT_FULLY_RETAINED : GL.GLenum
        val GLUT_PARTIALLY_RETAINED : GL.GLenum
        val GLUT_FULLY_COVERED : GL.GLenum

        (* Color index component selection values. *)
        val GLUT_RED : GL.GLenum
        val GLUT_GREEN : GL.GLenum
        val GLUT_BLUE : GL.GLenum

        (* Layers for use. *)
        val GLUT_NORMAL : GL.GLenum
        val GLUT_OVERLAY : GL.GLenum

        (* Stroke font constants (use these in GLUT program). *)
        val GLUT_STROKE_ROMAN : glutfont
        val GLUT_STROKE_MONO_ROMAN : glutfont

        (* Bitmap font constants (use these in GLUT program). *)
        val GLUT_BITMAP_9_BY_15 : glutfont
        val GLUT_BITMAP_8_BY_13 : glutfont
        val GLUT_BITMAP_TIMES_ROMAN_10 : glutfont
        val GLUT_BITMAP_TIMES_ROMAN_24 : glutfont
        (*#if (GLUT_API_VERSION >= 3)*)
        val GLUT_BITMAP_HELVETICA_10 : glutfont
        val GLUT_BITMAP_HELVETICA_12 : glutfont
        val GLUT_BITMAP_HELVETICA_18 : glutfont
        (*#endif *)

        (* glutGet parameters. *)
        val GLUT_WINDOW_X : GL.GLenum
        val GLUT_WINDOW_Y : GL.GLenum
        val GLUT_WINDOW_WIDTH : GL.GLenum
        val GLUT_WINDOW_HEIGHT : GL.GLenum
        val GLUT_WINDOW_BUFFER_SIZE : GL.GLenum
        val GLUT_WINDOW_STENCIL_SIZE : GL.GLenum
        val GLUT_WINDOW_DEPTH_SIZE : GL.GLenum
        val GLUT_WINDOW_RED_SIZE : GL.GLenum
        val GLUT_WINDOW_GREEN_SIZE : GL.GLenum
        val GLUT_WINDOW_BLUE_SIZE : GL.GLenum
        val GLUT_WINDOW_ALPHA_SIZE : GL.GLenum
        val GLUT_WINDOW_ACCUM_RED_SIZE : GL.GLenum
        val GLUT_WINDOW_ACCUM_GREEN_SIZE : GL.GLenum
        val GLUT_WINDOW_ACCUM_BLUE_SIZE : GL.GLenum
        val GLUT_WINDOW_ACCUM_ALPHA_SIZE : GL.GLenum
        val GLUT_WINDOW_DOUBLEBUFFER : GL.GLenum
        val GLUT_WINDOW_RGBA : GL.GLenum
        val GLUT_WINDOW_PARENT : GL.GLenum
        val GLUT_WINDOW_NUM_CHILDREN : GL.GLenum
        val GLUT_WINDOW_COLORMAP_SIZE : GL.GLenum
        (* #if (GLUT_API_VERSION >= 2) *)
        val GLUT_WINDOW_NUM_SAMPLES : GL.GLenum
        val GLUT_WINDOW_STEREO : GL.GLenum
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 3) *)
        val GLUT_WINDOW_CURSOR : GL.GLenum
        (* #endif *)
        val GLUT_SCREEN_WIDTH : GL.GLenum
        val GLUT_SCREEN_HEIGHT : GL.GLenum
        val GLUT_SCREEN_WIDTH_MM : GL.GLenum
        val GLUT_SCREEN_HEIGHT_MM : GL.GLenum
        val GLUT_MENU_NUM_ITEMS : GL.GLenum
        val GLUT_DISPLAY_MODE_POSSIBLE : GL.GLenum
        val GLUT_INIT_WINDOW_X : GL.GLenum
        val GLUT_INIT_WINDOW_Y : GL.GLenum
        val GLUT_INIT_WINDOW_WIDTH : GL.GLenum
        val GLUT_INIT_WINDOW_HEIGHT : GL.GLenum
        val GLUT_INIT_DISPLAY_MODE : GL.GLenum
        (* #if (GLUT_API_VERSION >= 2) *)
        val GLUT_ELAPSED_TIME : GL.GLenum
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13) *)
        val GLUT_WINDOW_FORMAT_ID : GL.GLenum
        (* #endif *)

        (* #if (GLUT_API_VERSION >= 2) *)
        (* glutDeviceGet parameters. *)
        val GLUT_HAS_KEYBOARD : GL.GLenum
        val GLUT_HAS_MOUSE : GL.GLenum
        val GLUT_HAS_SPACEBALL : GL.GLenum
        val GLUT_HAS_DIAL_AND_BUTTON_BOX : GL.GLenum
        val GLUT_HAS_TABLET : GL.GLenum
        val GLUT_NUM_MOUSE_BUTTONS : GL.GLenum
        val GLUT_NUM_SPACEBALL_BUTTONS : GL.GLenum
        val GLUT_NUM_BUTTON_BOX_BUTTONS : GL.GLenum
        val GLUT_NUM_DIALS : GL.GLenum
        val GLUT_NUM_TABLET_BUTTONS : GL.GLenum
        (* #endif *)
        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13) *)
        val GLUT_DEVICE_IGNORE_KEY_REPEAT : GL.GLenum
        val GLUT_DEVICE_KEY_REPEAT : GL.GLenum
        val GLUT_HAS_JOYSTICK : GL.GLenum
        val GLUT_OWNS_JOYSTICK : GL.GLenum
        val GLUT_JOYSTICK_BUTTONS : GL.GLenum
        val GLUT_JOYSTICK_AXES : GL.GLenum
        val GLUT_JOYSTICK_POLL_RATE : GL.GLenum
        (* #endif *)

        (* #if (GLUT_API_VERSION >= 3) *)
        (* glutLayerGet parameters. *)
        val GLUT_OVERLAY_POSSIBLE : GL.GLenum
        val GLUT_LAYER_IN_USE : GL.GLenum
        val GLUT_HAS_OVERLAY : GL.GLenum
        val GLUT_TRANSPARENT_INDEX : GL.GLenum
        val GLUT_NORMAL_DAMAGED : GL.GLenum
        val GLUT_OVERLAY_DAMAGED : GL.GLenum

        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9) *)
        (* glutVideoResizeGet parameters. *)
        val GLUT_VIDEO_RESIZE_POSSIBLE : GL.GLenum
        val GLUT_VIDEO_RESIZE_IN_USE : GL.GLenum
        val GLUT_VIDEO_RESIZE_X_DELTA : GL.GLenum
        val GLUT_VIDEO_RESIZE_Y_DELTA : GL.GLenum
        val GLUT_VIDEO_RESIZE_WIDTH_DELTA : GL.GLenum
        val GLUT_VIDEO_RESIZE_HEIGHT_DELTA : GL.GLenum
        val GLUT_VIDEO_RESIZE_X : GL.GLenum
        val GLUT_VIDEO_RESIZE_Y : GL.GLenum
        val GLUT_VIDEO_RESIZE_WIDTH : GL.GLenum
        val GLUT_VIDEO_RESIZE_HEIGHT : GL.GLenum
        (* #endif *)

        (* glutGetModifiers return mask. *)
        val GLUT_ACTIVE_SHIFT : GL.GLenum
        val GLUT_ACTIVE_CTRL : GL.GLenum
        val GLUT_ACTIVE_ALT : GL.GLenum

        (* glutSetCursor parameters. *)
        (* Basic arrows. *)
        val GLUT_CURSOR_RIGHT_ARROW : GL.GLenum
        val GLUT_CURSOR_LEFT_ARROW : GL.GLenum
        (* Symbolic cursor shapes. *)
        val GLUT_CURSOR_INFO : GL.GLenum
        val GLUT_CURSOR_DESTROY : GL.GLenum
        val GLUT_CURSOR_HELP : GL.GLenum
        val GLUT_CURSOR_CYCLE : GL.GLenum
        val GLUT_CURSOR_SPRAY : GL.GLenum
        val GLUT_CURSOR_WAIT : GL.GLenum
        val GLUT_CURSOR_TEXT : GL.GLenum
        val GLUT_CURSOR_CROSSHAIR : GL.GLenum
        (* Directional cursors. *)
        val GLUT_CURSOR_UP_DOWN : GL.GLenum
        val GLUT_CURSOR_LEFT_RIGHT : GL.GLenum
        (* Sizing cursors. *)
        val GLUT_CURSOR_TOP_SIDE : GL.GLenum
        val GLUT_CURSOR_BOTTOM_SIDE : GL.GLenum
        val GLUT_CURSOR_LEFT_SIDE : GL.GLenum
        val GLUT_CURSOR_RIGHT_SIDE : GL.GLenum
        val GLUT_CURSOR_TOP_LEFT_CORNER : GL.GLenum
        val GLUT_CURSOR_TOP_RIGHT_CORNER : GL.GLenum
        val GLUT_CURSOR_BOTTOM_RIGHT_CORNER : GL.GLenum
        val GLUT_CURSOR_BOTTOM_LEFT_CORNER : GL.GLenum
        (* Inherit from parent window. *)
        val GLUT_CURSOR_INHERIT : GL.GLenum
        (* Blank cursor. *)
        val GLUT_CURSOR_NONE : GL.GLenum
        (* Fullscreen crosshair (if available). *)
        val GLUT_CURSOR_FULL_CROSSHAIR : GL.GLenum
        (* #endif *)

        (* #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13) *)
        (* GLUT device control sub-API. *)
        (* glutSetKeyRepeat modes. *)
        val GLUT_KEY_REPEAT_OFF : GL.GLenum
        val GLUT_KEY_REPEAT_ON : GL.GLenum
        val GLUT_KEY_REPEAT_DEFAULT : GL.GLenum

        (* Joystick button masks. *)
        val GLUT_JOYSTICK_BUTTON_A : GL.GLenum
        val GLUT_JOYSTICK_BUTTON_B : GL.GLenum
        val GLUT_JOYSTICK_BUTTON_C : GL.GLenum
        val GLUT_JOYSTICK_BUTTON_D : GL.GLenum

        (* GLUT game mode sub-API. *)
        (* glutGameModeGet. *)
        val GLUT_GAME_MODE_ACTIVE : GL.GLenum
        val GLUT_GAME_MODE_POSSIBLE : GL.GLenum
        val GLUT_GAME_MODE_WIDTH : GL.GLenum
        val GLUT_GAME_MODE_HEIGHT : GL.GLenum
        val GLUT_GAME_MODE_PIXEL_DEPTH : GL.GLenum
        val GLUT_GAME_MODE_REFRESH_RATE : GL.GLenum
        val GLUT_GAME_MODE_DISPLAY_CHANGED : GL.GLenum

        val glutGetModifiers : unit -> GL.GLenum;

        val glutCreateMenu : (int -> unit) -> int
        val glutDestroyMenu : int -> unit
        val glutGetMenu : unit -> int
        val glutSetMenu : int -> unit
        val glutAddMenuEntry : string -> int -> unit
        val glutAddSubMenu : string -> int -> unit
        val glutChangeToMenuEntry : int -> string -> int -> unit
        val glutChangeToSubMenu : int -> string -> int -> unit
        val glutRemoveMenuItem : int -> unit
        val glutAttachMenu : GL.GLenum -> unit
        val glutDetachMenu : GL.GLenum -> unit

        val glutDisplayFunc: (unit -> unit) -> unit
        val glutIdleFunc : (unit -> unit ) -> unit
        val glutReshapeFunc : (int * int -> unit) -> unit
        val glutKeyboardFunc : (char * int * int -> unit) -> unit
        val glutMouseFunc : (GL.GLenum * GL.GLenum * int * int -> unit) -> unit
        val glutSpecialFunc : (int * int * int -> unit ) -> unit
        val glutVisibilityFunc : (Word32.word -> unit ) -> unit

        val glutInit: unit -> unit;
        val glutInitDisplayMode : GLenum -> unit
        (*val glutInit: int -> string list -> unit;*)
        val glutInitWindowPosition : int -> int -> unit
        val glutInitWindowSize : int -> int -> unit
        val glutCreateWindow: string -> int;
        val glutCreateSubWindow: int -> int -> int -> int -> int -> int;
        val glutDestroyWindow: int -> unit;
        val glutMainLoop: unit -> unit;
        val glutBitmapCharacter : glutfont -> char -> unit
        val glutPostRedisplay : unit -> unit
        val glutStrokeCharacter : glutfont -> char -> unit
        val glutSolidSphere : GLdouble -> int -> int -> unit
        val glutSolidIcosahedron : unit -> unit
        val glutSwapBuffers: unit -> unit;
    end
