open GL
signature GLU =
    sig
        type GLUnurb = MLton.Pointer.t
        (**** Generic constants ****)

        (* Version *)
        val GLU_VERSION_1_1 : GL.GLenum
        val GLU_VERSION_1_2 : GL.GLenum

        (* Errors: (return value 0 = no error) *)
        val GLU_INVALID_ENUM : GL.GLenum
        val GLU_INVALID_VALUE : GL.GLenum
        val GLU_OUT_OF_MEMORY : GL.GLenum
        val GLU_INCOMPATIBLE_GL_VERSION : GL.GLenum

        (* StringName *)
        val GLU_VERSION : GL.GLenum
        val GLU_EXTENSIONS : GL.GLenum

        (* Boolean *)
(* val GLU_TRUE : GL.GLenum
        val GLU_FALSE : GL.GLenum
*)

        (**** Quadric constants ****)

        (* QuadricNormal *)
        val GLU_SMOOTH : GL.GLenum
        val GLU_FLAT : GL.GLenum
        val GLU_NONE : GL.GLenum

        (* QuadricDrawStyle *)
        val GLU_POINT : GL.GLenum
        val GLU_LINE : GL.GLenum
        val GLU_FILL : GL.GLenum
        val GLU_SILHOUETTE : GL.GLenum

        (* QuadricOrientation *)
        val GLU_OUTSIDE : GL.GLenum
        val GLU_INSIDE : GL.GLenum

        (**** Tesselation constants ****)

        val GLU_TESS_MAX_COORD : real

        (* TessProperty *)
        val GLU_TESS_WINDING_RULE : GL.GLenum
        val GLU_TESS_BOUNDARY_ONLY : GL.GLenum
        val GLU_TESS_TOLERANCE : GL.GLenum

        (* TessWinding *)
        val GLU_TESS_WINDING_ODD : GL.GLenum
        val GLU_TESS_WINDING_NONZERO : GL.GLenum
        val GLU_TESS_WINDING_POSITIVE : GL.GLenum
        val GLU_TESS_WINDING_NEGATIVE : GL.GLenum
        val GLU_TESS_WINDING_ABS_GEQ_TWO : GL.GLenum

        (* TessCallback *)
        val GLU_TESS_BEGIN : GL.GLenum
        val GLU_TESS_VERTEX : GL.GLenum
        val GLU_TESS_END : GL.GLenum
        val GLU_TESS_ERROR : GL.GLenum
        val GLU_TESS_EDGE_FLAG : GL.GLenum
        val GLU_TESS_COMBINE : GL.GLenum
        val GLU_TESS_BEGIN_DATA : GL.GLenum
        val GLU_TESS_VERTEX_DATA : GL.GLenum
        val GLU_TESS_END_DATA : GL.GLenum
        val GLU_TESS_ERROR_DATA : GL.GLenum
        val GLU_TESS_EDGE_FLAG_DATA : GL.GLenum
        val GLU_TESS_COMBINE_DATA : GL.GLenum

        (* TessError *)
        val GLU_TESS_ERROR1 : GL.GLenum
        val GLU_TESS_ERROR2 : GL.GLenum
        val GLU_TESS_ERROR3 : GL.GLenum
        val GLU_TESS_ERROR4 : GL.GLenum
        val GLU_TESS_ERROR5 : GL.GLenum
        val GLU_TESS_ERROR6 : GL.GLenum
        val GLU_TESS_ERROR7 : GL.GLenum
        val GLU_TESS_ERROR8 : GL.GLenum

        val GLU_TESS_MISSING_BEGIN_POLYGON : GL.GLenum
        val GLU_TESS_MISSING_BEGIN_CONTOUR : GL.GLenum
        val GLU_TESS_MISSING_END_POLYGON : GL.GLenum
        val GLU_TESS_MISSING_END_CONTOUR : GL.GLenum
        val GLU_TESS_COORD_TOO_LARGE : GL.GLenum
        val GLU_TESS_NEED_COMBINE_CALLBACK : GL.GLenum

        (**** NURBS constants ****)

        (* NurbsProperty *)
        val GLU_AUTO_LOAD_MATRIX : GL.GLenum
        val GLU_CULLING : GL.GLenum
        val GLU_SAMPLING_TOLERANCE : GL.GLenum
        val GLU_DISPLAY_MODE : GL.GLenum
        val GLU_PARAMETRIC_TOLERANCE : GL.GLenum
        val GLU_SAMPLING_METHOD : GL.GLenum
        val GLU_U_STEP : GL.GLenum
        val GLU_V_STEP : GL.GLenum

        (* NurbsSampling *)
        val GLU_PATH_LENGTH : GL.GLenum
        val GLU_PARAMETRIC_ERROR : GL.GLenum
        val GLU_DOMAIN_DISTANCE : GL.GLenum


        (* NurbsTrim *)
        val GLU_MAP1_TRIM_2 : GL.GLenum
        val GLU_MAP1_TRIM_3 : GL.GLenum

        (* NurbsDisplay *)
        val GLU_OUTLINE_POLYGON : GL.GLenum
        val GLU_OUTLINE_PATCH : GL.GLenum

        (* NurbsErrors *)
        val GLU_NURBS_ERROR1 : GL.GLenum
        val GLU_NURBS_ERROR2 : GL.GLenum
        val GLU_NURBS_ERROR3 : GL.GLenum
        val GLU_NURBS_ERROR4 : GL.GLenum
        val GLU_NURBS_ERROR5 : GL.GLenum
        val GLU_NURBS_ERROR6 : GL.GLenum
        val GLU_NURBS_ERROR7 : GL.GLenum
        val GLU_NURBS_ERROR8 : GL.GLenum
        val GLU_NURBS_ERROR9 : GL.GLenum
        val GLU_NURBS_ERROR10 : GL.GLenum
        val GLU_NURBS_ERROR11 : GL.GLenum
        val GLU_NURBS_ERROR12 : GL.GLenum
        val GLU_NURBS_ERROR13 : GL.GLenum
        val GLU_NURBS_ERROR14 : GL.GLenum
        val GLU_NURBS_ERROR15 : GL.GLenum
        val GLU_NURBS_ERROR16 : GL.GLenum
        val GLU_NURBS_ERROR17 : GL.GLenum
        val GLU_NURBS_ERROR18 : GL.GLenum
        val GLU_NURBS_ERROR19 : GL.GLenum
        val GLU_NURBS_ERROR20 : GL.GLenum
        val GLU_NURBS_ERROR21 : GL.GLenum
        val GLU_NURBS_ERROR22 : GL.GLenum
        val GLU_NURBS_ERROR23 : GL.GLenum
        val GLU_NURBS_ERROR24 : GL.GLenum
        val GLU_NURBS_ERROR25 : GL.GLenum
        val GLU_NURBS_ERROR26 : GL.GLenum
        val GLU_NURBS_ERROR27 : GL.GLenum
        val GLU_NURBS_ERROR28 : GL.GLenum
        val GLU_NURBS_ERROR29 : GL.GLenum
        val GLU_NURBS_ERROR30 : GL.GLenum
        val GLU_NURBS_ERROR31 : GL.GLenum
        val GLU_NURBS_ERROR32 : GL.GLenum
        val GLU_NURBS_ERROR33 : GL.GLenum
        val GLU_NURBS_ERROR34 : GL.GLenum
        val GLU_NURBS_ERROR35 : GL.GLenum
        val GLU_NURBS_ERROR36 : GL.GLenum
        val GLU_NURBS_ERROR37 : GL.GLenum

        (* Contours types -- obsolete! *)
        val GLU_CW : GL.GLenum
        val GLU_CCW : GL.GLenum
        val GLU_INTERIOR : GL.GLenum
        val GLU_EXTERIOR : GL.GLenum
        val GLU_UNKNOWN : GL.GLenum

        (* Names without "TESS_" prefix *)
        val GLU_BEGIN : GL.GLenum
        val GLU_VERTEX : GL.GLenum
        val GLU_END : GL.GLenum
        val GLU_ERROR : GL.GLenum
        val GLU_EDGE_FLAG : GL.GLenum

        val c_gluBeginSurface : GLUnurb -> unit;
        val gluBeginSurface : GLUnurb -> unit;

        val c_gluEndSurface : GLUnurb -> unit;
        val gluEndSurface : GLUnurb -> unit;

          val c_gluOrtho2D : GLdouble * GLdouble * GLdouble * GLdouble -> unit
          val gluOrtho2D : GLdouble -> GLdouble -> GLdouble -> GLdouble -> unit

          val c_gluPerspective : GLdouble * GLdouble * GLdouble * GLdouble -> unit
          val gluPerspective : GLdouble -> GLdouble -> GLdouble -> GLdouble -> unit

          val c_gluLookAt : GLdouble * GLdouble * GLdouble * GLdouble *
                          GLdouble * GLdouble * GLdouble * GLdouble * GLdouble -> unit
          val gluLookAt : GLdouble -> GLdouble -> GLdouble -> GLdouble ->
                          GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> unit

          val c_gluNewNurbsRenderer : unit -> GLUnurb
          val gluNewNurbsRenderer : unit -> GLUnurb

          val c_gluNurbsProperty : GLUnurb * GLenum * GLreal -> unit
          val gluNurbsProperty : GLUnurb -> GLenum -> GLreal -> unit

          val c_gluNurbsSurface : GLUnurb * int * GLreal array * int * GLreal array
                                    * int * int * GLreal array * int * int * GLenum -> unit
          val gluNurbsSurface : GLUnurb -> int -> GLreal array -> int -> GLreal array
                                    -> int -> int -> GLreal array -> int -> int -> GLenum -> unit
  end
