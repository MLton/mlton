structure GLU :> GLU =
    struct
        type GLUnurb = MLton.Pointer.t
        (**** Generic constants ****)

        (* Version *)
        val GLU_VERSION_1_1 = 0w1
        val GLU_VERSION_1_2 = 0w1

        (* Errors: (return value 0 = no error) *)
        val GLU_INVALID_ENUM = 0w100900
        val GLU_INVALID_VALUE = 0w100901
        val GLU_OUT_OF_MEMORY = 0w100902
        val GLU_INCOMPATIBLE_GL_VERSION = 0w100903

        (* StringName *)
        val GLU_VERSION = 0w100800
        val GLU_EXTENSIONS = 0w100801

        (* Boolean *)
(* val GLU_TRUE = OpenGL.GL_TRUE
        val GLU_FALSE = OpenGL.GL_FALSE
*)

        (**** Quadric constants ****)

        (* QuadricNormal *)
        val GLU_SMOOTH = 0w100000
        val GLU_FLAT = 0w100001
        val GLU_NONE = 0w100002

        (* QuadricDrawStyle *)
        val GLU_POINT = 0w100010
        val GLU_LINE = 0w100011
        val GLU_FILL = 0w100012
        val GLU_SILHOUETTE = 0w100013

        (* QuadricOrientation *)
        val GLU_OUTSIDE = 0w100020
        val GLU_INSIDE = 0w100021

        (**** Tesselation constants ****)

        val GLU_TESS_MAX_COORD = 1.0e150

        (* TessProperty *)
        val GLU_TESS_WINDING_RULE = 0w100140
        val GLU_TESS_BOUNDARY_ONLY = 0w100141
        val GLU_TESS_TOLERANCE = 0w100142

        (* TessWinding *)
        val GLU_TESS_WINDING_ODD = 0w100130
        val GLU_TESS_WINDING_NONZERO = 0w100131
        val GLU_TESS_WINDING_POSITIVE = 0w100132
        val GLU_TESS_WINDING_NEGATIVE = 0w100133
        val GLU_TESS_WINDING_ABS_GEQ_TWO = 0w100134

        (* TessCallback *)
        val GLU_TESS_BEGIN = 0w100100
        val GLU_TESS_VERTEX = 0w100101
        val GLU_TESS_END = 0w100102
        val GLU_TESS_ERROR = 0w100103
        val GLU_TESS_EDGE_FLAG = 0w100104
        val GLU_TESS_COMBINE = 0w100105
        val GLU_TESS_BEGIN_DATA = 0w100106
        val GLU_TESS_VERTEX_DATA = 0w100107
        val GLU_TESS_END_DATA = 0w100108
        val GLU_TESS_ERROR_DATA = 0w100109
        val GLU_TESS_EDGE_FLAG_DATA = 0w100110
        val GLU_TESS_COMBINE_DATA = 0w100111

        (* TessError *)
        val GLU_TESS_ERROR1 = 0w100151
        val GLU_TESS_ERROR2 = 0w100152
        val GLU_TESS_ERROR3 = 0w100153
        val GLU_TESS_ERROR4 = 0w100154
        val GLU_TESS_ERROR5 = 0w100155
        val GLU_TESS_ERROR6 = 0w100156
        val GLU_TESS_ERROR7 = 0w100157
        val GLU_TESS_ERROR8 = 0w100158

        val GLU_TESS_MISSING_BEGIN_POLYGON = GLU_TESS_ERROR1
        val GLU_TESS_MISSING_BEGIN_CONTOUR = GLU_TESS_ERROR2
        val GLU_TESS_MISSING_END_POLYGON = GLU_TESS_ERROR3
        val GLU_TESS_MISSING_END_CONTOUR = GLU_TESS_ERROR4
        val GLU_TESS_COORD_TOO_LARGE = GLU_TESS_ERROR5
        val GLU_TESS_NEED_COMBINE_CALLBACK = GLU_TESS_ERROR6

        (**** NURBS constants ****)

        (* NurbsProperty *)
        val GLU_AUTO_LOAD_MATRIX = 0w100200
        val GLU_CULLING = 0w100201
        val GLU_SAMPLING_TOLERANCE = 0w100203
        val GLU_DISPLAY_MODE = 0w100204
        val GLU_PARAMETRIC_TOLERANCE = 0w100202
        val GLU_SAMPLING_METHOD = 0w100205
        val GLU_U_STEP = 0w100206
        val GLU_V_STEP = 0w100207

        (* NurbsSampling *)
        val GLU_PATH_LENGTH = 0w100215
        val GLU_PARAMETRIC_ERROR = 0w100216
        val GLU_DOMAIN_DISTANCE = 0w100217


        (* NurbsTrim *)
        val GLU_MAP1_TRIM_2 = 0w100210
        val GLU_MAP1_TRIM_3 = 0w100211

        (* NurbsDisplay *)
        val GLU_OUTLINE_POLYGON = 0w100240
        val GLU_OUTLINE_PATCH = 0w100241

        (* NurbsErrors *)
        val GLU_NURBS_ERROR1 = 0w100251
        val GLU_NURBS_ERROR2 = 0w100252
        val GLU_NURBS_ERROR3 = 0w100253
        val GLU_NURBS_ERROR4 = 0w100254
        val GLU_NURBS_ERROR5 = 0w100255
        val GLU_NURBS_ERROR6 = 0w100256
        val GLU_NURBS_ERROR7 = 0w100257
        val GLU_NURBS_ERROR8 = 0w100258
        val GLU_NURBS_ERROR9 = 0w100259
        val GLU_NURBS_ERROR10 = 0w100260
        val GLU_NURBS_ERROR11 = 0w100261
        val GLU_NURBS_ERROR12 = 0w100262
        val GLU_NURBS_ERROR13 = 0w100263
        val GLU_NURBS_ERROR14 = 0w100264
        val GLU_NURBS_ERROR15 = 0w100265
        val GLU_NURBS_ERROR16 = 0w100266
        val GLU_NURBS_ERROR17 = 0w100267
        val GLU_NURBS_ERROR18 = 0w100268
        val GLU_NURBS_ERROR19 = 0w100269
        val GLU_NURBS_ERROR20 = 0w100270
        val GLU_NURBS_ERROR21 = 0w100271
        val GLU_NURBS_ERROR22 = 0w100272
        val GLU_NURBS_ERROR23 = 0w100273
        val GLU_NURBS_ERROR24 = 0w100274
        val GLU_NURBS_ERROR25 = 0w100275
        val GLU_NURBS_ERROR26 = 0w100276
        val GLU_NURBS_ERROR27 = 0w100277
        val GLU_NURBS_ERROR28 = 0w100278
        val GLU_NURBS_ERROR29 = 0w100279
        val GLU_NURBS_ERROR30 = 0w100280
        val GLU_NURBS_ERROR31 = 0w100281
        val GLU_NURBS_ERROR32 = 0w100282
        val GLU_NURBS_ERROR33 = 0w100283
        val GLU_NURBS_ERROR34 = 0w100284
        val GLU_NURBS_ERROR35 = 0w100285
        val GLU_NURBS_ERROR36 = 0w100286
        val GLU_NURBS_ERROR37 = 0w100287

        (* Contours types -- obsolete! *)
        val GLU_CW = 0w100120
        val GLU_CCW = 0w100121
        val GLU_INTERIOR = 0w100122
        val GLU_EXTERIOR = 0w100123
        val GLU_UNKNOWN = 0w100124

        (* Names without "TESS_" prefix *)
        val GLU_BEGIN = GLU_TESS_BEGIN
        val GLU_VERTEX = GLU_TESS_VERTEX
        val GLU_END = GLU_TESS_END
        val GLU_ERROR = GLU_TESS_ERROR
        val GLU_EDGE_FLAG = GLU_TESS_EDGE_FLAG
            val c_gluBeginSurface = _import "gluBeginSurface" stdcall: GLUnurb -> unit;
            fun gluBeginSurface (a:GLUnurb) = c_gluBeginSurface (a) :unit

            val c_gluEndSurface = _import "gluEndSurface" stdcall: GLUnurb -> unit;
            fun gluEndSurface (a:GLUnurb) = c_gluEndSurface (a) :unit

            val c_gluOrtho2D = _import "gluOrtho2D" stdcall: GLdouble * GLdouble * GLdouble * GLdouble -> unit;
            fun gluOrtho2D (a:GLdouble) (b:GLdouble) (c:GLdouble) (d:GLdouble)
              = c_gluOrtho2D (a,b,c,d):unit

            val c_gluPerspective = _import "gluPerspective" stdcall: GLdouble * GLdouble * GLdouble * GLdouble -> unit;
            fun gluPerspective (a:GLdouble) (b:GLdouble) (c:GLdouble) (d:GLdouble)
              = c_gluPerspective (a,b,c,d):unit

            val c_gluLookAt = _import "gluLookAt" stdcall: GLdouble * GLdouble * GLdouble * GLdouble *
                                                           GLdouble * GLdouble * GLdouble * GLdouble * GLdouble -> unit;
            fun gluLookAt (a0:GLdouble) (a1:GLdouble) (a2:GLdouble) (a3:GLdouble) (a4:GLdouble) (a5:GLdouble) (a6:GLdouble)
                          (a7:GLdouble) (a8:GLdouble) = c_gluLookAt (a0,a1,a2,a3,a4,a5,a6,a7,a8):unit

            val c_gluNewNurbsRenderer = _import "gluNewNurbsRenderer" stdcall: unit -> GLUnurb;
            fun gluNewNurbsRenderer () = c_gluNewNurbsRenderer (): GLUnurb

            val c_gluNurbsProperty = _import "gluNurbsProperty" stdcall: GLUnurb * GLenum * GLreal -> unit;
            fun gluNurbsProperty (a:GLUnurb) (b:GLenum) (c:GLreal) = c_gluNurbsProperty (a,b,c) : unit

            val c_gluNurbsSurface = _import "gluNurbsSurface" stdcall: GLUnurb * int * GLreal array * int * GLreal array
                                                                       * int * int * GLreal array * int * int * GLenum -> unit;
            fun gluNurbsSurface (a:GLUnurb) (b:int) (c:GLreal array) (d:int) (e:GLreal array) (f:int) (g:int) (h:GLreal array)
                                 (i:int) (j:int) (k:GLenum) = c_gluNurbsSurface (a,b,c,d,e,f,g,h,i,j,k) : unit


    end
