(*
 * OpenGL library implementation
 *)

structure GL :> GL =
    struct






        type GLreal = Real32.real
        type GLdouble = real


        type GLenum = Word.word
        (* Specify attributes of (part) of a primitive *)
        (* needs to be extensible to different attributes and different formats,
         eg. ints and reals *)
        datatype realspec = realRGB of GLreal * GLreal * GLreal
        type realvertex = GLreal * GLreal * GLreal
        type realrgbacolour = GLreal list

        datatype intspec = intRGB of Word.word * Word.word * Word.word
        type intvertex = Word.word * Word.word * Word.word
        type intrgbacolour = Word.word * Word.word * Word.word * Word.word

        (* types of primitives *)
        type primitive = Word.word

        (* describes a collection of primitives *)
        type object = primitive * (realspec * realvertex list) list;

        (* AccumOp *)
        val GL_ACCUM = 0wx0100
        val GL_LOAD = 0wx0101
        val GL_RETURN = 0wx0102
        val GL_MULT = 0wx0103
        val GL_ADD = 0wx0104

        (* AlphaFunction *)
        val GL_NEVER = 0wx0200
        val GL_LESS = 0wx0201
        val GL_EQUAL = 0wx0202
        val GL_LEQUAL = 0wx0203
        val GL_GREATER = 0wx0204
        val GL_NOTEQUAL = 0wx0205
        val GL_GEQUAL = 0wx0206
        val GL_ALWAYS = 0wx0207

        (* AttribMask *)
        val GL_CURRENT_BIT = 0wx00000001
        val GL_POINT_BIT = 0wx00000002
        val GL_LINE_BIT = 0wx00000004
        val GL_POLYGON_BIT = 0wx00000008
        val GL_POLYGON_STIPPLE_BIT = 0wx00000010
        val GL_PIXEL_MODE_BIT = 0wx00000020
        val GL_LIGHTING_BIT = 0wx00000040
        val GL_FOG_BIT = 0wx00000080
        val GL_DEPTH_BUFFER_BIT = 0wx00000100
        val GL_ACCUM_BUFFER_BIT = 0wx00000200
        val GL_STENCIL_BUFFER_BIT = 0wx00000400
        val GL_VIEWPORT_BIT = 0wx00000800
        val GL_TRANSFORM_BIT = 0wx00001000
        val GL_ENABLE_BIT = 0wx00002000
        val GL_COLOR_BUFFER_BIT = 0wx00004000
        val GL_HINT_BIT = 0wx00008000
        val GL_EVAL_BIT = 0wx00010000
        val GL_LIST_BIT = 0wx00020000
        val GL_TEXTURE_BIT = 0wx00040000
        val GL_SCISSOR_BIT = 0wx00080000
        val GL_ALL_ATTRIB_BITS = 0wx000fffff

        (* BeginMode *)
        val GL_POINTS = 0wx0000
        val GL_LINES = 0wx0001
        val GL_LINE_LOOP = 0wx0002
        val GL_LINE_STRIP = 0wx0003
        val GL_TRIANGLES = 0wx0004
        val GL_TRIANGLE_STRIP = 0wx0005
        val GL_TRIANGLE_FAN = 0wx0006
        val GL_QUADS = 0wx0007
        val GL_QUAD_STRIP = 0wx0008
        val GL_POLYGON = 0wx0009

        (* BlendingFactorDest *)
        val GL_ZERO = 0w0
        val GL_ONE = 0w1
        val GL_SRC_COLOR = 0wx0300
        val GL_ONE_MINUS_SRC_COLOR = 0wx0301
        val GL_SRC_ALPHA = 0wx0302
        val GL_ONE_MINUS_SRC_ALPHA = 0wx0303
        val GL_DST_ALPHA = 0wx0304
        val GL_ONE_MINUS_DST_ALPHA = 0wx0305

        (* BlendingFactorSrc *)
        val GL_DST_COLOR = 0wx0306
        val GL_ONE_MINUS_DST_COLOR = 0wx0307
        val GL_SRC_ALPHA_SATURATE = 0wx0308

        (* Boolean *)
        val GL_TRUE = 0w1
        val GL_FALSE = 0w0

        (* ClipPlaneName *)
        val GL_CLIP_PLANE0 = 0wx3000
        val GL_CLIP_PLANE1 = 0wx3001
        val GL_CLIP_PLANE2 = 0wx3002
        val GL_CLIP_PLANE3 = 0wx3003
        val GL_CLIP_PLANE4 = 0wx3004
        val GL_CLIP_PLANE5 = 0wx3005

        (* DataType *)
        val GL_BYTE = 0wx1400
        val GL_UNSIGNED_BYTE = 0wx1401
        val GL_SHORT = 0wx1402
        val GL_UNSIGNED_SHORT = 0wx1403
        val GL_INT = 0wx1404
        val GL_UNSIGNED_INT = 0wx1405
        val GL_FLOAT = 0wx1406
        val GL_2_BYTES = 0wx1407
        val GL_3_BYTES = 0wx1408
        val GL_4_BYTES = 0wx1409
        val GL_DOUBLE = 0wx140A

        (* DrawBufferMode *)
        val GL_NONE = 0w0
        val GL_FRONT_LEFT = 0wx0400
        val GL_FRONT_RIGHT = 0wx0401
        val GL_BACK_LEFT = 0wx0402
        val GL_BACK_RIGHT = 0wx0403
        val GL_FRONT = 0wx0404
        val GL_BACK = 0wx0405
        val GL_LEFT = 0wx0406
        val GL_RIGHT = 0wx0407
        val GL_FRONT_AND_BACK = 0wx0408
        val GL_AUX0 = 0wx0409
        val GL_AUX1 = 0wx040A
        val GL_AUX2 = 0wx040B
        val GL_AUX3 = 0wx040C

        (* Enable *)
        (* ErrorCode *)
        val GL_NO_ERROR = 0w0
        val GL_INVALID_ENUM = 0wx0500
        val GL_INVALID_VALUE = 0wx0501
        val GL_INVALID_OPERATION = 0wx0502
        val GL_STACK_OVERFLOW = 0wx0503
        val GL_STACK_UNDERFLOW = 0wx0504
        val GL_OUT_OF_MEMORY = 0wx0505

        (* FeedBackMode *)
        val GL_2D = 0wx0600
        val GL_3D = 0wx0601
        val GL_3D_COLOR = 0wx0602
        val GL_3D_COLOR_TEXTURE = 0wx0603
        val GL_4D_COLOR_TEXTURE = 0wx0604

        (* FeedBackToken *)
        val GL_PASS_THROUGH_TOKEN = 0wx0700
        val GL_POINT_TOKEN = 0wx0701
        val GL_LINE_TOKEN = 0wx0702
        val GL_POLYGON_TOKEN = 0wx0703
        val GL_BITMAP_TOKEN = 0wx0704
        val GL_DRAW_PIXEL_TOKEN = 0wx0705
        val GL_COPY_PIXEL_TOKEN = 0wx0706
        val GL_LINE_RESET_TOKEN = 0wx0707

        (* FogMode *)
        val GL_EXP = 0wx0800
        val GL_EXP2 = 0wx0801

        (* FrontFaceDirection *)
        val GL_CW = 0wx0900
        val GL_CCW = 0wx0901

        (* GetMapTarget *)
        val GL_COEFF = 0wx0A00
        val GL_ORDER = 0wx0A01
        val GL_DOMAIN = 0wx0A02

        (* GetTarget *)
        val GL_CURRENT_COLOR = 0wx0B00
        val GL_CURRENT_INDEX = 0wx0B01
        val GL_CURRENT_NORMAL = 0wx0B02
        val GL_CURRENT_TEXTURE_COORDS = 0wx0B03
        val GL_CURRENT_RASTER_COLOR = 0wx0B04
        val GL_CURRENT_RASTER_INDEX = 0wx0B05
        val GL_CURRENT_RASTER_TEXTURE_COORDS = 0wx0B06
        val GL_CURRENT_RASTER_POSITION = 0wx0B07
        val GL_CURRENT_RASTER_POSITION_VALID = 0wx0B08
        val GL_CURRENT_RASTER_DISTANCE = 0wx0B09
        val GL_POINT_SMOOTH = 0wx0B10
        val GL_POINT_SIZE = 0wx0B11
        val GL_POINT_SIZE_RANGE = 0wx0B12
        val GL_POINT_SIZE_GRANULARITY = 0wx0B13
        val GL_LINE_SMOOTH = 0wx0B20
        val GL_LINE_WIDTH = 0wx0B21
        val GL_LINE_WIDTH_RANGE = 0wx0B22
        val GL_LINE_WIDTH_GRANULARITY = 0wx0B23
        val GL_LINE_STIPPLE = 0wx0B24
        val GL_LINE_STIPPLE_PATTERN = 0wx0B25
        val GL_LINE_STIPPLE_REPEAT = 0wx0B26
        val GL_LIST_MODE = 0wx0B30
        val GL_MAX_LIST_NESTING = 0wx0B31
        val GL_LIST_BASE = 0wx0B32
        val GL_LIST_INDEX = 0wx0B33
        val GL_POLYGON_MODE = 0wx0B40
        val GL_POLYGON_SMOOTH = 0wx0B41
        val GL_POLYGON_STIPPLE = 0wx0B42
        val GL_EDGE_FLAG = 0wx0B43
        val GL_CULL_FACE = 0wx0B44
        val GL_CULL_FACE_MODE = 0wx0B45
        val GL_FRONT_FACE = 0wx0B46
        val GL_LIGHTING = 0wx0B50
        val GL_LIGHT_MODEL_LOCAL_VIEWER = 0wx0B51
        val GL_LIGHT_MODEL_TWO_SIDE = 0wx0B52
        val GL_LIGHT_MODEL_AMBIENT = 0wx0B53
        val GL_SHADE_MODEL = 0wx0B54
        val GL_COLOR_MATERIAL_FACE = 0wx0B55
        val GL_COLOR_MATERIAL_PARAMETER = 0wx0B56
        val GL_COLOR_MATERIAL = 0wx0B57
        val GL_FOG = 0wx0B60
        val GL_FOG_INDEX = 0wx0B61
        val GL_FOG_DENSITY = 0wx0B62
        val GL_FOG_START = 0wx0B63
        val GL_FOG_END = 0wx0B64
        val GL_FOG_MODE = 0wx0B65
        val GL_FOG_COLOR = 0wx0B66
        val GL_DEPTH_RANGE = 0wx0B70
        val GL_DEPTH_TEST = 0wx0B71
        val GL_DEPTH_WRITEMASK = 0wx0B72
        val GL_DEPTH_CLEAR_VALUE = 0wx0B73
        val GL_DEPTH_FUNC = 0wx0B74
        val GL_ACCUM_CLEAR_VALUE = 0wx0B80
        val GL_STENCIL_TEST = 0wx0B90
        val GL_STENCIL_CLEAR_VALUE = 0wx0B91
        val GL_STENCIL_FUNC = 0wx0B92
        val GL_STENCIL_VALUE_MASK = 0wx0B93
        val GL_STENCIL_FAIL = 0wx0B94
        val GL_STENCIL_PASS_DEPTH_FAIL = 0wx0B95
        val GL_STENCIL_PASS_DEPTH_PASS = 0wx0B96
        val GL_STENCIL_REF = 0wx0B97
        val GL_STENCIL_WRITEMASK = 0wx0B98
        val GL_MATRIX_MODE = 0wx0BA0
        val GL_NORMALIZE = 0wx0BA1
        val GL_VIEWPORT = 0wx0BA2
        val GL_MODELVIEW_STACK_DEPTH = 0wx0BA3
        val GL_PROJECTION_STACK_DEPTH = 0wx0BA4
        val GL_TEXTURE_STACK_DEPTH = 0wx0BA5
        val GL_MODELVIEW_MATRIX = 0wx0BA6
        val GL_PROJECTION_MATRIX = 0wx0BA7
        val GL_TEXTURE_MATRIX = 0wx0BA8
        val GL_ATTRIB_STACK_DEPTH = 0wx0BB0
        val GL_CLIENT_ATTRIB_STACK_DEPTH = 0wx0BB1
        val GL_ALPHA_TEST = 0wx0BC0
        val GL_ALPHA_TEST_FUNC = 0wx0BC1
        val GL_ALPHA_TEST_REF = 0wx0BC2
        val GL_DITHER = 0wx0BD0
        val GL_BLEND_DST = 0wx0BE0
        val GL_BLEND_SRC = 0wx0BE1
        val GL_BLEND = 0wx0BE2
        val GL_LOGIC_OP_MODE = 0wx0BF0
        val GL_INDEX_LOGIC_OP = 0wx0BF1
        val GL_COLOR_LOGIC_OP = 0wx0BF2
        val GL_AUX_BUFFERS = 0wx0C00
        val GL_DRAW_BUFFER = 0wx0C01
        val GL_READ_BUFFER = 0wx0C02
        val GL_SCISSOR_BOX = 0wx0C10
        val GL_SCISSOR_TEST = 0wx0C11
        val GL_INDEX_CLEAR_VALUE = 0wx0C20
        val GL_INDEX_WRITEMASK = 0wx0C21
        val GL_COLOR_CLEAR_VALUE = 0wx0C22
        val GL_COLOR_WRITEMASK = 0wx0C23
        val GL_INDEX_MODE = 0wx0C30
        val GL_RGBA_MODE = 0wx0C31
        val GL_DOUBLEBUFFER = 0wx0C32
        val GL_STEREO = 0wx0C33
        val GL_RENDER_MODE = 0wx0C40
        val GL_PERSPECTIVE_CORRECTION_HINT = 0wx0C50
        val GL_POINT_SMOOTH_HINT = 0wx0C51
        val GL_LINE_SMOOTH_HINT = 0wx0C52
        val GL_POLYGON_SMOOTH_HINT = 0wx0C53
        val GL_FOG_HINT = 0wx0C54
        val GL_TEXTURE_GEN_S = 0wx0C60
        val GL_TEXTURE_GEN_T = 0wx0C61
        val GL_TEXTURE_GEN_R = 0wx0C62
        val GL_TEXTURE_GEN_Q = 0wx0C63
        val GL_PIXEL_MAP_I_TO_I = 0wx0C70
        val GL_PIXEL_MAP_S_TO_S = 0wx0C71
        val GL_PIXEL_MAP_I_TO_R = 0wx0C72
        val GL_PIXEL_MAP_I_TO_G = 0wx0C73
        val GL_PIXEL_MAP_I_TO_B = 0wx0C74
        val GL_PIXEL_MAP_I_TO_A = 0wx0C75
        val GL_PIXEL_MAP_R_TO_R = 0wx0C76
        val GL_PIXEL_MAP_G_TO_G = 0wx0C77
        val GL_PIXEL_MAP_B_TO_B = 0wx0C78
        val GL_PIXEL_MAP_A_TO_A = 0wx0C79
        val GL_PIXEL_MAP_I_TO_I_SIZE = 0wx0CB0
        val GL_PIXEL_MAP_S_TO_S_SIZE = 0wx0CB1
        val GL_PIXEL_MAP_I_TO_R_SIZE = 0wx0CB2
        val GL_PIXEL_MAP_I_TO_G_SIZE = 0wx0CB3
        val GL_PIXEL_MAP_I_TO_B_SIZE = 0wx0CB4
        val GL_PIXEL_MAP_I_TO_A_SIZE = 0wx0CB5
        val GL_PIXEL_MAP_R_TO_R_SIZE = 0wx0CB6
        val GL_PIXEL_MAP_G_TO_G_SIZE = 0wx0CB7
        val GL_PIXEL_MAP_B_TO_B_SIZE = 0wx0CB8
        val GL_PIXEL_MAP_A_TO_A_SIZE = 0wx0CB9
        val GL_UNPACK_SWAP_BYTES = 0wx0CF0
        val GL_UNPACK_LSB_FIRST = 0wx0CF1
        val GL_UNPACK_ROW_LENGTH = 0wx0CF2
        val GL_UNPACK_SKIP_ROWS = 0wx0CF3
        val GL_UNPACK_SKIP_PIXELS = 0wx0CF4
        val GL_UNPACK_ALIGNMENT = 0wx0CF5
        val GL_PACK_SWAP_BYTES = 0wx0D00
        val GL_PACK_LSB_FIRST = 0wx0D01
        val GL_PACK_ROW_LENGTH = 0wx0D02
        val GL_PACK_SKIP_ROWS = 0wx0D03
        val GL_PACK_SKIP_PIXELS = 0wx0D04
        val GL_PACK_ALIGNMENT = 0wx0D05
        val GL_MAP_COLOR = 0wx0D10
        val GL_MAP_STENCIL = 0wx0D11
        val GL_INDEX_SHIFT = 0wx0D12
        val GL_INDEX_OFFSET = 0wx0D13
        val GL_RED_SCALE = 0wx0D14
        val GL_RED_BIAS = 0wx0D15
        val GL_ZOOM_X = 0wx0D16
        val GL_ZOOM_Y = 0wx0D17
        val GL_GREEN_SCALE = 0wx0D18
        val GL_GREEN_BIAS = 0wx0D19
        val GL_BLUE_SCALE = 0wx0D1A
        val GL_BLUE_BIAS = 0wx0D1B
        val GL_ALPHA_SCALE = 0wx0D1C
        val GL_ALPHA_BIAS = 0wx0D1D
        val GL_DEPTH_SCALE = 0wx0D1E
        val GL_DEPTH_BIAS = 0wx0D1F
        val GL_MAX_EVAL_ORDER = 0wx0D30
        val GL_MAX_LIGHTS = 0wx0D31
        val GL_MAX_CLIP_PLANES = 0wx0D32
        val GL_MAX_TEXTURE_SIZE = 0wx0D33
        val GL_MAX_PIXEL_MAP_TABLE = 0wx0D34
        val GL_MAX_ATTRIB_STACK_DEPTH = 0wx0D35
        val GL_MAX_MODELVIEW_STACK_DEPTH = 0wx0D36
        val GL_MAX_NAME_STACK_DEPTH = 0wx0D37
        val GL_MAX_PROJECTION_STACK_DEPTH = 0wx0D38
        val GL_MAX_TEXTURE_STACK_DEPTH = 0wx0D39
        val GL_MAX_VIEWPORT_DIMS = 0wx0D3A
        val GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = 0wx0D3B
        val GL_SUBPIXEL_BITS = 0wx0D50
        val GL_INDEX_BITS = 0wx0D51
        val GL_RED_BITS = 0wx0D52
        val GL_GREEN_BITS = 0wx0D53
        val GL_BLUE_BITS = 0wx0D54
        val GL_ALPHA_BITS = 0wx0D55
        val GL_DEPTH_BITS = 0wx0D56
        val GL_STENCIL_BITS = 0wx0D57
        val GL_ACCUM_RED_BITS = 0wx0D58
        val GL_ACCUM_GREEN_BITS = 0wx0D59
        val GL_ACCUM_BLUE_BITS = 0wx0D5A
        val GL_ACCUM_ALPHA_BITS = 0wx0D5B
        val GL_NAME_STACK_DEPTH = 0wx0D70
        val GL_AUTO_NORMAL = 0wx0D80
        val GL_MAP1_COLOR_4 = 0wx0D90
        val GL_MAP1_INDEX = 0wx0D91
        val GL_MAP1_NORMAL = 0wx0D92
        val GL_MAP1_TEXTURE_COORD_1 = 0wx0D93
        val GL_MAP1_TEXTURE_COORD_2 = 0wx0D94
        val GL_MAP1_TEXTURE_COORD_3 = 0wx0D95
        val GL_MAP1_TEXTURE_COORD_4 = 0wx0D96
        val GL_MAP1_VERTEX_3 = 0wx0D97
        val GL_MAP1_VERTEX_4 = 0wx0D98
        val GL_MAP2_COLOR_4 = 0wx0DB0
        val GL_MAP2_INDEX = 0wx0DB1
        val GL_MAP2_NORMAL = 0wx0DB2
        val GL_MAP2_TEXTURE_COORD_1 = 0wx0DB3
        val GL_MAP2_TEXTURE_COORD_2 = 0wx0DB4
        val GL_MAP2_TEXTURE_COORD_3 = 0wx0DB5
        val GL_MAP2_TEXTURE_COORD_4 = 0wx0DB6
        val GL_MAP2_VERTEX_3 = 0wx0DB7
        val GL_MAP2_VERTEX_4 = 0wx0DB8
        val GL_MAP1_GRID_DOMAIN = 0wx0DD0
        val GL_MAP1_GRID_SEGMENTS = 0wx0DD1
        val GL_MAP2_GRID_DOMAIN = 0wx0DD2
        val GL_MAP2_GRID_SEGMENTS = 0wx0DD3
        val GL_TEXTURE_1D = 0wx0DE0
        val GL_TEXTURE_2D = 0wx0DE1
        val GL_FEEDBACK_BUFFER_POINTER = 0wx0DF0
        val GL_FEEDBACK_BUFFER_SIZE = 0wx0DF1
        val GL_FEEDBACK_BUFFER_TYPE = 0wx0DF2
        val GL_SELECTION_BUFFER_POINTER = 0wx0DF3
        val GL_SELECTION_BUFFER_SIZE = 0wx0DF4

        (* GetTextureParameter *)
        val GL_TEXTURE_WIDTH = 0wx1000
        val GL_TEXTURE_HEIGHT = 0wx1001
        val GL_TEXTURE_INTERNAL_FORMAT = 0wx1003
        val GL_TEXTURE_BORDER_COLOR = 0wx1004
        val GL_TEXTURE_BORDER = 0wx1005

        (* HintMode *)
        val GL_DONT_CARE = 0wx1100
        val GL_FASTEST = 0wx1101
        val GL_NICEST = 0wx1102

        (* LightName *)
        val GL_LIGHT0 = 0wx4000
        val GL_LIGHT1 = 0wx4001
        val GL_LIGHT2 = 0wx4002
        val GL_LIGHT3 = 0wx4003
        val GL_LIGHT4 = 0wx4004
        val GL_LIGHT5 = 0wx4005
        val GL_LIGHT6 = 0wx4006
        val GL_LIGHT7 = 0wx4007

        (* LightParameter *)
        val GL_AMBIENT = 0wx1200
        val GL_DIFFUSE = 0wx1201
        val GL_SPECULAR = 0wx1202
        val GL_POSITION = 0wx1203
        val GL_SPOT_DIRECTION = 0wx1204
        val GL_SPOT_EXPONENT = 0wx1205
        val GL_SPOT_CUTOFF = 0wx1206
        val GL_CONSTANT_ATTENUATION = 0wx1207
        val GL_LINEAR_ATTENUATION = 0wx1208
        val GL_QUADRATIC_ATTENUATION = 0wx1209

        (* ListMode *)
        val GL_COMPILE = 0wx1300
        val GL_COMPILE_AND_EXECUTE = 0wx1301

        (* LogicOp *)
        val GL_CLEAR = 0wx1500
        val GL_AND = 0wx1501
        val GL_AND_REVERSE = 0wx1502
        val GL_COPY = 0wx1503
        val GL_AND_INVERTED = 0wx1504
        val GL_NOOP = 0wx1505
        val GL_XOR = 0wx1506
        val GL_OR = 0wx1507
        val GL_NOR = 0wx1508
        val GL_EQUIV = 0wx1509
        val GL_INVERT = 0wx150A
        val GL_OR_REVERSE = 0wx150B
        val GL_COPY_INVERTED = 0wx150C
        val GL_OR_INVERTED = 0wx150D
        val GL_NAND = 0wx150E
        val GL_SET = 0wx150F

        (* MaterialParameter *)
        val GL_EMISSION = 0wx1600
        val GL_SHININESS = 0wx1601
        val GL_AMBIENT_AND_DIFFUSE = 0wx1602
        val GL_COLOR_INDEXES = 0wx1603

        (* MatrixMode *)
        val GL_MODELVIEW = 0wx1700
        val GL_PROJECTION = 0wx1701
        val GL_TEXTURE = 0wx1702

        (* PixelCopyType *)
        val GL_COLOR = 0wx1800
        val GL_DEPTH = 0wx1801
        val GL_STENCIL = 0wx1802

        (* PixelFormat *)
        val GL_COLOR_INDEX = 0wx1900
        val GL_STENCIL_INDEX = 0wx1901
        val GL_DEPTH_COMPONENT = 0wx1902
        val GL_RED = 0wx1903
        val GL_GREEN = 0wx1904
        val GL_BLUE = 0wx1905
        val GL_ALPHA = 0wx1906
        val GL_RGB = 0wx1907
        val GL_RGBA = 0wx1908
        val GL_LUMINANCE = 0wx1909
        val GL_LUMINANCE_ALPHA = 0wx190A

        (* PixelType *)
        val GL_BITMAP = 0wx1A00

        (* PolygonMode *)
        val GL_POINT = 0wx1B00
        val GL_LINE = 0wx1B01
        val GL_FILL = 0wx1B02

        (* RenderingMode *)
        val GL_RENDER = 0wx1C00
        val GL_FEEDBACK = 0wx1C01
        val GL_SELECT = 0wx1C02

        (* ShadingModel *)
        val GL_FLAT = 0wx1D00
        val GL_SMOOTH = 0wx1D01

        (* StencilOp *)
        val GL_KEEP = 0wx1E00
        val GL_REPLACE = 0wx1E01
        val GL_INCR = 0wx1E02
        val GL_DECR = 0wx1E03

        (* StringName *)
        val GL_VENDOR = 0wx1F00
        val GL_RENDERER = 0wx1F01
        val GL_VERSION = 0wx1F02
        val GL_EXTENSIONS = 0wx1F03

        (* TextureCoordName *)
        val GL_S = 0wx2000
        val GL_T = 0wx2001
        val GL_R = 0wx2002
        val GL_Q = 0wx2003

        (* TextureEnvMode *)
        val GL_MODULATE = 0wx2100
        val GL_DECAL = 0wx2101

        (* TextureEnvParameter *)
        val GL_TEXTURE_ENV_MODE = 0wx2200
        val GL_TEXTURE_ENV_COLOR = 0wx2201

        (* TextureEnvTarget *)
        val GL_TEXTURE_ENV = 0wx2300

        (* TextureGenMode *)
        val GL_EYE_LINEAR = 0wx2400
        val GL_OBJECT_LINEAR = 0wx2401
        val GL_SPHERE_MAP = 0wx2402

        (* TextureGenParameter *)
        val GL_TEXTURE_GEN_MODE = 0wx2500
        val GL_OBJECT_PLANE = 0wx2501
        val GL_EYE_PLANE = 0wx2502

        (* TextureMagFilter *)
        val GL_NEAREST = 0wx2600
        val GL_LINEAR = 0wx2601

        (* TextureMinFilter *)
        val GL_NEAREST_MIPMAP_NEAREST = 0wx2700
        val GL_LINEAR_MIPMAP_NEAREST = 0wx2701
        val GL_NEAREST_MIPMAP_LINEAR = 0wx2702
        val GL_LINEAR_MIPMAP_LINEAR = 0wx2703

        (* TextureParameterName *)
        val GL_TEXTURE_MAG_FILTER = 0wx2800
        val GL_TEXTURE_MIN_FILTER = 0wx2801
        val GL_TEXTURE_WRAP_S = 0wx2802
        val GL_TEXTURE_WRAP_T = 0wx2803

        (* TextureWrapMode *)
        val GL_CLAMP = 0wx2900
        val GL_REPEAT = 0wx2901

        (* ClientAttribMask *)
        val GL_CLIENT_PIXEL_STORE_BIT = 0wx00000001
        val GL_CLIENT_VERTEX_ARRAY_BIT = 0wx00000002
        (* val GL_CLIENT_ALL_ATTRIB_BITS = 0wxffffffff *)
        val GL_CLIENT_ALL_ATTRIB_BITS =
            Word8Vector.fromList [0wxFF, 0wxFF, 0wxFF, 0wxFF];
        (* polygon_offset *)
        val GL_POLYGON_OFFSET_FACTOR = 0wx8038
        val GL_POLYGON_OFFSET_UNITS = 0wx2A00
        val GL_POLYGON_OFFSET_POINT = 0wx2A01
        val GL_POLYGON_OFFSET_LINE = 0wx2A02
        val GL_POLYGON_OFFSET_FILL = 0wx8037

        (* texture *)
        val GL_ALPHA4 = 0wx803B
        val GL_ALPHA8 = 0wx803C
        val GL_ALPHA12 = 0wx803D
        val GL_ALPHA16 = 0wx803E
        val GL_LUMINANCE4 = 0wx803F
        val GL_LUMINANCE8 = 0wx8040
        val GL_LUMINANCE12 = 0wx8041
        val GL_LUMINANCE16 = 0wx8042
        val GL_LUMINANCE4_ALPHA4 = 0wx8043
        val GL_LUMINANCE6_ALPHA2 = 0wx8044
        val GL_LUMINANCE8_ALPHA8 = 0wx8045
        val GL_LUMINANCE12_ALPHA4 = 0wx8046
        val GL_LUMINANCE12_ALPHA12 = 0wx8047
        val GL_LUMINANCE16_ALPHA16 = 0wx8048
        val GL_INTENSITY = 0wx8049
        val GL_INTENSITY4 = 0wx804A
        val GL_INTENSITY8 = 0wx804B
        val GL_INTENSITY12 = 0wx804C
        val GL_INTENSITY16 = 0wx804D
        val GL_R3_G3_B2 = 0wx2A10
        val GL_RGB4 = 0wx804F
        val GL_RGB5 = 0wx8050
        val GL_RGB8 = 0wx8051
        val GL_RGB10 = 0wx8052
        val GL_RGB12 = 0wx8053
        val GL_RGB16 = 0wx8054
        val GL_RGBA2 = 0wx8055
        val GL_RGBA4 = 0wx8056
        val GL_RGB5_A1 = 0wx8057
        val GL_RGBA8 = 0wx8058
        val GL_RGB10_A2 = 0wx8059
        val GL_RGBA12 = 0wx805A
        val GL_RGBA16 = 0wx805B
        val GL_TEXTURE_RED_SIZE = 0wx805C
        val GL_TEXTURE_GREEN_SIZE = 0wx805D
        val GL_TEXTURE_BLUE_SIZE = 0wx805E
        val GL_TEXTURE_ALPHA_SIZE = 0wx805F
        val GL_TEXTURE_LUMINANCE_SIZE = 0wx8060
        val GL_TEXTURE_INTENSITY_SIZE = 0wx8061
        val GL_PROXY_TEXTURE_1D = 0wx8063
        val GL_PROXY_TEXTURE_2D = 0wx8064

        (* texture_object *)
        val GL_TEXTURE_PRIORITY = 0wx8066
        val GL_TEXTURE_RESIDENT = 0wx8067
        val GL_TEXTURE_BINDING_1D = 0wx8068
        val GL_TEXTURE_BINDING_2D = 0wx8069

        (* vertex_array *)
        val GL_VERTEX_ARRAY = 0wx8074
        val GL_NORMAL_ARRAY = 0wx8075
        val GL_COLOR_ARRAY = 0wx8076
        val GL_INDEX_ARRAY = 0wx8077
        val GL_TEXTURE_COORD_ARRAY = 0wx8078
        val GL_EDGE_FLAG_ARRAY = 0wx8079
        val GL_VERTEX_ARRAY_SIZE = 0wx807A
        val GL_VERTEX_ARRAY_TYPE = 0wx807B
        val GL_VERTEX_ARRAY_STRIDE = 0wx807C
        val GL_NORMAL_ARRAY_TYPE = 0wx807E
        val GL_NORMAL_ARRAY_STRIDE = 0wx807F
        val GL_COLOR_ARRAY_SIZE = 0wx8081
        val GL_COLOR_ARRAY_TYPE = 0wx8082
        val GL_COLOR_ARRAY_STRIDE = 0wx8083
        val GL_INDEX_ARRAY_TYPE = 0wx8085
        val GL_INDEX_ARRAY_STRIDE = 0wx8086
        val GL_TEXTURE_COORD_ARRAY_SIZE = 0wx8088
        val GL_TEXTURE_COORD_ARRAY_TYPE = 0wx8089
        val GL_TEXTURE_COORD_ARRAY_STRIDE = 0wx808A
        val GL_EDGE_FLAG_ARRAY_STRIDE = 0wx808C
        val GL_VERTEX_ARRAY_POINTER = 0wx808E
        val GL_NORMAL_ARRAY_POINTER = 0wx808F
        val GL_COLOR_ARRAY_POINTER = 0wx8090
        val GL_INDEX_ARRAY_POINTER = 0wx8091
        val GL_TEXTURE_COORD_ARRAY_POINTER = 0wx8092
        val GL_EDGE_FLAG_ARRAY_POINTER = 0wx8093
        val GL_V2F = 0wx2A20
        val GL_V3F = 0wx2A21
        val GL_C4UB_V2F = 0wx2A22
        val GL_C4UB_V3F = 0wx2A23
        val GL_C3F_V3F = 0wx2A24
        val GL_N3F_V3F = 0wx2A25
        val GL_C4F_N3F_V3F = 0wx2A26
        val GL_T2F_V3F = 0wx2A27
        val GL_T4F_V4F = 0wx2A28
        val GL_T2F_C4UB_V3F = 0wx2A29
        val GL_T2F_C3F_V3F = 0wx2A2A
        val GL_T2F_N3F_V3F = 0wx2A2B
        val GL_T2F_C4F_N3F_V3F = 0wx2A2C
        val GL_T4F_C4F_N3F_V4F = 0wx2A2D

        (* Extensions *)
        val GL_EXT_vertex_array = 0w1
        val GL_WIN_swap_hint = 0w1
        val GL_EXT_bgra = 0w1
        val GL_EXT_paletted_texture = 0w1

        (* EXT_vertex_array *)
        val GL_VERTEX_ARRAY_EXT = 0wx8074
        val GL_NORMAL_ARRAY_EXT = 0wx8075
        val GL_COLOR_ARRAY_EXT = 0wx8076
        val GL_INDEX_ARRAY_EXT = 0wx8077
        val GL_TEXTURE_COORD_ARRAY_EXT = 0wx8078
        val GL_EDGE_FLAG_ARRAY_EXT = 0wx8079
        val GL_VERTEX_ARRAY_SIZE_EXT = 0wx807A
        val GL_VERTEX_ARRAY_TYPE_EXT = 0wx807B
        val GL_VERTEX_ARRAY_STRIDE_EXT = 0wx807C
        val GL_VERTEX_ARRAY_COUNT_EXT = 0wx807D
        val GL_NORMAL_ARRAY_TYPE_EXT = 0wx807E
        val GL_NORMAL_ARRAY_STRIDE_EXT = 0wx807F
        val GL_NORMAL_ARRAY_COUNT_EXT = 0wx8080
        val GL_COLOR_ARRAY_SIZE_EXT = 0wx8081
        val GL_COLOR_ARRAY_TYPE_EXT = 0wx8082
        val GL_COLOR_ARRAY_STRIDE_EXT = 0wx8083
        val GL_COLOR_ARRAY_COUNT_EXT = 0wx8084
        val GL_INDEX_ARRAY_TYPE_EXT = 0wx8085
        val GL_INDEX_ARRAY_STRIDE_EXT = 0wx8086
        val GL_INDEX_ARRAY_COUNT_EXT = 0wx8087
        val GL_TEXTURE_COORD_ARRAY_SIZE_EXT = 0wx8088
        val GL_TEXTURE_COORD_ARRAY_TYPE_EXT = 0wx8089
        val GL_TEXTURE_COORD_ARRAY_STRIDE_EXT =0wx808A
        val GL_TEXTURE_COORD_ARRAY_COUNT_EXT = 0wx808B
        val GL_EDGE_FLAG_ARRAY_STRIDE_EXT = 0wx808C
        val GL_EDGE_FLAG_ARRAY_COUNT_EXT = 0wx808D
        val GL_VERTEX_ARRAY_POINTER_EXT = 0wx808E
        val GL_NORMAL_ARRAY_POINTER_EXT = 0wx808F
        val GL_COLOR_ARRAY_POINTER_EXT = 0wx8090
        val GL_INDEX_ARRAY_POINTER_EXT = 0wx8091
        val GL_TEXTURE_COORD_ARRAY_POINTER_EXT =0wx8092
        val GL_EDGE_FLAG_ARRAY_POINTER_EXT = 0wx8093
        val GL_DOUBLE_EXT = GL_DOUBLE

        (* EXT_bgra *)
        val GL_BGR_EXT = 0wx80E0
        val GL_BGRA_EXT = 0wx80E1

        (* EXT_paletted_texture *)
        (* These must match the GL_COLOR_TABLE_*_SGI enumerants *)
        val GL_COLOR_TABLE_FORMAT_EXT = 0wx80D8
        val GL_COLOR_TABLE_WIDTH_EXT = 0wx80D9
        val GL_COLOR_TABLE_RED_SIZE_EXT = 0wx80DA
        val GL_COLOR_TABLE_GREEN_SIZE_EXT = 0wx80DB
        val GL_COLOR_TABLE_BLUE_SIZE_EXT = 0wx80DC
        val GL_COLOR_TABLE_ALPHA_SIZE_EXT = 0wx80DD
        val GL_COLOR_TABLE_LUMINANCE_SIZE_EXT =0wx80DE
        val GL_COLOR_TABLE_INTENSITY_SIZE_EXT =0wx80DF

        val GL_COLOR_INDEX1_EXT = 0wx80E2
        val GL_COLOR_INDEX2_EXT = 0wx80E3
        val GL_COLOR_INDEX4_EXT = 0wx80E4
        val GL_COLOR_INDEX8_EXT = 0wx80E5
        val GL_COLOR_INDEX12_EXT = 0wx80E6
        val GL_COLOR_INDEX16_EXT = 0wx80E7

        (* For compatibility with OpenGL v1.0 *)

        val GL_LOGIC_OP = GL_INDEX_LOGIC_OP
        val GL_TEXTURE_COMPONENTS = GL_TEXTURE_INTERNAL_FORMAT
        val c_glBegin = _import "glBegin" stdcall: GLenum -> unit;
        fun glBegin (a:GLenum)= c_glBegin (a): unit;

        val c_glBitmap = _import "glBitmap" stdcall: int * int * GLreal * GLreal * GLreal * GLreal * Word8Vector.vector -> unit;
        fun glBitmap (a:int) (b:int) (c:GLreal) (d:GLreal) (e:GLreal) (f:GLreal) (g:Word8Vector.vector) = c_glBitmap (a,b,c,d,e,f,g) :unit;

        val c_glEnd = _import "glEnd" stdcall: unit -> unit;
        fun glEnd ()= c_glEnd (): unit;

        val c_glBlendFunc = _import "glBlendFunc" stdcall: GLenum * GLenum -> unit;
        fun glBlendFunc (a:GLenum) (b:GLenum) = c_glBlendFunc (a,b) :unit

        val c_glCallList = _import "glCallList" stdcall: int -> unit;
        fun glCallList (a:int) = c_glCallList (a): unit;

        val c_glClearColor = _import "glClearColor" stdcall:
                               GLreal * GLreal * GLreal * GLreal -> unit;
        fun glClearColor (a:GLreal) (b:GLreal) (c:GLreal) (d:GLreal)
          = c_glClearColor (a,b,c,d) : unit

        val c_glClearDepth = _import "glClearDepth" stdcall: GLreal -> unit;
        fun glClearDepth (a:GLreal) = c_glClearDepth a : unit

        val c_glLineWidth = _import "glLineWidth" stdcall: GLreal -> unit;
        fun glLineWidth (a:GLreal) = c_glLineWidth a : unit

        val c_glColor3d = _import "glColor3d" stdcall: GLdouble * GLdouble * GLdouble -> unit;
        fun glColor3d (a:GLdouble) (b:GLdouble) (c:GLdouble)
          = c_glColor3d (a,b,c) : unit

        val c_glColor3f = _import "glColor3f" stdcall: GLreal * GLreal * GLreal -> unit;
        fun glColor3f (a:GLreal) (b:GLreal) (c:GLreal)
          = c_glColor3f (a,b,c) : unit

        val c_glColor3ub = _import "glColor3ub" stdcall: Word8.word * Word8.word * Word8.word -> unit;
        fun glColor3ub (a:Word8.word) (b:Word8.word) (c:Word8.word)
          = c_glColor3ub (a,b,c) : unit

        val c_glColor4d = _import "glColor4d" stdcall: GLdouble * GLdouble * GLdouble * GLdouble -> unit;
        fun glColor4d (a:GLdouble) (b:GLdouble) (c:GLdouble) (d:GLdouble)
          = c_glColor4d (a,b,c,d) : unit

        val c_glColor4f = _import "glColor4f" stdcall: GLreal * GLreal * GLreal * GLreal -> unit;
        fun glColor4f (a:GLreal) (b:GLreal) (c:GLreal) (d:GLreal)
          = c_glColor4f (a,b,c,d) : unit

        val c_glColor4ub = _import "glColor4ub" stdcall: Word8.word * Word8.word * Word8.word * Word8.word -> unit;
        fun glColor4ub (a:Word8.word) (b:Word8.word) (c:Word8.word) (d:Word8.word)
          = c_glColor4ub (a,b,c,d) : unit

        val c_glColorMaterial = _import "glColorMaterial" stdcall: GLenum * GLenum -> unit;
        fun glColorMaterial (a:GLenum) (b:GLenum) = c_glColorMaterial (a,b) : unit

        val c_glDisable = _import "glDisable" stdcall: GLenum -> unit;
        fun glDisable (a:GLenum)= c_glDisable (a): unit;

        val c_glEnable = _import "glEnable" stdcall: GLenum -> unit;
        fun glEnable (a:GLenum)= c_glEnable (a): unit;

        val c_glRasterPos2i = _import "glRasterPos2i" stdcall: int * int -> unit;
        fun glRasterPos2i (a:int) (b:int)
          = c_glRasterPos2i (a,b) : unit

        val c_glRasterPos2f = _import "glRasterPos2f" stdcall: GLreal * GLreal -> unit;
        fun glRasterPos2f (a:GLreal) (b:GLreal)
          = c_glRasterPos2f (a,b) : unit

        val c_glRasterPos2d = _import "glRasterPos2d" stdcall: GLdouble * GLdouble -> unit;
        fun glRasterPos2d (a:GLdouble) (b:GLdouble)
          = c_glRasterPos2d (a,b) : unit

        val c_glShadeModel = _import "glShadeModel" stdcall: GLenum -> unit;
        fun glShadeModel (a:GLenum)= c_glShadeModel (a): unit;

        val c_glClear = _import "glClear" stdcall: GLenum -> unit;
        fun glClear (a:GLenum)= c_glClear (a): unit;

        val c_glEndList = _import "glEndList" stdcall: unit -> unit;
        fun glEndList () = c_glEndList (): unit;

        val c_glFinish = _import "glFinish" stdcall: unit -> unit;
        fun glFinish () = c_glFinish (): unit;

        val c_glFlush = _import "glFlush" stdcall: unit -> unit;
        fun glFlush () = c_glFlush (): unit;

        val c_glFrontFace = _import "glFrontFace" stdcall: GLenum -> unit;
        fun glFrontFace (a:GLenum)= c_glFrontFace (a): unit;

        val c_glLightfv = _import "glLightfv" stdcall: GLenum * GLenum * GLreal array -> unit;
        fun glLightfv (a:GLenum) (c:GLenum) (b:realrgbacolour) =
            let
                val rgba = Array.fromList b
            in
                c_glLightfv (a, c, rgba)
            end :unit

        val c_glLightModelfv = _import "glLightModelfv" stdcall: GLenum * GLreal array -> unit;
        fun glLightModelfv (a:GLenum) (b:realrgbacolour) =
            let
                val rgba = Array.fromList b
            in
                c_glLightModelfv (a, rgba)
            end :unit

        val c_glLoadIdentity = _import "glLoadIdentity" stdcall: unit -> unit;
        fun glLoadIdentity () = c_glLoadIdentity (): unit;

        val c_glMaterialfv = _import "glMaterialfv" stdcall: GLenum * GLenum * GLreal array -> unit;
        fun glMaterialfv (a:GLenum) (c:GLenum) (b:GLreal array) = c_glMaterialfv (a, c, b) :unit;

        val c_glMatrixMode = _import "glMatrixMode" stdcall: GLenum -> unit;
        fun glMatrixMode (a:GLenum)= c_glMatrixMode (a): unit;

        val c_glNewList = _import "glNewList" stdcall: int * GLenum -> unit;
        fun glNewList (b:int) (a:GLenum)= c_glNewList (b,a): unit;

        val c_glOrtho = _import "glOrtho" stdcall: GLdouble * GLdouble * GLdouble * GLdouble * GLdouble * GLdouble -> unit;
        fun glOrtho (a0 : GLdouble) (a1 : GLdouble) (a2 : GLdouble)
            (a3 : GLdouble) (a4 : GLdouble) (a5 : GLdouble) =
                c_glOrtho (a0, a1, a2, a3, a4, a5)

        val c_glPixelTransferi = _import "glPixelTransferi" stdcall: GLenum * int -> unit;
        fun glPixelTransferi (a:GLenum) (b:int) = c_glPixelTransferi (a,b):unit;

        val c_glPushMatrix = _import "glPushMatrix" stdcall: unit -> unit;
        fun glPushMatrix () = c_glPushMatrix (): unit;

        val c_glPopAttrib = _import "glPopAttrib" stdcall: unit -> unit;
        fun glPopAttrib () = c_glPopAttrib (): unit;

        val c_glPushAttrib = _import "glPushAttrib" stdcall: GLenum -> unit;
        fun glPushAttrib (a:GLenum)= c_glPushAttrib (a): unit;

        val c_glPolygonMode = _import "glPolygonMode" stdcall: GLenum * GLenum -> unit;
        fun glPolygonMode (a:GLenum) (b:GLenum) = c_glPolygonMode (a,b) :unit

        val c_glPopMatrix = _import "glPopMatrix" stdcall: unit -> unit;
        fun glPopMatrix () = c_glPopMatrix (): unit;

        val c_glTranslated = _import "glTranslated" stdcall: GLdouble * GLdouble * GLdouble -> unit;
        fun glTranslated (a:GLdouble) (b:GLdouble) (c:GLdouble)
          = c_glTranslated (a,b,c) : unit

        val c_glTranslatef = _import "glTranslatef" stdcall: GLreal * GLreal * GLreal -> unit;
        fun glTranslatef (a:GLreal) (b:GLreal) (c:GLreal)
          = c_glTranslatef (a,b,c) : unit

        val c_glViewport = _import "glViewport" stdcall: int * int * int * int -> unit;
        fun glViewport (a:int) (b:int) (c:int) (d:int) = c_glViewport (a,b,c,d) : unit

        val c_glRotatef = _import "glRotatef" stdcall: GLreal * GLreal * GLreal * GLreal -> unit;
        fun glRotatef (a:GLreal) (b:GLreal) (c:GLreal) (d:GLreal)
          = c_glRotatef (a,b,c,d) : unit

        val c_glRotated = _import "glRotated" stdcall: GLdouble * GLdouble * GLdouble * GLdouble -> unit;
        fun glRotated (a:GLdouble) (b:GLdouble) (c:GLdouble) (d:GLdouble)
          = c_glRotated (a,b,c,d) : unit

        val c_glVertex2f = _import "glVertex2f" stdcall: GLreal * GLreal -> unit;
        fun glVertex2f (a:GLreal) (b:GLreal)
          = c_glVertex2f (a,b) : unit

        val c_glVertex2d = _import "glVertex2d" stdcall: GLdouble * GLdouble -> unit;
        fun glVertex2d (a:GLdouble) (b:GLdouble)
          = c_glVertex2d (a,b) : unit

        val c_glVertex3d = _import "glVertex3d" stdcall: GLdouble * GLdouble * GLdouble -> unit;
        fun glVertex3d (a:GLdouble) (b:GLdouble) (c:GLdouble)
          = c_glVertex3d (a,b,c) : unit

        val c_glVertex3f = _import "glVertex3f" stdcall: GLreal * GLreal * GLreal -> unit;
        fun glVertex3f (a:GLreal) (b:GLreal) (c:GLreal)
          = c_glVertex3f (a,b,c) : unit
    end
