(*
 * Open GL library interface
 *)
signature GL =
    sig






        type GLreal = Real32.real
        type GLdouble = real

        type GLenum = Word.word
        datatype realspec = realRGB of GLreal * GLreal * GLreal
        type realvertex = GLreal * GLreal * GLreal
        type realrgbacolour = GLreal list

        datatype intspec = intRGB of Word.word * Word.word * Word.word
        type intvertex = Word.word * Word.word * Word.word
        type intrgbacolour = Word.word * Word.word * Word.word * Word.word

        (* Constants *)
        val GL_ACCUM : GLenum
        val GL_LOAD : GLenum
        val GL_RETURN : GLenum
        val GL_MULT : GLenum
        val GL_ADD : GLenum

        (* AlphaFunction *)
        val GL_NEVER : GLenum
        val GL_LESS : GLenum
        val GL_EQUAL : GLenum
        val GL_LEQUAL : GLenum
        val GL_GREATER : GLenum
        val GL_NOTEQUAL : GLenum
        val GL_GEQUAL : GLenum
        val GL_ALWAYS : GLenum

        (* AttribMask *)
        val GL_CURRENT_BIT : GLenum
        val GL_POINT_BIT : GLenum
        val GL_LINE_BIT : GLenum
        val GL_POLYGON_BIT : GLenum
        val GL_POLYGON_STIPPLE_BIT : GLenum
        val GL_PIXEL_MODE_BIT : GLenum
        val GL_LIGHTING_BIT : GLenum
        val GL_FOG_BIT : GLenum
        val GL_DEPTH_BUFFER_BIT : GLenum
        val GL_ACCUM_BUFFER_BIT : GLenum
        val GL_STENCIL_BUFFER_BIT : GLenum
        val GL_VIEWPORT_BIT : GLenum
        val GL_TRANSFORM_BIT : GLenum
        val GL_ENABLE_BIT : GLenum
        val GL_COLOR_BUFFER_BIT : GLenum
        val GL_HINT_BIT : GLenum
        val GL_EVAL_BIT : GLenum
        val GL_LIST_BIT : GLenum
        val GL_TEXTURE_BIT : GLenum
        val GL_SCISSOR_BIT : GLenum
        val GL_ALL_ATTRIB_BITS : GLenum

        (* BeginMode *)
        val GL_POINTS : GLenum
        val GL_LINES : GLenum
        val GL_LINE_LOOP : GLenum
        val GL_LINE_STRIP : GLenum
        val GL_TRIANGLES : GLenum
        val GL_TRIANGLE_STRIP : GLenum
        val GL_TRIANGLE_FAN : GLenum
        val GL_QUADS : GLenum
        val GL_QUAD_STRIP : GLenum
        val GL_POLYGON : GLenum

        (* BlendingFactorDest *)
        val GL_ZERO : GLenum
        val GL_ONE : GLenum
        val GL_SRC_COLOR : GLenum
        val GL_ONE_MINUS_SRC_COLOR : GLenum
        val GL_SRC_ALPHA : GLenum
        val GL_ONE_MINUS_SRC_ALPHA : GLenum
        val GL_DST_ALPHA : GLenum
        val GL_ONE_MINUS_DST_ALPHA : GLenum

        (* BlendingFactorSrc *)
        val GL_DST_COLOR : GLenum
        val GL_ONE_MINUS_DST_COLOR : GLenum
        val GL_SRC_ALPHA_SATURATE : GLenum

        (* Boolean *)
        val GL_TRUE : GLenum
        val GL_FALSE : GLenum

        (* ClipPlaneName *)
        val GL_CLIP_PLANE0 : GLenum
        val GL_CLIP_PLANE1 : GLenum
        val GL_CLIP_PLANE2 : GLenum
        val GL_CLIP_PLANE3 : GLenum
        val GL_CLIP_PLANE4 : GLenum
        val GL_CLIP_PLANE5 : GLenum

        (* ColorMaterialFace *)
        (* DataType *)
        val GL_BYTE : GLenum
        val GL_UNSIGNED_BYTE : GLenum
        val GL_SHORT : GLenum
        val GL_UNSIGNED_SHORT : GLenum
        val GL_INT : GLenum
        val GL_UNSIGNED_INT : GLenum
        val GL_FLOAT : GLenum
        val GL_2_BYTES : GLenum
        val GL_3_BYTES : GLenum
        val GL_4_BYTES : GLenum
        val GL_DOUBLE : GLenum

        (* DrawBufferMode *)
        val GL_NONE : GLenum
        val GL_FRONT_LEFT : GLenum
        val GL_FRONT_RIGHT : GLenum
        val GL_BACK_LEFT : GLenum
        val GL_BACK_RIGHT : GLenum
        val GL_FRONT : GLenum
        val GL_BACK : GLenum
        val GL_LEFT : GLenum
        val GL_RIGHT : GLenum
        val GL_FRONT_AND_BACK : GLenum
        val GL_AUX0 : GLenum
        val GL_AUX1 : GLenum
        val GL_AUX2 : GLenum
        val GL_AUX3 : GLenum

        (* ErrorCode *)
        val GL_NO_ERROR : GLenum
        val GL_INVALID_ENUM : GLenum
        val GL_INVALID_VALUE : GLenum
        val GL_INVALID_OPERATION : GLenum
        val GL_STACK_OVERFLOW : GLenum
        val GL_STACK_UNDERFLOW : GLenum
        val GL_OUT_OF_MEMORY : GLenum

        (* FeedBackMode *)
        val GL_2D : GLenum
        val GL_3D : GLenum
        val GL_3D_COLOR : GLenum
        val GL_3D_COLOR_TEXTURE : GLenum
        val GL_4D_COLOR_TEXTURE : GLenum

        (* FeedBackToken *)
        val GL_PASS_THROUGH_TOKEN : GLenum
        val GL_POINT_TOKEN : GLenum
        val GL_LINE_TOKEN : GLenum
        val GL_POLYGON_TOKEN : GLenum
        val GL_BITMAP_TOKEN : GLenum
        val GL_DRAW_PIXEL_TOKEN : GLenum
        val GL_COPY_PIXEL_TOKEN : GLenum
        val GL_LINE_RESET_TOKEN : GLenum

        (* FogMode *)
        val GL_EXP : GLenum
        val GL_EXP2 : GLenum

        (* FrontFaceDirection *)
        val GL_CW : GLenum
        val GL_CCW : GLenum

        (* GetMapTarget *)
        val GL_COEFF : GLenum
        val GL_ORDER : GLenum
        val GL_DOMAIN : GLenum

        val GL_CURRENT_COLOR : GLenum
        val GL_CURRENT_INDEX : GLenum
        val GL_CURRENT_NORMAL : GLenum
        val GL_CURRENT_TEXTURE_COORDS : GLenum
        val GL_CURRENT_RASTER_COLOR : GLenum
        val GL_CURRENT_RASTER_INDEX : GLenum
        val GL_CURRENT_RASTER_TEXTURE_COORDS : GLenum
        val GL_CURRENT_RASTER_POSITION : GLenum
        val GL_CURRENT_RASTER_POSITION_VALID : GLenum
        val GL_CURRENT_RASTER_DISTANCE : GLenum
        val GL_POINT_SMOOTH : GLenum
        val GL_POINT_SIZE : GLenum
        val GL_POINT_SIZE_RANGE : GLenum
        val GL_POINT_SIZE_GRANULARITY : GLenum
        val GL_LINE_SMOOTH : GLenum
        val GL_LINE_WIDTH : GLenum
        val GL_LINE_WIDTH_RANGE : GLenum
        val GL_LINE_WIDTH_GRANULARITY : GLenum
        val GL_LINE_STIPPLE : GLenum
        val GL_LINE_STIPPLE_PATTERN : GLenum
        val GL_LINE_STIPPLE_REPEAT : GLenum
        val GL_LIST_MODE : GLenum
        val GL_MAX_LIST_NESTING : GLenum
        val GL_LIST_BASE : GLenum
        val GL_LIST_INDEX : GLenum
        val GL_POLYGON_MODE : GLenum
        val GL_POLYGON_SMOOTH : GLenum
        val GL_POLYGON_STIPPLE : GLenum
        val GL_EDGE_FLAG : GLenum
        val GL_CULL_FACE : GLenum
        val GL_CULL_FACE_MODE : GLenum
        val GL_FRONT_FACE : GLenum
        val GL_LIGHTING : GLenum
        val GL_LIGHT_MODEL_LOCAL_VIEWER : GLenum
        val GL_LIGHT_MODEL_TWO_SIDE : GLenum
        val GL_LIGHT_MODEL_AMBIENT : GLenum
        val GL_SHADE_MODEL : GLenum
        val GL_COLOR_MATERIAL_FACE : GLenum
        val GL_COLOR_MATERIAL_PARAMETER : GLenum
        val GL_COLOR_MATERIAL : GLenum
        val GL_FOG : GLenum
        val GL_FOG_INDEX : GLenum
        val GL_FOG_DENSITY : GLenum
        val GL_FOG_START : GLenum
        val GL_FOG_END : GLenum
        val GL_FOG_MODE : GLenum
        val GL_FOG_COLOR : GLenum
        val GL_DEPTH_RANGE : GLenum
        val GL_DEPTH_TEST : GLenum
        val GL_DEPTH_WRITEMASK : GLenum
        val GL_DEPTH_CLEAR_VALUE : GLenum
        val GL_DEPTH_FUNC : GLenum
        val GL_ACCUM_CLEAR_VALUE : GLenum
        val GL_STENCIL_TEST : GLenum
        val GL_STENCIL_CLEAR_VALUE : GLenum
        val GL_STENCIL_FUNC : GLenum
        val GL_STENCIL_VALUE_MASK : GLenum
        val GL_STENCIL_FAIL : GLenum
        val GL_STENCIL_PASS_DEPTH_FAIL : GLenum
        val GL_STENCIL_PASS_DEPTH_PASS : GLenum
        val GL_STENCIL_REF : GLenum
        val GL_STENCIL_WRITEMASK : GLenum
        val GL_MATRIX_MODE : GLenum
        val GL_NORMALIZE : GLenum
        val GL_VIEWPORT : GLenum
        val GL_MODELVIEW_STACK_DEPTH : GLenum
        val GL_PROJECTION_STACK_DEPTH : GLenum
        val GL_TEXTURE_STACK_DEPTH : GLenum
        val GL_MODELVIEW_MATRIX : GLenum
        val GL_PROJECTION_MATRIX : GLenum
        val GL_TEXTURE_MATRIX : GLenum
        val GL_ATTRIB_STACK_DEPTH : GLenum
        val GL_CLIENT_ATTRIB_STACK_DEPTH : GLenum
        val GL_ALPHA_TEST : GLenum
        val GL_ALPHA_TEST_FUNC : GLenum
        val GL_ALPHA_TEST_REF : GLenum
        val GL_DITHER : GLenum
        val GL_BLEND_DST : GLenum
        val GL_BLEND_SRC : GLenum
        val GL_BLEND : GLenum
        val GL_LOGIC_OP_MODE : GLenum
        val GL_INDEX_LOGIC_OP : GLenum
        val GL_COLOR_LOGIC_OP : GLenum
        val GL_AUX_BUFFERS : GLenum
        val GL_DRAW_BUFFER : GLenum
        val GL_READ_BUFFER : GLenum
        val GL_SCISSOR_BOX : GLenum
        val GL_SCISSOR_TEST : GLenum
        val GL_INDEX_CLEAR_VALUE : GLenum
        val GL_INDEX_WRITEMASK : GLenum
        val GL_COLOR_CLEAR_VALUE : GLenum
        val GL_COLOR_WRITEMASK : GLenum
        val GL_INDEX_MODE : GLenum
        val GL_RGBA_MODE : GLenum
        val GL_DOUBLEBUFFER : GLenum
        val GL_STEREO : GLenum
        val GL_RENDER_MODE : GLenum
        val GL_PERSPECTIVE_CORRECTION_HINT : GLenum
        val GL_POINT_SMOOTH_HINT : GLenum
        val GL_LINE_SMOOTH_HINT : GLenum
        val GL_POLYGON_SMOOTH_HINT : GLenum
        val GL_FOG_HINT : GLenum
        val GL_TEXTURE_GEN_S : GLenum
        val GL_TEXTURE_GEN_T : GLenum
        val GL_TEXTURE_GEN_R : GLenum
        val GL_TEXTURE_GEN_Q : GLenum
        val GL_PIXEL_MAP_I_TO_I : GLenum
        val GL_PIXEL_MAP_S_TO_S : GLenum
        val GL_PIXEL_MAP_I_TO_R : GLenum
        val GL_PIXEL_MAP_I_TO_G : GLenum
        val GL_PIXEL_MAP_I_TO_B : GLenum
        val GL_PIXEL_MAP_I_TO_A : GLenum
        val GL_PIXEL_MAP_R_TO_R : GLenum
        val GL_PIXEL_MAP_G_TO_G : GLenum
        val GL_PIXEL_MAP_B_TO_B : GLenum
        val GL_PIXEL_MAP_A_TO_A : GLenum
        val GL_PIXEL_MAP_I_TO_I_SIZE : GLenum
        val GL_PIXEL_MAP_S_TO_S_SIZE : GLenum
        val GL_PIXEL_MAP_I_TO_R_SIZE : GLenum
        val GL_PIXEL_MAP_I_TO_G_SIZE : GLenum
        val GL_PIXEL_MAP_I_TO_B_SIZE : GLenum
        val GL_PIXEL_MAP_I_TO_A_SIZE : GLenum
        val GL_PIXEL_MAP_R_TO_R_SIZE : GLenum
        val GL_PIXEL_MAP_G_TO_G_SIZE : GLenum
        val GL_PIXEL_MAP_B_TO_B_SIZE : GLenum
        val GL_PIXEL_MAP_A_TO_A_SIZE : GLenum
        val GL_UNPACK_SWAP_BYTES : GLenum
        val GL_UNPACK_LSB_FIRST : GLenum
        val GL_UNPACK_ROW_LENGTH : GLenum
        val GL_UNPACK_SKIP_ROWS : GLenum
        val GL_UNPACK_SKIP_PIXELS : GLenum
        val GL_UNPACK_ALIGNMENT : GLenum
        val GL_PACK_SWAP_BYTES : GLenum
        val GL_PACK_LSB_FIRST : GLenum
        val GL_PACK_ROW_LENGTH : GLenum
        val GL_PACK_SKIP_ROWS : GLenum
        val GL_PACK_SKIP_PIXELS : GLenum
        val GL_PACK_ALIGNMENT : GLenum
        val GL_MAP_COLOR : GLenum
        val GL_MAP_STENCIL : GLenum
        val GL_INDEX_SHIFT : GLenum
        val GL_INDEX_OFFSET : GLenum
        val GL_RED_SCALE : GLenum
        val GL_RED_BIAS : GLenum
        val GL_ZOOM_X : GLenum
        val GL_ZOOM_Y : GLenum
        val GL_GREEN_SCALE : GLenum
        val GL_GREEN_BIAS : GLenum
        val GL_BLUE_SCALE : GLenum
        val GL_BLUE_BIAS : GLenum
        val GL_ALPHA_SCALE : GLenum
        val GL_ALPHA_BIAS : GLenum
        val GL_DEPTH_SCALE : GLenum
        val GL_DEPTH_BIAS : GLenum
        val GL_MAX_EVAL_ORDER : GLenum
        val GL_MAX_LIGHTS : GLenum
        val GL_MAX_CLIP_PLANES : GLenum
        val GL_MAX_TEXTURE_SIZE : GLenum
        val GL_MAX_PIXEL_MAP_TABLE : GLenum
        val GL_MAX_ATTRIB_STACK_DEPTH : GLenum
        val GL_MAX_MODELVIEW_STACK_DEPTH : GLenum
        val GL_MAX_NAME_STACK_DEPTH : GLenum
        val GL_MAX_PROJECTION_STACK_DEPTH : GLenum
        val GL_MAX_TEXTURE_STACK_DEPTH : GLenum
        val GL_MAX_VIEWPORT_DIMS : GLenum
        val GL_MAX_CLIENT_ATTRIB_STACK_DEPTH : GLenum
        val GL_SUBPIXEL_BITS : GLenum
        val GL_INDEX_BITS : GLenum
        val GL_RED_BITS : GLenum
        val GL_GREEN_BITS : GLenum
        val GL_BLUE_BITS : GLenum
        val GL_ALPHA_BITS : GLenum
        val GL_DEPTH_BITS : GLenum
        val GL_STENCIL_BITS : GLenum
        val GL_ACCUM_RED_BITS : GLenum
        val GL_ACCUM_GREEN_BITS : GLenum
        val GL_ACCUM_BLUE_BITS : GLenum
        val GL_ACCUM_ALPHA_BITS : GLenum
        val GL_NAME_STACK_DEPTH : GLenum
        val GL_AUTO_NORMAL : GLenum
        val GL_MAP1_COLOR_4 : GLenum
        val GL_MAP1_INDEX : GLenum
        val GL_MAP1_NORMAL : GLenum
        val GL_MAP1_TEXTURE_COORD_1 : GLenum
        val GL_MAP1_TEXTURE_COORD_2 : GLenum
        val GL_MAP1_TEXTURE_COORD_3 : GLenum
        val GL_MAP1_TEXTURE_COORD_4 : GLenum
        val GL_MAP1_VERTEX_3 : GLenum
        val GL_MAP1_VERTEX_4 : GLenum
        val GL_MAP2_COLOR_4 : GLenum
        val GL_MAP2_INDEX : GLenum
        val GL_MAP2_NORMAL : GLenum
        val GL_MAP2_TEXTURE_COORD_1 : GLenum
        val GL_MAP2_TEXTURE_COORD_2 : GLenum
        val GL_MAP2_TEXTURE_COORD_3 : GLenum
        val GL_MAP2_TEXTURE_COORD_4 : GLenum
        val GL_MAP2_VERTEX_3 : GLenum
        val GL_MAP2_VERTEX_4 : GLenum
        val GL_MAP1_GRID_DOMAIN : GLenum
        val GL_MAP1_GRID_SEGMENTS : GLenum
        val GL_MAP2_GRID_DOMAIN : GLenum
        val GL_MAP2_GRID_SEGMENTS : GLenum
        val GL_TEXTURE_1D : GLenum
        val GL_TEXTURE_2D : GLenum
        val GL_FEEDBACK_BUFFER_POINTER : GLenum
        val GL_FEEDBACK_BUFFER_SIZE : GLenum
        val GL_FEEDBACK_BUFFER_TYPE : GLenum
        val GL_SELECTION_BUFFER_POINTER : GLenum
        val GL_SELECTION_BUFFER_SIZE : GLenum

        (* GetTextureParameter *)
        val GL_TEXTURE_WIDTH : GLenum
        val GL_TEXTURE_HEIGHT : GLenum
        val GL_TEXTURE_INTERNAL_FORMAT : GLenum
        val GL_TEXTURE_BORDER_COLOR : GLenum
        val GL_TEXTURE_BORDER : GLenum

        (* HGLenumMode *)
        val GL_DONT_CARE : GLenum
        val GL_FASTEST : GLenum
        val GL_NICEST : GLenum

        (* LightName *)
        val GL_LIGHT0 : GLenum
        val GL_LIGHT1 : GLenum
        val GL_LIGHT2 : GLenum
        val GL_LIGHT3 : GLenum
        val GL_LIGHT4 : GLenum
        val GL_LIGHT5 : GLenum
        val GL_LIGHT6 : GLenum
        val GL_LIGHT7 : GLenum

        (* LightParameter *)
        val GL_AMBIENT : GLenum
        val GL_DIFFUSE : GLenum
        val GL_SPECULAR : GLenum
        val GL_POSITION : GLenum
        val GL_SPOT_DIRECTION : GLenum
        val GL_SPOT_EXPONENT : GLenum
        val GL_SPOT_CUTOFF : GLenum
        val GL_CONSTANT_ATTENUATION : GLenum
        val GL_LINEAR_ATTENUATION : GLenum
        val GL_QUADRATIC_ATTENUATION : GLenum

        (* ListMode *)
        val GL_COMPILE : GLenum
        val GL_COMPILE_AND_EXECUTE : GLenum

        (* LogicOp *)
        val GL_CLEAR : GLenum
        val GL_AND : GLenum
        val GL_AND_REVERSE : GLenum
        val GL_COPY : GLenum
        val GL_AND_INVERTED : GLenum
        val GL_NOOP : GLenum
        val GL_XOR : GLenum
        val GL_OR : GLenum
        val GL_NOR : GLenum
        val GL_EQUIV : GLenum
        val GL_INVERT : GLenum
        val GL_OR_REVERSE : GLenum
        val GL_COPY_INVERTED : GLenum
        val GL_OR_INVERTED : GLenum
        val GL_NAND : GLenum
        val GL_SET : GLenum

        (* MaterialParameter *)
        val GL_EMISSION : GLenum
        val GL_SHININESS : GLenum
        val GL_AMBIENT_AND_DIFFUSE : GLenum
        val GL_COLOR_INDEXES : GLenum

        (* MatrixMode *)
        val GL_MODELVIEW : GLenum
        val GL_PROJECTION : GLenum
        val GL_TEXTURE : GLenum

        (* PixelCopyType *)
        val GL_COLOR : GLenum
        val GL_DEPTH : GLenum
        val GL_STENCIL : GLenum

        (* PixelFormat *)
        val GL_COLOR_INDEX : GLenum
        val GL_STENCIL_INDEX : GLenum
        val GL_DEPTH_COMPONENT : GLenum
        val GL_RED : GLenum
        val GL_GREEN : GLenum
        val GL_BLUE : GLenum
        val GL_ALPHA : GLenum
        val GL_RGB : GLenum
        val GL_RGBA : GLenum
        val GL_LUMINANCE : GLenum
        val GL_LUMINANCE_ALPHA : GLenum

        (* PixelType *)
        val GL_BITMAP : GLenum

        (* PolygonMode *)
        val GL_POINT : GLenum
        val GL_LINE : GLenum
        val GL_FILL : GLenum

        (* RenderingMode *)
        val GL_RENDER : GLenum
        val GL_FEEDBACK : GLenum
        val GL_SELECT : GLenum

        (* ShadingModel *)
        val GL_FLAT : GLenum
        val GL_SMOOTH : GLenum

        (* StencilOp *)
        val GL_KEEP : GLenum
        val GL_REPLACE : GLenum
        val GL_INCR : GLenum
        val GL_DECR : GLenum

        (* StringName *)
        val GL_VENDOR : GLenum
        val GL_RENDERER : GLenum
        val GL_VERSION : GLenum
        val GL_EXTENSIONS : GLenum

        (* TextureCoordName *)
        val GL_S : GLenum
        val GL_T : GLenum
        val GL_R : GLenum
        val GL_Q : GLenum

        (* TextureEnvMode *)
        val GL_MODULATE : GLenum
        val GL_DECAL : GLenum

        (* TextureEnvParameter *)
        val GL_TEXTURE_ENV_MODE : GLenum
        val GL_TEXTURE_ENV_COLOR : GLenum

        (* TextureEnvTarget *)
        val GL_TEXTURE_ENV : GLenum

        (* TextureGenMode *)
        val GL_EYE_LINEAR : GLenum
        val GL_OBJECT_LINEAR : GLenum
        val GL_SPHERE_MAP : GLenum

        (* TextureGenParameter *)
        val GL_TEXTURE_GEN_MODE : GLenum
        val GL_OBJECT_PLANE : GLenum
        val GL_EYE_PLANE : GLenum

        (* TextureMagFilter *)
        val GL_NEAREST : GLenum
        val GL_LINEAR : GLenum

        (* TextureMinFilter *)
        val GL_NEAREST_MIPMAP_NEAREST : GLenum
        val GL_LINEAR_MIPMAP_NEAREST : GLenum
        val GL_NEAREST_MIPMAP_LINEAR : GLenum
        val GL_LINEAR_MIPMAP_LINEAR : GLenum

        (* TextureParameterName *)
        val GL_TEXTURE_MAG_FILTER : GLenum
        val GL_TEXTURE_MIN_FILTER : GLenum
        val GL_TEXTURE_WRAP_S : GLenum
        val GL_TEXTURE_WRAP_T : GLenum

        (* TextureWrapMode *)
        val GL_CLAMP : GLenum
        val GL_REPEAT : GLenum

        (* ClientAttribMask *)
        val GL_CLIENT_PIXEL_STORE_BIT : GLenum
        val GL_CLIENT_VERTEX_ARRAY_BIT : GLenum
        val GL_CLIENT_ALL_ATTRIB_BITS : Word8Vector.vector

        (* polygon_offset *)
        val GL_POLYGON_OFFSET_FACTOR : GLenum
        val GL_POLYGON_OFFSET_UNITS : GLenum
        val GL_POLYGON_OFFSET_POINT : GLenum
        val GL_POLYGON_OFFSET_LINE : GLenum
        val GL_POLYGON_OFFSET_FILL : GLenum

        (* texture *)
        val GL_ALPHA4 : GLenum
        val GL_ALPHA8 : GLenum
        val GL_ALPHA12 : GLenum
        val GL_ALPHA16 : GLenum
        val GL_LUMINANCE4 : GLenum
        val GL_LUMINANCE8 : GLenum
        val GL_LUMINANCE12 : GLenum
        val GL_LUMINANCE16 : GLenum
        val GL_LUMINANCE4_ALPHA4 : GLenum
        val GL_LUMINANCE6_ALPHA2 : GLenum
        val GL_LUMINANCE8_ALPHA8 : GLenum
        val GL_LUMINANCE12_ALPHA4 : GLenum
        val GL_LUMINANCE12_ALPHA12 : GLenum
        val GL_LUMINANCE16_ALPHA16 : GLenum
        val GL_INTENSITY : GLenum
        val GL_INTENSITY4 : GLenum
        val GL_INTENSITY8 : GLenum
        val GL_INTENSITY12 : GLenum
        val GL_INTENSITY16 : GLenum
        val GL_R3_G3_B2 : GLenum
        val GL_RGB4 : GLenum
        val GL_RGB5 : GLenum
        val GL_RGB8 : GLenum
        val GL_RGB10 : GLenum
        val GL_RGB12 : GLenum
        val GL_RGB16 : GLenum
        val GL_RGBA2 : GLenum
        val GL_RGBA4 : GLenum
        val GL_RGB5_A1 : GLenum
        val GL_RGBA8 : GLenum
        val GL_RGB10_A2 : GLenum
        val GL_RGBA12 : GLenum
        val GL_RGBA16 : GLenum
        val GL_TEXTURE_RED_SIZE : GLenum
        val GL_TEXTURE_GREEN_SIZE : GLenum
        val GL_TEXTURE_BLUE_SIZE : GLenum
        val GL_TEXTURE_ALPHA_SIZE : GLenum
        val GL_TEXTURE_LUMINANCE_SIZE : GLenum
        val GL_TEXTURE_INTENSITY_SIZE : GLenum
        val GL_PROXY_TEXTURE_1D : GLenum
        val GL_PROXY_TEXTURE_2D : GLenum

        (* texture_object *)
        val GL_TEXTURE_PRIORITY : GLenum
        val GL_TEXTURE_RESIDENT : GLenum
        val GL_TEXTURE_BINDING_1D : GLenum
        val GL_TEXTURE_BINDING_2D : GLenum

        (* vertex_array *)
        val GL_VERTEX_ARRAY : GLenum
        val GL_NORMAL_ARRAY : GLenum
        val GL_COLOR_ARRAY : GLenum
        val GL_INDEX_ARRAY : GLenum
        val GL_TEXTURE_COORD_ARRAY : GLenum
        val GL_EDGE_FLAG_ARRAY : GLenum
        val GL_VERTEX_ARRAY_SIZE : GLenum
        val GL_VERTEX_ARRAY_TYPE : GLenum
        val GL_VERTEX_ARRAY_STRIDE : GLenum
        val GL_NORMAL_ARRAY_TYPE : GLenum
        val GL_NORMAL_ARRAY_STRIDE : GLenum
        val GL_COLOR_ARRAY_SIZE : GLenum
        val GL_COLOR_ARRAY_TYPE : GLenum
        val GL_COLOR_ARRAY_STRIDE : GLenum
        val GL_INDEX_ARRAY_TYPE : GLenum
        val GL_INDEX_ARRAY_STRIDE : GLenum
        val GL_TEXTURE_COORD_ARRAY_SIZE : GLenum
        val GL_TEXTURE_COORD_ARRAY_TYPE : GLenum
        val GL_TEXTURE_COORD_ARRAY_STRIDE : GLenum
        val GL_EDGE_FLAG_ARRAY_STRIDE : GLenum
        val GL_VERTEX_ARRAY_POINTER : GLenum
        val GL_NORMAL_ARRAY_POINTER : GLenum
        val GL_COLOR_ARRAY_POINTER : GLenum
        val GL_INDEX_ARRAY_POINTER : GLenum
        val GL_TEXTURE_COORD_ARRAY_POINTER : GLenum
        val GL_EDGE_FLAG_ARRAY_POINTER : GLenum
        val GL_V2F : GLenum
        val GL_V3F : GLenum
        val GL_C4UB_V2F : GLenum
        val GL_C4UB_V3F : GLenum
        val GL_C3F_V3F : GLenum
        val GL_N3F_V3F : GLenum
        val GL_C4F_N3F_V3F : GLenum
        val GL_T2F_V3F : GLenum
        val GL_T4F_V4F : GLenum
        val GL_T2F_C4UB_V3F : GLenum
        val GL_T2F_C3F_V3F : GLenum
        val GL_T2F_N3F_V3F : GLenum
        val GL_T2F_C4F_N3F_V3F : GLenum
        val GL_T4F_C4F_N3F_V4F : GLenum

        (* Extensions *)
        val GL_EXT_vertex_array : GLenum
        val GL_WIN_swap_hint : GLenum
        val GL_EXT_bgra : GLenum
        val GL_EXT_paletted_texture : GLenum

        (* EXT_vertex_array *)
        val GL_VERTEX_ARRAY_EXT : GLenum
        val GL_NORMAL_ARRAY_EXT : GLenum
        val GL_COLOR_ARRAY_EXT : GLenum
        val GL_INDEX_ARRAY_EXT : GLenum
        val GL_TEXTURE_COORD_ARRAY_EXT : GLenum
        val GL_EDGE_FLAG_ARRAY_EXT : GLenum
        val GL_VERTEX_ARRAY_SIZE_EXT : GLenum
        val GL_VERTEX_ARRAY_TYPE_EXT : GLenum
        val GL_VERTEX_ARRAY_STRIDE_EXT : GLenum
        val GL_VERTEX_ARRAY_COUNT_EXT : GLenum
        val GL_NORMAL_ARRAY_TYPE_EXT : GLenum
        val GL_NORMAL_ARRAY_STRIDE_EXT : GLenum
        val GL_NORMAL_ARRAY_COUNT_EXT : GLenum
        val GL_COLOR_ARRAY_SIZE_EXT : GLenum
        val GL_COLOR_ARRAY_TYPE_EXT : GLenum
        val GL_COLOR_ARRAY_STRIDE_EXT : GLenum
        val GL_COLOR_ARRAY_COUNT_EXT : GLenum
        val GL_INDEX_ARRAY_TYPE_EXT : GLenum
        val GL_INDEX_ARRAY_STRIDE_EXT : GLenum
        val GL_INDEX_ARRAY_COUNT_EXT : GLenum
        val GL_TEXTURE_COORD_ARRAY_SIZE_EXT : GLenum
        val GL_TEXTURE_COORD_ARRAY_TYPE_EXT : GLenum
        val GL_TEXTURE_COORD_ARRAY_STRIDE_EXT : GLenum
        val GL_TEXTURE_COORD_ARRAY_COUNT_EXT : GLenum
        val GL_EDGE_FLAG_ARRAY_STRIDE_EXT : GLenum
        val GL_EDGE_FLAG_ARRAY_COUNT_EXT : GLenum
        val GL_VERTEX_ARRAY_POINTER_EXT : GLenum
        val GL_NORMAL_ARRAY_POINTER_EXT : GLenum
        val GL_COLOR_ARRAY_POINTER_EXT : GLenum
        val GL_INDEX_ARRAY_POINTER_EXT : GLenum
        val GL_TEXTURE_COORD_ARRAY_POINTER_EXT : GLenum
        val GL_EDGE_FLAG_ARRAY_POINTER_EXT : GLenum
        val GL_DOUBLE_EXT : GLenum

        (* EXT_bgra *)
        val GL_BGR_EXT : GLenum
        val GL_BGRA_EXT : GLenum

        (* EXT_paletted_texture *)
        (* These must match the GL_COLOR_TABLE_*_SGI enumerants *)
        val GL_COLOR_TABLE_FORMAT_EXT : GLenum
        val GL_COLOR_TABLE_WIDTH_EXT : GLenum
        val GL_COLOR_TABLE_RED_SIZE_EXT : GLenum
        val GL_COLOR_TABLE_GREEN_SIZE_EXT : GLenum
        val GL_COLOR_TABLE_BLUE_SIZE_EXT : GLenum
        val GL_COLOR_TABLE_ALPHA_SIZE_EXT : GLenum
        val GL_COLOR_TABLE_LUMINANCE_SIZE_EXT : GLenum
        val GL_COLOR_TABLE_INTENSITY_SIZE_EXT : GLenum

        val GL_COLOR_INDEX1_EXT : GLenum
        val GL_COLOR_INDEX2_EXT : GLenum
        val GL_COLOR_INDEX4_EXT : GLenum
        val GL_COLOR_INDEX8_EXT : GLenum
        val GL_COLOR_INDEX12_EXT : GLenum
        val GL_COLOR_INDEX16_EXT : GLenum

        (* For compatibility with OpenGL v1.0 *)
        val GL_LOGIC_OP : GLenum
        val GL_TEXTURE_COMPONENTS : GLenum
        val c_glBegin : GLenum -> unit
        val glBegin : GLenum -> unit

        val c_glBitmap : int * int * GLreal * GLreal * GLreal * GLreal * Word8Vector.vector -> unit
        val glBitmap : int -> int -> GLreal -> GLreal -> GLreal -> GLreal -> Word8Vector.vector -> unit

        val c_glBlendFunc : GLenum * GLenum -> unit
        val glBlendFunc : GLenum -> GLenum -> unit

        val c_glCallList : int -> unit
        val glCallList : int -> unit

        val c_glClearColor: GLreal * GLreal * GLreal * GLreal -> unit
        val glClearColor: GLreal -> GLreal -> GLreal -> GLreal -> unit

        val c_glClearDepth : GLreal -> unit
        val glClearDepth : GLreal -> unit

        val c_glColor3d : GLdouble * GLdouble * GLdouble -> unit
        val glColor3d : GLdouble -> GLdouble -> GLdouble -> unit

        val c_glColor3f : GLreal * GLreal * GLreal -> unit
        val glColor3f : GLreal -> GLreal -> GLreal -> unit

        val c_glColor3ub : Word8.word * Word8.word * Word8.word -> unit
        val glColor3ub : Word8.word -> Word8.word -> Word8.word -> unit

        val c_glColor4d : GLdouble * GLdouble * GLdouble * GLdouble -> unit
        val glColor4d : GLdouble -> GLdouble -> GLdouble -> GLdouble -> unit

        val c_glColor4f : GLreal * GLreal * GLreal * GLreal -> unit
        val glColor4f : GLreal -> GLreal -> GLreal -> GLreal -> unit

        val c_glColor4ub : Word8.word * Word8.word * Word8.word * Word8.word -> unit
        val glColor4ub : Word8.word -> Word8.word -> Word8.word -> Word8.word -> unit

        val c_glColorMaterial : GLenum * GLenum -> unit
        val glColorMaterial : GLenum -> GLenum -> unit

        val c_glDisable : GLenum -> unit
        val glDisable : GLenum -> unit

        val c_glEnable : GLenum -> unit
        val glEnable : GLenum -> unit

        val c_glEnd : unit -> unit
        val glEnd : unit -> unit

        val c_glFinish : unit -> unit
        val glFinish : unit -> unit

        val c_glEndList : unit -> unit
        val glEndList : unit -> unit

        val c_glRasterPos2i : int * int -> unit
        val glRasterPos2i : int -> int -> unit

        val c_glRasterPos2f : GLreal * GLreal -> unit
        val glRasterPos2f : GLreal -> GLreal -> unit

        val c_glRasterPos2d : GLdouble * GLdouble -> unit
        val glRasterPos2d : GLdouble -> GLdouble -> unit

        val c_glClear: GLenum -> unit
        val glClear: GLenum -> unit

        val c_glFlush: unit -> unit
        val glFlush: unit -> unit

        val c_glFrontFace : GLenum -> unit
        val glFrontFace : GLenum -> unit

        val c_glLightfv : GLenum * GLenum * GLreal array -> unit
        val glLightfv : GLenum -> GLenum -> realrgbacolour -> unit

        val c_glLightModelfv : GLenum * GLreal array -> unit
        val glLightModelfv : GLenum -> realrgbacolour -> unit

        val c_glLineWidth : GLreal -> unit
        val glLineWidth : GLreal -> unit

        val c_glLoadIdentity : unit -> unit
        val glLoadIdentity : unit -> unit

        val c_glMaterialfv : GLenum * GLenum * GLreal array -> unit
        val glMaterialfv : GLenum -> GLenum -> GLreal array -> unit

        val c_glMatrixMode : GLenum -> unit
        val glMatrixMode : GLenum -> unit

        val c_glNewList : int * GLenum -> unit
        val glNewList : int -> GLenum -> unit

        val c_glOrtho : GLdouble * GLdouble * GLdouble * GLdouble * GLdouble * GLdouble -> unit
        val glOrtho : GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> unit

        val c_glPixelTransferi : GLenum * int -> unit
        val glPixelTransferi : GLenum -> int -> unit

        val c_glPushMatrix : unit -> unit
        val glPushMatrix : unit -> unit

        val c_glTranslated : GLdouble * GLdouble * GLdouble -> unit
        val glTranslated : GLdouble -> GLdouble -> GLdouble -> unit

        val c_glTranslatef : GLreal * GLreal * GLreal -> unit
        val glTranslatef : GLreal -> GLreal -> GLreal -> unit

        val c_glPolygonMode : GLenum * GLenum -> unit
        val glPolygonMode : GLenum -> GLenum -> unit

        val c_glPopMatrix : unit -> unit
        val glPopMatrix : unit -> unit

        val c_glPopAttrib : unit -> unit
        val glPopAttrib : unit -> unit

        val c_glPushAttrib : GLenum -> unit
        val glPushAttrib : GLenum -> unit

        val c_glRotatef: GLreal * GLreal * GLreal * GLreal -> unit
        val glRotatef: GLreal -> GLreal -> GLreal -> GLreal -> unit

        val c_glRotated: GLdouble * GLdouble * GLdouble * GLdouble -> unit
        val glRotated: GLdouble -> GLdouble -> GLdouble -> GLdouble -> unit

        val c_glShadeModel : GLenum -> unit
        val glShadeModel : GLenum -> unit

        val c_glVertex2d : GLdouble * GLdouble -> unit
        val glVertex2d : GLdouble -> GLdouble -> unit

        val c_glVertex3d : GLdouble * GLdouble * GLdouble -> unit
        val glVertex3d : GLdouble -> GLdouble -> GLdouble -> unit

        val c_glVertex2f : GLreal * GLreal -> unit
        val glVertex2f : GLreal -> GLreal -> unit

        val c_glVertex3f : GLreal * GLreal * GLreal -> unit
        val glVertex3f : GLreal -> GLreal -> GLreal -> unit

        val c_glViewport : int * int * int * int -> unit
        val glViewport : int -> int -> int -> int -> unit
    end
