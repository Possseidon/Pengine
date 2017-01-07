unit GLEnums;

interface

uses
  dglOpenGL, sysutils;

type
  TGLAttribMask = (
    amNone = 0,
    amDepth = GL_DEPTH_BUFFER_BIT,
    amStencil = GL_STENCIL_BUFFER_BIT,
    amColor = GL_COLOR_BUFFER_BIT,
    amDepthStencil = Ord(amDepth) or Ord(amStencil){%H-},
    amColorDepth = Ord(amColor) or Ord(amDepth),
    amColorStencil = Ord(amColor) or Ord(amStencil),
    amColorDepthStencil = Ord(amColor) or Ord(amDepth) or Ord(amStencil)
  );

  TGLBoolean = (
    blFalse = GL_FALSE,
    blTrue
  );

  TGLBeginMode = (
    rmPoints = GL_POINTS,
    rmLines,
    rmLineLoop,
    rmLineStrip,
    rmTriangles,
    rmTriangleStrip,
    rmTriangleFan,
    rmLinesAdjacency = GL_LINES_ADJACENCY,
    rmLineStripAdjacency,
    rmTrianglesAdjacency,
    rmTriangleStripAdjacency,
    rmPatches
    );

  TGLCompareFunction = (
    cfNever = GL_NEVER,
    cfLess,
    cfEqual,
    cfLEqual,
    cfGreater,
    cfNotEqual,
    cfGEqual,
    cfAlways
  );

  TGLBlendingFactorSrc = (
    bfsZero = GL_ZERO,
    bfsOne,
    bfsSrcColor = GL_SRC_COLOR,
    bfsOneMinusSrcColor,
    bfsSrcAlpha,
    bfsOneMinusSrcAlpha,
    bfsDstAlpha,
    bfsOneMinusDstAlpha,
    bfsDstColor,
    bfsOneMinusDstColor,
    bfsSrcAlphaSaturate,
    bfsConstantColor = GL_CONSTANT_COLOR_ARB,
    bfsOneMinusConstantColor,
    bfsConstantAlpha,
    bfsOneMinusConstantAlpha
  );

  TGLBlendingFactorDest = (
    bfdZero = GL_ZERO,
    bfdOne,
    bfdSrcColor = GL_SRC_COLOR,
    bfdOneMinusSrcColor,
    bfdSrcAlpha,
    bfdOneMinusSrcAlpha,
    bfdDstAlpha,
    bfdOneMinusDstAlpha,
    bfdDstColor,
    bfdOneMinusDstColor,
    bfdConstantColor = GL_CONSTANT_COLOR_ARB,
    bfdOneMinusConstantColor,
    bfdConstantAlpha,
    bfdOneMinusConstantAlpha
  );

  TGLBlendingFunc = record
    Src: TGLBlendingFactorSrc;
    Dest: TGLBlendingFactorDest;
  end;

  TGLDrawBufferMode = (
    dbmNone = GL_NONE,
    dbmFrontLeft = GL_FRONT_LEFT,
    dbmFrontRight,
    dbmBackLeft,
    dbmBackRight,
    dbmFront,
    dbmBack,
    dbmLeft,
    dbmRight,
    dbmFrontAndBack
  );

  TGLErrorCode = (
    ecNoError = GL_NO_ERROR,
    ecInvalidEnum = GL_INVALID_ENUM,
    ecInvalidValue,
    ecInvaliedOperation,
    ecOutOfMemory
  );

  TGLFrontFaceDirection = (
    ffdCW = GL_CW,
    ffdCCW
  );

  TGLHintMode = (
    hmDontCare = GL_DONT_CARE,
    hmFastest,
    hmNicest
  );

  TGLDataType = (
    dtByte = GL_BYTE,
    dtUByte,
    dtShort,
    dtUShort,
    dtInt,
    dtUInt,
    dtFloat,
    dtDouble = GL_DOUBLE
  );

  {
  TGLPNames = (
    pnPointSize = GL_POINT_SIZE,
    pnPointSizeRange,
    pnPointSizeGranularity,
    pnLineSmooth = GL_LINE_SMOOTH,
    pnLineWidth,
    pnLineWidthRange,
    pnLineWidthGranularity,
    pnPolygonSmooth = GL_POLYGON_SMOOTH,
    pnCullFace = GL_CULL_FACE,
    pnCullFaceMode,
    pnFrontFace,
    pnDepthRange = GL_DEPTH_RANGE,
    pnDepthTest,
    pnDepthWriteMask,
    pnDepthClearValue,
    pnDepthFunc,
    pnStencilTest = GL_STENCIL_TEST,
    pnStencilClearValue,
    pnStencilFunc,
    pnStencilValueMask,
    pnStencilFail,
    pnpnStencilPassDepthFail,
    pnStencilPassDepthPass,
    pnStencilRef,
    pnStencilWriteMask,
    pnViewport = GL_VIEWPORT,
    pnDither = GL_DITHER,
    pnBlendDst = GL_BLEND_DST,
    pnBlendSrc,
    pnBlend,
    pnLogicOpMode = GL_LOGIC_OP_MODE,
    pnDrawBuffer = GL_DRAW_BUFFER,
    pnReadBuffer,
    pnScissorBox = GL_SCISSOR_BOX,
    pnScissorTest,
    pnColorClearValue = GL_COLOR_CLEAR_VALUE,
    pnColorWritemask,
    pnDoublebuffere = GL_DOUBLEBUFFER,
    pnStereo,
    pnLineSmoothHint = GL_LINE_SMOOTH_HINT,
    pnPolygonSmoothHint,
    pnUnpackSwapBytes = GL_UNPACK_SWAP_BYTES,
    pnUnpackLsbFirst,
    pnUnpackRowLength,
    pnUnpackSkipRows,
    pnUnpackSkipPixels,
    pnUnpackAlignment,
    pnPackSwapBytes = GL_PACK_SWAP_BYTES,
    pnPackLsbFirst,
    pnPackRowLength,
    pnPackSkipRows,
    pnPackSkipPixels,
    pnPackAlignment,
    pnMaxTextureSize = GL_MAX_TEXTURE_SIZE,
    pnMaxViewportDims = GL_MAX_VIEWPORT_DIMS,
    pnSubpixelBits = GL_SUBPIXEL_BITS,
    pnTexture1D = GL_TEXTURE_1D,
    pnTexture2D,
    pnPolygonOffsetUnits = GL_POLYGON_OFFSET_UNITS,
    pnPolygonOffsetPoint,
    pnPolygonOffsetLine,
    pnPolygonOffsetFill = GL_POLYGON_OFFSET_FILL,
    pnPolygonOffsetFactor,
    pnTextureBinding1D = GL_TEXTURE_BINDING_1D,
    pntextureBinding2D,
  );
  }

  TGLLogicOp = (
    loClear = GL_CLEAR,  // = 0
    loAND,               // = s and d
    loANDReverse,        // = s and not d
    loCopy,              // = s
    loANDInverted,       // = not s and d
    loNoOp,              // = d
    loXOR,               // = s xor d
    loOR,                // = s or d
    loNOR,               // = not (s or d)
    loEQU,               // = not (s xor d)
    loInvert,            // = not d
    loORReverse,          // = s or not d
    loCopyInverted,      // = not s
    loORInverted,        // = not s or d
    loNAND,              // = not (s and d)
    loSet                // = 1
  );

  TGLPixelCopyType = (
    pctColor = GL_COLOR,
    pctDepth,
    pctStencil
  );

  TGLPixelFormat = (
    pfStencilIndex = GL_STENCIL_INDEX,
    pfDepthComponent,
    pfRed,
    pfGreen,
    pfBlue,
    pfAlpha,
    pfRGB,
    pfRGBA,
    pfDepthComponent16 = GL_DEPTH_COMPONENT16,
    pfDepthComponent24,
    pfDepthComponent32
  );

  TGLPolygonMode = (
    pmPoint = GL_POINT,
    pmLine,
    pmFill
  );

  TShaderType = (
    stFragment = GL_FRAGMENT_SHADER,
    stVertex,
    stGeometry = GL_GEOMETRY_SHADER,
    stCompute = GL_COMPUTE_SHADER
  );

  TGLStencilOp = (
    soKeep = GL_KEEP,
    soReplace,
    soIncr,
    soDecr
  );

  TGLStringName = (
    snVendor = GL_VENDOR,
    snRenderer,
    snVersion,
    snExtensions
  );

  TGLTextureMagFilter= (
    magNearest = GL_NEAREST,
    magLinear
  );

  TGLTextureMinFilter = (
    minNearest = GL_NEAREST,
    minLinear,
    minNearestMipmapNearest = GL_NEAREST_MIPMAP_NEAREST,
    minLinearMipmapNearest,
    minNearestMipmapLinear,
    minLinearMipmapLinear
  );

  TGLTextureParameterName = (
    tpnTextureMagFilter = GL_TEXTURE_MAG_FILTER,
    tpnTextureMinFilter,
    tpnTextureWrapS,
    tpnTextureWrapT
  );

  TGLPixelInternalFormat = (
    pifR3G3B2 = GL_R3_G3_B2,
    pifRGB4 = GL_RGB4,
    pifRGB5,
    pifRGB8,
    pifRGB10,
    pifRGB12,
    pifRGB16,
    pifRGBA2,
    pifRGBA4,
    pifRGB5_A1,
    pifRGBA8,
    pifRGB10_A2,
    pifRGBA12,
    pifRGBA16
  );

  TGLBufferAccess = (
    baReadOnly = GL_READ_ONLY,
    baWriteOnly,
    baReadWrite
  );

  TGLBufferTarget = (
    btArrayBuffer = GL_ARRAY_BUFFER,
    btElementArrayBuffer
  );

  TGLBufferUsage = (
    buStreamDraw = GL_STREAM_DRAW,
    buStreamRead,
    buStreamCopy,
    buStaticDraw = GL_STATIC_DRAW,
    buStaticRead,
    buStaticCopy,
    buDynamicDraw = GL_DYNAMIC_DRAW,
    buDynamicRead,
    buDynamicCopy
  );

  TGLFBOAttachment = (
    fbaColor = GL_COLOR_ATTACHMENT0,
    fbaDepth = GL_DEPTH_ATTACHMENT,
    fbaStencil = GL_STENCIL_ATTACHMENT
  );

  TGLCubeMapSide = (
    cmsPosX = GL_TEXTURE_CUBE_MAP_POSITIVE_X,
    cmsNegX,
    cmsPosY,
    cmsNegY,
    cmsPosZ,
    cmsNegZ
  );

  TGLTextureCompareMode = (
    tcmNone = GL_NONE,
    tcmCompareRefToTexture = GL_COMPARE_REF_TO_TEXTURE
  );

  TGLCullFace = (
    cfFront = GL_FRONT,
    cfBack,
    cfBoth = GL_FRONT_AND_BACK
  );

function GetDataSize(VarType: TGLDataType): Cardinal; inline;
procedure GLErrorMessage;
function ToByteBool(V: TGLBoolean): ByteBool; inline;

implementation

function GetDataSize(VarType: TGLDataType): Cardinal;
begin
  case VarType of
    dtDouble:
      Exit(8);
    dtFloat, dtInt, dtUInt:
      Exit(4);
    dtShort, dtUShort:
      Exit(2);
    dtByte, dtUByte:
      Exit(1);
  end;
  Exit(0);
end;

procedure GLErrorMessage;
var
  ErrorCode: Cardinal;
begin
  ErrorCode := glGetError;
  if TGLErrorCode(ErrorCode) <> ecNoError then
    raise Exception.Create('OpenGL Error: ' + gluErrorString(ErrorCode));
end;

function ToByteBool(V: TGLBoolean): ByteBool;
begin
  Result := ByteBool(Ord(V));
end;

end.
