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

  {
  TGLBoolean = (
    blFalse = GL_FALSE,
    blTrue
    );
  }

  TGLBeginMode = (
    rmPoints = GL_POINTS,
    rmLines,
    rmLineLoop,
    rmLineStrip,
    rmTriangles,
    rmTriangleStrip,
    rmTriangleFan,
    rmQuads,
    rmQuadStrip,
    rmPolygon,
    rmLinesAdjacency,
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

  /// <remarks>
  /// Don't use:
  /// <para>
  /// <c>dtByte</c>,
  /// <c>dtUByte</c>,
  /// <c>dtShort</c>,
  /// <c>dtUShort</c>,
  /// <c>dt2Bytes</c>,
  /// <c>dt3Bytes</c>,
  /// <c>dt4Bytes</c>
  /// </para>
  /// </remarks>
  TGLDataType = (
    dtByte = GL_BYTE, // don't use
    dtUByte, // don't use
    dtShort, // don't use
    dtUShort, // don't use
    dtInt,
    dtUInt,
    dtFloat,
    dt2Bytes, // don't use
    dt3Bytes, // don't use
    dt4Bytes, // don't use
    dtDouble,

    dtVec2 = GL_FLOAT_VEC2,
    dtVec3,
    dtVec4,
    dtIVec2,
    dtIVec3,
    dtIVec4,
    dtBoolean,
    dtBVec2,
    dtBVec3,
    dtBVec4,
    dtMat2,
    dtMat3,
    dtMat4,
    dtSampler1D,
    dtSampler2D,
    dtSampler3D,
    dtSamplerCube,
    dtSampler1DShadow,
    dtSampler2DShadow,
    dtSampler2DRect,
    dtSampler2DRectShadow,
    dtMat2x3,
    dtMat2x4,
    dtMat3x2,
    dtMat3x4,
    dtMat4x2,
    dtMat4x3,

    dtSampler1DArray = GL_SAMPLER_1D_ARRAY,
    dtSampler2DArray,
    dtSamplerBuffer,
    dtSampler1DArrayShadow,
    dtSampler2DArrayShadow,
    dtSamplerCubeShadow,
    dtUVec2,
    dtUVec3,
    dtUVec4,
    dtISampler1D,
    dtISampler2D,
    dtISampler3D,
    dtISamplerCube,
    dtISampler2DRect,
    dtISampler1DArray,
    dtISampler2DArray,
    dtISamplerBuffer,
    dtUSampler1D,
    dtUSampler2D,
    dtUSampler3D,
    dtUSamplerCube,
    dtUSampler2DRect,
    dtUSampler1DArray,
    dtUSampler2DArray,
    dtUSamplerBuffer,

    dtDMat2 = GL_DOUBLE_MAT2,
    dtDMat3,
    dtDMat4,
    dtDMat2x3,
    dtDMat2x4,
    dtDMat3x2,
    dtDMat3x4,
    dtDMat4x2,
    dtDMat4x3,

    dtDVec2 = GL_DOUBLE_VEC2,
    dtDVec3,
    dtDVec4,

    dtSamplerCubeMapArray = GL_SAMPLER_CUBE_MAP_ARRAY,
    dtSamplerCubeMapArrayShadow,
    dtISamplerCubeMapArray,
    dtUSamplerCubeMapArray,

    dtSampler2DMS = GL_SAMPLER_2D_MULTISAMPLE,
    dtISampler2DMS,
    dtUSampler2DMS,
    dtSampler2DMSArray,
    dtISampler2DMSArray,
    dtUSampler2DMSArray
    );

  TGLBaseDataType = (
    bdtInt = Ord(dtInt),
    bdtUInt = Ord(dtUInt),
    bdtFloat = Ord(dtFloat),
    bdtDouble = Ord(dtDouble)
    );

  TGLLogicOp = (
    loClear = GL_CLEAR, // = 0
    loAND, // = s and d
    loANDReverse, // = s and not d
    loCopy, // = s
    loANDInverted, // = not s and d
    loNoOp, // = d
    loXOR, // = s xor d
    loOR, // = s or d
    loNOR, // = not (s or d)
    loEQU, // = not (s xor d)
    loInvert, // = not d
    loORReverse, // = s or not d
    loCopyInverted, // = not s
    loORInverted, // = not s or d
    loNAND, // = not (s and d)
    loSet // = 1
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

  TGLShaderType = (
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

  TGLDebugMessageSource = (
    dmsAPI = GL_DEBUG_SOURCE_API,
    dmsWindowSystem,
    dmsShaderCompiler,
    dmsThirdParty,
    dmsApplication,
    dmsOther
  );

  TGLDebugMessageType = (
    dmtError = GL_DEBUG_TYPE_ERROR,
    dmtDeprecatedBehaviour,
    dmtUndefinedBehaviour,
    dmtPortability,
    dmtPerformance,
    dmtOther,

    dmtMarker = GL_DEBUG_TYPE_MARKER,
    dmtPushGroup,
    dmtPopGroup
  );

  TGLDebugMessageSeverity = (
    dmsHigh = GL_DEBUG_SEVERITY_HIGH,
    dmsMedium,
    dmsLow,
    dmsNotification = GL_DEBUG_SEVERITY_NOTIFICATION
  );

function GLDataTypeSize(ADataType: TGLDataType): Integer; inline;
function GLDataTypeName(ADataType: TGLDataType): string; inline;
function GLDataTypeIsSampler(ADataType: TGLDataType): Boolean; inline;
function GLBaseDataType(ADataType: TGLDataType): TGLBaseDataType; inline;

function GLDebugMessageSourceName(ASource: TGLDebugMessageSource): string;
function GLDebugMessageTypeName(AType: TGLDebugMessageType): string;
function GLDebugMessageSeverityName(ASeverity: TGLDebugMessageSeverity): string;

procedure GLErrorMessage;

implementation

function GLDataTypeSize(ADataType: TGLDataType): Integer;
begin
  case ADataType of
    dtByte: Exit(1);
    dtUByte: Exit(1);
    dtShort: Exit(2);
    dtUShort: Exit(2);
    dtInt: Exit(4);
    dtUInt: Exit(4);
    dtFloat: Exit(4);
    dt2Bytes: Exit(2);
    dt3Bytes: Exit(3);
    dt4Bytes: Exit(4);
    dtDouble: Exit(8);

    dtVec2: Exit(2 * 4);
    dtVec3: Exit(3 * 4);
    dtVec4: Exit(4 * 4);
    dtIVec2: Exit(2 * 4);
    dtIVec3: Exit(3 * 4);
    dtIVec4: Exit(4 * 4);
    dtBoolean: Exit(4);
    dtBVec2: Exit(2 * 4);
    dtBVec3: Exit(3 * 4);
    dtBVec4: Exit(4 * 4);
    dtMat2: Exit(2 * 2 * 4);
    dtMat3: Exit(3 * 3 * 4);
    dtMat4: Exit(4 * 4 * 4);

    dtMat2x3: Exit(2 * 3 * 4);
    dtMat2x4: Exit(2 * 4 * 4);
    dtMat3x2: Exit(3 * 2 * 4);
    dtMat3x4: Exit(3 * 4 * 4);
    dtMat4x2: Exit(4 * 2 * 4);
    dtMat4x3: Exit(4 * 3 * 4);

    dtUVec2: Exit(2 * 4);
    dtUVec3: Exit(3 * 4);
    dtUVec4: Exit(4 * 4);

    dtDMat2: Exit(2 * 2 * 8);
    dtDMat3: Exit(3 * 3 * 8);
    dtDMat4: Exit(4 * 4 * 8);
    dtDMat2x3: Exit(2 * 3 * 8);
    dtDMat2x4: Exit(2 * 4 * 8);
    dtDMat3x2: Exit(3 * 2 * 8);
    dtDMat3x4: Exit(3 * 4 * 8);
    dtDMat4x2: Exit(4 * 2 * 8);
    dtDMat4x3: Exit(4 * 3 * 8);

    dtDVec2: Exit(2 * 8);
    dtDVec3: Exit(3 * 8);
    dtDVec4: Exit(4 * 8);

  else
    if GLDataTypeIsSampler(ADataType) then
      Exit(4)
    else
      raise Exception.Create('Unknown GLDataType');
  end;
end;

function GLDataTypeName(ADataType: TGLDataType): string;
begin
  case ADataType of
    dtByte: Result := 'byte';
    dtUByte: Result := 'ubyte';
    dtShort: Result := 'short';
    dtUShort: Result := 'ushort';
    dtInt: Result := 'int';
    dtUInt: Result := 'uint';
    dtFloat: Result := 'float';
    dt2Bytes: Result := 'byte[2]';
    dt3Bytes: Result := 'byte[3]';
    dt4Bytes: Result := 'byte[4]';
    dtDouble: Result := 'double';
    dtVec2: Result := 'vec2';
    dtVec3: Result := 'vec3';
    dtVec4: Result := 'vec4';
    dtIVec2: Result := 'ivec2';
    dtIVec3: Result := 'ivec3';
    dtIVec4: Result := 'ivec4';
    dtBoolean: Result := 'bool';
    dtBVec2: Result := 'bvec2';
    dtBVec3: Result := 'bvec3';
    dtBVec4: Result := 'bvec4';
    dtMat2: Result := 'mat2';
    dtMat3: Result := 'mat2';
    dtMat4: Result := 'mat4';
    dtSampler1D: Result := 'sampler1D';
    dtSampler2D: Result := 'sampler2D';
    dtSampler3D: Result := 'sampler3D';
    dtSamplerCube: Result := 'samplerCube';
    dtSampler1DShadow: Result := 'sampler1DShadow';
    dtSampler2DShadow: Result := 'sampler2DShadow';
    dtSampler2DRect: Result := 'sampler2DRect';
    dtSampler2DRectShadow: Result := 'sampler2DRectShadow';
    dtMat2x3: Result := 'mat2x3';
    dtMat2x4: Result := 'mat2x4';
    dtMat3x2: Result := 'mat3x2';
    dtMat3x4: Result := 'mat3x4';
    dtMat4x2: Result := 'mat4x2';
    dtMat4x3: Result := 'mat4x3';
    dtSampler1DArray: Result := 'sampler1DArray';
    dtSampler2DArray: Result := 'sampler2DArray';
    dtSamplerBuffer: Result := 'samplerBuffer';
    dtSampler1DArrayShadow: Result := 'sampler1DArrayShadow';
    dtSampler2DArrayShadow: Result := 'sampler2DArrayShadow';
    dtSamplerCubeShadow: Result := 'samplerCubeShadow';
    dtUVec2: Result := 'uvec2';
    dtUVec3: Result := 'uvec3';
    dtUVec4: Result := 'uvec4';
    dtISampler1D: Result := 'isampler1D';
    dtISampler2D: Result := 'isampler2D';
    dtISampler3D: Result := 'isampler3D';
    dtISamplerCube: Result := 'isamplerCube';
    dtISampler2DRect: Result := 'isampler2DRect';
    dtISampler1DArray: Result := 'isampler1DArray';
    dtISampler2DArray: Result := 'isampler2DArray';
    dtISamplerBuffer: Result := 'isamplerBuffer';
    dtUSampler1D: Result := 'usampler1D';
    dtUSampler2D: Result := 'usampler2D';
    dtUSampler3D: Result := 'usampler3D';
    dtUSamplerCube: Result := 'usamplerCube';
    dtUSampler2DRect: Result := 'usampler2DRect';
    dtUSampler1DArray: Result := 'usampler1DArray';
    dtUSampler2DArray: Result := 'usampler2DArray';
    dtUSamplerBuffer: Result := 'usamplerBuffer';
    dtDMat2: Result := 'dmat2';
    dtDMat3: Result := 'dmat3';
    dtDMat4: Result := 'dmat4';
    dtDMat2x3: Result := 'dmat2x3';
    dtDMat2x4: Result := 'dmat2x4';
    dtDMat3x2: Result := 'dmat3x2';
    dtDMat3x4: Result := 'dmat3x4';
    dtDMat4x2: Result := 'dmat4x2';
    dtDMat4x3: Result := 'dmat4x3';
    dtDVec2: Result := 'dvec2';
    dtDVec3: Result := 'dvec3';
    dtDVec4: Result := 'dvec4';
    dtSamplerCubeMapArray: Result := 'samplerCubeArray';
    dtSamplerCubeMapArrayShadow: Result := 'samplerCubeArrayShadow';
    dtISamplerCubeMapArray: Result := 'isamplerCubeArray';
    dtUSamplerCubeMapArray: Result := 'usamplerCubeArray';
    dtSampler2DMS: Result := 'sampler2DMS';
    dtISampler2DMS: Result := 'isampler2DMS';
    dtUSampler2DMS: Result := 'usampler2DMS';
    dtSampler2DMSArray: Result := 'sampler2DMSArray';
    dtISampler2DMSArray: Result := 'isampler2DMSArray';
    dtUSampler2DMSArray: Result := 'usampler2DMSArray';
  else
    Result := 'unknown';
  end;
end;

function GLDataTypeIsSampler(ADataType: TGLDataType): Boolean;
begin
  case ADataType of
    dtSampler1D .. dtSampler2DRectShadow,
      dtSampler1DArray .. dtSamplerCubeShadow,
      dtISampler1D .. dtUSamplerBuffer,
      dtSampler2DMS .. dtUSampler2DMSArray,
      dtSamplerCubeMapArray .. dtUSamplerCubeMapArray:
      Result := True;
  else
    Result := False;
  end;
end;

function GLBaseDataType(ADataType: TGLDataType): TGLBaseDataType;
begin
  case ADataType of
    dtFloat,
    dtVec2 .. dtVec4,
    dtMat2 .. dtMat4,
    dtMat2x3 .. dtMat4x3:
      Result := bdtFloat;

    dtUInt,
    dtUVec2 .. dtUVec4:
      Result := bdtUInt;

    dtDVec2 .. dtDVec4,
    dtDMat2 .. dtDMat4,
    dtDMat2x3 .. dtDMat4x2:
      Result := bdtDouble;

  else
    Result := bdtInt;
  end;
end;

function GLDebugMessageSourceName(ASource: TGLDebugMessageSource): string;
begin
  case ASource of
    dmsAPI: Result := 'API';
    dmsWindowSystem: Result := 'Windows System';
    dmsShaderCompiler: Result := 'Shader Compiler';
    dmsThirdParty: Result := 'Third Party';
    dmsApplication: Result := 'Application';
    dmsOther: Result := 'Unknown';
  end;
end;

function GLDebugMessageTypeName(AType: TGLDebugMessageType): string;
begin
  case AType of
    dmtError: Result := 'Error';
    dmtDeprecatedBehaviour: Result := 'Deprecated Behaviour';
    dmtUndefinedBehaviour: Result := 'Undefined Behaviour';
    dmtPortability: Result := 'Portability';
    dmtPerformance: Result := 'Performance';
    dmtOther: Result := 'Unknown';
    dmtMarker: Result := 'Marker';
    dmtPushGroup: Result := 'Push Group';
    dmtPopGroup: Result := 'Pop Group';
  end;
end;

function GLDebugMessageSeverityName(ASeverity: TGLDebugMessageSeverity): string;
begin
  case ASeverity of
    dmsHigh: Result := 'High';
    dmsMedium: Result := 'Medium';
    dmsLow: Result := 'Low';
    dmsNotification: Result := 'Notification';
  end;
end;

procedure GLErrorMessage;
var
  ErrorCode: Cardinal;
begin
  ErrorCode := glGetError;
  if TGLErrorCode(ErrorCode) <> ecNoError then
    raise Exception.Create('OpenGL Error: ' + gluErrorString(ErrorCode));
end;

end.
