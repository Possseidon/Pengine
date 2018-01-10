unit Pengine.Texture;

{$POINTERMATH ON}

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,
  System.Classes,

  Vcl.Graphics,
  Vcl.Imaging.pngimage,

  Winapi.Windows,

  Pengine.Collections,
  Pengine.Bitfield,
  Pengine.GLEnums,
  Pengine.GLProgram,
  Pengine.IntMaths,
  Pengine.GLState,
  Pengine.Color;

type

  /// <summary>Stores data for a single 2D-texture.</summary>
  TTextureData = class
  private
    FSize: TIntVector2;
    FData: PByte;

    function GetPixelCount: Integer;
    function GetDataSize: Integer;
    function GetPixel(APos: TIntVector2): TColorRGBA;
    function GetPixelB(APos: TIntVector2): TColorRGBA.TBytes;
    procedure SetPixel(APos: TIntVector2; const Value: TColorRGBA);
    procedure SetPixelB(APos: TIntVector2; const Value: TColorRGBA.TBytes);

    function GetData: TArray<TColorRGBA.TBytes>;
    procedure SetData(const Value: TArray<TColorRGBA.TBytes>);
    function GetSubData(ABounds: TIntBounds2): TArray<TColorRGBA.TBytes>;
    procedure SetSubData(ABounds: TIntBounds2; const Value: TArray<TColorRGBA.TBytes>);

  public
    /// <summary>Creates an unintialized TTextureData object.</summary>
    constructor Create; overload;
    /// <summary>Creates a TTextureData object and reserves memory for the required amount of pixel-data.</summary>
    constructor Create(ASize: TIntVector2); overload;
    destructor Destroy; override;

    /// <summary>Creates a new texture directly from a file.</summary>
    class function CreateFromFile(AFileName: string): TTextureData;
    /// <summary>Creates a new texture directly from a resource.</summary>
    class function CreateFromResource(AResource: string): TTextureData;

    /// <summary>Loads a texture from a stream.</summary>
    procedure LoadFromStream(AStream: TStream);
    /// <summary>Loads a texture from a file.</summary>
    procedure LoadFromFile(AFileName: string);
    /// <summary>Loads a texture from a resource.</summary>
    procedure LoadFromResource(AResource: string);
    /// <summary>Loads a texture from a TPngImage object.</summary>
    procedure LoadFromPngImage(AImage: TPngImage);

    /// <summary>If present, frees all memory for the pixel-data and sets its pointer to nil.</summary>
    procedure FreeData;

    /// <summary>The width and height of the texture.</summary>
    property Size: TIntVector2 read FSize;
    /// <summary>The width of the texture.</summary>
    property Width: Integer read FSize.X;
    /// <summary>The height of the texture.</summary>
    property Height: Integer read FSize.Y;
    /// <summary>The amount of pixels in the texture. Equal to width*heigt.</summary>
    property PixelCount: Integer read GetPixelCount;
    /// <summary>A pointer to the pixel-data.</summary>
    property DataPointer: PByte read FData;
    /// <summary>The size of the data in bytes. Equal to pixelcount*bpp.</summary>
    property DataSize: Integer read GetDataSize;

    property PixelsB[APos: TIntVector2]: TColorRGBA.TBytes read GetPixelB write SetPixelB;
    property Pixels[APos: TIntVector2]: TColorRGBA read GetPixel write SetPixel; default;

    property Data: TArray<TColorRGBA.TBytes> read GetData write SetData;
    property SubData[ABounds: TIntBounds2]: TArray<TColorRGBA.TBytes> read GetSubData write SetSubData;

  end;

  /// <summary>Raised, if too many texture units are active at once.</summary>
  ETextureTooManyUnits = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if an operation requires the texture to be active.</summary>
  ETextureNotActive = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>An abstract base class for OpenGL texture objects.</summary>
  TTexture = class abstract(TGLObject)
  public type

    TUnit = type Integer;

    TBinding = class(TGLObjectBinding<TTexture>)
    private
      FUsedUnits: TBitfield;
      FLastUnusedUnit: TUnit;
      FActiveUnit: TUnit;

      procedure SetActiveUnit(const Value: TUnit);

    public
      constructor Create; override;
      destructor Destroy; override;

      function Add: TUnit;
      procedure Del(ATextureUnit: TUnit);

      property ActiveUnit: TUnit read FActiveUnit write SetActiveUnit;

    end;

  private
    FUnitID: TUnit;

    // Fields for glTexParam
    FDepthStencilMode: TGLDepthStencilTextureMode;

    FBorderColor: TColorRGBA;

    FBaseLevel: Integer;
    FMaxLevel: Integer;

    FCompareFunc: TGLCompareFunction;
    FCompareMode: TGLTextureCompareMode;

    FMinFilter: TGLTextureMinFilter;
    FMagFilter: TGLTextureMagFilter;

    FMinLOD: Single;
    FMaxLOD: Single;
    FLODBias: Single;

    FSwizzleR: TGLTextureSwizzle;
    FSwizzleG: TGLTextureSwizzle;
    FSwizzleB: TGLTextureSwizzle;
    FSwizzleA: TGLTextureSwizzle;

    FWrapS: TGLTextureWrap;
    FWrapT: TGLTextureWrap;
    FWrapR: TGLTextureWrap;

    // Fields for glTexImage        
    FUpdateCount: Integer;
    FUpdateChanged: Boolean;

    FPixelFormat: TGLPixelFormat;
    
    procedure SetDepthStencilMode(const Value: TGLDepthStencilTextureMode);

    procedure SetBorderColor(const Value: TColorRGBA);

    procedure SetBaseLevel(const Value: Integer);
    procedure SetMaxLevel(const Value: Integer);

    procedure SetCompareFunc(const Value: TGLCompareFunction);
    procedure SetCompareMode(const Value: TGLTextureCompareMode);

    procedure SetMinFilter(const Value: TGLTextureMinFilter);
    procedure SetMagFilter(const Value: TGLTextureMagFilter);

    procedure SetMinLOD(const Value: Single);
    procedure SetMaxLOD(const Value: Single);
    procedure SetLODBias(const Value: Single);

    procedure SetSwizzleR(const Value: TGLTextureSwizzle);
    procedure SetSwizzleG(const Value: TGLTextureSwizzle);
    procedure SetSwizzleB(const Value: TGLTextureSwizzle);
    procedure SetSwizzleA(const Value: TGLTextureSwizzle);

    procedure SetWrapS(const Value: TGLTextureWrap);
    procedure SetWrapT(const Value: TGLTextureWrap);
    procedure SetWrapR(const Value: TGLTextureWrap);

    procedure SetPixelFormat(const Value: TGLPixelFormat);
    
    procedure InitTexParams;
    
  protected
    procedure GenObject(out AGLName: GLuint); override;
    procedure DeleteObject(const AGLName: GLuint); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

    function Binding: TBinding;

    procedure Changed; inline;

    constructor Create(AGLState: TGLState; APixelFormat: TGLPixelFormat = pfRGBA);

  public
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    function TargetType: Cardinal; virtual; abstract;

    /// <returns>Wether the texture is currently assigned to a texture unit.</returns>
    function Active: Boolean;

    /// <summary>Assigns a texutre unit if necessary and sets it as active.</summary>
    procedure Activate;
    /// <summary>If active, resets the texture unit.</summary>
    procedure Deactivate;

    /// <summary>Sends the texture unit to the uniform sampler.</summary>
    procedure Uniform(AUniform: TGLProgram.TUniformSampler);

    /// <summary>The texture unit id of the texture. -1 if not active.</summary>
    property UnitID: TUnit read FUnitID;

    property DepthStencilMode: TGLDepthStencilTextureMode read FDepthStencilMode write SetDepthStencilMode;
    property BorderColor: TColorRGBA read FBorderColor write SetBorderColor;

    property BaseLevel: Integer read FBaseLevel write SetBaseLevel;
    property MaxLevel: Integer read FMaxLevel write SetMaxLevel;

    property CompareFunc: TGLCompareFunction read FCompareFunc write SetCompareFunc;
    property CompareMode: TGLTextureCompareMode read FCompareMode write SetCompareMode;

    property MinFilter: TGLTextureMinFilter read FMinFilter write SetMinFilter;
    property MagFilter: TGLTextureMagFilter read FMagFilter write SetMagFilter;

    property MinLOD: Single read FMinLOD write SetMinLOD;
    property MaxLOD: Single read FMaxLOD write SetMaxLOD;
    property LODBias: Single read FLODBias write SetLODBias;

    property SwizzleR: TGLTextureSwizzle read FSwizzleR write SetSwizzleR;
    property SwizzleG: TGLTextureSwizzle read FSwizzleG write SetSwizzleG;
    property SwizzleB: TGLTextureSwizzle read FSwizzleB write SetSwizzleB;
    property SwizzleA: TGLTextureSwizzle read FSwizzleA write SetSwizzleA;

    property WrapS: TGLTextureWrap read FWrapS write SetWrapS;
    property WrapT: TGLTextureWrap read FWrapT write SetWrapT;
    property WrapR: TGLTextureWrap read FWrapR write SetWrapR;

    procedure BeginUpdate;
    procedure EndUpdate;

    property PixelFormat: TGLPixelFormat read FPixelFormat write SetPixelFormat;

    procedure Generate; virtual; abstract;

  end;

  TTexture1D = class(TTexture)
  private
    FWidth: Integer;

    procedure SetWidth(const Value: Integer);

  public
    constructor Create(AGLState: TGLState; APixelFormat: TGLPixelFormat = pfRGBA); overload;
    constructor Create(AGLState: TGLState; AWidth: Integer; APixelFormat: TGLPixelFormat = pfRGBA); overload;

    function TargetType: Cardinal; override;

    property Width: Integer read FWidth write SetWidth;

    procedure Generate; override;

  end;

  TTexture2D = class(TTexture)
  private
    FSize: TIntVector2;

    procedure SetSize(const Value: TIntVector2);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);

  public
    constructor Create(AGLState: TGLState; APixelFormat: TGLPixelFormat = pfRGBA); overload;
    constructor Create(AGLState: TGLState; ASize: TIntVector2; APixelFormat: TGLPixelFormat = pfRGBA); overload;

    function TargetType: Cardinal; override;

    property Size: TIntVector2 read FSize write SetSize;
    property Width: Integer read FSize.X write SetWidth;
    property Height: Integer read FSize.Y write SetHeight;

    procedure Generate; override;

    procedure Data(AData: PByte);
    procedure SubData(ABounds: TIntBounds2; AData: PByte);

    procedure Fill(AColor: TColorRGBA); overload;
    procedure Fill(ABounds: TIntBounds2; AColor: TColorRGBA); overload;

    procedure LoadTexture(ATexture: TTextureData);

  end;

  TTexture3D = class(TTexture)
  private
    FSize: TIntVector3;

    procedure SetSize(const Value: TIntVector3);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetDepth(const Value: Integer);

  public
    constructor Create(AGLState: TGLState; APixelFormat: TGLPixelFormat = pfRGBA); overload;
    constructor Create(AGLState: TGLState; ASize: TIntVector3; APixelFormat: TGLPixelFormat = pfRGBA); overload;

    function TargetType: Cardinal; override;

    property Size: TIntVector3 read FSize write SetSize;
    property Width: Integer read FSize.X write SetWidth;
    property Height: Integer read FSize.Y write SetHeight;
    property Depth: Integer read FSize.Z write SetDepth;

    procedure Generate; override;

  end;

  TTexture1DArray = class(TTexture)
  public
    function TargetType: Cardinal; override;
  end;

  TTexture2DArray = class(TTexture)
  private
    FSize: TIntVector3;

    procedure SetSize(const Value: TIntVector3);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetLayers(const Value: Integer);

  public
    constructor Create(AGLState: TGLState; APixelFormat: TGLPixelFormat = pfRGBA); overload;
    constructor Create(AGLState: TGLState; ASize: TIntVector3; APixelFormat: TGLPixelFormat = pfRGBA); overload;

    function TargetType: Cardinal; override;

    property Size: TIntVector3 read FSize write SetSize;
    property Width: Integer read FSize.X write SetWidth;
    property Height: Integer read FSize.Y write SetHeight;
    property Layers: Integer read FSize.Z write SetLayers;

    procedure Generate; override;

  end;

  TTextureRectangle = class(TTexture)
  public
    // TODO
    function TargetType: Cardinal; override;
  end;

  TTextureCubeMap = class(TTexture)
  public
    // TODO
    function TargetType: Cardinal; override;
  end;

  TTextureCubeMapArray = class(TTexture)
  private
    FSize: Integer;
    FLayers: Integer;

    procedure SetSize(const Value: Integer);
    procedure SetLayer(const Value: Integer);

  public
    constructor Create(AGLState: TGLState; APixelFormat: TGLPixelFormat = pfRGBA); overload;
    constructor Create(AGLState: TGLState; ASize: Integer; ALayers: Integer; APixelFormat: TGLPixelFormat = pfRGBA); overload;

    function TargetType: Cardinal; override;

    property Size: Integer read FSize write SetSize;
    property Layers: Integer read FLayers write SetLayer;

    procedure Generate; override;

  end;

  TTextureBuffer = class(TTexture)
  public
    // TODO
    function TargetType: Cardinal; override;
  end;

  TTexture2DMS = class(TTexture2D)
  private
    FSamples: Integer;
    
    procedure SetSamples(const Value: Integer);

  public
    constructor Create(AGLState: TGLState; ASize: TIntVector2; ASamples: Integer; APixelFormat: TGLPixelFormat = pfRGBA);
  
    function TargetType: Cardinal; override;

    procedure Generate; override;

    property Samples: Integer read FSamples write SetSamples;
    
  end;

  TTexture2DMSArray = class(TTexture2DArray)
  private
    FSamples: Integer;

    procedure SetSamples(const Value: Integer);

  public
    constructor Create(AGLState: TGLState; ASize: TIntVector3; ASamples: Integer; APixelFormat: TGLPixelFormat = pfRGBA);

    function TargetType: Cardinal; override;

    procedure Generate; override;

    property Samples: Integer read FSamples write SetSamples;

  end;

implementation

{ TTextureData }

function TTextureData.GetData: TArray<TColorRGBA.TBytes>;
var
  P: TIntVector2;
  Y: Integer;
begin
  Result := TArray<TColorRGBA.TBytes>.Create;
  Result.Capacity := PixelCount;
  Result.ForceCount(PixelCount);
  for Y := 0 to Size.Y - 1 do
    Move(
    (TColorRGBA.PBytes(FData) + Y * Size.X)^,
    (TColorRGBA.PBytes(Result.DataPointer) + Y * Size.X)^,
    Size.X * SizeOf(TColorRGBA.TBytes)
    );
end;

function TTextureData.GetDataSize: Integer;
begin
  Result := PixelCount * 4;
end;

function TTextureData.GetPixel(APos: TIntVector2): TColorRGBA;
begin
  Result := PixelsB[APos].Convert;
end;

function TTextureData.GetPixelB(APos: TIntVector2): TColorRGBA.TBytes;
begin
  Result := (TColorRGBA.PBytes(FData) + APos.X mod Size.X + APos.Y * Size.X)^;
end;

function TTextureData.GetPixelCount: Integer;
begin
  Result := Width * Height;
end;

function TTextureData.GetSubData(ABounds: TIntBounds2): TArray<TColorRGBA.TBytes>;
var
  Y: Integer;
begin
  Result := TArray<TColorRGBA.TBytes>.Create;
  Result.Capacity := ABounds.Width * ABounds.Height;
  Result.ForceCount(Result.Capacity);
  for Y := 0 to ABounds.Height - 1 do
    Move(
    (TColorRGBA.PBytes(FData) + ABounds.C1.X + (ABounds.C1.Y + Y) * Size.X)^,
    (TColorRGBA.PBytes(Result.DataPointer) + Y * ABounds.Width)^,
    ABounds.Width * SizeOf(TColorRGBA.TBytes)
    );
end;

constructor TTextureData.Create;
begin
  // nothing
end;

constructor TTextureData.Create(ASize: TIntVector2);
begin
  FSize := ASize;
  GetMem(FData, DataSize);
end;

class function TTextureData.CreateFromFile(AFileName: string): TTextureData;
begin
  Result := TTextureData.Create;
  Result.LoadFromFile(AFileName);
end;

class function TTextureData.CreateFromResource(AResource: string): TTextureData;
begin
  Result := TTextureData.Create;
  Result.LoadFromResource(AResource);
end;

destructor TTextureData.Destroy;
begin
  FreeData;
  inherited;
end;

procedure TTextureData.LoadFromStream(AStream: TStream);
var
  Image: TPngImage;
begin
  Image := TPngImage.Create;
  try
    Image.LoadFromStream(AStream);
    LoadFromPngImage(Image);
  finally
    Image.Free;
  end;
end;

procedure TTextureData.SetData(const Value: TArray<TColorRGBA.TBytes>);
var
  Y: Integer;
begin
  for Y := 0 to Size.Y - 1 do
    Move(
    (TColorRGBA.PBytes(Value.DataPointer) + Y * Size.X)^,
    (TColorRGBA.PBytes(FData) + Y * Size.X)^,
    Size.X * SizeOf(TColorRGBA.TBytes)
    );
end;

procedure TTextureData.SetPixel(APos: TIntVector2; const Value: TColorRGBA);
begin
  PixelsB[APos] := Value.ToBytes;
end;

procedure TTextureData.SetPixelB(APos: TIntVector2; const Value: TColorRGBA.TBytes);
begin
  (TColorRGBA.PBytes(FData) + APos.X mod Size.X + APos.Y * Size.X)^ := Value;
end;

procedure TTextureData.SetSubData(ABounds: TIntBounds2; const Value: TArray<TColorRGBA.TBytes>);
var
  Y: Integer;
begin
  for Y := 0 to ABounds.Height - 1 do
    Move(
    (TColorRGBA.PBytes(Value.DataPointer) + Y * ABounds.Width)^,
    (TColorRGBA.PBytes(FData) + Y * Size.X)^,
    ABounds.Width * SizeOf(TColorRGBA.TBytes)
    );
end;

procedure TTextureData.LoadFromFile(AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTextureData.LoadFromResource(AResource: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, AResource, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTextureData.LoadFromPngImage(AImage: TPngImage);
var
  X, Y, Color: Integer;
  Res: PByte;
begin
  FreeData;

  FSize := IVec2(AImage.Width, AImage.Height);
  
  GetMem(FData, DataSize);
  Res := FData;

  for Y := AImage.Height - 1 downto 0 do
  begin
    for X := 0 to Width - 1 do
    begin
      // Color := ByteSwap(AImage.Pixels[X, Y]) shr 8;
      Color := AImage.Pixels[X, Y];
      Move(Color, Res^, 3);
      Inc(Res, 3);
      case AImage.TransparencyMode of
        ptmNone:
          Res^ := $FF;
        ptmBit:
          if Color = AImage.TransparentColor then
            Res^ := 0
          else
            Res^ := $FF;
        ptmPartial:
          Res^ := AImage.AlphaScanline[Y]^[X];
      end;
      Inc(Res, 1);
    end;
  end;
end;

procedure TTextureData.FreeData;
begin
  if FData = nil then
    Exit;
  FreeMem(FData);
  FData := nil;
end;

{ ETextureTooManyUnits }

constructor ETextureTooManyUnits.Create;
begin
  inherited Create('There are too many texture units active at once.');
end;

{ ETextureNotActive }

constructor ETextureNotActive.Create;
begin
  inherited Create('The operation requires the texture to be active.');
end;

{ TTexture.TBinding }

procedure TTexture.TBinding.SetActiveUnit(const Value: TUnit);
begin
  if FActiveUnit = Value then
    Exit;
  FActiveUnit := Value;
  glActiveTexture(GL_TEXTURE0 + FActiveUnit);
end;

constructor TTexture.TBinding.Create;
var
  Units: Integer;
begin
  glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, @Units);
  FUsedUnits := TBitfield.Create(Units);
end;

destructor TTexture.TBinding.Destroy;
begin
  FUsedUnits.Free;
  inherited;
end;

function TTexture.TBinding.Add: TUnit;
begin
  if FLastUnusedUnit = FUsedUnits.Size then
    raise ETextureTooManyUnits.Create;
  Result := FLastUnusedUnit;
  repeat
    Inc(FLastUnusedUnit);
  until not FUsedUnits[FLastUnusedUnit];
end;

procedure TTexture.TBinding.Del(ATextureUnit: TUnit);
begin
  FUsedUnits[ATextureUnit] := False;
  FLastUnusedUnit := Min(ATextureUnit, FLastUnusedUnit);
end;

{ TTexture }

procedure TTexture.SetDepthStencilMode(const Value: TGLDepthStencilTextureMode);
begin
  if DepthStencilMode = Value then
    Exit;
  FDepthStencilMode := Value;
  Bind;
  glTexParameteri(TargetType, GL_DEPTH_STENCIL_TEXTURE_MODE, Ord(Value));
end;

procedure TTexture.SetBaseLevel(const Value: Integer);
begin
  if BaseLevel = Value then
    Exit;
  FBaseLevel := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_BASE_LEVEL, Value);
end;

procedure TTexture.SetMaxLevel(const Value: Integer);
begin
  if MaxLevel = Value then
    Exit;
  FMaxLevel := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_MAX_LEVEL, Value);
end;

procedure TTexture.SetBorderColor(const Value: TColorRGBA);
begin
  if BorderColor = Value then
    Exit;
  FBorderColor := Value;
  Bind;
  glTexParameterfv(TargetType, GL_TEXTURE_BORDER_COLOR, @Value);
end;

procedure TTexture.SetCompareFunc(const Value: TGLCompareFunction);
begin
  if CompareFunc = Value then
    Exit;
  FCompareFunc := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_COMPARE_FUNC, Ord(Value));
end;

procedure TTexture.SetCompareMode(const Value: TGLTextureCompareMode);
begin
  if CompareMode = Value then
    Exit;
  FCompareMode := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_COMPARE_MODE, Ord(Value));
end;

procedure TTexture.SetMinFilter(const Value: TGLTextureMinFilter);
begin
  if MinFilter = Value then
    Exit;
  FMinFilter := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_MIN_FILTER, Ord(Value));
end;

procedure TTexture.SetMagFilter(const Value: TGLTextureMagFilter);
begin
  if MagFilter = Value then
    Exit;
  FMagFilter := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_MAG_FILTER, Ord(Value));
end;

procedure TTexture.SetMinLOD(const Value: Single);
begin
  if MinLOD = Value then
    Exit;
  FMinLOD := Value;
  Bind;
  glTexParameterf(TargetType, GL_TEXTURE_MIN_LOD, Value);
end;

procedure TTexture.SetPixelFormat(const Value: TGLPixelFormat);
begin
  if PixelFormat = Value then
    Exit;
  FPixelFormat := Value;
  Changed;
end;

procedure TTexture.SetMaxLOD(const Value: Single);
begin
  if MaxLOD = Value then
    Exit;
  FMaxLOD := Value;
  Bind;
  glTexParameterf(TargetType, GL_TEXTURE_MAX_LOD, Value);
end;

procedure TTexture.SetLODBias(const Value: Single);
begin
  if LODBias = Value then
    Exit;
  FLODBias := Value;
  Bind;
  glTexParameterf(TargetType, GL_TEXTURE_LOD_BIAS, Value);
end;

procedure TTexture.SetSwizzleR(const Value: TGLTextureSwizzle);
begin
  if SwizzleR = Value then
    Exit;
  FSwizzleR := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_SWIZZLE_R, Ord(Value));
end;

procedure TTexture.SetSwizzleG(const Value: TGLTextureSwizzle);
begin
  if SwizzleG = Value then
    Exit;
  FSwizzleG := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_SWIZZLE_G, Ord(Value));
end;

procedure TTexture.SetSwizzleB(const Value: TGLTextureSwizzle);
begin
  if SwizzleB = Value then
    Exit;
  FSwizzleB := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_SWIZZLE_B, Ord(Value));
end;

procedure TTexture.SetSwizzleA(const Value: TGLTextureSwizzle);
begin
  if SwizzleA = Value then
    Exit;
  FSwizzleA := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_SWIZZLE_A, Ord(Value));
end;

procedure TTexture.SetWrapS(const Value: TGLTextureWrap);
begin
  if WrapS = Value then
    Exit;
  FWrapS := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_WRAP_S, Ord(Value));
end;

procedure TTexture.SetWrapT(const Value: TGLTextureWrap);
begin
  if WrapT = Value then
    Exit;
  FWrapT := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_WRAP_T, Ord(Value));
end;

procedure TTexture.SetWrapR(const Value: TGLTextureWrap);
begin
  if WrapR = Value then
    Exit;
  FWrapR := Value;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_WRAP_R, Ord(Value));
end;

procedure TTexture.GenObject(out AGLName: GLuint);
begin
  glGenTextures(1, @AGLName);
end;

procedure TTexture.DeleteObject(const AGLName: GLuint);
begin
  glDeleteTextures(1, @AGLName);
end;

procedure TTexture.BeginUpdate;
begin
  if FUpdateCount = 0 then
    FUpdateChanged := False;
  Inc(FUpdateCount);
end;

procedure TTexture.BindGLObject;
begin
  Activate;
  glBindTexture(TargetType, GLName);
end;

procedure TTexture.UnbindGLObject;
begin
  glBindTexture(TargetType, 0);
end;

function TTexture.Binding: TBinding;
begin
  Result := TBinding(inherited Binding);
end;

procedure TTexture.Changed;
begin
  if FUpdateCount > 0 then
    FUpdateChanged := True
  else
    Generate;
end;

constructor TTexture.Create(AGLState: TGLState; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState);
  FUnitID := -1;
  InitTexParams;
  FPixelFormat := APixelFormat;
end;

destructor TTexture.Destroy;
begin
  Deactivate;
  inherited Destroy;
end;

procedure TTexture.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FUpdateChanged then
    Generate;
end;

class function TTexture.GetObjectType: TGLObjectType;
begin
  Result := otTexture;
end;

procedure TTexture.InitTexParams;
begin      
  FDepthStencilMode := tmDepthComponent;
  FBorderColor := 0;

  FBaseLevel := 0;
  FMaxLevel := 1000;

  FCompareFunc := cfLEqual;
  FCompareMode := tcmNone;

  FMinFilter := minNearestMipmapLinear;
  MinFilter := minLinear;
  FMagFilter := magLinear;

  FMinLOD := -1000;
  FMaxLOD := 1000;
  FLODBias := 0;

  FSwizzleR := tsRed;
  FSwizzleG := tsGreen;
  FSwizzleB := tsBlue;
  FSwizzleA := tsAlpha;

  FWrapS := twRepeat;
  FWrapT := twRepeat;
  FWrapR := twRepeat;   
end;

class function TTexture.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TBinding;
end;

function TTexture.Active: Boolean;
begin
  Result := FUnitID <> -1;
end;

procedure TTexture.Activate;
begin
  if not Active then
    FUnitID := Binding.Add;
  Binding.ActiveUnit := FUnitID;
end;

procedure TTexture.Deactivate;
begin
  if not Active then
    Exit;
  Binding.Del(FUnitID);
  FUnitID := -1;
end;

procedure TTexture.Uniform(AUniform: TGLProgram.TUniformSampler);
begin
  Activate;
  AUniform.Value := FUnitID;
end;

{ TTexture1D }

procedure TTexture1D.SetWidth(const Value: Integer);
begin
  if Width = Value then
    Exit;
  FWidth := Value;
  Changed;
end;

constructor TTexture1D.Create(AGLState: TGLState; APixelFormat: TGLPixelFormat);
begin
  inherited;
end;

constructor TTexture1D.Create(AGLState: TGLState; AWidth: Integer; APixelFormat: TGLPixelFormat = pfRGBA);
begin
  inherited Create(AGLState, APixelFormat);
  FWidth := AWidth;
  Generate;
end;

function TTexture1D.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_1D;
end;

procedure TTexture1D.Generate;
begin
  Bind;
  glTexImage1D(TargetType, 0, Ord(PixelFormat), Width, 0, Ord(PixelFormat), GL_UNSIGNED_BYTE, nil);
end;

{ TTexture2D }

procedure TTexture2D.SetSize(const Value: TIntVector2);
begin
  if Size = Value then
    Exit;
  FSize := Value;
  Changed;
end;

procedure TTexture2D.SetWidth(const Value: Integer);
begin
  if Width = Value then
    Exit;
  FSize.X := Value;
  Changed;
end;

procedure TTexture2D.SetHeight(const Value: Integer);
begin
  if Height = Value then
    Exit;
  FSize.Y := Value;
  Changed;
end;

constructor TTexture2D.Create(AGLState: TGLState; APixelFormat: TGLPixelFormat);
begin
  inherited;
end;

constructor TTexture2D.Create(AGLState: TGLState; ASize: TIntVector2; APixelFormat: TGLPixelFormat = pfRGBA);
begin
  inherited Create(AGLState, APixelFormat);
  FSize := ASize;
  Generate;
end;

function TTexture2D.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D;
end;

procedure TTexture2D.Generate;
begin
  Bind;
  glTexImage2D(TargetType, 0, Ord(PixelFormat), Width, Height, 0, Ord(PixelFormat), GL_UNSIGNED_BYTE, nil);
end;

procedure TTexture2D.Data(AData: PByte);
begin
  Bind;
  glTexImage2D(TargetType, 0, Ord(PixelFormat), Width, Height, 0, Ord(PixelFormat), GL_UNSIGNED_BYTE, AData);
end;

procedure TTexture2D.SubData(ABounds: TIntBounds2; AData: PByte);
begin
  Bind;
  glTexSubImage2D(TargetType, 0, ABounds.C1.X, ABounds.C1.Y, ABounds.Width, ABounds.Height, Ord(PixelFormat), GL_UNSIGNED_BYTE, AData);
end;

procedure TTexture2D.Fill(AColor: TColorRGBA);
var
  PixelData: array of TColorRGBA.TBytes;
  I: Integer;
begin
  if Size = 0 then
    Exit;
  SetLength(PixelData, Width * Height);
  PixelData[0] := AColor.ToBytes;
  for I := 1 to Length(PixelData) - 1 do
    PixelData[I] := PixelData[0];
  Data(@PixelData[0]);
end;

procedure TTexture2D.Fill(ABounds: TIntBounds2; AColor: TColorRGBA);
var
  PixelData: array of TColorRGBA.TBytes;
  I: Integer;
begin
  SetLength(PixelData, ABounds.Width * ABounds.Height);
  PixelData[0] := AColor.ToBytes;
  for I := 1 to Length(PixelData) - 1 do
    PixelData[I] := PixelData[0];
  SubData(ABounds, @PixelData[0]);
end;

procedure TTexture2D.LoadTexture(ATexture: TTextureData);
begin
  FSize := ATexture.Size;
  Data(ATexture.DataPointer);
end;

{ TTexture3D }

procedure TTexture3D.SetSize(const Value: TIntVector3);
begin
  if Size = Value then
    Exit;
  FSize := Value;
  Changed;
end;

procedure TTexture3D.SetWidth(const Value: Integer);
begin
  if Width = Value then
    Exit;
  FSize.X := Value;
  Changed;
end;

procedure TTexture3D.SetHeight(const Value: Integer);
begin
  if Height = Value then
    Exit;
  FSize.Y := Value;
  Changed;
end;

procedure TTexture3D.SetDepth(const Value: Integer);
begin
  if Depth = Value then
    Exit;
  FSize.Z := Value;
  Changed;
end;

constructor TTexture3D.Create(AGLState: TGLState; APixelFormat: TGLPixelFormat = pfRGBA);
begin
  inherited;
end;

constructor TTexture3D.Create(AGLState: TGLState; ASize: TIntVector3; APixelFormat: TGLPixelFormat = pfRGBA);
begin
  inherited Create(AGLState, APixelFormat);
  FSize := ASize;
  Generate;
end;

function TTexture3D.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_3D;
end;

procedure TTexture3D.Generate;
begin
  Bind;
  glTexImage3D(TargetType, 0, Ord(PixelFormat), Width, Height, Depth, 0, Ord(PixelFormat), GL_UNSIGNED_BYTE, nil);
end;

{ TTexture1DArray }

function TTexture1DArray.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_1D_ARRAY;
end;

{ TTexture2DArray }

procedure TTexture2DArray.SetSize(const Value: TIntVector3);
begin
  if Size = Value then
    Exit;
  FSize := Value;
  Changed;
end;

procedure TTexture2DArray.SetWidth(const Value: Integer);
begin
  if Width = Value then
    Exit;
  FSize.X := Value;
  Changed;
end;

procedure TTexture2DArray.SetHeight(const Value: Integer);
begin
  if Height = Value then
    Exit;
  FSize.Y := Value;
  Changed;
end;

procedure TTexture2DArray.SetLayers(const Value: Integer);
begin
  if Layers = Value then
    Exit;
  FSize.Z := Value;
  Changed;
end;

constructor TTexture2DArray.Create(AGLState: TGLState; APixelFormat: TGLPixelFormat);
begin
  inherited;
end;

constructor TTexture2DArray.Create(AGLState: TGLState; ASize: TIntVector3; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState, APixelFormat);
  FSize := ASize;
  Generate;
end;

function TTexture2DArray.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D_ARRAY;
end;

procedure TTexture2DArray.Generate;
begin
  Bind;
  glTexImage3D(TargetType, 0, Ord(PixelFormat), Width, Height, Layers, 0, Ord(PixelFormat), GL_UNSIGNED_BYTE, nil);
end;

{ TTextureRectangle }

function TTextureRectangle.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_RECTANGLE;
end;

{ TTextureCubeMap }

function TTextureCubeMap.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_CUBE_MAP;
end;

{ TTextureCubeMapArray }

procedure TTextureCubeMapArray.SetSize(const Value: Integer);
begin
  if Size = Value then
    Exit;
  FSize := Value;
  Changed;
end;

procedure TTextureCubeMapArray.SetLayer(const Value: Integer);
begin
  if Layers = Value then
    Exit;
  FLayers := Value;
  Changed;
end;

constructor TTextureCubeMapArray.Create(AGLState: TGLState; APixelFormat: TGLPixelFormat);
begin
  inherited;
end;

constructor TTextureCubeMapArray.Create(AGLState: TGLState; ASize, ALayers: Integer; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState, APixelFormat);
  FSize := ASize;
  FLayers := ALayers;
  Generate;
end;

function TTextureCubeMapArray.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_CUBE_MAP_ARRAY;
end;

procedure TTextureCubeMapArray.Generate;
begin
  glTexImage3D(TargetType, 0, Ord(PixelFormat), Size, Size, Layers * 6, 0, Ord(PixelFormat), GL_UNSIGNED_BYTE, nil);
end;

{ TTextureBuffer }

function TTextureBuffer.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_BUFFER;
end;

{ TTexture2DMS }

procedure TTexture2DMS.SetSamples(const Value: Integer);
begin
  if Samples = Value then
    Exit;
  FSamples := Value;
  Changed;
end;

function TTexture2DMS.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D_MULTISAMPLE;
end;

constructor TTexture2DMS.Create(AGLState: TGLState; ASize: TIntVector2; ASamples: Integer; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState, APixelFormat);
  FSize := ASize;
  FSamples := ASamples;
  Generate;    
end;

procedure TTexture2DMS.Generate;
begin
  Bind;
  glTexImage2DMultisample(TargetType, Samples, Ord(PixelFormat), Width, Height, True);
end;

{ TTexture2DMSArray }

procedure TTexture2DMSArray.SetSamples(const Value: Integer);
begin
  if Samples = Value then
    Exit;
  FSamples := Value;
  Changed;
end;

constructor TTexture2DMSArray.Create(AGLState: TGLState; ASize: TIntVector3; ASamples: Integer; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState, APixelFormat);
  FSize := ASize;
  FSamples := ASamples;
  Generate;
end;

function TTexture2DMSArray.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D_MULTISAMPLE_ARRAY;
end;

procedure TTexture2DMSArray.Generate;
begin
  glTexImage3DMultisample(TargetType, Samples, Ord(PixelFormat), Width, Height, Layers, True);
end;

end.
