unit Pengine.Texture;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,
  System.Classes,

  Vcl.Graphics,
  Vcl.Imaging.pngimage,

  Winapi.Windows,

  Pengine.BitField,
  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.GLEnums,
  Pengine.GLProgram,
  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.GLState;

type

  /// <summary>Stores data for a single 2D-texture.</summary>
  TTextureData = class
  private
    FSize: TIntVector2;
    FBpp: Integer;
    FData: PByte;

    function GetDataSize: Integer;
    
  public
    /// <summary>Creates an unintialized TTextureData object.</summary>
    constructor Create; overload;
    /// <summary>Creates a TTextureData object and reserves memory for the required amount of pixel-data.</summary>
    constructor Create(ASize: TIntVector2; ABpp: Integer); overload;
    destructor Destroy; override;

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
    /// <summary>The amount of bytes per pixel.</summary>
    property Bpp: Integer read FBpp;
    /// <summary>A pointer to the pixel-data.</summary>
    property Data: PByte read FData;
    /// <summary>The size of the data in bytes. Equal to width*height*bpp.</summary>
    property DataSize: Integer read GetDataSize;

  end;

  ETooManyTextureUnits = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>An abstract base class for OpenGL texture objects.</summary>
  TTexture = class abstract(TGLObject)
  public type

    TUnit = type Integer;

    TBinding = class(TGLObjectBinding<TTexture>)
    private
      FUsedUnits: TBitField;
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

  protected
    procedure GenObject(out AGLName: GLuint); override;
    procedure DeleteObject(const AGLName: GLuint); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

    function Binding: TBinding;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    function TargetType: Cardinal; virtual; abstract;

    function Active: Boolean;

    procedure Activate;
    procedure Deactivate;

    procedure Uniform(AUniform: TGLProgram.TUniformSampler);

    property UnitID: TUnit read FUnitID;

  end;

  TTexture2D = class(TTexture)
  private
    FMagFilter: TGLTextureMagFilter;
    FMinFilter: TGLTextureMinFilter;
    FTextureCompareMode: TGLTextureCompareMode;

    procedure SetMagFilter(AValue: TGLTextureMagFilter);
    procedure SetMinFilter(AValue: TGLTextureMinFilter);
    procedure SetTextureCompareMode(AValue: TGLTextureCompareMode);

  public
    constructor Create(AGLState: TGLState);

    function TargetType: Cardinal; override;

    property MagFilter: TGLTextureMagFilter read FMagFilter write SetMagFilter;
    property MinFilter: TGLTextureMinFilter read FMinFilter write SetMinFilter;
    property TextureCompareMode: TGLTextureCompareMode read FTextureCompareMode write SetTextureCompareMode;

  end;

  TTexture2DMS = class(TTexture)
  public
    function TargetType: Cardinal; override;
  end;

  TTexture2DArray = class(TTexture2D)
  public
    function TargetType: Cardinal; override;
  end;

  TTextureCubeMap = class(TTexture2D)
  public
    function TargetType: Cardinal; override;
  end;

  TTextureCubeMapArray = class(TTexture2D)
  public
    function TargetType: Cardinal; override;
  end;

  TEmptyTexture2D = class(TTexture2D)
  private
    FPixelFormat: TGLPixelFormat;

  public
    constructor Create(AGLState: TGLState; AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat);

    procedure Resize(AWidth, AHeight: Cardinal);
  end;

  TEmptyTexture2DMS = class(TTexture2DMS)
  private
    FPixelFormat: TGLPixelFormat;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FSamples: Cardinal;

    procedure Change;

  public
    constructor Create(AGLState: TGLState; AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat; ASamples: Cardinal);

    procedure Resize(AWidth, AHeight: Cardinal);
    procedure SetSamples(ASamples: Cardinal);
  end;

  TEmptyTexture2DArray = class(TTexture2DArray)
  private
    FPixelFormat: TGLPixelFormat;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FLayers: Cardinal;

    procedure Change;

  public
    constructor Create(AGLState: TGLState; AWidth, AHeight, ALayers: Cardinal; APixelFormat: TGLPixelFormat);

    procedure Resize(AWidth, AHeight: Cardinal);
    procedure SetLayers(ALayers: Cardinal);

  end;

  TEmptyTextureCubeMapArray = class(TTextureCubeMapArray)
  private
    FPixelFormat: TGLPixelFormat;
    FSize: Cardinal;
    FLayers: Cardinal;

    procedure Change;

  public
    constructor Create(AGLState: TGLState; ASize, ALayers: Cardinal; APixelFormat: TGLPixelFormat);

    procedure Resize(ASize: Cardinal);
    procedure SetLayers(ALayers: Cardinal);
  end;

  { TSingleTexture }

  TSingleTexture = class(TTexture2D)
  private
    FTexture: TTextureData;
    FReferenced: Boolean;

    procedure SetTexture(AValue: TTextureData);

  public
    constructor Create(AGLState: TGLState); overload;
    constructor Create(AGLState: TGLState; AFileName: string); overload;
    destructor Destroy; override;

    property Texture: TTextureData read FTexture write SetTexture;
  end;

  { TTexturePage }

  TTexturePage = class(TTexture2D)
  private type
    TTextures = TRefArray<TTextureItem>;
    TTextureIDs = TMap<string, TTextureID, TStringHasher>;

  private
    FTextures: TTextures;
    FTextureIDs: TTextureIDs;
    FPxlSize: Integer;

    FSubTextures: array [TSubTextureType] of TTexture2D;
    FSizeChanged: Boolean;

    function GetSubTexture(ASubTextureType: TSubTextureType): TTexture2D;
    function GetTexture(ID: TTextureID): TTextureItem;
    function GetTextureID(AName: string): TTextureID;
    function GetTextureName(ID: TTextureID): string;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    procedure EnableSubType(ASubType: TSubTextureType);

    procedure AddTexture(const ATexture: TTextureItem; const AName: string); overload;
    procedure AddTexture(const ATexture: TTextureItem); overload; inline;

    procedure AddTextureFromFile(const AFileName: string); overload; inline;
    procedure AddTextureFromFile(const AFileName, AName: string); overload; inline;

    procedure AddTextureFromResource(const AResourceName: string); overload; inline;
    procedure AddTextureFromResource(const AResourceName, AName: string); overload; inline;

    procedure DelTexture(const AName: string);
    procedure Clear;

    procedure BuildPage(ASegmentResolution: Integer; AFreeTextures: Boolean = True);

    function GetTexCoord(const AName: string; const ATexCoord: TVector2): TVector2; overload;
    function GetTexCoord(const AName: string; S, T: Single): TVector2; overload; inline;
    function GetTexBounds(const AName: string; ABounds: TBounds2): TBounds2; overload; inline;

    function GetTexCoord(ID: TTextureID; const ATexCoord: TVector2): TVector2; overload;
    function GetTexCoord(ID: TTextureID; S, T: Single): TVector2; overload; inline;
    function GetTexBounds(ID: TTextureID; ABounds: TBounds2): TBounds2; overload; inline;

    function HalfPixelInset(ABounds: TBounds2): TBounds2;

    function GetBounds(ID: TTextureID): TBounds2; overload;
    function GetBounds(AName: string): TBounds2; overload; inline;

    property Textures[ID: TTextureID]: TTextureItem read GetTexture;

    property TextureIDs[AName: string]: TTextureID read GetTextureID;
    property TextureNames[ID: TTextureID]: string read GetTextureName;
    property SubTexture[ASubTextureType: TSubTextureType]: TTexture2D read GetSubTexture;

    function TextureExists(const AName: string): Boolean;

    property SizeChanged: Boolean read FSizeChanged;
    procedure NotifySizeChange;

    procedure Uniform(AUniform: TGLProgram.TUniformSampler; ATextureType: TTextureType);
    procedure UniformDefaults(AShader: TGLProgram);
  end;

implementation

{ TTextureData }

function TTextureData.GetDataSize: Integer;
begin
  Result := Width * Height * Bpp;
end;

constructor TTextureData.Create;
begin
  // nothing
end;

constructor TTextureData.Create(ASize: TIntVector2; ABpp: Integer);
begin
  FSize := ASize;
  FBpp := ABpp;
  GetMem(FData, DataSize);
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
  FBpp := 4;

  GetMem(FData, DataSize);
  Res := FData;

  for Y := AImage.Height - 1 downto 0 do
  begin
    for X := 0 to Width - 1 do
    begin
      Color := ByteSwap(AImage.Pixels[X, Y]) shr 8;
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

{ TEmptyTextureCubeMapArray }

procedure TEmptyTextureCubeMapArray.Change;
begin
  Bind;
  glTexImage3D(TargetType, 0, Ord(FPixelFormat), FSize, FSize, FLayers * 6, 0, Ord(FPixelFormat),
    GL_UNSIGNED_BYTE, nil);
end;

constructor TEmptyTextureCubeMapArray.Create(AGLState: TGLState; ASize, ALayers: Cardinal; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState);
  FPixelFormat := APixelFormat;
  FSize := ASize;
  FLayers := ALayers;
  Change;
end;

procedure TEmptyTextureCubeMapArray.Resize(ASize: Cardinal);
begin
  if FSize = ASize then
    Exit;
  FSize := ASize;
  Change;
end;

procedure TEmptyTextureCubeMapArray.SetLayers(ALayers: Cardinal);
begin
  if FLayers = ALayers then
    Exit;
  FLayers := ALayers;
  Change;
end;

{ TTextureCubeMap }

function TTextureCubeMap.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_CUBE_MAP;
end;

{ TTextureCubeMapArray }

function TTextureCubeMapArray.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_CUBE_MAP_ARRAY;
end;

{ TTexture2DArray }

function TTexture2DArray.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D_ARRAY;
end;

{ TEmptyTexture2DArray }

procedure TEmptyTexture2DArray.Change;
begin
  Bind;
  glTexImage3D(TargetType, 0, Ord(FPixelFormat), FWidth, FHeight, FLayers, 0, Ord(FPixelFormat), GL_UNSIGNED_BYTE, nil);
end;

constructor TEmptyTexture2DArray.Create(AGLState: TGLState; AWidth, AHeight, ALayers: Cardinal; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState);
  FWidth := AWidth;
  FHeight := AHeight;
  FLayers := ALayers;
  FPixelFormat := APixelFormat;
  Change;
end;

procedure TEmptyTexture2DArray.Resize(AWidth, AHeight: Cardinal);
begin
  if (FWidth = AWidth) and (FHeight = AHeight) then
    Exit;
  FWidth := AWidth;
  FHeight := AHeight;
  Change;
end;

procedure TEmptyTexture2DArray.SetLayers(ALayers: Cardinal);
begin
  if FLayers = ALayers then
    Exit;
  FLayers := ALayers;
  Change;
end;

{ TTexture2DMS }

function TTexture2DMS.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D_MULTISAMPLE;
end;

{ TTexture2D }

procedure TTexture2D.SetMagFilter(AValue: TGLTextureMagFilter);
begin
  if MagFilter = AValue then
    Exit;
  FMagFilter := AValue;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_MAG_FILTER, Ord(AValue));
end;

procedure TTexture2D.SetMinFilter(AValue: TGLTextureMinFilter);
begin
  if MinFilter = AValue then
    Exit;
  FMinFilter := AValue;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_MIN_FILTER, Ord(AValue));
end;

procedure TTexture2D.SetTextureCompareMode(AValue: TGLTextureCompareMode);
begin
  if TextureCompareMode = AValue then
    Exit;
  FTextureCompareMode := AValue;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_COMPARE_MODE, Ord(AValue));
end;

function TTexture2D.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D;
end;

constructor TTexture2D.Create(AGLState: TGLState);
begin
  inherited;
  MinFilter := minNearest;
  MagFilter := magNearest;
  TextureCompareMode := tcmNone;
end;

{ TEmptyTexture2DMS }

procedure TEmptyTexture2DMS.Resize(AWidth, AHeight: Cardinal);
begin
  if (FWidth = AWidth) and (FHeight = AHeight) then
    Exit;
  FWidth := AWidth;
  FHeight := AHeight;
  Change;
end;

procedure TEmptyTexture2DMS.SetSamples(ASamples: Cardinal);
begin
  if FSamples = ASamples then
    Exit;
  FSamples := ASamples;
  Change;
end;

procedure TEmptyTexture2DMS.Change;
begin
  Bind;
  glTexImage2DMultisample(TargetType, FSamples, Ord(FPixelFormat), FWidth, FHeight, False);
end;

constructor TEmptyTexture2DMS.Create(AGLState: TGLState; AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat; ASamples: Cardinal);
begin
  inherited Create(AGLState);
  FPixelFormat := APixelFormat;
  FSamples := ASamples;
  FWidth := AWidth;
  FHeight := AHeight;
  Change;
end;

{ TEmptyTexture2D }

constructor TEmptyTexture2D.Create(AGLState: TGLState; AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState);
  FPixelFormat := APixelFormat;
  Resize(AWidth, AHeight);
end;

procedure TEmptyTexture2D.Resize(AWidth, AHeight: Cardinal);
begin
  Bind;
  glTexImage2D(TargetType, 0, Ord(FPixelFormat), AWidth, AHeight, 0, Ord(FPixelFormat), GL_UNSIGNED_BYTE, nil);
end;

{ EMissingTextureName }

constructor EMissingTextureName.Create(AName: string);
begin
  inherited Create('Texture with Name "' + AName + '" does not exist!');
end;

{ TSingleTexture }

procedure TSingleTexture.SetTexture(AValue: TTextureData);
begin
  if Pointer(FTexture) = Pointer(AValue) then
    Exit;
  if (FTexture <> nil) and (not FReferenced) then
    FTexture.Free;
  FTexture := AValue;
  Bind;
  glTexImage2D(
    TargetType,
    0,
    GL_RGBA,
    FTexture.Width,
    FTexture.Height,
    0,
    GL_BGRA,
    GL_UNSIGNED_BYTE,
    FTexture.Data[ttMain]
    );
  FReferenced := True;
end;

constructor TSingleTexture.Create(AGLState: TGLState);
begin
  inherited;
  FReferenced := True;
end;

constructor TSingleTexture.Create(AGLState: TGLState; AFileName: string);
begin
  inherited Create(AGLState);
  Texture := TTextureData.Create(AFileName);
  FReferenced := False;
end;

destructor TSingleTexture.Destroy;
begin
  if (FTexture <> nil) and not FReferenced then
    FTexture.Free;
  inherited Destroy;
end;

{ TTexture.TBinding }

constructor TTexture.TBinding.Create;
var
  Units: Integer;
begin
  glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, @Units);
  FUsedUnits := TBitField.Create(Units);
end;

destructor TTexture.TBinding.Destroy;
begin
  FUsedUnits.Free;
  inherited;
end;

procedure TTexture.TBinding.SetActiveUnit(const Value: TUnit);
begin
  if FActiveUnit = Value then
    Exit;
  FActiveUnit := Value;
  glActiveTexture(GL_TEXTURE0 + FActiveUnit);
end;

function TTexture.TBinding.Add: TUnit;
begin
  if FLastUnusedUnit = FUsedUnits.Size then
    raise ETooManyTextureUnits.Create;
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

constructor TTexture.Create(AGLState: TGLState);
begin
  inherited;
  FUnitID := -1;
end;

destructor TTexture.Destroy;
begin
  Deactivate;
  inherited Destroy;
end;

procedure TTexture.GenObject(out AGLName: GLuint);
begin
  glGenTextures(1, @AGLName);
end;

class function TTexture.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TBinding;
end;

class function TTexture.GetObjectType: TGLObjectType;
begin
  Result := otTexture;
end;

procedure TTexture.BindGLObject;
begin
  Activate;
  glBindTexture(TargetType, GLName);
end;

function TTexture.Binding: TBinding;
begin
  Result := TBinding(inherited Binding);
end;

function TTexture.Active: Boolean;
begin
  Result := FUnitID <> -1;
end;

procedure TTexture.Activate;
begin
  if FUnitID = -1 then
    FUnitID := Binding.Add;
  Binding.ActiveUnit := FUnitID;
end;

procedure TTexture.Deactivate;
begin
  Binding.Del(FUnitID);
  FUnitID := -1;
end;

procedure TTexture.DeleteObject(const AGLName: GLuint);
begin
  glDeleteTextures(1, @AGLName);
end;

procedure TTexture.UnbindGLObject;
begin
  glBindTexture(TargetType, 0);
end;

procedure TTexture.Uniform(AUniform: TGLProgram.TUniformSampler);
begin
  AUniform.Value := FUnitID;
end;

{ EMissingTextureID }

constructor EMissingTextureID.Create(ID: TTextureID);
begin
  inherited CreateFmt('Texture with ID "%d" does not exist!', [ID]);
end;

{ ETooManyTextureUnits }

constructor ETooManyTextureUnits.Create;
begin
  inherited Create('Too many texture units.');
end;

{ TTexturePage }

procedure TTexturePage.AddTexture(const ATexture: TTextureItem; const AName: string);
begin
  if TextureExists(AName) then
    raise Exception.Create('Tried to create multiple textures with name ' + string(AName));
  FTextureIDs[AName] := FTextures.Count;
  FTextures.Add(ATexture);
end;

procedure TTexturePage.AddTexture(const ATexture: TTextureItem);
begin
  AddTexture(ATexture, ATexture.Name);
end;

procedure TTexturePage.AddTextureFromResource(const AResourceName: string);
begin
  AddTexture(TTextureItem.Create(AResourceName));
end;

procedure TTexturePage.AddTextureFromResource(const AResourceName, AName: string);
begin
  AddTexture(TTextureItem.Create(AResourceName, True), AName);
end;

procedure TTexturePage.AddTextureFromFile(const AFileName: string);
begin
  AddTexture(TTextureItem.Create(AFileName));
end;

procedure TTexturePage.AddTextureFromFile(const AFileName, AName: string);
begin
  AddTexture(TTextureItem.Create(AFileName), AName);
end;

function TTexturePage.TextureExists(const AName: string): Boolean;
begin
  Result := FTextureIDs.KeyExists(AName);
end;

procedure TTexturePage.NotifySizeChange;
begin
  FSizeChanged := False;
end;

procedure TTexturePage.Uniform(AUniform: TGLProgram.TUniformSampler; ATextureType: TTextureType);
begin
  if ATextureType = ttMain then
    inherited Uniform(AUniform)
  else
  begin
    if FSubTextures[ATextureType] = nil then
      EnableSubType(ATextureType);
    AUniform.Value := FSubTextures[ATextureType].UnitID;
  end;
end;

procedure TTexturePage.UniformDefaults(AShader: TGLProgram);
begin
  Uniform(AShader.UniformSampler('diffusemap'), ttMain);
  Uniform(AShader.UniformSampler('specularmap'), ttSpecular);
  Uniform(AShader.UniformSampler('normalmap'), ttNormal);
end;

procedure TTexturePage.BuildPage(ASegmentResolution: Integer; AFreeTextures: Boolean);
var
  Pxl: Cardinal;
  Size, MinSize: Integer;
  X, Y: Integer;
  S, T: Integer;
  Map: array of array of Boolean;
  Fits: Boolean;
  TexItem, TexItem2: TTextureItem;
  TexType: TTextureType;
  OldPixelSize: Integer;
begin
  OldPixelSize := FPxlSize;
  Pxl := 0;
  FPxlSize := 0;
  MinSize := 0;
  for TexItem in FTextures do
  begin
    Pxl := Pxl + TexItem.Width * TexItem.Height;
    MinSize := Max(Max(FPxlSize, TexItem.Width), TexItem.Height);
  end;
  FPxlSize := Max(FPxlSize, Floor(Power(2, Ceil(ln(Pxl) / (2 * ln(2))))));
  while MinSize > FPxlSize do
    FPxlSize := FPxlSize * 2;

  Size := FPxlSize div ASegmentResolution;

  SetLength(Map, Size, Size);

  for TexItem in FTextures do
  begin
    S := 0;
    T := 0;
    while True do
    begin
      while T + TexItem.Height / ASegmentResolution > Size do
      begin
        Size := Size * 2;
        FPxlSize := FPxlSize * 2;
        SetLength(Map, Size, Size);
        for TexItem2 in FTextures do
          TexItem2.TexCoord := TexItem2.TexCoord / 2;
      end;
      if S + TexItem.Width / ASegmentResolution <= Size then
      begin
        Fits := True;
        for X := 0 to TexItem.Width div ASegmentResolution - 1 do
        begin
          for Y := 0 to TexItem.Height div ASegmentResolution - 1 do
            if Map[S + X, T + Y] then
            begin
              Fits := False;
              Break;
            end;
          if not Fits then
            Break;
        end;
        if Fits then
          Break;
      end;
      S := S + 1;
      if S = Size then
      begin
        S := 0;
        T := T + 1;
      end;
    end;
    for X := 0 to TexItem.Width div ASegmentResolution - 1 do
      for Y := 0 to TexItem.Height div ASegmentResolution - 1 do
        Map[S + X, T + Y] := True;
    TexItem.TexCoord := TTexCoord2.Create(S, T) / Size;

    while T + TexItem.Height / ASegmentResolution > Size do
    begin
      Size := Size * 2;
      FPxlSize := FPxlSize * 2;
      SetLength(Map, Size, Size);
      for TexItem2 in FTextures do
      begin
        TexItem2.TexCoord := TexItem2.TexCoord / 2;
      end;
    end;
    for X := 0 to TexItem.Width div ASegmentResolution - 1 do
      for Y := 0 to TexItem.Height div ASegmentResolution - 1 do
        Map[S + X, T + Y] := True;
    TexItem.TexCoord := TTexCoord2.Create(S, T) / Size;
  end;

  for TexType := Low(TTextureType) to High(TTextureType) do
  begin
    if TexType = ttMain then
      Bind
    else if FSubTextures[TexType] <> nil then
      FSubTextures[TexType].Bind
    else
      Continue;
    glTexImage2D(TargetType, 0, GL_RGBA, FPxlSize, FPxlSize, 0, GL_BGRA, GL_UNSIGNED_BYTE, nil);
  end;

  for TexItem in FTextures do
  begin
    for TexType := Low(TTextureType) to High(TTextureType) do
    begin
      if TexType = ttMain then
        Bind
      else if FSubTextures[TexType] <> nil then
        FSubTextures[TexType].Bind
      else
        Continue;

      glTexSubImage2D(
        TargetType,
        0,
        Floor(TexItem.TexCoord.X * FPxlSize),
        Floor(TexItem.TexCoord.Y * FPxlSize),
        TexItem.Width,
        TexItem.Height,
        GL_BGRA,
        GL_UNSIGNED_BYTE,
        TexItem.Data[TexType]
        );
    end;

    if AFreeTextures then
      TTextureData(TexItem).FreeData;
  end;

  if OldPixelSize <> FPxlSize then
    FSizeChanged := True;
end;

function TTexturePage.GetTexture(ID: TTextureID): TTextureItem;
begin
  Result := FTextures[ID];
end;

function TTexturePage.GetSubTexture(ASubTextureType: TSubTextureType): TTexture2D;
begin
  Result := FSubTextures[ASubTextureType];
end;

constructor TTexturePage.Create(AGLState: TGLState);
begin
  inherited Create(AGLState);
  FTextures := TTextures.Create(True);
  FTextureIDs := TTextureIDs.Create;
end;

procedure TTexturePage.DelTexture(const AName: string);
begin
  FTextures.DelAt(TextureIDs[AName]);
  FTextureIDs.Del(AName);
end;

procedure TTexturePage.Clear;
begin
  FTextures.Clear;
end;

destructor TTexturePage.Destroy;
var
  T: TTextureType;
begin
  FTextures.Free;
  FTextureIDs.Free;
  for T := Low(TSubTextureType) to High(TSubTextureType) do
    FSubTextures[T].Free;
  inherited;
end;

procedure TTexturePage.EnableSubType(ASubType: TSubTextureType);
begin
  if FSubTextures[ASubType] = nil then
    FSubTextures[ASubType] := TTexturePage.Create(GLState);
end;

function TTexturePage.GetTexCoord(const AName: string; const ATexCoord: TVector2): TVector2;
begin
  Result := GetTexCoord(GetTextureID(AName), ATexCoord);
end;

function TTexturePage.GetTexCoord(const AName: string; S, T: Single): TVector2;
begin
  Result := GetTexCoord(GetTextureID(AName), S, T);
end;

function TTexturePage.GetTexBounds(const AName: string; ABounds: TBounds2): TBounds2;
begin
  Result := GetTexBounds(GetTextureID(AName), ABounds);
end;

function TTexturePage.GetTexCoord(ID: TTextureID; const ATexCoord: TVector2): TVector2;
begin
  Result := GetBounds(ID)[ATexCoord];
end;

function TTexturePage.GetTexCoord(ID: TTextureID; S, T: Single): TVector2;
begin
  Result := GetTexCoord(ID, TTexCoord2.Create(S, T));
end;

function TTexturePage.GetTexBounds(ID: TTextureID; ABounds: TBounds2): TBounds2;
begin
  Result.C1 := GetTexCoord(ID, ABounds.C1);
  Result.C2 := GetTexCoord(ID, ABounds.C2);
end;

function TTexturePage.HalfPixelInset(ABounds: TBounds2): TBounds2;
begin
  Result := ABounds.Inset(1 / (FPxlSize * 2));
end;

function TTexturePage.GetBounds(ID: TTextureID): TBounds2;
begin
  if not FTextures.RangeCheck(ID) then
    raise EMissingTextureID.Create(ID);
  with FTextures[ID] as TTextureItem do
  begin
    Result.C1 := TexCoord;
    Result.C2 := TexCoord + TVector2.Create(Width, Height) / FPxlSize;
  end;
end;

function TTexturePage.GetBounds(AName: string): TBounds2;
begin
  Result := GetBounds(TextureIDs[AName]);
end;

function TTexturePage.GetTextureID(AName: string): TTextureID;
begin
  if not FTextureIDs.Get(AName, Result) then
    raise EMissingTextureName.Create(AName);
end;

function TTexturePage.GetTextureName(ID: TTextureID): string;
var
  Pair: TPair<string, TTextureID>;
begin
  for Pair in FTextureIDs do
    if Pair.Value = ID then
      Result := Pair.Key;
end;

end.
