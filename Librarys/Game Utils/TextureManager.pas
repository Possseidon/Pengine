unit TextureManager;

interface

uses
  SysUtils, Graphics, dglOpenGL, VectorGeometry, Shaders, Lists, GLEnums, Math, Windows, BitField
  {$IFNDEF FPC}
  ,pngimage
  {$ENDIF}
  ;

type

  { TTextureData }

  TTextureType = (
    ttMain,
    ttSpecular,
    ttNormal
  );
  TSubTextureType = ttSpecular .. High(TTextureType);
  TSubTextureTypes = set of TSubTextureType;

  TTextureData = class
  private
    FWidth, FHeight: Word;
    FBpp: Byte;
    FData: array [TTextureType] of PByte;
    FName: String;

    const
      FileExtension = '.png';
      FileTypeMarker: array [TSubTextureType] of String = (
        'specularmap',
        'normalmap'
      );

    function GetData(T: TTextureType): PByte;
  public
    constructor Create(AFileName: String; AResource: Boolean = False); overload;
    constructor Create(AWidth, AHeight: Word; ABpp: Byte; AName: String); overload;

    destructor Destroy; override;
    procedure FreeData;

    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property Bpp: Byte read FBpp;
    property Data[T: TTextureType]: PByte read GetData;
    property Name: String read FName;

  end;

  { TTextureItem }

  TTextureItem = class (TTextureData)
  private
    FTexCoord: TTexCoord2;
  public
    constructor Create(ATextureData: TTextureData); overload;
    //constructor Create(AFileName: String; AResource: Boolean = False); overload;
    //constructor Create(AWidth, AHeight: Word; ABpp: Byte; AName: String); overload;

    property TexCoord: TTexCoord2 read FTexCoord write FTexCoord;
  end;

  { ETooManyTextureUnits }

  ETooManyTextureUnits = class (Exception)
  public
    constructor Create;
  end;

  TTexture = class abstract
  private
    FID: Integer; // GL-ID for binding
    FUnitID: Integer; // Unit-ID for GL_TEXTURE0 + I

    class var
      Initialized: Boolean;
      BoundTexture: TTexture;
      UsedUnits: TBitField;

    class constructor Create;
    class procedure Init;
    class destructor Destroy;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Bind;

    function IsActive: Boolean;

    procedure Activate;
    procedure Deactivate;

    procedure Uniform(AShader: TShader; AName: PAnsiChar);

    property ID: Integer read FID;
    property UnitID: Integer read FUnitID;

    function TargetType: Cardinal; virtual; abstract;

    class function MaxUnits: Integer;

  end;

  { TTexture2D }

  TTexture2D = class (TTexture)
  private
    FMagFilter: TGLTextureMagFilter;
    FMinFilter: TGLTextureMinFilter;
    FTextureCompareMode: TGLTextureCompareMode;

    procedure SetMagFilter(AValue: TGLTextureMagFilter);
    procedure SetMinFilter(AValue: TGLTextureMinFilter);
    procedure SetTextureCompareMode(AValue: TGLTextureCompareMode);

  public
    constructor Create;

    property MagFilter: TGLTextureMagFilter read FMagFilter write SetMagFilter;
    property MinFilter: TGLTextureMinFilter read FMinFilter write SetMinFilter;
    property TextureCompareMode: TGLTextureCompareMode read FTextureCompareMode write SetTextureCompareMode;

    function TargetType: Cardinal; override;

  end;

  { TTexture2DMS }

  TTexture2DMS = class (TTexture)
  public
    function TargetType: Cardinal; override;
  end;

  { TTexture2DArray }

  TTexture2DArray = class (TTexture2D)
  public
    function TargetType: Cardinal; override;
  end;

  { TTextureCubeMap }

  TTextureCubeMap = class (TTexture2D)
  public
    function TargetType: Cardinal; override;
  end;

  { TTextureCubeMapArray }

  TTextureCubeMapArray = class (TTexture2D)
  public
    function TargetType: Cardinal; override;
  end;

  { TEmptyTexture2D }

  TEmptyTexture2D = class (TTexture2D)
  private
    FPixelFormat: TGLPixelFormat;

  public
    constructor Create(AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat);

    procedure Resize(AWidth, AHeight: Cardinal);
  end;

  { TEmptyTexture2DMS }

  TEmptyTexture2DMS = class (TTexture2DMS)
  private
    FPixelFormat: TGLPixelFormat;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FSamples: Cardinal;

    procedure Change;

  public
    constructor Create(AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat; ASamples: Cardinal);

    procedure Resize(AWidth, AHeight: Cardinal);
    procedure SetSamples(ASamples: Cardinal);
  end;

  { TEmptyTexture2DArray }

  TEmptyTexture2DArray = class (TTexture2DArray)
  private
    FPixelFormat: TGLPixelFormat;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FLayers: Cardinal;

    procedure Change;

  public
    constructor Create(AWidth, AHeight, ALayers: Cardinal; APixelFormat: TGLPixelFormat);

    procedure Resize(AWidth, AHeight: Cardinal);
    procedure SetLayers(ALayers: Cardinal);

  end;

  { TEmptyTextureCubeMapArray }

  TEmptyTextureCubeMapArray = class (TTextureCubeMapArray)
  private
    FPixelFormat: TGLPixelFormat;
    FSize: Cardinal;
    FLayers: Cardinal;

    procedure Change;

  public
    constructor Create(ASize, ALayers: Cardinal; APixelFormat: TGLPixelFormat);

    procedure Resize(ASize: Cardinal);
    procedure SetLayers(ALayers: Cardinal);
  end;

  { TSingleTexture }

  TSingleTexture = class (TTexture2D)
  private
    FTexture: TTextureData;
    FReferenced: Boolean;
    procedure SetTexture(AValue: TTextureData);

  public
    constructor Create; overload;
    constructor Create(AFilename: String); overload;
    destructor Destroy; override;

    property Texture: TTextureData read FTexture write SetTexture;
  end;

  TTextureID = Cardinal;

  { EMissingTextureID }

  EMissingTextureID = class (Exception)
    constructor Create(ID: TTextureID);
  end;

  { EMissingTextureName }

  EMissingTextureName = class (Exception)
    constructor Create(AName: String);
  end;

  { TTexturePage }

  TTexturePage = class (TTexture2D)
  private
    FTextures: TObjectArray<TTextureItem>;
    FTextureIDs: TStringMap<TTextureID>;
    FPxlSize: Integer;

    FSubTextures: array [TSubTextureType] of TTexture2D;
    FSizeChanged: Boolean;

    function GetSubTexture(ASubTextureType: TSubTextureType): TTexture2D;
    function GetTexture(ID: TTextureID): TTextureItem;
    function GetTextureID(AName: String): TTextureID;
    function GetTextureName(ID: TTextureID): String;

  public
    constructor Create;
    destructor Destroy; override;

    procedure EnableSubType(ASubType: TSubTextureType);

    procedure AddTexture(const ATexture: TTextureItem; const AName: String); overload;
    procedure AddTexture(const ATexture: TTextureItem); overload; inline;

    procedure AddTextureFromFile(const AFileName: String); overload; inline;
    procedure AddTextureFromFile(const AFileName, AName:  String); overload; inline;

    procedure AddTextureFromResource(const AResourceName: String); overload; inline;
    procedure AddTextureFromResource(const AResourceName, AName: String); overload; inline;

    procedure DelTexture(const AName: String);
    procedure DelAll;

    procedure BuildPage(ASegmentResolution: Integer; AFreeTextures: Boolean = True);

    function GetTexCoord(const AName: String; const ATexCoord: TGVector2): TGVector2; overload;
    function GetTexCoord(const AName: String; S, T: Single): TGVector2; overload; inline;
    function GetTexBounds(const AName: String; ABounds: TGBounds2): TGBounds2; overload; inline;

    function GetTexCoord(ID: TTextureID; const ATexCoord: TGVector2): TGVector2; overload;
    function GetTexCoord(ID: TTextureID; S, T: Single): TGVector2; overload; inline;
    function GetTexBounds(ID: TTextureID; ABounds: TGBounds2): TGBounds2; overload; inline;

    function HalfPixelInset(ABounds: TGBounds2): TGBounds2;

    function GetBounds(ID: TTextureID): TGBounds2; overload;
    function GetBounds(AName: String): TGBounds2; overload; inline;

    property Textures[ID: TTextureID]: TTextureItem read GetTexture;

    property TextureIDs[AName: String]: TTextureID read GetTextureID;
    property TextureNames[ID: TTextureID]: String read GetTextureName;
    property SubTexture[ASubTextureType: TSubTextureType]: TTexture2D read GetSubTexture;

    function TextureExists(const AName: String): Boolean;

    property SizeChanged: Boolean read FSizeChanged;
    procedure NotifySizeChange;

    // Automatically Enables Needed Types
    procedure Uniform(AShader: TShader; AName: PAnsiChar; ATextureType: TTextureType = ttMain);
    procedure UniformDefaults(AShader: TShader);
  end;

implementation

{ TEmptyTextureCubeMapArray }

procedure TEmptyTextureCubeMapArray.Change;
begin
  Bind;
  glTexImage3D(TargetType, 0, Ord(FPixelFormat), FSize, FSize, FLayers * 6, 0, Ord(FPixelFormat), GL_UNSIGNED_BYTE, nil);
end;

constructor TEmptyTextureCubeMapArray.Create(ASize, ALayers: Cardinal; APixelFormat: TGLPixelFormat);
begin
  inherited Create;
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

constructor TEmptyTexture2DArray.Create(AWidth, AHeight, ALayers: Cardinal; APixelFormat: TGLPixelFormat);
begin
  inherited Create;
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
  FMagFilter := AValue;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_MAG_FILTER, Ord(AValue));
end;

procedure TTexture2D.SetMinFilter(AValue: TGLTextureMinFilter);
begin
  FMinFilter := AValue;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_MIN_FILTER, Ord(AValue));
end;

procedure TTexture2D.SetTextureCompareMode(AValue: TGLTextureCompareMode);
begin
  FTextureCompareMode := AValue;
  Bind;
  glTexParameteri(TargetType, GL_TEXTURE_COMPARE_MODE, Ord(AValue));
end;

function TTexture2D.TargetType: Cardinal;
begin
  Result := GL_TEXTURE_2D;
end;

constructor TTexture2D.Create;
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

constructor TEmptyTexture2DMS.Create(AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat; ASamples: Cardinal);
begin
  inherited Create;
  FPixelFormat := APixelFormat;
  FSamples := ASamples;
  FWidth := AWidth;
  FHeight := AHeight;
  Change;
end;

{ TEmptyTexture2D }

constructor TEmptyTexture2D.Create(AWidth, AHeight: Cardinal; APixelFormat: TGLPixelFormat);
begin
  inherited Create;
  FPixelFormat := APixelFormat;
  Resize(AWidth, AHeight);
end;

procedure TEmptyTexture2D.Resize(AWidth, AHeight: Cardinal);
begin
  Bind;
  glTexImage2D(TargetType, 0, Ord(FPixelFormat), AWidth, AHeight, 0, Ord(FPixelFormat), GL_UNSIGNED_BYTE, nil);
end;

{ EMissingTextureName }

constructor EMissingTextureName.Create(AName: String);
begin
  inherited Create('Texture with Name "' + AName + '" does not exist!');
end;

{ TTextureItem }

constructor TTextureItem.Create(ATextureData: TTextureData);
var
  T: TTextureType;
  DataSize: Cardinal;
begin
  FWidth := ATextureData.Width;
  FHeight := ATextureData.Height;
  FBpp := ATextureData.Bpp;
  FName := ATextureData.Name;
  DataSize := ATextureData.Bpp * ATextureData.Width * ATextureData.Height;
  for T := Low(TTextureType) to High(TTextureType) do
    if ATextureData.Data[T] <> nil then
    begin
      GetMem(FData[T], DataSize);
      Move(ATextureData.Data[T]^, FData[T]^, DataSize);
    end;
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

constructor TSingleTexture.Create;
begin
  inherited Create;
  FReferenced := True;
end;

constructor TSingleTexture.Create(AFilename: String);
begin
  inherited Create;
  Texture := TTextureData.Create(AFilename);
  FReferenced := False;
end;

destructor TSingleTexture.Destroy;
begin
  if (FTexture <> nil) and not FReferenced then
    FTexture.Free;
  inherited Destroy;
end;

{ TTexture }

class constructor TTexture.Create;
begin
  UsedUnits := TBitField.Create;
end;

class procedure TTexture.Init;
var
  MaxUnits: Integer;
begin
  glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, @MaxUnits);
  UsedUnits.SetSize(MaxUnits);
  Initialized := True;
end;

class destructor TTexture.Destroy;
begin
  UsedUnits.Free;
end;

constructor TTexture.Create;
begin
  if not Initialized then
    Init;

  FUnitID := -1;

  glGenTextures(1, @FID);
end;

destructor TTexture.Destroy;
begin
  Deactivate;
  glDeleteTextures(1, @FID);
  inherited Destroy;
end;

procedure TTexture.Bind;
begin
  if Pointer(BoundTexture) <> Pointer(Self) then
  begin
    BoundTexture := Self;
    Activate;
    glBindTexture(TargetType, FID);
  end;
end;

function TTexture.IsActive: Boolean;
begin
  Result := FUnitID <> -1;
end;

procedure TTexture.Activate;
var
  FirstSlot: Integer;
begin
  if IsActive then
    Exit;
  FirstSlot := UsedUnits.FirstZero;
  if FirstSlot = -1 then
    raise ETooManyTextureUnits.Create;
  FUnitID := FirstSlot;
  UsedUnits[FUnitID] := True;
  glActiveTexture(GL_TEXTURE0 + FUnitID);
end;

procedure TTexture.Deactivate;
begin
  if not IsActive then
    Exit;
  UsedUnits[FUnitID] := False;
end;

procedure TTexture.Uniform(AShader: TShader; AName: PAnsiChar);
begin
  AShader.UniformInt(AName, FUnitID);
end;

class function TTexture.MaxUnits: Integer;
begin
  Result := UsedUnits.Size;
end;

{ EMissingTextureID }

constructor EMissingTextureID.Create(ID: TTextureID);
begin
  inherited CreateFmt('Texture with ID "%d" does not exist!', [ID]);
end;

{ ETooManyTextureUnits }

constructor ETooManyTextureUnits.Create;
begin
  inherited CreateFmt('Too many Texture Units! Maximum: %d', [TTexturePage.MaxUnits]);
end;

function TTextureData.GetData(T: TTextureType): PByte;
begin
  Result := FData[T];
end;

constructor TTextureData.Create(AFileName: String; AResource: Boolean);
const
  Normal: Cardinal = $007F7FFF;

  function ConvertFileName(AFileName: String; ATextureType: TSubTextureType): String;
  begin
    // test.png > test_MARKER.png
    Result := StringReplace(
      AFileName,
      FileExtension,
      '_' + FileTypeMarker[ATextureType] + FileExtension,
      [rfIgnoreCase]
    );
  end;

  function ConvertResourceName(AName: String; ATextureType: TSubTextureType): String;
  begin
    Result := AName + '_' + UpperCase(FileTypeMarker[ATextureType]);
  end;

  {
  procedure LoadTexture(AFilename: String; AResource: Boolean = False; ATextureType: TTextureType = ttMain);
  var
    Png: TPngImage;
    Res: ^Byte;
    Alpha: pByteArray;
    Y, X: Integer;
    C: TColor;
  begin
    raise Exception.Create('Delphi can''t load textures correctly. Fix it!');

    Png := TPngImage.Create;

    if AResource then
    begin
      if FindResource(hInstance, PChar(AName), RT_RCDATA) = 0 then
        raise Exception.Create('Cannot find resource ' + AName);
      Png.LoadFromResourceName(hInstance, AName);
    end
    else
    begin
      if not FileExists(AName) then
        raise Exception.Create('Cannot find file ' + AName);
      Png.LoadFromFile(AName);
    end;

    if ATextureType = ttMain then
    begin
      FWidth := Png.Width;
      FHeight := Png.Height;
      FBpp := 4;
    end
    else if (FWidth <> Png.Width) or (FHeight <> Png.Height) then
      raise Exception.Create('Texture size for sub textures must be equal to main texture!');

    Size := Png.Width * Png.Height * FBpp;
    GetMem(Res, Size);
    FData[ATextureType] := Res;

    for Y := Png.Height - 1 downto 0 do
    begin
      Alpha := Png.AlphaScanline[Y];
      for X := 0 to Png.Width - 1 do
      begin
        C := ByteSwap(Png.Pixels[X, Y]) shr 8;
        Move(C, Res^, 3);
        Inc(Res, 3);
        case Png.TransparencyMode of
          ptmNone:
            Res^ := $FF;
          ptmBit:
            if C = Png.TransparentColor then
              Res^ := 0
            else
              Res^ := $FF;
          ptmPartial:
            Res^ := Alpha^[X];
        end;
        Inc(Res, 1);
      end;
    end;
    Png.Free;
  end;
  }
  procedure LoadTexture(AName: String; AResource: Boolean = False; ATextureType: TTextureType = ttMain);
  var
    Res: PByte;
    X, Y: Integer;
    NoSubTexture: Boolean;
    Size, I: Integer;
    T: TTextureType;
    Name: String;
    {$IFDEF FPC}
    Png: TPortableNetworkGraphic;
    P: ^Byte;
    {$ELSE}
    Png: TPngImage;
    Alpha: PByteArray;
    C: TColor;
    {$ENDIF}
  begin
    {$IFDEF FPC}
    Png := TPortableNetworkGraphic.Create;
    {$ELSE}
    Png := TPngImage.Create;
    {$ENDIF}

    if AResource then
    begin
      if FindResource(hInstance, PChar(AName), RT_RCDATA) = 0 then
        raise Exception.Create('Cannot find resource ' + AName);
      Png.LoadFromResourceName(hInstance, AName);
    end
    else
    begin
      if not FileExists(AName) then
        raise Exception.Create('Cannot find file ' + AName);
      Png.LoadFromFile(AName);
    end;

    if ATextureType = ttMain then
    begin
      FWidth := Png.Width;
      FHeight := Png.Height;
      FBpp := 4;
    end
    else if (FWidth <> Png.Width) or (FHeight <> Png.Height) then
      raise Exception.Create('Texture size for sub textures must be equal to main texture!');

    Size := Png.Width * Png.Height * FBpp;
    GetMem(Res, Size);
    FData[ATextureType] := Res;

    {$IFDEF FPC}
    if Png.Transparent then
    begin
      P := Png.RawImage.Data + Png.RawImage.DataSize;
      for Y := Png.Height - 1 downto 0 do
      begin
        Dec(P, Png.Width * 4);
        Move(P^, Res^, Png.Width * 4);
        Inc(Res, Png.Width * 4);
      end;
    end
    else
    begin
      P := Png.RawImage.Data + Png.RawImage.DataSize;
      for Y := Png.Height - 1 downto 0 do
      begin
        Dec(P, Png.Width * 4);
        Move(P^, Res^, Png.Width * 4);
        Inc(Res, 3);
        for X := 0 to Png.Width - 1 do
        begin
          Res^ := $FF;
          Inc(Res, 4);
        end;
        Dec(Res, 3);
      end;
    end;
    {$ELSE}
    for Y := Png.Height - 1 downto 0 do
    begin
      Alpha := Png.AlphaScanline[Y];
      for X := 0 to Png.Width - 1 do
      begin
        C := ByteSwap(Png.Pixels[X, Y]) shr 8;
        Move(C, Res^, 3);
        Inc(Res, 3);
        case Png.TransparencyMode of
          ptmNone:
            Res^ := $FF;
          ptmBit:
            if C = Png.TransparentColor then
              Res^ := 0
            else
              Res^ := $FF;
          ptmPartial:
            Res^ := Alpha^[X];
        end;
        Inc(Res, 1);
      end;
    end;
    {$ENDIF}
    Png.Free;

    if ATextureType = ttMain then
      for T := Low(TSubTextureType) to High(TSubTextureType) do
      begin
        NoSubTexture := False;
        if AResource then
        begin
          Name := ConvertResourceName(AName, T);
          if FindResource(hInstance, PChar(Name), RT_RCDATA) = 0 then
            NoSubtexture := True;
        end
        else
        begin
          Name := ConvertFileName(AName, T);
          if not FileExists(Name) then
            NoSubTexture := True;
        end;

        if NoSubTexture then
        begin
          GetMem(FData[T], Size);
          case T of
          ttMain, ttSpecular:
            FillChar(FData[T]^, Size, 0);
          ttNormal:
            for I := 0 to Size div SizeOf(Normal) - 1 do
              Move(Normal, FData[T][I * SizeOf(Normal)], SizeOf(Normal));
          end;
        end
        else
          LoadTexture(Name, AResource, T);
      end;
  end;

begin
  FName := ExtractFileName(AFileName);
  LoadTexture(AFileName, AResource);
end;

constructor TTextureData.Create(AWidth, AHeight: Word; ABpp: Byte; AName: String);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FBpp := ABpp;
  FName := AName;
  GetMem(FData[ttMain], AWidth * AHeight * ABpp);
end;

destructor TTextureData.Destroy;
var
  P: PByte;
begin
  for P in FData do
    if P <> nil then
      FreeMem(P);
end;

procedure TTextureData.FreeData;
var
  T: TTextureType;
begin
  for T := Low(TTextureType) to High(TTextureType) do
  begin
    FreeMem(FData[T]);
    FData[T] := nil;
  end;
end;

{ TTexturePage }

procedure TTexturePage.AddTexture(const ATexture: TTextureItem; const AName: String);
begin
  if TextureExists(AName) then
    raise Exception.Create('Tried to create multiple textures with name ' + String(AName));
  FTextureIDs[AName] := FTextures.Count;
  FTextures.Add(ATexture);
end;

procedure TTexturePage.AddTexture(const ATexture: TTextureItem);
begin
  AddTexture(ATexture, ATexture.Name);
end;

procedure TTexturePage.AddTextureFromResource(const AResourceName: String);
begin
  AddTexture(TTextureItem.Create(AResourceName));
end;

procedure TTexturePage.AddTextureFromResource(const AResourceName, AName: String);
begin
  AddTexture(TTextureItem.Create(AResourceName, True), AName);
end;

procedure TTexturePage.AddTextureFromFile(const AFileName: String);
begin
  AddTexture(TTextureItem.Create(AFileName));
end;

procedure TTexturePage.AddTextureFromFile(const AFileName, AName: String);
begin
  AddTexture(TTextureItem.Create(AFileName), AName);
end;

function TTexturePage.TextureExists(const AName: String): Boolean;
begin
  Result := FTextureIDs.HasKey(AName);
end;

procedure TTexturePage.NotifySizeChange;
begin
  FSizeChanged := False;
end;

procedure TTexturePage.Uniform(AShader: TShader; AName: PAnsiChar; ATextureType: TTextureType);
begin
  if ATextureType = ttMain then
    inherited Uniform(AShader, AName)
  else
  begin
    if FSubTextures[ATextureType] = nil then
      EnableSubType(ATextureType);
    AShader.UniformInt(AName, FSubTextures[ATextureType].UnitID);
  end;
end;

procedure TTexturePage.UniformDefaults(AShader: TShader);
begin
  Uniform(AShader, 'diffusemap', ttMain);
  Uniform(AShader, 'specularmap', ttSpecular);
  Uniform(AShader, 'normalmap', ttNormal);
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
  OldPixelSize:Integer;
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

constructor TTexturePage.Create;
begin
  inherited Create;
  FTextures := TObjectArray<TTextureItem>.Create;
  FTextureIDs := TStringMap<TTextureID>.Create;
end;

procedure TTexturePage.DelTexture(const AName: String);
begin
  FTextures.Del(TextureIDs[AName]);
  FTextureIDs.Del(AName);
end;

procedure TTexturePage.DelAll;
begin
  FTextures.DelAll;
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
    FSubTextures[ASubType] := TTexturePage.Create;
end;

function TTexturePage.GetTexCoord(const AName: String; const ATexCoord: TGVector2): TGVector2;
begin
  Result := GetTexCoord(GetTextureID(AName), ATexCoord);
end;

function TTexturePage.GetTexCoord(const AName: String; S, T: Single): TGVector2;
begin
  Result := GetTexCoord(GetTextureID(AName), S, T);
end;

function TTexturePage.GetTexBounds(const AName: String; ABounds: TGBounds2): TGBounds2;
begin
  Result := GetTexBounds(GetTextureID(AName), ABounds);
end;

function TTexturePage.GetTexCoord(ID: TTextureID; const ATexCoord: TGVector2): TGVector2;
begin
  Result := GetBounds(ID)[ATexCoord];
end;

function TTexturePage.GetTexCoord(ID: TTextureID; S, T: Single): TGVector2;
begin
  Result := GetTexCoord(ID, TTexCoord2.Create(S, T));
end;

function TTexturePage.GetTexBounds(ID: TTextureID; ABounds: TGBounds2): TGBounds2;
begin
  Result.C1 := GetTexCoord(ID, ABounds.C1);
  Result.C2 := GetTexCoord(ID, ABounds.C2);
end;

function TTexturePage.HalfPixelInset(ABounds: TGBounds2): TGBounds2;
var
  Amount: Single;
begin
  Amount := 1 / (FPxlSize * 2);
  Result.C1 := ABounds.C1 + Amount;
  Result.C2 := ABounds.C2 - Amount;
end;

function TTexturePage.GetBounds(ID: TTextureID): TGBounds2;
begin
  if not FTextures.RangeCheck(ID) then
    raise EMissingTextureID.Create(ID);
  with FTextures[ID] as TTextureItem do
  begin
    Result.C1 := TexCoord;
    Result.C2 := TexCoord + TGVector2.Create(Width, Height) / FPxlSize;
  end;
end;

function TTexturePage.GetBounds(AName: String): TGBounds2;
begin
  Result := GetBounds(TextureIDs[AName]);
end;

function TTexturePage.GetTextureID(AName: String): TTextureID;
begin
  if not FTextureIDs.Get(AName, Result) then
    raise EMissingTextureName.Create(AName);
end;

function TTexturePage.GetTextureName(ID: TTextureID): String;
var
  Pair: TPair<String, TTextureID>;
begin
  for Pair in FTextureIDs do
    if Pair.Data = ID then
      Result := Pair.Key;
end;

end.