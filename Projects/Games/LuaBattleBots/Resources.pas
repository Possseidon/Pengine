unit Resources;

interface

uses
  Pengine.ResourceManager,
  Pengine.Shader,
  Pengine.Texture,
  Pengine.VAO,
  Pengine.Vector,
  Pengine.GLEnums,
  Pengine.Skybox,
  Pengine.Collections,
  Pengine.IntMaths,
  Pengine.Hasher;

type

  // --- Shader Resources ---

  { TResModelShader }

  TResModelShader = class(TShaderResource)
  public type

    TData = record
      Pos: TVector3;
      TexCoord: TTexCoord2;
      Normal: TVector3;
      Tangent: TVector3;
      Bitangent: TVector3;
      Border: TBounds2;
    end;

  protected
    class function GetShaderSource: string; override;
    class function GetAttributeOrder: TShader.TAttributeOrder; override;

  public
    class constructor Create;

  end;

  { TResSkyDomeShader }

  TResSkyboxShader = class abstract(TSkyboxShaderBase)
  protected
    class function GetShaderSource: string; override;

  public
    class constructor Create;

  end;

  // --- Texture Resources ---

  { TResTexturePage }

  TResTexturePage = class(TResource<TTexturePage>)
  protected
    class function CreateData: TTexturePage; override;

  public
    class constructor Create;
  end;

  // --- VAO Resources ---

  { TResCubeVAO}

  TResCubeVAOParams = class(TResourceParameter)
  private
    FSize: Single;
    FTexture: string;

  protected
    function GetHash: Cardinal; override;
    function Equals(AOther: TResourceParameter): Boolean; override;

  public
    constructor Create; override;

    property Size: Single read FSize write FSize;
    property Texture: string read FTexture write FTexture;

  end;

  TResCubeVAO = class(TParamResource<TVAO, TResCubeVAOParams>)
  protected
    class procedure CreateData(var AData: TVAO; AParams: TResCubeVAOParams); override;
  end;

  { TResFloorVAO }

  TResFloorVAOParams = class(TResourceParameter)
  private
    FTileSize: Single;
    FSize: TIntVector2;
    FTexture: string;

  protected
    function GetHash: Cardinal; override;
    function Equals(AOther: TResourceParameter): Boolean; override;

  public
    constructor Create; override;

    property TileSize: Single read FTileSize write FTileSize;
    property Size: TIntVector2 read FSize write FSize;
    property Texture: string read FTexture write FTexture;

  end;

  TResFloorVAO = class(TParamResource<TVAO, TResFloorVAOParams>)
  protected
    class procedure CreateData(var AData: TVAO; AParams: TResFloorVAOParams); override;
  end;

implementation

{ TResModelShader }

class function TResModelShader.GetShaderSource: string;
begin
  Result := 'Data/model';
end;

class function TResModelShader.GetAttributeOrder: TShader.TAttributeOrder;
begin
  Result := [
    'vpos',
    'vtexcoord',
    'vnormal',
    'vtangent',
    'vbitangent',
    'vborderlow',
    'vborderhigh'
    ];
end;

class constructor TResModelShader.Create;
begin
  AddToResourceManager;
end;

{ TResSkyDomeShader }

class function TResSkyboxShader.GetShaderSource: string;
begin
  Result := 'Data/skydome';
end;

class constructor TResSkyboxShader.Create;
begin
  AddToResourceManager;
end;

{ TResTexturePage }

class function TResTexturePage.CreateData: TTexturePage;
begin
  Result := TTexturePage.Create;
  Result.UniformDefaults(TResModelShader.Data);
  Result.AddTextureFromFile('Data/stone_bricks.png', 'stone_bricks');
  Result.AddTextureFromFile('Data/grass_top.png', 'grass_top');
  Result.AddTextureFromFile('Data/log_side.png', 'log_side');
  Result.AddTextureFromFile('Data/iron.png', 'iron');
  Result.AddTextureFromFile('Data/holed_ironplating.png', 'holed_ironplating');
  Result.BuildPage(32);
end;

class constructor TResTexturePage.Create;
begin
  AddToResourceManager;
end;

{ TResCubeVAOParams }

function TResCubeVAOParams.GetHash: Cardinal;
begin
  Result := HashOf(Size) xor HashOf(Texture);
end;

function TResCubeVAOParams.Equals(AOther: TResourceParameter): Boolean;
var
  Other: TResCubeVAOParams;
begin
  Other := TResCubeVAOParams(AOther);
  Result :=
    (Size = Other.Size) and
    (Texture = Other.Texture);
end;

constructor TResCubeVAOParams.Create;
begin
  Size := 1;
end;

{ TResCubeVAO }

class procedure TResCubeVAO.CreateData(var AData: TVAO; AParams: TResCubeVAOParams);
var
  Data: TResModelShader.TData;
  P: TPlane3;
  T: TTexCoord2;
  TexturePage: TTexturePage;
begin
  AData := TVAO.Create(TResModelShader.Data);
  AData.Generate(6 * 6, buStaticDraw);
  AData.Map(baWriteOnly);

  TexturePage := TResTexturePage.Data;
  for P in CubePlanes do
  begin
    Data.Border := TexturePage.GetTexBounds(AParams.Texture, Bounds2(0, 1));
    Data.Normal := P.Normal;
    Data.Tangent := P.D1;
    Data.Bitangent := P.D2;
    for T in QuadTexCoords do
    begin
      Data.Pos := P[T] * AParams.Size;
      Data.TexCoord := Data.Border[T];
      AData.AddVertex(Data);
    end;
    Data.Border := TexturePage.HalfPixelInset(Data.Border);
  end;

  AData.Unmap;
end;

{ TResFloorVAOParams }

function TResFloorVAOParams.GetHash: Cardinal;
begin
  Result := HashOf(FTileSize) xor HashOf(FSize) xor HashOf(Texture);
end;

function TResFloorVAOParams.Equals(AOther: TResourceParameter): Boolean;
var
  Other: TResFloorVAOParams;
begin
  Other := TResFloorVAOParams(AOther);
  Result :=
    (TileSize = Other.TileSize) and
    (Size = Other.Size) and
    (Texture = Other.Texture);
end;

constructor TResFloorVAOParams.Create;
begin
  TileSize := 1;
  Size := 1;
end;

{ TResFloorVAO }

class procedure TResFloorVAO.CreateData(var AData: TVAO; AParams: TResFloorVAOParams);
const
  Plane: TPlane3 = (
    S: (X: 0; Y: 0; Z: 0);
    D1: (X: 0; Y: 0; Z: 1);
    D2: (X: 1; Y: 0; Z: 0)
    );
var
  Data: TResModelShader.TData;
  T: TVector2;
  GridPos: TIntVector2;
  Grid: TIntBounds2;
  Offset: TVector2;
begin
  Grid := IBounds2(AParams.Size);

  AData := TVAO.Create(TResModelShader.Data);
  AData.Generate(6 * Grid.Area, buStaticDraw);

  AData.Map(baWriteOnly);

  Data.Normal := Vec3(0, 1, 0);
  Data.Tangent := Vec3(1, 0, 0);
  Data.Bitangent := Vec3(0, 0, 1);
  Data.Border := TResTexturePage.Data.GetTexBounds(AParams.Texture, Bounds2(0, 1));

  Offset := TVector2(Grid.Size.XY) / 2;
  for GridPos in Grid do
  begin
    for T in QuadTexCoords do
    begin
      Data.Pos := Plane[T];
      Data.Pos.XZ := Data.Pos.XZ + GridPos - Offset;
      Data.TexCoord := Data.Border[T];
      AData.AddVertex(Data);
    end;
  end;

  Data.Border := TResTexturePage.Data.HalfPixelInset(Data.Border);

  AData.Unmap;
end;

end.
