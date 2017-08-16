unit Resources;

interface

uses
  ResourceManager, Shaders, TextureManager, VAOManager, VectorGeometry, GLEnums, SkyDome, Lists;

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
    class function GetAttributeOrder: TShaderAttributeOrder; override;

  public
    class constructor Create;

  end;

  { TResSkyDomeShader }

  TResSkyDomeShader = class abstract(TSkyDomeShaderBase)
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
    function GetHash(ARange: Integer): Integer; override;
    function EqualTo(AOther: TResourceParameter): Boolean; override;
   
  public
    constructor Create; override;

    property Size: Single read FSize write FSize;
    property Texture: string read FTexture write FTexture;

  end;

  TResCubeVAO = class(TParamResource<TVAO, TResCubeVAOParams>)
  protected
    class procedure CreateData(var AData: TVAO; AParams: TResCubeVAOParams); override;
  end;

implementation

{ TResModelShader }

class function TResModelShader.GetShaderSource: string;
begin
  Result := 'Data/model';
end;

class function TResModelShader.GetAttributeOrder: TShaderAttributeOrder;
begin
  Result := TShaderAttributeOrder.Create(
    'vpos',
    'vtexcoord',
    'vnormal',
    'vtangent',
    'vbitangent',
    'vborderlow',
    'vborderhigh'
    );
end;

class constructor TResModelShader.Create;
begin
  AddToResourceManager;
end;

{ TResSkyDomeShader }

class function TResSkyDomeShader.GetShaderSource: string;
begin
  Result := 'Data/skydome';
end;

class constructor TResSkyDomeShader.Create;
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

function TResCubeVAOParams.GetHash(ARange: Integer): Integer;
begin
  Result :=
    (Lists.GetHash(Size, ARange) xor
    Lists.GetHash(Texture, ARange)) mod ARange;
end;

function TResCubeVAOParams.EqualTo(AOther: TResourceParameter): Boolean;
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
    Data.Border := TexturePage.GetTexBounds(AParams.Texture, FRange2(0, 1));
    Data.Normal := P.Normal;
    Data.Tangent := P.DVS;
    Data.Bitangent := P.DVT;
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


end.
