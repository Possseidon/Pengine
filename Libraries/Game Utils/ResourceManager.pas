unit ResourceManager;

interface

uses
  Shaders, TextureManager, VAOManager, GLEnums, VectorGeometry, Lists, SysUtils;

type
  TBaseVAOType = (vtCube, vtPlane);

  TData = record
    Pos: TVector3;
    TexCoord: TTexCoord2;
    Normal: TVector3;
    Tangent: TVector3;
    Bitangent: TVector3;
    Border: TBounds2;
  end;

  { TResourceBase }

  TResourceBase = class abstract
  private
    class procedure Load; virtual; abstract;
    class procedure Unload; virtual; abstract;
  end;

  TResourceClass = class of TResourceBase;

  { TResource }

  TResource<T: class> = class abstract(TResourceBase)
  private class var
    FData: T;

    class procedure Load; override;
    class procedure Unload; override;

  protected
    class procedure CreateData(var AData: T); virtual; abstract;

    /// <summary>Call this in the class constructor of each non-abstract sub-class</summary>
    class procedure AddToResourceManager;

  public
    /// <summary>Query the Data behind the Resource</summary>
    class function Data: T;

  end;

  { TResourceParameter }

  TResourceParameter = class abstract
  public
    constructor Create; virtual;
  end;

  { TParamResoruce<T> }

  // TODO: Change this, so that it won't create two instances for same parameters
  //       by adding an "Equals" and a reference counter for each parameter and such
  //       (create HashMap<TParam, TParamRes>, add refcounter to TParamRes and Destroy -> Release)

  /// <summary>
  /// An alternative to TResource<T>, which can contain Parameters.
  /// Create a new Resource of this type, using Make and Free it after use.
  /// </summary>
  TParamResource<T: class; P: TResourceParameter> = class abstract
  protected
    class procedure CreateData(var AData: T; AParam: P); virtual; abstract;
  public
    /// <summary>Create a new parametrized resource</summary>
    /// <remarks>In contrary to static Resources, this has to get freed after use</remarks>
    class function Make(AParams: P): T; overload;
    /// <summary>Create a new resource with default parameters</summary>
    /// <remarks>In contrary to static Resources, this has to get freed after use</remarks>
    class function Make: T; overload;
  end;

  { TResourceManager }

  TResourceManager = class
  private
    class procedure Add(AResourceClass: TResourceClass);

  private class var
    FResourceClasses: TGenericArray<TResourceClass>;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure Init;
    class procedure Finalize;

  end;

  // TODO: put this in different units, then they also won't see soem private stuff they don't need

  TResModelShader = class(TResource<TShader>)
  protected
    class procedure CreateData(var AData: TShader); override;

  public
    class constructor Create;

  end;

  TResTexturePage = class(TResource<TTexturePage>)
  protected
    class procedure CreateData(var AData: TTexturePage); override;

  public
    class constructor Create;
  end;

  TResCubeVAOParams = class(TResourceParameter)
  private
    FSize: Single;
  public
    constructor Create; override;

    property Size: Single read FSize write FSize;
    // TODO: Texture
  end;

  TResCubeVAO = class(TParamResource<TVAO, TResCubeVAOParams>)
  protected
    class procedure CreateData(var AData: TVAO; AParams: TResCubeVAOParams); override;
  end;

implementation

{ TResource<T> }

class procedure TResource<T>.Load;
begin
  Data;
end;

class procedure TResource<T>.Unload;
begin
  FData.Free;
end;

class procedure TResource<T>.AddToResourceManager;
begin
  TResourceManager.Add(Self);
end;

class function TResource<T>.Data: T;
begin
  if FData = nil then
    CreateData(FData);
  Result := FData;
end;

{ TResourceParameter }

constructor TResourceParameter.Create;
begin
  // nothing by default
end;

{ TParamResource<T, P> }

class function TParamResource<T, P>.Make(AParams: P): T;
begin
  CreateData(Result, AParams);
  AParams.Free;
end;

class function TParamResource<T, P>.Make: T;
var
  Params: P;
begin
  Params := P.Create;
  CreateData(Result, Params);
  Params.Free;
end;

{ TResourceManager }

class constructor TResourceManager.Create;
begin
  FResourceClasses := TGenericArray<TResourceClass>.Create;
end;

class destructor TResourceManager.Destroy;
begin
  FResourceClasses.Free;
end;

class procedure TResourceManager.Init;
var
  ResourceClass: TResourceClass;
begin
  for ResourceClass in FResourceClasses do
    ResourceClass.Load;
end;

class procedure TResourceManager.Finalize;
var
  ResourceClass: TResourceClass;
begin
  for ResourceClass in FResourceClasses do
    ResourceClass.Unload;
end;

class procedure TResourceManager.Add(AResourceClass: TResourceClass);
begin
  FResourceClasses.Add(AResourceClass);
end;

// TODO: put this in different unit

{ TResModelShader }

class procedure TResModelShader.CreateData(var AData: TShader);
const
  Attributes: array [0 .. 6] of AnsiString = (
    'vpos',
    'vtexcoord',
    'vnormal',
    'vtangent',
    'vbitangent',
    'vborderlow',
    'vborderhigh'
    );
begin
  AData := TShader.Create;
  AData.LoadFromFile('Data/model');
  AData.SetAttributeOrder(Attributes);
end;

class constructor TResModelShader.Create;
begin
  AddToResourceManager;
end;

{ TResTexturePage }

class procedure TResTexturePage.CreateData(var AData: TTexturePage);
begin
  AData := TTexturePage.Create;
  AData.UniformDefaults(TResModelShader.Data);
  AData.AddTextureFromFile('Data/stone_bricks.png', 'stone_bricks');
  AData.AddTextureFromFile('Data/grass_top.png', 'grass_top');
  AData.AddTextureFromFile('Data/log_side.png', 'log_side');
  AData.BuildPage(32);
end;

class constructor TResTexturePage.Create;
begin
  AddToResourceManager;
end;

{ TResCubeVAOParams }

constructor TResCubeVAOParams.Create;
begin
  Size := 1;
end;

{ TResCubeVAO }

class procedure TResCubeVAO.CreateData(var AData: TVAO; AParams: TResCubeVAOParams);
var
  Data: TData;
  P: TPlane3;
  T: TTexCoord2;
begin
  AData := TVAO.Create(TResModelShader.Data);
  AData.Generate(6 * 6, buStaticDraw);
  AData.Map(baWriteOnly);

  for P in CubePlanes do
  begin
    Data.Border := TResTexturePage.Data.GetTexBounds('stone_bricks', FRange2(0, 1));
    Data.Normal := P.Normal;
    Data.Tangent := P.DVS;
    Data.Bitangent := P.DVT;
    for T in QuadTexCoords do
    begin
      Data.Pos := P[T] * AParams.Size;
      Data.TexCoord := Data.Border[T];
      AData.AddVertex(Data);
    end;
    Data.Border := TResTexturePage.Data.HalfPixelInset(Data.Border);
  end;

  AData.Unmap;
end;

end.
