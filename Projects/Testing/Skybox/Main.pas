unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.GLState,
  Pengine.GLContext,
  Pengine.ControlledCamera,
  Pengine.Skybox,
  Pengine.Color,
  Pengine.VAO,
  Pengine.Camera,
  Pengine.CollectionInterfaces,
  Pengine.Collections,
  Pengine.Vector,
  Pengine.Shader,
  Pengine.GLEnums,
  Pengine.ResourceManager,
  Pengine.Light,
  Pengine.Texture,
  Pengine.InputHandler,
  Pengine.IntMaths;

type

  TSkyboxShader = class(TSkyboxShaderBase)
  protected
    class function GetShaderSource: string; override;
  end;

  TModelShader = class(TShaderResource)                                   
  protected
    class function GetAttributeOrder: TShader.TAttributeOrder; override;
    class function GetShaderSource: string; override;
  end;

  TTextureAtlas = class(TResource<TTexturePage>)
  protected
    class function CreateData: TTexturePage; override;
  end;
  
  TCube = class(TRenderable)
  private
    FVAO: TVAO;
    FTexture: string;
    FLocation: TLocation;
    FOcclusionPoints: TArray<TVector3>;
    FChildren: TRefArray<TCube>;
    FRenderableChildren: TLinkedInterfaceArray<IRenderable, TCube>;

    procedure BuildVAO;

    procedure SetTexture(const Value: string);

  protected
    function GetLocation: TLocation; override;

  public
    constructor Create(AShader: TShader);
    destructor Destroy; override;

    function CullPoints: IIterable<TVector3>; override;
    function CullRadius: Single; override;

    procedure Render; override;

    property Texture: string read FTexture write SetTexture;
    function RenderableChildren: IIterable<IRenderable>; override;

    procedure AddChild(ARenderable: TCube);

  end;

  TfrmMain = class(TGLForm)
  private
    FSkybox: TSkybox;
    FCamera: TSmoothControlledCamera;
    FCubes: TRefArray<TCube>;
    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure UpdateGL; override;
    procedure RenderGL; override;
    procedure ResizeGL; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
var
  I: Integer;
  Cube: TCube;
  J: Integer;
  SubCube: TCube;
begin
  VSync := True;

  RandSeed := 0;

  FCamera := TSmoothControlledCamera.Create(60, Aspect, 0.01, 100, Self);
  FCamera.OrthoFactor := 0.1;
  FCamera.Location.OffsetZ := 10;
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;

  FCamera.AddUniforms(TSkyboxShader.Data);
  FCamera.AddUniforms(TModelShader.Data);

  FSkybox := TSkybox.Create(State, TSkyboxShader);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FCubes := TRefArray<TCube>.Create(True);

  FCamera.AddRenderable(FSkybox);

  FLightSystem := TLightSystem.Create(Self);
  FLightSystem.BindToShader(TModelShader.Data);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := -Vec3(0.3, 1, 0.3);
  FSun.Size := 20;

  FCubes.Capacity := 100;
  for I := 0 to FCubes.Capacity - 1 do
  begin
    Cube := FCubes.Add(TCube.Create(TModelShader.Data));
    // Cube.Location.Pos := TVector3.RandomNormal * 5;

    FCamera.AddRenderable(Cube);
    FSun.AddOccluder(Cube);

    for J := 0 to -1 do
    begin
      SubCube := TCube.Create(TModelShader.Data);
      SubCube.Location.Offset := TVector3.RandomNormal * 1;
      Cube.AddChild(SubCube);
    end;
  end;
end;

procedure TfrmMain.Finalize;
begin
  FSun.Free;
  FLightSystem.Free;
  FCamera.Free;
  FCubes.Free;
  FSkybox.Free;
end;

procedure TfrmMain.UpdateGL;
var
  Cube: TCube;
begin
  if MustUpdateFPS then
    Caption := Format('FPS: %d', [FPSInt]);
  FCamera.Update;

  if Input.KeyTyped('A') then
    FCubes[Random(FCubes.Count)].Texture := 'stone_bricks.png';
  if Input.KeyTyped('S') then
    FCubes[Random(FCubes.Count)].Texture := 'holed_ironplating.png';
  if Input.KeyTyped('D') then
    FCubes[Random(FCubes.Count)].Texture := 'stone.png';
  if Input.KeyTyped('F') then
    FCubes[Random(FCubes.Count)].Texture := 'wooden_planks.png';

  //for Cube in FCubes do
  //  Cube.Location.Rotate(Vec3(DeltaTime * 10, DeltaTime * 90, DeltaTime * 30));

  FSun.Direction := FSun.Direction.Rotate(Vec3(0, 1, 0), DeltaTime * 20);

end;

procedure TfrmMain.RenderGL;
begin
  FLightSystem.RenderShadows;
  FCamera.Render;
end;

procedure TfrmMain.ResizeGL;
begin
  FCamera.Aspect := Aspect;
end;

{ TSkyboxShader }

class function TSkyboxShader.GetShaderSource: string;
begin
  Result := 'Data\skybox';
end;

{ TCube }

procedure TCube.AddChild(ARenderable: TCube);
begin
  FChildren.Add(ARenderable);
  ARenderable.Location.Parent := FLocation;
end;

procedure TCube.BuildVAO;
type
  TData = record   
    Pos: TVector3;
    TexCoord: TVector2;
    Border: TBounds2;
    Normal: TVector3;
    Tangent: TVector3;
    Bitangent: TVector3;
  end;

var
  P: TPlane3;
  T: TTexCoord2;
  Data: TData;
  Offset: TVector3;
begin
  FVAO.Generate(6 * 2 * 3, buStaticDraw);
  FVAO.Map(baWriteOnly);

  Data.Border := TTextureAtlas.Data.GetBounds(Texture);
  Offset := TVector3.RandomNormal * 5;
  for P in CubePlanes do
  begin
    Data.Normal := P.Normal;
    Data.Tangent := P.D1;
    Data.Bitangent := P.D2;
    for T in QuadTexCoords do
    begin
      Data.Pos := P[T] - 0.5 + Offset;
      Data.Pos := Data.Pos;
      Data.TexCoord := Data.Border[T];

      FVAO.AddVertex(Data);
    end;
  end;
  Data.Border := TTextureAtlas.Data.HalfPixelInset(Data.Border);

  FVAO.Unmap;
end;

constructor TCube.Create(AShader: TShader);
var
  Corner: TVector3;
begin
  FVAO := TVAO.Create(AShader);
  FLocation := TLocation.Create;
  {
  FOcclusionPoints := TArray<TVector3>.Create;
  FOcclusionPoints.Capacity := TBounds3.CornerCount;
  for Corner in Bounds3(-0.5, 0.5).GetCorners do
    FOcclusionPoints.Add(Corner);
  }
  FChildren := TRefArray<TCube>.Create(True);
  FRenderableChildren := TLinkedInterfaceArray<IRenderable, TCube>.Create(FChildren);
  Texture := 'stone_bricks.png';
end;

destructor TCube.Destroy;
begin
  FOcclusionPoints.Free;
  FRenderableChildren.Free;
  FChildren.Free;
  FLocation.Free;
  FVAO.Free;
  inherited;
end;

function TCube.GetLocation: TLocation;
begin
  Result := nil//FLocation;
end;

function TCube.CullPoints: IIterable<TVector3>;
begin
  Result := FOcclusionPoints;
end;

function TCube.CullRadius: Single;
begin
  Result := Sqrt(0.75);
end;

procedure TCube.Render;
begin
  FVAO.Render;
end;

function TCube.RenderableChildren: IIterable<IRenderable>;
begin
  Result := FRenderableChildren;
end;

procedure TCube.SetTexture(const Value: string);
begin
  if FTexture = Value then
    Exit;
  FTexture := Value;
  BuildVAO;
end;

{ TModelShader }

class function TModelShader.GetAttributeOrder: TShader.TAttributeOrder;
begin
  Result := [
    'vpos',
    'vtexcoord',
    'vborderlow',
    'vborderhigh',
    'vnormal',
    'vtangent',
    'vbitangent'
    ];
end;

class function TModelShader.GetShaderSource: string;
begin
  Result := 'Data\model';
end;

{ TTextureAtlas }

class function TTextureAtlas.CreateData: TTexturePage;
begin
  Result := TTexturePage.Create;
  Result.UniformDefaults(TModelShader.Data);
  Result.AddTextureFromFile('Data\stone_bricks.png');
  Result.AddTextureFromFile('Data\holed_ironplating.png');
  Result.AddTextureFromFile('Data\stone.png');
  Result.AddTextureFromFile('Data\wooden_planks.png');
  Result.BuildPage(32);
end;

end.
