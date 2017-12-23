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
  Pengine.GLProgram,
  Pengine.GLEnums,
  Pengine.ResourceManager,
  Pengine.Light,
  Pengine.Texture,
  Pengine.InputHandler,
  Pengine.IntMaths;

type

  TSkyboxGLProgram = class(TSkyboxGLProgramBase)
  protected
    class function GetFileName: string; override;
  end;

  TModelGLProgram = class(TGLProgramResource)
  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
    class function GetFileName: string; override;
  end;

  TTextureAtlas = class(TParamResource<TTexturePage, TGLObjectParam>)
  protected
    class function CreateData(AParam: TGLObjectParam): TTexturePage; override;
    class procedure ReleaseReferences(AParam: TGLObjectParam); override;
  end;
  
  TCube = class(TRenderable)
  private
    FGLProgramResource: TGLProgramResourceClass;
    FTexturePage: TTexturePage;
    FVAO: TVAO;
    FTexture: string;
    FLocation: TLocation;
    FOcclusionPoints: TArray<TVector3>;
    FChildren: TRefArray<TCube>;
    FRenderableChildren: TLinkedInterfaceArray<IRenderable, TCube>;
    FGLProgram: TGLProgram;

    procedure BuildVAO;

    procedure SetTexture(const Value: string);

  protected
    function GetLocation: TLocation; override;

  public
    constructor Create(AGLState: TGLState; AGLProgramResource: TGLProgramResourceClass);
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
    FSkyboxGLProgram: TGLProgram;
    FModelGLProgram: TGLProgram;
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
  I, J: Integer;
  Cube: TCube;
  SubCube: TCube;
begin
  Context.VSync := False;

  RandSeed := 0;

  FCamera := TSmoothControlledCamera.Create(60, Aspect, 0.01, 100, Self);
  FCamera.OrthoFactor := 0.1;
  FCamera.Location.OffsetZ := 10;
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;

  FSkyboxGLProgram := TSkyboxGLProgram.Make(GLState.ResourceParam);
  FModelGLProgram := TModelGLProgram.Make(GLState.ResourceParam);

  FCamera.AddUniforms(FSkyboxGLProgram);
  FCamera.AddUniforms(FModelGLProgram);

  FSkybox := TSkybox.Create(GLState, TSkyboxGLProgram);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FCubes := TRefArray<TCube>.Create(True);

  FCamera.AddRenderable(FSkybox);

  FLightSystem := TLightSystem.Create(GLState);
  FLightSystem.BindToGLProgram(FModelGLProgram);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := -Vec3(0.3, 1, 0.3);
  FSun.Size := 20;

  FCubes.Capacity := 500;

  for I := 0 to FCubes.Capacity - 1 do
  begin
    Cube := FCubes.Add(TCube.Create(GLState, TModelGLProgram));
    Cube.Location.Pos := TVector3.RandomNormal * 5;

    FCamera.AddRenderable(Cube);
    FSun.AddOccluder(Cube);

    for J := 0 to -1 do
    begin
      SubCube := TCube.Create(GLState, TModelGLProgram);
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
  TSkyboxGLProgram.Release(GLState.ResourceParam);
  TModelGLProgram.Release(GLState.ResourceParam);
end;

procedure TfrmMain.UpdateGL;
var
  Cube: TCube;
begin
  if Context.MustUpdateFPS then
    Caption := Format('FPS: %d', [Context.FPSInt]);
  FCamera.Update;

  if Input.KeyTyped('A') then
    FCubes[Random(FCubes.Count)].Texture := 'stone_bricks.png';
  if Input.KeyTyped('S') then
    FCubes[Random(FCubes.Count)].Texture := 'holed_ironplating.png';
  if Input.KeyTyped('D') then
    FCubes[Random(FCubes.Count)].Texture := 'stone.png';
  if Input.KeyTyped('F') then
    FCubes[Random(FCubes.Count)].Texture := 'wooden_planks.png';

  for Cube in FCubes do
    Cube.Location.Rotate(Vec3(DeltaTime * 10, DeltaTime * 90, DeltaTime * 30));

  FSun.Direction := FSun.Direction.Rotate(Vec3(0, 1, 0), Context.DeltaTime * 20);

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

{ TSkyboxGLProgram }

class function TSkyboxGLProgram.GetFileName: string;
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
  I: Integer;
begin
  FVAO.Generate(6 * 2 * 3, buStaticDraw);
  FVAO.Map(baWriteOnly);

  Data.Border := FTexturePage.GetBounds(Texture);
  Offset := 0; //TVector3.RandomNormal * 5;
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
  Data.Border := FTexturePage.HalfPixelInset(Data.Border);

  FVAO.Unmap;
end;

constructor TCube.Create(AGLState: TGLState; AGLProgramResource: TGLProgramResourceClass);
var
  Corner: TVector3;
begin
  FGLProgramResource := AGLProgramResource;
  FGLProgram := FGLProgramResource.Make(AGLState.ResourceParam);
  FTexturePage := TTextureAtlas.Make(AGLState.ResourceParam);
  FVAO := TVAO.Create(FGLProgram);
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
  TTextureAtlas.Release(FTexturePage.GLState.ResourceParam);
  FGLProgramResource.Release(FVAO.GLState.ResourceParam);
  FVAO.Free;
  inherited;
end;

function TCube.GetLocation: TLocation;
begin
  Result := FLocation;
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

{ TModelGLProgram }

class function TModelGLProgram.GetAttributeOrder: TGLProgram.TAttributeOrder;
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

class function TModelGLProgram.GetFileName: string;
begin
  Result := 'Data\model';
end;

{ TTextureAtlas }

class function TTextureAtlas.CreateData(AParam: TGLObjectParam): TTexturePage;
begin
  Result := TTexturePage.Create(AParam.GLState);
  Result.UniformDefaults(TModelGLProgram.Make(AParam.GLState.ResourceParam));
  Result.AddTextureFromFile('Data\stone_bricks.png');
  Result.AddTextureFromFile('Data\holed_ironplating.png');
  Result.AddTextureFromFile('Data\stone.png');
  Result.AddTextureFromFile('Data\wooden_planks.png');
  Result.BuildPage(32);
end;

class procedure TTextureAtlas.ReleaseReferences(AParam: TGLObjectParam);
begin
  TModelGLProgram.Release(AParam.GLState.ResourceParam);
end;

end.
