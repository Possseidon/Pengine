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

  Pengine.EventHandling,
  Pengine.Hasher,
  Pengine.TimeManager,
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
  Pengine.TextureAtlas,
  Pengine.InputHandler,
  Pengine.IntMaths,
  Pengine.GLGame,
  Pengine.GLForm;

type

  TSkyboxGLProgram = class(TSkyboxGLProgramBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

  TModelGLProgram = class(TGLProgramResource)
  public type

    TData = record
      Pos: TVector3;
      TexCoord: TVector2;
      Border: TBounds2;
      Normal: TVector3;
      Tangent: TVector3;
      Bitangent: TVector3;
    end;

  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TTextureMap = class(TParamResource<TTextureAtlas, TGLState>)
  private
    FGLProgram: IResource<TGLProgram>;

  public
    function CreateData(AGLState: TGLState): TTextureAtlas; override;

  end;

  TCube = class(TRenderable)
  public type

    TVAO = TVAOMutable<TModelGLProgram.TData>;

  private
    FGLProgramResource: TGLProgramResourceClass;
    FTextureAtlas: IResource<TTextureAtlas>;
    FTextureTile: TTextureAtlas.TTile;
    FVAO: TVAO;
    FTexture: string;
    FLocation: TLocation3;
    FOcclusionPoints: TArray<TVector3>;
    FChildren: TRefArray<TCube>;
    FRenderableChildren: TLinkedInterfaceArray<IRenderable, TCube>;
    FGLProgram: IResource<TGLProgram>;
    FVAOChanged: Boolean;

    procedure VAOChanged;
    procedure BuildVAO;
    procedure UpdateVAO;

    procedure SetTexture(const Value: string);
    function GetTextureTile: TTextureAtlas.TTile;

  protected
    function GetLocation: TLocation3; override;

  public
    constructor Create(AGLState: TGLState; AGLProgramResource: TGLProgramResourceClass);
    destructor Destroy; override;

    function CullPoints: IIterable<TVector3>; override;
    function CullRadius: Single; override;

    procedure Render; override;

    property Texture: string read FTexture write SetTexture;
    property TextureTile: TTextureAtlas.TTile read GetTextureTile;

    function RenderableChildren: IIterable<IRenderable>; override;

    procedure AddChild(ARenderable: TCube);

  end;

  TfrmMain = class(TGLForm)
  private
    FSkyboxGLProgram: IResource<TGLProgram>;
    FModelGLProgram: IResource<TGLProgram>;
    FSkybox: TSkybox;
    FCamera: TSmoothControlledCamera;
    FCubes: TRefArray<TCube>;
    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;
    FTextureAtlas: IResource<TTextureAtlas>;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure GameUpdate;
    procedure UpdateFPS;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
var
  Cube: TCube;
  Pos: TIntVector3;
begin
  Game.OnUpdate.Add(GameUpdate);
  Game.Timer.OnFPSUpdate.Add(UpdateFPS);

  Context.VSync := False;

  // Context.GLDebugRaiseLevels := [dmsLow, dmsMedium, dmsHigh];

  // Context.Samples := Context.MaxSamples;

  RandSeed := 0;

  FSkyboxGLProgram := TSkyboxGLProgram.Get(GLState);
  FModelGLProgram := TModelGLProgram.Get(GLState);

  FLightSystem := TLightSystem.Create(Context);
  FLightSystem.BindToGLProgram(FModelGLProgram.Data);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := -Vec3(0.3, 1, 0.3);
  FSun.Size := 40;

  FCamera := TSmoothControlledCamera.Create(Game, 60, 0.01, 100);
  FCamera.Location.OffsetZ := 9;
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;

  FCamera.AddUniforms(FSkyboxGLProgram.Data);
  FCamera.AddUniforms(FModelGLProgram.Data);

  FSkybox := TSkybox.Create(FSkyboxGLProgram.Data);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FCubes := TRefArray<TCube>.Create(True);

  FCamera.AddRenderable(FSkybox);

  for Pos in IBounds3I(-5, 5) do
  begin
    if not(TVector3(Pos).Length in Bounds1(6, 7) + Random) then
      Continue;

    Cube := FCubes.Add(TCube.Create(GLState, TModelGLProgram));
    Cube.Location.Pos := Pos;

    FCamera.AddRenderable(Cube);
    FSun.AddOccluder(Cube);
  end;

  FTextureAtlas := TTextureMap.Get(GLState);

  Game.OnRender.Add(FLightSystem.RenderShadows);

end;

procedure TfrmMain.UpdateFPS;
begin
  Caption := Format('FPS: %d', [Context.FPSInt]);
end;

procedure TfrmMain.Finalize;
begin
  FSun.Free;
  FLightSystem.Free;
  FCamera.Free;
  FCubes.Free;
  FSkybox.Free;
end;

procedure TfrmMain.GameUpdate;
begin
  if Input.KeyDown('A') then
    FCubes[Random(FCubes.Count)].Texture := 'stone_bricks';
  if Input.KeyDown('S') then
    FCubes[Random(FCubes.Count)].Texture := 'holed_ironplating';
  if Input.KeyDown('D') then
    FCubes[Random(FCubes.Count)].Texture := 'stone';
  if Input.KeyDown('F') then
    FCubes[Random(FCubes.Count)].Texture := 'wooden_planks';

  // for Cube in FCubes do
  // Cube.Location.Rotate(Cube.Location.Pos * DeltaTime * 10);

  FSun.Direction := FSun.Direction.Rotate(Vec3(0, 1, 0), Context.DeltaTime * 20);

  if Input.KeyTyped('M') then
    Context.MultiSampled := not Context.MultiSampled;
end;

{ TSkyboxGLProgram }

class procedure TSkyboxGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AName := 'SKYBOX';
  AResource := True;
end;

{ TCube }

procedure TCube.AddChild(ARenderable: TCube);
begin
  FChildren.Add(ARenderable);
  ARenderable.Location.Parent := FLocation;
end;

procedure TCube.BuildVAO;
var
  P: TPlane3;
  T: TTexCoord2;
  Data: TVAO.TData;
  Border: TBounds2;
  Offset: TVector3;
  Writer: TVAO<TModelGLProgram.TData>.IWriter;
begin
  FVAO.VBO.Generate(6 * 2 * 3, buStaticDraw);

  Writer := FVAO.VBO.Map;

  Border := TextureTile.Bounds;
  Data.Border := TextureTile.BoundsHalfPixelInset;
  Offset := 0;

  for P in CubePlanes do
  begin
    Data.Normal := P.Normal;
    Data.Tangent := P.DX;
    Data.Bitangent := P.DY;
    for T in QuadTexCoords do
    begin
      Data.Pos := P[T] - 0.5 + Offset;
      Data.TexCoord := Border[T];
      Writer.AddToBuffer(Data);
    end;
  end;

  FVAOChanged := False;
end;

constructor TCube.Create(AGLState: TGLState; AGLProgramResource: TGLProgramResourceClass);
begin
  FGLProgramResource := AGLProgramResource;
  FGLProgram := FGLProgramResource.Get(AGLState);
  FTextureAtlas := TTextureMap.Get(AGLState);
  FTextureAtlas.Data.OnChanged.Add(VAOChanged);
  FVAO := TVAO.Create(FGLProgram.Data);
  FLocation := TLocation3.Create;
  {
    FOcclusionPoints := TArray<TVector3>.Create;
    FOcclusionPoints.Capacity := TBounds3.CornerCount;
    for Corner in Bounds3(-0.5, 0.5).GetCorners do
    FOcclusionPoints.Add(Corner);
  }
  FChildren := TRefArray<TCube>.Create(True);
  FRenderableChildren := TLinkedInterfaceArray<IRenderable, TCube>.Create(FChildren);
  Texture := 'stone_bricks';
  BuildVAO;
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

function TCube.GetLocation: TLocation3;
begin
  Result := FLocation;
end;

function TCube.GetTextureTile: TTextureAtlas.TTile;
begin
  if FTextureTile = nil then
    FTextureTile := FTextureAtlas.Data[Texture];
  Result := FTextureTile;
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
  if FVAOChanged then
    UpdateVAO;
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
  FTextureTile := nil;
  VAOChanged;
end;

procedure TCube.UpdateVAO;
var
  Border: TBounds2;
  Writer: TVBO<TModelGLProgram.TData>.IWriter;
begin
  Writer := FVAO.VBO.Map;

  Border := TextureTile.Bounds;
  for var D := Low(TBasicDir3) to High(TBasicDir3) do
  begin
    for var T in QuadTexCoords do
    begin
      Writer.Current.Border := TextureTile.BoundsHalfPixelInset;
      Writer.Current.TexCoord := Border[T];
      Writer.NextBufferPos;
    end;
  end;

  FVAOChanged := False;
end;

procedure TCube.VAOChanged;
begin
  FVAOChanged := True;
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

class procedure TModelGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AName := 'MODEL';
  AResource := True;
end;

{ TTextureMap }

function TTextureMap.CreateData(AGLState: TGLState): TTextureAtlas;
begin
  Result := TTextureAtlas.Create(AGLState);

  FGLProgram := TModelGLProgram.Get(AGLState);
  Result.Uniform(FGLProgram.Data.UniformSampler('diffusemap'));
  Result.AddSubType('nmap', FGLProgram.Data.UniformSampler('normalmap'), ColorRGB(0.5, 1, 0.5));
  Result.AddSubType('smap', FGLProgram.Data.UniformSampler('specularmap'), ColorRGB(0, 0, 0));

  Result.Texture.MagFilter := magNearest;
  Result.SubTypes[0].Texture.MagFilter := magNearest;
  Result.SubTypes[1].Texture.MagFilter := magNearest;

  Result.AddFromResource('stone_bricks', 'STONE_BRICKS');
  Result.AddFromResource('holed_ironplating', 'HOLED_IRONPLATING');
  Result.AddFromResource('stone', 'STONE');
  Result.AddFromResource('wooden_planks', 'WOODEN_PLANKS');

end;

end.
