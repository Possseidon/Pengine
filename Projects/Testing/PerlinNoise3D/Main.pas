unit Main;

interface

uses
  System.SysUtils,
  System.Classes,

  Pengine.ResourceManager,
  Pengine.GLForm,
  Pengine.ControlledCamera,
  Pengine.Noise,
  Pengine.VAO,
  Pengine.Light,
  Pengine.Skybox,
  Pengine.GLProgram,
  Pengine.TextureAtlas,
  Pengine.Vector,
  Pengine.Color,
  Pengine.GLEnums,
  Pengine.ICollections,
  Pengine.IntMaths,
  Pengine.Random;

type

  TGradient3 = class(TInterfacedObject, IGradientSource3)
  private
    FData: array of array of TVector3;

    function GetGradient(APos: TIntVector3): TVector3;
    function GetBounds: TIntBounds3;

  public
    property Gradients[APos: TIntVector3]: TVector3 read GetGradient; default;
    property Bounds: TIntBounds3 read GetBounds;
    function HasBounds: Boolean;

  end;

  TSkyboxGLProgram = class(TSkyboxGLProgramBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TModelGLProgram = class(TGLProgramResource)
  public type

    TData = record
      Pos: TVector3;
      TexCoord: TTexCoord2;
      Border: TBounds2;
      Normal: TVector3;
      Tangent: TVector3;
      Bitangent: TVector3;
    end;

  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TfrmMain = class(TGLForm)
  private
    FSkyboxGLProgram: IResource<TGLProgram>;
    FModelGLProgram: IResource<TGLProgram>;
    FCamera: TSmoothControlledCamera;
    FSkybox: TSkybox;
    FVAO: TVAOMutable<TModelGLProgram.TData>;
    FTextureAtlas: TTextureAtlas;
    FStoneTexture: TTexTile;
    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;
    FCameraLight: TPointLight;
    FNoise: TPerlinNoise3;

    procedure UpdateFPS;
    procedure GameUpdate;

    procedure BuildVAO;

  public
    procedure Init; override;
    procedure Finalize; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TSkyboxGLProgram }

class procedure TSkyboxGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := False;
  if AResource then
    AName := 'SKYBOX'
  else
    AName := 'Data/skybox';
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
  AResource := False;
  if AResource then
    AName := 'MODEL'
  else
    AName := 'Data/model';
end;

{ TfrmMain }

procedure TfrmMain.UpdateFPS;
begin
  Caption := Format('Perlin Noise - FPS: %.0f', [Game.FPS]);
end;

procedure TfrmMain.GameUpdate;
begin
  FCameraLight.Position := FCamera.Location.RealPosition;
end;

procedure TfrmMain.BuildVAO;
var
  Map: array of array of array of Byte;
  VBOData: IList<TModelGLProgram.TData>;
  Plane: TPlane3;
  TexCoord: TTexCoord2;
  I: Integer;
  P: TIntVector3;
  Size: TIntVector3;
  Dir: TBasicDir3;
  CheckPos: TIntVector3;
  Data: TModelGLProgram.TData;
begin
  Size := 128;

  SetLength(Map, Size.X, Size.Y, Size.Z);

  for P in Size do
    if FNoise[TVector3(P) / Size * 4] < 0.3 then
      Map[P.X, P.Y, P.Z] := 1;

  VBOData := TList<TModelGLProgram.TData>.Create;

  for P in Size do
  begin
    for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    begin
      CheckPos := P + Vec3Dir[Dir];
      if Map[P.X, P.Y, P.Z] = 0 then
        Continue;
      if (CheckPos in Size) and (Map[CheckPos.X, CheckPos.Y, CheckPos.Z] <> 0) then
        Continue;
      Plane := CubePlanes[Dir];
      for TexCoord in QuadTexCoords do
      begin
        Data.Pos := (Plane[TexCoord] + P - TVector3(Size) / 2) * 0.1;
        Data.TexCoord := FStoneTexture.Bounds[TexCoord];
        Data.Border := FStoneTexture.BoundsHalfPixelInset;
        Data.Normal := Plane.Normal;
        Data.Tangent := Plane.DX;
        Data.Bitangent := Plane.DY;
        VBOData.Add(Data);
      end;
    end;
  end;

  FVAO.VBO.Generate(VBOData.Count, buStaticDraw, VBOData.DataPointer);
end;

procedure TfrmMain.Init;
begin
  FModelGLProgram := TModelGLProgram.Get(GLState);
  FSkyboxGLProgram := TSkyboxGLProgram.Get(GLState);

  FCamera := TSmoothControlledCamera.Create(Game, 60, 0.01, 100);
  FVAO := TVAOMutable<TModelGLProgram.TData>.Create(FModelGLProgram.Data);
  FSkybox := TSkybox.Create(FSkyboxGLProgram.Data);
  FTextureAtlas := TTextureAtlas.Create(GLState);
  FLightSystem := TLightSystem.Create(Context);
  FNoise := TPerlinNoise3.Create;
  FNoise.GradientSource := TGradient3.Create;

  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.8), -90);
  FSkybox.AddStripe(ColorRGB(0.5, 0.7, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.5, 0.8, 0.9), 90);

  FCamera.AddUniforms(FSkyboxGLProgram.Data);
  FCamera.AddUniforms(FModelGLProgram.Data);

  FCamera.AddRenderable(FSkybox);
  FCamera.AddRenderable(FVAO);

  FCamera.Location.Offset := Vec3(0, 0, 2);
  FCamera.Location.TurnAngle := 30;
  FCamera.Location.PitchAngle := -20;

  FTextureAtlas.Uniform(FModelGLProgram.Data.UniformSampler('diffusemap'));
  FTextureAtlas.AddSubType('smap', FModelGLProgram.Data.UniformSampler('specularmap'), 0);
  FTextureAtlas.AddSubType('nmap', FModelGLProgram.Data.UniformSampler('normalmap'), ColorRGB(0.5, 0.5, 1.0));
  FTextureAtlas.Texture.MagFilter := magLinear;
  FTextureAtlas.SubTypes[0].Texture.MagFilter := magLinear;
  FTextureAtlas.SubTypes[1].Texture.MagFilter := magLinear;

  FStoneTexture := FTextureAtlas.AddFromFile('stone', 'Data/stone.png');

  FTextureAtlas.Generate;

  FLightSystem.BindToGLProgram(FModelGLProgram.Data);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := Vec3(-0.2, -1, -0.3);
  FSun.AddOccluder(FVAO);
  FSun.Size := 50;

  FCameraLight := TPointLight.Create(FLightSystem);
  
  Game.OnUpdate.Add(GameUpdate);
  Game.OnRender.Add(FLightSystem.RenderShadows);
  Game.Timer.OnFPSUpdate.Add(UpdateFPS);

  BuildVAO;
end;

procedure TfrmMain.Finalize;
begin
  FNoise.Free;
  FCameraLight.Free;
  FSun.Free;
  FLightSystem.Free;
  FTextureAtlas.Free;
  FVAO.Free;
  FSkybox.Free;
  FCamera.Free;
end;

{ TGradient3 }

function TGradient3.GetBounds: TIntBounds3;
begin
  Result := IBounds3(0);
end;

function TGradient3.GetGradient(APos: TIntVector3): TVector3;
var
  Rand: TRandom;
  O: Single;
  U: Single;
begin
  Rand := TRandom.FromSeed(TDefault<TIntVector3>.Hash(APos));
  O := Rand.NextSingle * 2 * Pi;
  U := Rand.NextSingle * 2 - 1;
  Result.X := Sqrt(1 - Sqr(U)) * Sin(O);
  Result.Y := Sqrt(1 - Sqr(U)) * Cos(O);
  Result.Z := U;
end;

function TGradient3.HasBounds: Boolean;
begin
  Result := False;
end;

end.
