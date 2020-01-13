unit Main;

interface

uses
  Winapi.Windows,

  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Threading,
  System.Math,
  Vcl.Controls,

  Pengine.InputHandler,
  Pengine.DebugConsole,
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
  Pengine.Random,
  Pengine.MarchingCubes;

type

  TGradient3 = class(TInterfacedObject, IGradientSource3)
  private
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
      TexFactors: TVector3;
      TexBorders: array [0 .. 2] of TBounds2;
      Normal: TVector3;
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
    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;
    FCameraLight: TPointLight;
    FNoise: TPerlinNoise3;
    FTriFactorsCubicUniform: TGLProgram.TUniform<Integer>;
    FTexFactorsCubicUniform: TGLProgram.TUniform<Integer>;
    FMap: array of array of array of Byte;
    FSize: TIntVector3;

    procedure UpdateFPS;
    procedure GameUpdate;

    procedure BuildVAO;

    function GetMap(APos: TIntVector3): Byte;
    procedure SetMap(APos: TIntVector3; const Value: Byte);

    procedure Build(ABreak: Boolean);

    procedure Generate;

    property Map[APos: TIntVector3]: Byte read GetMap write SetMap;

  public
    procedure Init; override;
    procedure Finalize; override;

  end;

var
  frmMain: TfrmMain;

implementation

uses
  dglOpenGL;

{$R *.dfm}

{ TSkyboxGLProgram }

class procedure TSkyboxGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
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
    'vtex_factors',
    'vtex_border0',
    'vtex_border1',
    'vtex_border2',
    'vnormal'];
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
  if Input.KeyDown('W') then
    FCamera.Location.Move(DeltaTime);
  if Input.KeyDown('S') then
    FCamera.Location.Move(-DeltaTime);
  if Input.KeyDown('D') then
    FCamera.Location.Slide(DeltaTime);
  if Input.KeyDown('A') then
    FCamera.Location.Slide(-DeltaTime);

  if Input.KeyTyped('Q') then
    FTriFactorsCubicUniform.Value := (FTriFactorsCubicUniform.Value + 1) mod 3;
  if Input.KeyTyped('E') then
    FTexFactorsCubicUniform.Value := (FTexFactorsCubicUniform.Value + 1) mod 3;

  {
    if not FCamera.Moving then
    begin
    if Input.ButtonPressed(mbLeft) then
    Build(True);
    if Input.ButtonPressed(mbRight) then
    Build(False);
    end;
  }

  FCameraLight.Position := FCamera.Location.RealPosition;
  // FSun.Direction := FSun.Direction.Rotate(Vec3(0, 1, 0), DeltaTime * 2);
end;

procedure TfrmMain.BuildVAO;
var
  VBOData: IList<TModelGLProgram.TData>;
  Normals: IMap<TVector3, TVector3>;
  P: TIntVector3;
  Corners: TCorners3;
  Corner: TCorner3;
  PlaneInfo: TMarchingCubes.TPlaneInfo;
  I: Integer;
  Stopwatch: TStopwatch;
  NormalPair: TPair<TVector3, TVector3>;
  Texture: Integer;
  Data: TModelGLProgram.TData;
  Plane: TPlane3;
  Normal: TVector3;
begin
  Stopwatch := TStopwatch.StartNew;

  Normals := TMap<TVector3, TVector3>.Create;
  VBOData := TList<TModelGLProgram.TData>.Create;
  for P in IBounds3(-1, FSize) do
  begin
    // build set of all adjacent block directions
    Corners := [];
    for Corner := Low(TCorner3) to High(TCorner3) do
    begin
      Texture := Map[P + Corner3Pos[Corner]];
      if Texture <> 0 then
        Include(Corners, Corner);
    end;

    for PlaneInfo in TMarchingCubes.GetPlanes(Corners) do
    begin
      Plane := PlaneInfo.MakePlane;
      for I := 0 to 2 do
      begin
        Data.TexBorders[I] := FTextureAtlas[Map[P + Corner3Pos[PlaneInfo.Points[I].Corner]].ToString].Bounds;
        Data.TexFactors := 0;
        Data.TexFactors[TCoordAxis3(I + Ord(caX))] := 1;
        Data.Pos := (Plane[TriangleTexCoords[I]] + P - TVector3(FSize) / 2) * 0.1;
        if Normals.Get(Data.Pos, Normal) then
          Normals[Data.Pos] := Normal + Plane.Normal
        else
          Normals[Data.Pos] := Plane.Normal;
        // Data.Normal := Plane.Normal;
        VBOData.Add(Data);
      end;
    end;

  end;

  for NormalPair in Normals do
    Normals[NormalPair.Key] := NormalPair.Value.Normalize;

  TParallel.For(0, VBOData.MaxIndex,
    procedure(I: Integer)
    var
      Data: TModelGLProgram.TData;
    begin
      Data := VBOData[I];
      Data.Normal := Normals[Data.Pos];
      VBOData[I] := Data;
    end);

  Writeln('Filling VBO: ', Stopwatch.Elapsed.ToString);
  Writeln('Triangles: ', VBOData.Count);

  Stopwatch := TStopwatch.StartNew;
  FVAO.VBO.Generate(VBOData.Count, buDynamicDraw, VBOData.DataPointer);
  Writeln('Sending VBO to GPU: ', Stopwatch.Elapsed.ToString);

  FLightSystem.RenderShadows;
end;

function TfrmMain.GetMap(APos: TIntVector3): Byte;
begin
  if APos in FSize then
    Exit(FMap[APos.X, APos.Y, APos.Z]);
  Result := 0;
end;

procedure TfrmMain.SetMap(APos: TIntVector3; const Value: Byte);
begin
  if APos in FSize then
    FMap[APos.X, APos.Y, APos.Z] := Value;
end;

procedure TfrmMain.Build(ABreak: Boolean);
var
  Raycaster: TBlock3Raycaster;
begin
  Raycaster := TBlock3Raycaster.Create(FSize);
  Raycaster.Start(FCamera.GetCursorLine(Input.MousePos));
  while Raycaster.Next do
    if Map[Raycaster.Current] <> 0 then
      Break;

  if ABreak then
    Map[Raycaster.Current] := 0
  else
    Map[Raycaster.Current + Vec3Dir[Raycaster.LastDirection]] := 1;

  Raycaster.Free;
  BuildVAO;
end;

procedure TfrmMain.Generate;
var
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;

  FSize := 128;
  SetLength(FMap, FSize.X, FSize.Y, FSize.Z);

  TParallel.For(0, FSize.Volume - 1,
    procedure(I: Integer)
    var
      P: TIntVector3;
      D: TVector3;
      V: Single;
    begin
      P := IVec3(I mod FSize.X, I div FSize.X mod FSize.Y, I div FSize.X div FSize.Y);
      D := TVector3(P).VectorTo(64).Normalize;
      {
        if FNoise[Vec3(P.X * 5 / FSize.X, 0, P.Z * 7 / FSize.Z)] > (P.Y / FSize.Y * 4 - 3.5) then
        Map[P] := 2;
      }
      if TVector3(P).DistanceTo(64) / 64 < FNoise[D * 3] * 0.25 + 0.75 then
      begin
        V := FNoise[D * 4];
        if V < -0.2 then
          Map[P] := 1
        else if V < -0.1 then
          Map[P] := 2
        else if V < 0 then
          Map[P] := 3
        else if V < 0.1 then
          Map[P] := 4
        else if V < 0.2 then
          Map[P] := 5
        else
          Map[P] := 6;
      end;

      if (FNoise[TVector3(P * 3 + 51) / FSize * 2] + 0.2 * FNoise[TVector3(P) / FSize * 7]) > 0.2 then
        Map[P] := 0;

    end);
  Writeln('Generating Map: ', Stopwatch.Elapsed.ToString);

  BuildVAO;
end;

procedure TfrmMain.Init;
begin
  Context.VSync := False;
  // Context.Samples := Context.MaxSamples;

  FModelGLProgram := TModelGLProgram.Get(GLState);
  FSkyboxGLProgram := TSkyboxGLProgram.Get(GLState);

  FCamera := TSmoothControlledCamera.Create(Game, 60, 0.01, 100);
  FVAO := TVAOMutable<TModelGLProgram.TData>.Create(FModelGLProgram.Data);
  FSkybox := TSkybox.Create(FSkyboxGLProgram.Data);
  FTextureAtlas := TTextureAtlas.Create(GLState);
  FLightSystem := TLightSystem.Create(Context);
  FNoise := TPerlinNoise3.Create;
  FNoise.GradientSource := TGradient3.Create;

  FTriFactorsCubicUniform := FModelGLProgram.Data.Uniform<Integer>('triFactorsStyle');
  FTexFactorsCubicUniform := FModelGLProgram.Data.Uniform<Integer>('texFactorsStyle');

  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.8), -90);
  FSkybox.AddStripe(ColorRGB(0.5, 0.7, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.5, 0.8, 0.9), 90);

  FCamera.AddUniforms(FSkyboxGLProgram.Data);
  FCamera.AddUniforms(FModelGLProgram.Data);

  FCamera.AddRenderable(FSkybox);
  FCamera.AddRenderable(FVAO);

  FCamera.Location.Offset := Vec3(0, 0, 10);
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;

  FTextureAtlas.Uniform(FModelGLProgram.Data.UniformSampler('diffusemap'));
  FTextureAtlas.AddSubType('smap', FModelGLProgram.Data.UniformSampler('specularmap'), 0);
  // Default color should be different! Something fishy!
  FTextureAtlas.AddSubType('nmap', FModelGLProgram.Data.UniformSampler('normalmap'), ColorRGB(1.0, 0.5, 0.5));
  FTextureAtlas.Texture.MagFilter := magLinear;
  FTextureAtlas.SubTypes[0].Texture.MagFilter := magLinear;
  FTextureAtlas.SubTypes[1].Texture.MagFilter := magLinear;

  // FStoneTexture := FTextureAtlas.AddFromFile('stone', 'Data/stone.png');
  // FTextureAtlas.AddFromResource('1', 'STONE');
  FTextureAtlas.AddFromFile('1', 'Data/realstone.png');
  FTextureAtlas.AddFromFile('2', 'Data/dirt.jpg');
  FTextureAtlas.AddFromFile('3', 'Data/rectangles.png');
  FTextureAtlas.AddFromFile('4', 'Data/pebbles.png');
  FTextureAtlas.AddFromFile('5', 'Data/test.png');
  FTextureAtlas.AddFromFile('6', 'Data/grass.jpg');
  {
  }
  // FTextureAtlas.AddFromFile('3', 'Data/grass.jpg');
  FTextureAtlas.Generate;

  FLightSystem.BindToGLProgram(FModelGLProgram.Data);
  FLightSystem.Ambient := 1.0;

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Color := ColorRGB(0.7, 0.7, 0.7);
  FSun.Direction := Vec3(-0.2, -1, -0.3);
  FSun.AddOccluder(FVAO);
  FSun.Size := 24;

  FCameraLight := TPointLight.Create(FLightSystem);
  FCameraLight.Attenuation := 1;
  FCameraLight.Color := ColorRGB(1.0, 0.7, 0.5);

  Game.OnUpdate.Add(GameUpdate);
  Game.Timer.OnFPSUpdate.Add(UpdateFPS);

  // Game.OnRender.Add(FLightSystem.RenderShadows);

  // glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);

  Generate;
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
  Rand := TRandom.FromSeed(TDefault.Hash(APos));
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
