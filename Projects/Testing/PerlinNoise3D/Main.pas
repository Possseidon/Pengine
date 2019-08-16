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
    FLineThicknessUniform: TGLProgram.TUniform<Single>;
    FMap: array of array of array of Byte;
    FSize: TIntVector3;

    procedure UpdateFPS;
    procedure GameUpdate;

    procedure BuildVAO;

    procedure SetLineThickness(const Value: Single);
    function GetLineThickness: Single;
    function GetMap(APos: TIntVector3): Byte;
    procedure SetMap(APos: TIntVector3; const Value: Byte);

    property LineThickness: Single read GetLineThickness write SetLineThickness;

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
  AResource := True;
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
var
  I: Integer;
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
    LineThickness := LineThickness / 2;
  if Input.KeyTyped('E') then
    LineThickness := LineThickness * 2;

  if not FCamera.Moving then
  begin
    if Input.ButtonPressed(mbLeft) then
      Build(True);
    if Input.ButtonPressed(mbRight) then
      Build(False);
  end;

  FCameraLight.Position := FCamera.Location.RealPosition;
  // FSun.Direction := FSun.Direction.Rotate(Vec3(0, 1, 0), DeltaTime * 10);
end;

function TfrmMain.GetLineThickness: Single;
begin
  Result := FLineThicknessUniform.Value;
end;

function TfrmMain.GetMap(APos: TIntVector3): Byte;
begin
  if APos in FSize then
    Exit(FMap[APos.X, APos.Y, APos.Z]);
  Result := 0;
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

  FSize := 16;
  SetLength(FMap, FSize.X, FSize.Y, FSize.Z);

  TParallel.For(0, FSize.Volume - 1,
    procedure(I: Integer)
    var
      P: TIntVector3;
      D: TVector3;
    begin
      P := IVec3(I mod FSize.X, I div FSize.X mod FSize.Y, I div FSize.X div FSize.Y);
      D := TVector3(P).VectorTo(4).Normalize;

      // if FNoise[Vec3(P.X * 5 / FSize.X, 0, P.Z * 7 / FSize.Z)] > (P.Y / FSize.Y * 4 - 3.5) then
      // Map[P] := 1;

      if TVector3(P).DistanceTo(4) / 4 < FNoise[D * 3] * 0.25 + 0.75 then
        Map[P] := 1;

      if (FNoise[TVector3(P * 3 + 51) / FSize * 2] + 0.2 * FNoise[TVector3(P) / FSize * 7]) > 0.2 then
        Map[P] := 0;

      Map[P] := Random(2);

    end);
  Writeln('Generating Map: ', Stopwatch.Elapsed.ToString);
  
  BuildVAO;
end;

procedure TfrmMain.BuildVAO;
var
  VBOData: IList<TModelGLProgram.TData>;
  P: TIntVector3;
  Corners: TCorners3;
  Corner: TCorner3;
  Plane: TPlane3;
  I: Integer;
  Stopwatch: TStopwatch;

  procedure AddTriangle(APlane: TPlane3; ATexTile: TTexTile; APos: TVector3; AVBOData: IList<TModelGLProgram.TData>;
  ASize: TIntVector3); inline;
  var
    TexCoord: TTexCoord2;
    Data: TModelGLProgram.TData;
  begin
    Data.Border := ATexTile.BoundsHalfPixelInset;
    Data.Normal := APlane.Normal;
    Data.Tangent := APlane.DX;
    Data.Bitangent := APlane.DY;
    for TexCoord in TriangleTexCoords do
    begin
      Data.Pos := (APlane[TexCoord] + APos - TVector3(ASize) / 2);
      Data.TexCoord := ATexTile.Bounds[TexCoord];
      AVBOData.Add(Data);
    end;
  end;

begin
  Stopwatch := TStopwatch.StartNew;
  VBOData := TList<TModelGLProgram.TData>.Create;
  for P in IBounds3(-1, FSize) do
  begin
    for I := 1 to 1 do
    begin
      // build set of all adjacent block directions
      Corners := [];
      for Corner := Low(TCorner3) to High(TCorner3) do
        if Map[P + Corner3Pos[Corner]] = I then
          Include(Corners, Corner);

      for Plane in TMarchingCubes.GetTriangles(Corners) do
        AddTriangle(Plane, FTextureAtlas[I.ToString], P, VBOData, FSize);
    end;
  end;

  //Writeln('Filling VBO: ', Stopwatch.Elapsed.ToString);
  //Writeln('Triangles: ', VBOData.Count);

  Stopwatch := TStopwatch.StartNew;
  FVAO.VBO.Generate(VBOData.Count, buStaticDraw, VBOData.DataPointer);
  //Writeln('Sending VBO to GPU: ', Stopwatch.Elapsed.ToString);

  FLightSystem.RenderShadows;
end;

procedure TfrmMain.Init;
var
  B: Byte;
  M: Integer;
begin
  M := 0;
  for B := 0 to 255 do
    M := Max(M, Length(TMarchingCubes.GetTriangles(TCorners3(B))));
  Writeln('Max triangles: ', M);
    
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

  FLineThicknessUniform := FModelGLProgram.Data.Uniform<Single>('linethickness');

  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.8), -90);
  FSkybox.AddStripe(ColorRGB(0.5, 0.7, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.5, 0.8, 0.9), 90);

  FCamera.AddUniforms(FSkyboxGLProgram.Data);
  FCamera.AddUniforms(FModelGLProgram.Data);

  FCamera.AddRenderable(FSkybox);
  FCamera.AddRenderable(FVAO);

  FCamera.Location.Offset := Vec3(0, 0, 20);
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;

  FTextureAtlas.Uniform(FModelGLProgram.Data.UniformSampler('diffusemap'));
  FTextureAtlas.AddSubType('smap', FModelGLProgram.Data.UniformSampler('specularmap'), 0);
  // Default color should be different! Something fishy!
  FTextureAtlas.AddSubType('nmap', FModelGLProgram.Data.UniformSampler('normalmap'), ColorRGB(1.0, 0.5, 0.5));
  FTextureAtlas.Texture.MagFilter := magNearest;
  FTextureAtlas.SubTypes[0].Texture.MagFilter := magNearest;
  FTextureAtlas.SubTypes[1].Texture.MagFilter := magNearest;

  // FStoneTexture := FTextureAtlas.AddFromFile('stone', 'Data/stone.png');
  FTextureAtlas.AddFromResource('1', 'STONE');

  FTextureAtlas.Generate;

  FLightSystem.BindToGLProgram(FModelGLProgram.Data);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := Vec3(-0.2, -1, -0.3);
  FSun.AddOccluder(FVAO);
  FSun.Size := 24;

  FCameraLight := TPointLight.Create(FLightSystem);
  FCameraLight.Attenuation := 1;
  FCameraLight.Color := ColorRGB(1.0, 0.7, 0.5);

  Game.OnUpdate.Add(GameUpdate);
  Game.Timer.OnFPSUpdate.Add(UpdateFPS);

  // Game.OnRender.Add(FLightSystem.RenderShadows);

  Generate;
  BuildVAO;
end;

procedure TfrmMain.SetLineThickness(const Value: Single);
begin
  FLineThicknessUniform.Value := Value;
end;

procedure TfrmMain.SetMap(APos: TIntVector3; const Value: Byte);
begin
  if APos in FSize then
    FMap[APos.X, APos.Y, APos.Z] := Value;
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
