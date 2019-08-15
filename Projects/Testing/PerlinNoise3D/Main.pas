unit Main;

interface

uses
  Winapi.Windows,

  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Threading,

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
    FOffset: Single;
    FLineThicknessUniform: TGLProgram.TUniform<Single>;

    procedure UpdateFPS;
    procedure GameUpdate;

    procedure BuildVAO;

    procedure SetLineThickness(const Value: Single);
    function GetLineThickness: Single;

    property LineThickness: Single read GetLineThickness write SetLineThickness;

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

  if Input.KeyTyped(VK_DOWN) then
  begin
    FOffset := FOffset - 0.1;
    BuildVAO;
  end;
  if Input.KeyTyped(VK_UP) then
  begin
    FOffset := FOffset + 0.1;
    BuildVAO;
  end;

  FCameraLight.Position := FCamera.Location.RealPosition;
end;

function TfrmMain.GetLineThickness: Single;
begin
  Result := FLineThicknessUniform.Value;
end;

procedure TfrmMain.BuildVAO;
var
  Map: array of array of array of Byte;
  VBOData: IList<TModelGLProgram.TData>;
  Normals: IMap<TVector3, IList<TVector3>>;
  Size: TIntVector3;
  Data: TModelGLProgram.TData;
  P: TIntVector3;
  Corners: TCorners3;
  Corner: TCorner3;
  Plane: TPlane3;
  I, J: Integer;
  Stopwatch: TStopwatch;
  NormalList: IList<TVector3>;
  Combined: TVector3;
  Normal: TVector3;

  function MapAt(APos: TIntVector3): Byte;
  begin
    if APos in Size then
      Exit(Map[APos.X, APos.Y, APos.Z]);
    Result := 0;
  end;

  procedure AddTriangle(APlane: TPlane3);
  var
    TexCoord: TTexCoord2;
  begin
    for TexCoord in TriangleTexCoords do
    begin
      Data.Pos := (APlane[TexCoord] + P - TVector3(Size) / 2) * 0.1;
      Data.TexCoord := FStoneTexture.Bounds[TexCoord];
      Data.Border := FStoneTexture.BoundsHalfPixelInset;
      Data.Normal := APlane.Normal;
      Data.Tangent := APlane.DX;
      Data.Bitangent := APlane.DY;
      VBOData.Add(Data);

      if not Normals.ContainsKey(Data.Pos) then
        Normals[Data.Pos] := TList<TVector3>.Create;
      Normals[Data.Pos].Add(APlane.Normal);
    end;
    Inc(I);
  end;

begin
  Size := 128; // IVec3(3 * 256, 2, 2);

  Normals := TMap<TVector3, IList<TVector3>>.Create;

  Stopwatch := TStopwatch.StartNew;
  SetLength(Map, Size.X, Size.Y, Size.Z);
  Writeln('Creating Map storage: ', Stopwatch.Elapsed.ToString);

  Stopwatch := TStopwatch.StartNew;
  {
    for I := 0 to Size.Volume - 1 do
    begin
    P := IVec3(I mod Size.X, I div Size.X mod Size.Y, I div Size.X div Size.Y);
    if Abs(FNoise[TVector3(P + 51) / Size * 7] + 0.2 * Abs(FNoise[TVector3(P) / Size * 9])) < 0.2 then
    Map[P.Z, P.Y, P.X] := 1;
    end;
  }

  TParallel.&For(0, Size.Volume - 1,
    procedure(I: Integer)
    var
      P: TIntVector3;
      D: TVector3;
    begin
      P := IVec3(I mod Size.X, I div Size.X mod Size.Y, I div Size.X div Size.Y);
      D := TVector3(P).VectorTo(64).Normalize;
      
      //if FNoise[Vec3(P.X * 5 / Size.X, 0, P.Z * 7 / Size.Z)] > (P.Y / Size.Y * 4 - 3.5) then
      //  Map[P.X, P.Y, P.Z] := 1;

      if TVector3(P).DistanceTo(64) / 64 < FNoise[D * 3] * 0.25 + 0.75 then
        Map[P.X, P.Y, P.Z] := 1;

      if (FNoise[TVector3(P * 3 + 51) / Size * 2] + 0.2 * FNoise[TVector3(P) / Size * 7]) > 0.2 then
        Map[P.X, P.Y, P.Z] := 0;

    end);

  I := 0;

  Writeln('Generating Map: ', Stopwatch.Elapsed.ToString);
  Stopwatch := TStopwatch.StartNew;
  VBOData := TList<TModelGLProgram.TData>.Create;
  for P in IBounds3(-1, Size) do
  begin
    // build set of all adjacent block directions
    Corners := [];
    for Corner := Low(TCorner3) to High(TCorner3) do
      if MapAt(P + Corner3Pos[Corner]) <> 0 then
        Include(Corners, Corner);

    for Plane in TMarchingCubes.GetTriangles(Corners, FOffset) do
      AddTriangle(Plane);

  end;

  Writeln('Filling VBO: ', Stopwatch.Elapsed.ToString);

  for NormalList in Normals.Values do
  begin
    Combined := 0;
    for Normal in NormalList do
      Combined := Combined + Normal;
    NormalList.Clear;
    NormalList.Add(Combined.Normalize);
  end;

  Stopwatch := TStopwatch.StartNew;
  TParallel.&For(0, VBOData.MaxIndex,
    procedure(I: Integer)
    var
      Data: TModelGLProgram.TData;
    begin
      Data := VBOData[I];
      Data.Normal := Normals[Data.Pos][0];
      VBOData[I] := Data;
    end);
  Writeln('Generating normals: ', Stopwatch.Elapsed.ToString);

  Stopwatch := TStopwatch.StartNew;
  FVAO.VBO.Generate(VBOData.Count, buStaticDraw, VBOData.DataPointer);
  Writeln('Sending VBO to GPU: ', Stopwatch.Elapsed.ToString);

  Writeln('Double-Triangle Quad Count: ', I);

  FLightSystem.RenderShadows;
end;

procedure TfrmMain.Init;
begin
  Context.VSync := False;
  Context.Samples := Context.MaxSamples;

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
  FStoneTexture := FTextureAtlas.AddFromResource('stone', 'STONE');

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

  FOffset := 0.5;

  BuildVAO;
end;

procedure TfrmMain.SetLineThickness(const Value: Single);
begin
  FLineThicknessUniform.Value := Value;
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
