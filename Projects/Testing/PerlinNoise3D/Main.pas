unit Main;

interface

uses
  Winapi.Windows,

  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Threading,

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
  if Input.KeyDown('W') then
    FCamera.Location.Move(DeltaTime);
  if Input.KeyDown('S') then
    FCamera.Location.Move(-DeltaTime);
  if Input.KeyDown('D') then
    FCamera.Location.Slide(DeltaTime);
  if Input.KeyDown('A') then
    FCamera.Location.Slide(-DeltaTime);
  if Input.KeyDown(VK_SPACE) then
    FCamera.Location.Lift(DeltaTime);
  if Input.KeyDown(VK_SHIFT) then
    FCamera.Location.Lift(-DeltaTime);

  FCameraLight.Position := FCamera.Location.RealPosition;
end;

procedure TfrmMain.BuildVAO;
var
  Map: array of array of array of Byte;
  VBOData: IList<TModelGLProgram.TData>;
  Plane: TPlane3;
  I: Integer;
  P: TIntVector3;
  Size: TIntVector3;
  Dir: TBasicDir3;
  Dirs: TBasicDirs3;
  DirArray: array [0 .. 1] of TBasicDir3;
  Axis: TCoordAxis3;
  CornerIndex2: TBounds2.TCornerIndex;
  CornerIndex3: TBounds3.TCornerIndex;
  CheckPos: TIntVector3;
  Data: TModelGLProgram.TData;
  Edge: TLine3;
label
  NextBlock;

  function MapAt(APos: TIntVector3): Byte;
  begin
    if APos in Size then
      Exit(Map[APos.X, APos.Y, APos.Z]);
    Result := 0;
  end;

  procedure AddQuad(APlane: TPlane3);
  var
    TexCoord: TTexCoord2;
  begin
    for TexCoord in QuadTexCoords do
    begin
      Data.Pos := (APlane[TexCoord] + P - TVector3(Size) / 2) * 0.1;
      Data.TexCoord := FStoneTexture.Bounds[TexCoord];
      Data.Border := FStoneTexture.BoundsHalfPixelInset;
      Data.Normal := APlane.Normal;
      Data.Tangent := APlane.DX;
      Data.Bitangent := APlane.DY;
      VBOData.Add(Data);
    end;
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
    end;
  end;

begin
  Size := 16;

  var Stopwatch := TStopwatch.StartNew;
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
    begin
      var P := IVec3(I mod Size.X, I div Size.X mod Size.Y, I div Size.X div Size.Y);
      // if Abs(FNoise[TVector3(P + 51) / Size * 2] + 0.2 * Abs(FNoise[TVector3(P) / Size * 5])) < 0.2 then
      if TVector3(P).DistanceTo(8) < 8 then
        Map[P.X, P.Y, P.Z] := 1;
    end);

  {
    for P in Size do
    if Abs(FNoise[TVector3(P + 51) / Size * 7] + 0.2 * Abs(FNoise[TVector3(P) / Size * 9])) < 0.2 then
    Map[P.X, P.Y, P.Z] := 1;
  }
  Writeln('Generating Map: ', Stopwatch.Elapsed.ToString);
  Stopwatch := TStopwatch.StartNew;
  VBOData := TList<TModelGLProgram.TData>.Create;
  I := 0;
  for P in Size do
  begin
    if MapAt(P) = 0 then
      Continue;
  
    // build set of all adjacent block directions
    Dirs := [];
    for Dir := Low(TBasicDir3) to High(TBasicDir3) do
      if MapAt(P + Vec3Dir[Dir]) = 0 then
        Include(Dirs, Dir);

    // slopes
    // 12 different, along each edge
    for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    begin
      for CornerIndex2 := Low(TBounds2.TCornerIndex) to High(TBounds2.TCornerIndex) do
      begin
        if Dirs <> CubeEdgeDirections[Axis, CornerIndex2] then
          Continue;
        Edge := CubeEdges[Axis, CornerIndex2];
        AddQuad(Plane3(
          Edge.S - Vec3Dir[CubeEdgeDirectionArrays[Axis, CornerIndex2, 0]], 
          Edge.D, 
          Vec3Dir[CubeEdgeDirectionArrays[Axis, CornerIndex2, 0]] - 
          Vec3Dir[CubeEdgeDirectionArrays[Axis, CornerIndex2, 1]]));
        goto NextBlock;
      end;
    end;
    
    // triangles
    // 8 different, on each vertex
    for CornerIndex3 := Low(TIntBounds3.TCornerIndex) to High(TIntBounds3.TCornerIndex) do
    begin
      if Dirs <> CubeVertexDirections[CornerIndex3] then
        Continue;
      AddTriangle(Plane3(
        CubeVerticies[CornerIndex3] - Vec3Dir[CubeVertexDirectionArrays[CornerIndex3, 0]],
        Vec3Dir[CubeVertexDirectionArrays[CornerIndex3, 0]] - Vec3Dir[CubeVertexDirectionArrays[CornerIndex3, 1]],
        Vec3Dir[CubeVertexDirectionArrays[CornerIndex3, 0]] - Vec3Dir[CubeVertexDirectionArrays[CornerIndex3, 2]]
      ));
      goto NextBlock;
    end;

    // faces
    // 6 along each face
    for Dir in Dirs do
      AddQuad(CubePlanes[Dir]);

  NextBlock:
  end;
  Writeln('Filling VBO: ', Stopwatch.Elapsed.ToString);

  Stopwatch := TStopwatch.StartNew;
  FVAO.VBO.Generate(VBOData.Count, buStaticDraw, VBOData.DataPointer);
  Writeln('Sending VBO to GPU: ', Stopwatch.Elapsed.ToString);

  // Writeln('Double-Triangle Quad Count: ', I);
end;

procedure TfrmMain.Init;
begin
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

  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.8), -90);
  FSkybox.AddStripe(ColorRGB(0.5, 0.7, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.5, 0.8, 0.9), 90);

  FCamera.AddUniforms(FSkyboxGLProgram.Data);
  FCamera.AddUniforms(FModelGLProgram.Data);

  FCamera.AddRenderable(FSkybox);
  FCamera.AddRenderable(FVAO);

  FCamera.Location.Offset := Vec3(0, 0, 3);
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;

  FTextureAtlas.Uniform(FModelGLProgram.Data.UniformSampler('diffusemap'));
  FTextureAtlas.AddSubType('smap', FModelGLProgram.Data.UniformSampler('specularmap'), 0);
  FTextureAtlas.AddSubType('nmap', FModelGLProgram.Data.UniformSampler('normalmap'), ColorRGB(0.5, 0.5, 1.0));
  FTextureAtlas.Texture.MagFilter := magNearest;
  FTextureAtlas.SubTypes[0].Texture.MagFilter := magNearest;
  FTextureAtlas.SubTypes[1].Texture.MagFilter := magNearest;

  FStoneTexture := FTextureAtlas.AddFromFile('stone', 'Data/stone.png');

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

  BuildVAO;

  FLightSystem.RenderShadows;
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
