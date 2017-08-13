unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OpenGLContext, Camera, Shaders, VAOManager, VectorGeometry, IntfBase,
  Matrix, Lists, TextureManager, Lights, ControlledCamera, GLEnums, IntegerMaths, Color, SkyDome, LuaHeader,
  Game, EntityDefine, DebugConsoleDefine, InputHandler;

type

  TData = record
    Pos: TVector3;
    TexCoord: TTexCoord2;
    Normal: TVector3;
    Tangent: TVector3;
    Bitangent: TVector3;
    Border: TBounds2;
  end;

  { TForm1 }

  TfrmMain = class(TGLForm)
  private
    FGame: TGame;

    FShader: TShader;
    FCamera: TControlledCamera;
    FTexturePage: TTexturePage;
    FFloorVAO: TVAO;
    FCubeVAO: TVAO;

    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;

    FSkyDomeShader: TShader;
    FSkyDome: TSkyDome;

    procedure InitCamera;
    procedure InitShader;
    procedure InitSkyDomeShader;
    procedure InitTexturePage;
    procedure InitFloorVAO;
    procedure InitCubeVAO;
    procedure InitLightSystem;
    procedure InitSkyDome;
    procedure InitGame;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure ResizeFunc; override;
    procedure UpdateFunc; override;
    procedure RenderFunc; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TForm1 }

procedure TfrmMain.Finalize;
begin
  FGame.Free;
  FSkyDome.Free;
  FSkyDomeShader.Free;
  FSun.Free;
  FLightSystem.Free;
  FCubeVAO.Free;
  FFloorVAO.Free;
  FTexturePage.Free;
  FCamera.Free;
  FShader.Free;
end;

procedure TfrmMain.Init;
begin
  DebugConsole := TDebugConsole.Create(Self);
  DebugConsole.Show;

  State.DebugOutput := False;
  VSync := True;
  // FPSLimit := 300;

  InitCamera;

  InitShader;
  InitSkyDomeShader;

  InitSkyDome;

  InitTexturePage;

  InitFloorVAO;
  InitCubeVAO;
  
  InitLightSystem;
  InitGame;
end;

procedure TfrmMain.InitCamera;
begin
  FCamera := TControlledCamera.Create(60, Aspect, 0.05, 420, Input);
  FCamera.Location.OffsetZ := 3;
  FCamera.Location.PitchAngle := -20;
  FCamera.Location.TurnAngle := -30;
  FCamera.PitchUpperLimit := -4.2;
  FCamera.PosLowerLimitY := 0.1;
end;

procedure TfrmMain.InitCubeVAO;
var
  P: TPlane3;
  Data: TData;
  T: TTexCoord2;
begin
  FCubeVAO := TVAO.Create(FShader);
  FCubeVAO.Generate(6 * 6, buStaticDraw);
  FCubeVAO.Map(baWriteOnly);

  for P in CubePlanes do
  begin
    Data.Border := FTexturePage.GetTexBounds('stone_bricks', FRange2(0, 1));
    Data.Normal := P.Normal;
    Data.Tangent := P.DVS;
    Data.Bitangent := P.DVT;
    for T in QuadTexCoords do
    begin
      Data.Pos := P[T];
      Data.TexCoord := Data.Border[T];
      FCubeVAO.AddVertex(Data);
    end;
    Data.Border := FTexturePage.HalfPixelInset(Data.Border);
  end;

  FCubeVAO.Unmap;
end;

procedure TfrmMain.InitFloorVAO;
const
  Plane: TPlane3 = (
    SV: (X: 0; Y: 0; Z: 0);
    DVS: (X: 0; Y: 0; Z: 1);
    DVT: (X: 1; Y: 0; Z: 0)
    );
var
  Data: TData;
  T: TVector2;
  GridPos: TIntVector2;
  Grid: TIntBounds2;
begin
  Grid.Create(-20, 20);

  FFloorVAO := TVAO.Create(FShader);
  FFloorVAO.Generate(6 * Grid.Area, buStaticDraw);

  FFloorVAO.Map(baWriteOnly);

  Data.Normal := Vec3(0, 1, 0);
  Data.Tangent := Vec3(1, 0, 0);
  Data.Bitangent := Vec3(0, 0, 1);
  Data.Border := FTexturePage.GetTexBounds('grass_top', FRange2(0, 1));

  for GridPos in Grid do
  begin
    for T in QuadTexCoords do
    begin
      Data.Pos := Plane[T];
      Data.Pos.XZ := Data.Pos.XZ + GridPos;
      Data.TexCoord := Data.Border[T];
      FFloorVAO.AddVertex(Data);
    end;
  end;

  Data.Border := FTexturePage.HalfPixelInset(Data.Border);

  FFloorVAO.Unmap;

  FCamera.AddRenderObject(FFloorVAO);
end;

procedure TfrmMain.InitLightSystem;
begin
  FLightSystem := TLightSystem.Create(Self);
  FLightSystem.Ambient := TColorRGB.Gray(0.2);
  FLightSystem.BindToShader(FShader);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := Vec3(-1, -2, -1).Normalize;
  FSun.Color := TColorRGB.Gray(0.8);
  FSun.Size := 60;
  FSun.AddOccluder(FFloorVAO);
end;

procedure TfrmMain.InitShader;
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
  FShader := TShader.Create;
  FShader.LoadFromFile('Data/model');
  FShader.SetAttributeOrder(Attributes);
  FShader.Uniform<Boolean>('depthonly').Value := False;

  FCamera.AddUniforms(FShader);
end;

procedure TfrmMain.InitSkyDome;
begin
  FSkyDome := TSkyDome.Create(Self, FCamera, FSkyDomeShader);
  FSkyDome.AddStripe(TColorRGB.Create(0.7, 1.0, 0.9), -90);
  FSkyDome.AddStripe(TColorRGB.Create(0.4, 0.6, 0.9), 0);
  FSkyDome.AddStripe(TColorRGB.Create(0.1, 0.2, 0.9), +90);

  FCamera.AddRenderObject(FSkyDome);
end;

procedure TfrmMain.InitSkyDomeShader;
begin
  FSkyDomeShader := TShader.Create;
  FSkyDomeShader.LoadFromFile('Data/skydome');
  FSkyDomeShader.SetAttributeOrder(['vpos', 'vpitch']);

  FCamera.AddUniforms(FSkyDomeShader);
end;

procedure TfrmMain.InitTexturePage;
begin
  FTexturePage := TTexturePage.Create;
  FTexturePage.UniformDefaults(FShader);
  FTexturePage.AddTextureFromFile('Data/stone_bricks.png', 'stone_bricks');
  FTexturePage.AddTextureFromFile('Data/grass_top.png', 'grass_top');
  FTexturePage.AddTextureFromFile('Data/log_side.png', 'log_side');
  FTexturePage.BuildPage(32);
end;

procedure TfrmMain.RenderFunc;
begin
  FLightSystem.RenderShadows;
  FCamera.Render;
end;

procedure TfrmMain.InitGame;
var
  TestBot: TLuaEntity;
  Code: TStrings;
begin
  FGame := TGame.Create(FCamera);

  TestBot := TLuaEntity.Create;

  try
    Code := TStringList.Create;
    try
      Code.LoadFromFile('Data/TestCode.lua');
      TestBot.SetUpdateFunction(AnsiString(Code.Text));
    finally
      Code.Free;
    end;
  except
    DebugConsole.WriteLine('Error while trying to load TestCode!');
  end;

  FGame.AddEntity(TestBot);
end;

procedure TfrmMain.ResizeFunc;
begin
  FCamera.Aspect := Aspect;
end;

procedure TfrmMain.UpdateFunc;
begin
  if MustUpdateFPS then
    Caption := Format('LuaBattleBots - FPS: %d', [FPSInt]);

  // if Input.ButtonPressed(mbMiddle) then
  // FSun.Position := FCamera.Location.RealPosition;
  // FSun.Direction := FSun.Direction.Rotate(Vec3(1, 1, 0).Normalize, 30 * DeltaTime);

  FCamera.Update;

  FGame.Update(DeltaTime);

  if Input.KeyPressed(VK_F10) then
    DebugConsole.Visible := not DebugConsole.Visible;

  DebugConsole.Update;
end;

end.
