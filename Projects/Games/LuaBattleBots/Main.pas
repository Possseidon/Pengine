unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OpenGLContext, Camera, Shaders, VAOManager, VectorGeometry, IntfBase,
  Matrix, Lists, TextureManager, Lights, ControlledCamera, GLEnums, IntegerMaths, Color, SkyDome, LuaHeader,
  Game, EntityDefine, DebugConsoleDefine, InputHandler, Resources, CustomModules, System.Win.ScktComp;

type

  { TForm1 }

  TfrmMain = class(TGLForm)
  private
    FGame: TGame;

    FCamera: TControlledCamera;
    FFloorVAO: TVAO;
    FCubeVAO: TVAO;

    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;

    FSkyDome: TSkyDome;

    FTestBot: TBotCore;

    procedure InitCamera;
    procedure InitFloorVAO;
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
  FSun.Free;
  FLightSystem.Free;
  FCubeVAO.Free;
  FFloorVAO.Free;
  FCamera.Free;
end;

procedure TfrmMain.Init;
begin
  State.DebugOutput := False;
  VSync := False;
  FPSLimit := 300;

  InitCamera;
  InitSkyDome;
  InitFloorVAO;
  InitLightSystem;
  InitGame;
end;

procedure TfrmMain.InitCamera;
begin
  DebugWrite('Initializing Camera...');
  FCamera := TControlledCamera.Create(60, Aspect, 0.05, 420, Input);
  FCamera.Location.OffsetZ := 3;
  FCamera.Location.PitchAngle := -20;
  FCamera.Location.TurnAngle := -30;
  FCamera.PitchUpperLimit := -4.2;
  FCamera.PosLowerLimitY := 0.1;

  FCamera.AddUniforms(TResModelShader.Data);
  FCamera.AddUniforms(TResSkyDomeShader.Data);
  DebugWriteLine(' Done!');
end;

procedure TfrmMain.InitFloorVAO;
const
  Plane: TPlane3 = (
    SV: (X: 0; Y: 0; Z: 0);
    DVS: (X: 0; Y: 0; Z: 1);
    DVT: (X: 1; Y: 0; Z: 0)
    );
var
  Data: TResModelShader.TData;
  T: TVector2;
  GridPos: TIntVector2;
  Grid: TIntBounds2;
begin
  DebugWrite('Initializing Floor...');
  Grid.Create(-16, 16);

  FFloorVAO := TVAO.Create(TResModelShader.Data);
  FFloorVAO.Generate(6 * Grid.Area, buStaticDraw);

  FFloorVAO.Map(baWriteOnly);

  Data.Normal := Vec3(0, 1, 0);
  Data.Tangent := Vec3(1, 0, 0);
  Data.Bitangent := Vec3(0, 0, 1);
  Data.Border := TResTexturePage.Data.GetTexBounds('grass_top', FRange2(0, 1));

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

  Data.Border := TResTexturePage.Data.HalfPixelInset(Data.Border);

  FFloorVAO.Unmap;

  FCamera.AddRenderObject(FFloorVAO);
  DebugWriteLine(' Done!');
end;

procedure TfrmMain.InitLightSystem;
begin
  DebugWrite('Initializing Light System...');
  FLightSystem := TLightSystem.Create(Self);
  FLightSystem.Ambient := TColorRGB.Gray(0.25);
  FLightSystem.BindToShader(TResModelShader.Data);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Direction := Vec3(-1, -2, -1).Normalize;
  FSun.Color := TColorRGB.Gray(0.9);
  FSun.Size := Sqrt(Sqr(32) + Sqr(32));
  FSun.AddOccluder(FFloorVAO);
  DebugWriteLine(' Done!');
end;

procedure TfrmMain.InitSkyDome;
begin
  DebugWrite('Initializing SkyDome...');

  FSkyDome := TSkyDome.Create(Self, FCamera, TResSkyDomeShader);

  FSkyDome.AddStripe(TColorRGB.Create(0.7, 1.0, 0.9), -90);
  FSkyDome.AddStripe(TColorRGB.Create(0.4, 0.6, 0.9), 0);
  FSkyDome.AddStripe(TColorRGB.Create(0.1, 0.2, 0.9), +90);

  FCamera.AddRenderObject(FSkyDome);
  DebugWriteLine(' Done!');
end;

procedure TfrmMain.RenderFunc;
begin
  FLightSystem.RenderShadows;
  FCamera.Render;
end;

procedure TfrmMain.InitGame;
var
  Code: TStrings;
begin
  DebugWrite('Initializing Game...');
  FGame := TGame.Create(FCamera);

  FTestBot := TBotCore.Create;

  FTestBot.Location.TurnAngle := 0;
  FTestBot.Location.PitchAngle := 0;
  FTestBot.Location.RollAngle := 0;

  FTestBot.AttachModule(sdUp, TWheelModule);
  FTestBot.AttachModule(sdLeft, TWheelModule);
  FTestBot.AttachModule(sdRight, TWheelModule);
  FTestBot.AttachModule(sdFront, TWheelModule);
  FTestBot.AttachModule(sdBack, TWheelModule);

  try
    Code := TStringList.Create;
    try
      Code.LoadFromFile('Data/TestCode.lua');
      FTestBot.SetUpdateFunction(AnsiString(Code.Text));
    finally
      Code.Free;
    end;
  except
    DebugWriteLine('Error while trying to load TestCode!');
  end;

  FGame.AddEntity(FTestBot);

  FSun.AddOccluder(FTestBot);
  DebugWriteLine(' Done!');
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
  FSun.Direction := FSun.Direction.Rotate(Vec3(0, 1, 0.2).Normalize, DeltaTime);

  FCamera.Update;

  FGame.Update(DeltaTime);

  if Input.KeyDown('A') then
    FTestBot.Location.Slide(-DeltaTime);
  if Input.KeyDown('D') then
    FTestBot.Location.Slide(+DeltaTime);

  if Input.KeyDown('S') then
    FTestBot.Location.Move(-DeltaTime);
  if Input.KeyDown('W') then
    FTestBot.Location.Move(+DeltaTime);

  if Input.KeyDown(VK_SHIFT) then
    FTestBot.Location.Lift(-DeltaTime);
  if Input.KeyDown(VK_SPACE) then
    FTestBot.Location.Lift(+DeltaTime);

  if Input.KeyDown(VK_LEFT) then
    FTestBot.Location.Turn(-DeltaTime * 30);
  if Input.KeyDown(VK_Right) then
    FTestBot.Location.Turn(+DeltaTime * 30);

  if Input.KeyDown(VK_DOWN) then
    FTestBot.Location.Pitch(-DeltaTime * 30);
  if Input.KeyDown(VK_UP) then
    FTestBot.Location.Pitch(+DeltaTime * 30);

  if Input.KeyDown('Q') then
    FTestBot.Location.Roll(-DeltaTime * 30);
  if Input.KeyDown('E') then
    FTestBot.Location.Roll(+DeltaTime * 30);

end;

end.
