unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OpenGLContext, Camera, Shaders, VAOManager, VectorGeometry, IntfBase,
  Matrix, Lists, TextureManager, Lights, ControlledCamera, GLEnums, IntegerMaths, Color, SkyDome, LuaHeader,
  Game, EntityDefine, DebugConsoleDefine, InputHandler, Resources, CustomModules, System.Win.ScktComp, ModelDefine;

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

    function GetFloorParams: TResFloorVAOParams;

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
  TResFloorVAO.Release(GetFloorParams);
  FCubeVAO.Free;
  FCamera.Free;
end;

procedure TfrmMain.Init;
begin
  State.DebugOutput := False;
  VSync := False;
  //FPSLimit := 300;

  InitCamera;
  InitSkyDome;
  InitFloorVAO;

  InitLightSystem;
  InitGame;
end;

procedure TfrmMain.InitCamera;
begin
  DebugWrite('Initializing Camera...');
  FCamera := TSmoothControlledCamera.Create(60, Aspect, 0.05, 420, Self);
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
begin
  DebugWrite('Initializing Floor...');
  FFloorVAO := TResFloorVAO.Make(GetFloorParams);
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
  FSun.Direction := Vec3(-1, -2, -1);
  FSun.Color := TColorRGB.Gray(0.9);
  FSun.Size := Sqrt(Sqr(64) + Sqr(64));
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
  I: Integer;
  P: TVector3;
begin
  DebugWrite('Initializing Game...');
  FGame := TGame.Create(FCamera);

  P.Y := 0;
  for I := 0 to 99 do
  begin
    FTestBot := TBotCore.Create;

    P.XZ := TVector2.RandomBox * 30;
    FTestBot.Location.Pos := P;

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
  end;

  DebugWriteLine(' Done!');
end;

function TfrmMain.GetFloorParams: TResFloorVAOParams;
begin
  Result := TResFloorVAOParams.Create;
  Result.Size := 64;
  Result.Texture := 'grass_top';
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

  FTestBot.Location.FreeTranslate(-FTestBot.Location.Offset);

  if Input.KeyDown('A') then
    FTestBot.Location.FreeTranslate(Vec3(-DeltaTime * 2, 0, 0));
  if Input.KeyDown('D') then
    FTestBot.Location.FreeTranslate(Vec3(+DeltaTime * 2, 0, 0));

  if Input.KeyDown('S') then
    FTestBot.Location.FreeTranslate(Vec3(0, 0, +DeltaTime * 2));
  if Input.KeyDown('W') then
    FTestBot.Location.FreeTranslate(Vec3(0, 0, -DeltaTime * 2));

  if Input.KeyDown(VK_SHIFT) then
    FTestBot.Location.FreeTranslate(Vec3(0, -DeltaTime * 2, 0));
  if Input.KeyDown(VK_SPACE) then
    FTestBot.Location.FreeTranslate(Vec3(0, +DeltaTime * 2, 0));

  if Input.KeyDown(VK_LEFT) then
    FTestBot.Location.FreeTurn(-DeltaTime * 90);
  if Input.KeyDown(VK_Right) then
    FTestBot.Location.FreeTurn(+DeltaTime * 90);

  if Input.KeyDown(VK_DOWN) then
    FTestBot.Location.FreePitch(+DeltaTime * 90);
  if Input.KeyDown(VK_UP) then
    FTestBot.Location.FreePitch(-DeltaTime * 90);

  if Input.KeyDown('Q') then
    FTestBot.Location.FreeRoll(-DeltaTime * 90);
  if Input.KeyDown('E') then
    FTestBot.Location.FreeRoll(+DeltaTime * 90);

  FTestBot.Location.FreeTranslate(FTestBot.Location.Offset);

end;

end.
