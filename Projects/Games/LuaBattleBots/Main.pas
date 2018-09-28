unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Pengine.GLContext,
  Pengine.Camera,
  Pengine.Shader,
  Pengine.VAO,
  Pengine.Vector,
  Pengine.Interfaces,
  Pengine.Matrix,
  Pengine.Collections,
  Pengine.Texture,
  Pengine.Light,
  Pengine.ControlledCamera,
  Pengine.GLEnums,
  Pengine.IntMaths,
  Pengine.Color,
  Pengine.Skybox,
  Pengine.LuaHeader,
  Game,
  EntityDefine,
  Pengine.DebugConsole,
  Pengine.InputHandler,
  CustomModules,
  System.Win.ScktComp,
  Pengine.Lua,
  Pengine.LuaDefaultLibs,
  Resources;

type

  { TForm1 }

  TfrmMain = class(TGLForm)
  private
    FGame: TGame;

    FLua: TLua;

    FCamera: TControlledCamera;
    FFloorVAO: TVAO;
    FCubeVAO: TVAO;

    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;

    FSkybox: TSkybox;

    FTestBot: TBotCore;

    FTestModel: TVAO;

    procedure InitCamera;
    procedure InitFloorVAO;
    procedure InitLightSystem;
    procedure InitSkyDome;
    procedure InitGame;

    procedure InitTestModel;

    function GetFloorParams: TResFloorVAOParams;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure ResizeGL; override;
    procedure UpdateGL; override;
    procedure RenderGL; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TForm1 }

procedure TfrmMain.Finalize;
begin
  FGame.Free;
  FSkybox.Free;
  FSun.Free;
  FLightSystem.Free;
  TResFloorVAO.Release(GetFloorParams);
  FCubeVAO.Free;
  FCamera.Free;
  FLua.Free;
  FTestModel.Free;
end;

procedure TfrmMain.Init;
begin
  State.DebugOutput := False;
  VSync := False;
  //FPSLimit := 300;

  FLua := TLua.Create;

  InitCamera;
  InitSkyDome;
  InitFloorVAO;

  InitLightSystem;
  InitTestModel;
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
  FCamera.PosLowerBoundY := 0.1;

  FCamera.AddUniforms(TResModelShader.Data);
  FCamera.AddUniforms(TResSkyboxShader.Data);
  DebugWriteLine(' Done!');
end;

procedure TfrmMain.InitFloorVAO;
begin
  DebugWrite('Initializing Floor...');
  FFloorVAO := TResFloorVAO.Make(GetFloorParams);
  FCamera.AddRenderable(FFloorVAO);
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

  FSkybox := TSkybox.Create(Self, TResSkyboxShader);

  FSkybox.AddStripe(TColorRGB.Create(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(TColorRGB.Create(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(TColorRGB.Create(0.1, 0.2, 0.9), +90);

  FCamera.AddRenderable(FSkybox);
  DebugWriteLine(' Done!');
end;

procedure TfrmMain.InitTestModel;
var
  Model: TModelOBJ;
begin
  Model := TModelOBJ.Create;

  StartTimer;
  Model.LoadFromFile('Data/Galaxy.obj');
  ShowMessage(StopTimerGetString(tfMilliseconds));

  if Model.Log.Entries.Count > 0 then
    ShowMessage(Model.Log.ToString);

  FTestModel := TVAO.Create(TResModelShader.Data);
  Model.GenerateVAO(FTestModel);

  FCamera.AddRenderObject(FTestModel);
  FSun.AddOccluder(FTestModel);

  Model.Free;
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
  for I := 0 to 0 do
  begin
    FTestBot := TBotCore.Create;

    P.XZ := TVector2.RandomBox * 30;
    FTestBot.Location.Pos := P;

    FTestBot.Location.TurnAngle := 0;
    FTestBot.Location.PitchAngle := 0;
    FTestBot.Location.RollAngle := 0;
    {
    FTestBot.AttachModule(sdUp, TWheelModule);
    FTestBot.AttachModule(sdLeft, TWheelModule);
    FTestBot.AttachModule(sdRight, TWheelModule);
    FTestBot.AttachModule(sdFront, TWheelModule);
    FTestBot.AttachModule(sdBack, TWheelModule);
    }
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

  FTestBot.SourceVAO := FTestModel;

  DebugWriteLine(' Done!');
end;

function TfrmMain.GetFloorParams: TResFloorVAOParams;
begin
  Result := TResFloorVAOParams.Create;
  Result.Size := 64;
  Result.Texture := 'grass_top';
end;

procedure TfrmMain.ResizeGL;
begin
  FCamera.Aspect := Aspect;
end;

procedure TfrmMain.UpdateGL;
begin
  if MustUpdateFPS then
    Caption := Format('LuaBattleBots - FPS: %d', [FPSInt]);

  // if Input.ButtonPressed(mbMiddle) then
  // FSun.Position := FCamera.Location.RealPosition;
  FSun.Direction := FSun.Direction.Rotate(Vec3(0, 1, 0.2).Normalize, DeltaTime);

  FCamera.Update;

  FGame.Update(DeltaTime);

//  FTestBot.Location.FreeTranslate(-FTestBot.Location.Offset);

  if Input.KeyDown('A') then
    FTestBot.Location.FreeTranslate(Vec3(-DeltaTime * 10, 0, 0));
  if Input.KeyDown('D') then
    FTestBot.Location.FreeTranslate(Vec3(+DeltaTime * 10, 0, 0));

  if Input.KeyDown('S') then
    FTestBot.Location.FreeTranslate(Vec3(0, 0, +DeltaTime * 10));
  if Input.KeyDown('W') then
    FTestBot.Location.FreeTranslate(Vec3(0, 0, -DeltaTime * 10));

  if Input.KeyDown(VK_SHIFT) then
    FTestBot.Location.FreeTranslate(Vec3(0, -DeltaTime * 10, 0));
  if Input.KeyDown(VK_SPACE) then
    FTestBot.Location.FreeTranslate(Vec3(0, +DeltaTime * 10, 0));

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
