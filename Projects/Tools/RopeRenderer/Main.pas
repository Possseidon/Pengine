unit Main;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,

  VCL.Dialogs,

  Pengine.GLContext,
  Pengine.GLState,
  Pengine.GLForm,
  Pengine.Camera,
  Pengine.ControlledCamera,
  Pengine.Skybox,
  Pengine.Color,
  Pengine.Vector,
  Pengine.Light,
  Pengine.GLEnums,
  Pengine.Collections,
  Pengine.GLProgram,
  Pengine.InputHandler,
  Pengine.IntMaths,
  Pengine.TimeManager,

  ModelShader,
  RopeDefine;

type

  TSkyboxGLProgram = class(TSkyboxGLProgramBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

  TRopes = TObjectArray<TRopeProxy>;

  TfrmMain = class(TGLForm)
  private
    FCamera: TSmoothControlledCamera;
    FSkyboxGLProgram: TGLProgram;
    FModelGLProgram: TGLProgram;
    FSkybox: TSkybox;
    FBaseRope: TRope;
    FRopes: TRopes;
    FLightSystem: TLightSystem;
    FSun: TDirectionalLightShaded;

    procedure UpdateGame;
    procedure UpdateFPS(ATimer: TDeltaTimer.TEventInfo);

  public
    procedure Init; override;
    procedure Finalize; override;

  end;

var
  frmMain: TfrmMain;

implementation

uses
  Winapi.Windows;

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
var
  I: Integer;
  Rope: TRopeProxy;
  Step: TIntVector2;
  A, B, C, D: TVector3;
  P: TPlane3;
begin
  Context.VSync := True;

  Context.Samples := Context.MaxSamples;

  Context.GLDebugLogLevels := [dmsNotification];

  GLState[stClearColor] := $7f7f7f;

  GLState[stBlend] := True;
  GLState[stBlendFunc] := TGLBlendFunc.Make(bfsOne, bfdOneMinusSrcAlpha);

  FSkyboxGLProgram := TSkyboxGLProgram.Make(GLState.ResParam);
  FModelGLProgram := TModelGLProgram.Make(GLState.ResParam);

  FSkybox := TSkybox.Create(FSkyboxGLProgram);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FLightSystem := TLightSystem.Create(Game);
  FLightSystem.BindToGLProgram(FModelGLProgram);
  FLightSystem.Ambient := 0.25;

  FCamera := TSmoothControlledCamera.Create(Game, 60, 0.01, 100);
  FCamera.Location.OffsetZ := 15;
  FCamera.Location.TurnAngle := -70;
  FCamera.Location.PitchAngle := -30;
  FCamera.AddUniforms(FSkyboxGLProgram);
  FCamera.AddUniforms(FModelGLProgram);

  FCamera.AddRenderable(FSkybox);

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Size := 20;
  FSun.Direction := -Vec3(0.2, 1, 0.2);

  FBaseRope := TRope.Create(GLState, Vec3(-3, 2, 0), Vec3(3, 2, 0), 2);
  FBaseRope.Radius := Bounds1(0.3, 0.4);
  FBaseRope.StepsX := 16;
  FBaseRope.StepsR := 16;
  FBaseRope.Smooth := False;

  FCamera.AddRenderable(FBaseRope);
  FSun.AddOccluder(FBaseRope);

  FRopes := TRopes.Create;

  FRopes.Capacity := 10;
  for I := 0 to FRopes.Capacity - 1 do
  begin
    Rope := FRopes.Add(TRopeProxy.Create(FBaseRope, Vec3(-3, 2, 9.5 - I * 2), Vec3(3, 2, 8.5 - I * 2), (10 - I)));
    Rope.Point1 := Rope.Point1 + TVector3.RandomNormal * 1.5;
    Rope.Point2 := Rope.Point2 + TVector3.RandomNormal * 1.5;
    FCamera.AddRenderable(Rope);
    FSun.AddOccluder(Rope);
  end;

  Game.Timer.OnFPSUpdate.Add(UpdateFPS);
  Game.OnUpdate.Add(UpdateGame);
end;

procedure TfrmMain.Finalize;
begin
  if FSkyboxGLProgram <> nil then
    TSkyboxGLProgram.Release(GLState.ResParam);
  if FModelGLProgram <> nil then
    TModelGLProgram.Release(GLState.ResParam);
  FSun.Free;
  FLightSystem.Free;
  FCamera.Free;
  FRopes.Free;
  FBaseRope.Free;
  FSkybox.Free;
end;

procedure TfrmMain.UpdateFPS(ATimer: TDeltaTimer.TEventInfo);
begin
  Caption := Format('Rope Renderer - FPS: %d', [Context.FPSInt]);
end;

procedure TfrmMain.UpdateGame;
begin
  if Input.KeyDown(VK_SPACE) then
    FSun.Direction := FCamera.Location.Look;

  if Input.KeyTyped('M') then
    Context.MultiSampled := not Context.MultiSampled;
end;

{ TSkyboxGLProgram }

class procedure TSkyboxGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  if AResource then
    AName := 'SKYBOX'
  else
    AName := 'Data/skybox';
end;

end.
