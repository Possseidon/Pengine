unit Main;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,

  Pengine.GLContext,
  Pengine.GLState,
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

  ModelShader,
  RopeDefine;

type

  TSkyboxGLProgram = class(TSkyboxGLProgramBase)
  protected
    class function GetFileName: string; override;
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

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure UpdateGL; override;
    procedure RenderGL; override;
    procedure ResizeGL; override;

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
begin
  Context.VSync := True;

  Context.Samples := Context.MaxSamples;

  GLState[stBlend] := True;
  GLState[stBlendFunc] := TGLBlendFunc.Make(bfsOne, bfdOneMinusSrcAlpha);

  FSkyboxGLProgram := TSkyboxGLProgram.Make(GLState.ResourceParam);
  FModelGLProgram := TModelGLProgram.Make(GLState.ResourceParam);

  FSkybox := TSkybox.Create(FSkyboxGLProgram);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FCamera := TSmoothControlledCamera.Create(60, Aspect, 0.01, 100, Self);
  FCamera.Location.OffsetZ := 15;
  FCamera.Location.TurnAngle := -70;
  FCamera.Location.PitchAngle := -30;
  FCamera.AddUniforms(FSkyboxGLProgram);
  FCamera.AddUniforms(FModelGLProgram);

  FCamera.AddRenderable(FSkybox);

  FLightSystem := TLightSystem.Create(GLState);
  FLightSystem.BindToGLProgram(FModelGLProgram);
  FLightSystem.Ambient := 0.25;

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Size := 20;
  FSun.Direction := -Vec3(0.2, 1, 0.2);

  FBaseRope := TRope.Create(GLState, Vec3(-3, 2, 0), Vec3(3, 2, 0), 3);
  FBaseRope.Radius := Bounds1(0.3, 0.4);
  FBaseRope.StepsX := 16;
  FBaseRope.StepsR := 16;
  FBaseRope.Smooth := False;

  // FCamera.AddRenderable(FBaseRope);
  // FSun.AddOccluder(FBaseRope);

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
end;

procedure TfrmMain.Finalize;
begin
  TModelGLProgram.Release(GLState.ResourceParam);
  TSkyboxGLProgram.Release(GLState.ResourceParam);
  FSun.Free;
  FLightSystem.Free;
  FCamera.Free;
  FRopes.Free;
  FBaseRope.Free;
  FSkybox.Free;
end;

procedure TfrmMain.UpdateGL;
var
  Rope: TRopeProxy;
begin
  if Context.MustUpdateFPS then
    Caption := Format('Rope Renderer - FPS: %d', [Context.FPSInt]);

  FCamera.Update;

  if Input.KeyDown(VK_SPACE) then
    FSun.Direction := FCamera.Location.Look;

end;

procedure TfrmMain.RenderGL;
begin
  FLightSystem.RenderShadows;
  FCamera.Render;
end;

procedure TfrmMain.ResizeGL;
begin
  FCamera.Aspect := Aspect;
end;

{ TSkyboxShader }

class function TSkyboxGLProgram.GetFileName: string;
begin
  Result := 'Data\skybox';
end;

end.
