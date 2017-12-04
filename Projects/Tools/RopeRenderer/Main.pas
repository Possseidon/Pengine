unit Main;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,

  Pengine.GLContext,
  Pengine.Camera,
  Pengine.ControlledCamera,
  Pengine.Skybox,
  Pengine.Color,
  Pengine.Vector,
  Pengine.Light,
  Pengine.GLEnums,
  Pengine.Collections,

  ModelShader,
  RopeDefine;

type

  TSkyboxShader = class(TSkyboxShaderBase)
  protected
    class function GetShaderSource: string; override;
  end;

  TfrmMain = class(TGLForm)
  private
    FCamera: TSmoothControlledCamera;
    FSkybox: TSkybox;
    FRopes: TRefArray<TRope>;
    FLightSystem: TLightSystem;
    FSun: TDirectionalLight;

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

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
var
  I: Integer;
  Rope: TRope;
begin
  VSync := False;

  FSkybox := TSkybox.Create(Self, TSkyboxShader);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FCamera := TSmoothControlledCamera.Create(60, Aspect, 0.01, 100, Self);
  FCamera.Location.OffsetZ := 0;
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;
  FCamera.AddUniforms(TSkyboxShader.Data);
  FCamera.AddUniforms(TModelShader.Data);

  FCamera.AddRenderable(FSkybox);

  FRopes := TRefArray<TRope>.Create(True);

  FRopes.Capacity := 10;
  for I := 1 to FRopes.Capacity do
  begin
    Rope := FRopes.Add(TRope.Create(Vec3(-3, 2, -I), Vec3(3, 2, -I), (I + 1) / 2));
    Rope.Radius := Bounds1(0.3, 0.4);
    Rope.StepsX := 200;
    Rope.StepsR := 90;
    FCamera.AddRenderable(Rope);
  end;

  FLightSystem := TLightSystem.Create(Self);
  FLightSystem.BindToShader(TModelShader.Data);
  FLightSystem.Ambient := 0.2;

  FSun := TDirectionalLight.Create(FLightSystem);
  // FSun.Size := 10;
  FSun.Direction := -Vec3(0.2, 1, 0.2);
  // FSun.AddOccluder(FRope);
end;

procedure TfrmMain.Finalize;
begin
  FSun.Free;
  FLightSystem.Free;
  FCamera.Free;
  FRopes.Free;
  FSkybox.Free;
end;

procedure TfrmMain.UpdateGL;
begin
  if MustUpdateFPS then
    Caption := Format('Rope Renderer - FPS: %d', [FPSInt]);

  FCamera.Update;
  FSun.Direction := FCamera.Location.Look;

end;

procedure TfrmMain.RenderGL;
begin
  // FLightSystem.RenderShadows;
  FCamera.Render;
end;

procedure TfrmMain.ResizeGL;
begin
  FCamera.Aspect := Aspect;
end;

{ TSkyboxShader }

class function TSkyboxShader.GetShaderSource: string;
begin
  Result := 'Data\skybox';
end;

end.
