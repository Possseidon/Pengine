unit Main;

interface

uses
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
    FRope: TRope;
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

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
begin
  FSkybox := TSkybox.Create(Self, TSkyboxShader);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FRope := TRope.Create(Vec3(-3, 3, 1), Vec3(3, 3, -1), 1.5);

  FCamera := TSmoothControlledCamera.Create(60, Aspect, 0.01, 100, Self);
  FCamera.Location.OffsetZ := 10;
  FCamera.Location.TurnAngle := -30;
  FCamera.Location.PitchAngle := -20;
  FCamera.AddUniforms(TSkyboxShader.Data);
  FCamera.AddUniforms(TModelShader.Data);

  FCamera.AddRenderable(FSkybox);
  FCamera.AddRenderable(FRope);

  FLightSystem := TLightSystem.Create(Self);
  FLightSystem.BindToShader(TModelShader.Data);
  FLightSystem.Ambient := 0.2;

  FSun := TDirectionalLightShaded.Create(FLightSystem);
  FSun.Size := 10;
  FSun.Direction := -Vec3(0.2, 1, 0.2);
  FSun.AddOccluder(FRope);
end;

procedure TfrmMain.Finalize;
begin
  FSun.Free;
  FLightSystem.Free;
  FCamera.Free;
  FRope.Free;
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
  FLightSystem.RenderShadows;
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
