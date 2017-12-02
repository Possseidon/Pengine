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
  Pengine.ControlledCamera,
  Pengine.Skybox,
  Pengine.Color;

type

  TSkyboxShader = class(TSkyboxShaderBase)
  protected
    class function GetShaderSource: string; override;
  end;

  TForm5 = class(TGLForm)
  private
    FSkybox: TSkybox;
    FCamera: TControlledCamera;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure UpdateGL; override;
    procedure RenderGL; override;
    procedure ResizeGL; override;

  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

{ TForm5 }

procedure TForm5.Init;
begin
  FCamera := TControlledCamera.Create(60, Aspect, 0.01, 100, Input);

  FCamera.AddUniforms(TSkyboxShader.Data);

  FSkybox := TSkybox.Create(Self, TSkyboxShader);

  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);

  FCamera.AddRenderObject(FSkybox);
end;

procedure TForm5.Finalize;
begin
  FSkybox.Free;
  FCamera.Free;
end;

procedure TForm5.UpdateGL;
begin
  if MustUpdateFPS then
    Caption := Format('FPS: %d', [FPSInt]);
  FCamera.Update;
end;

procedure TForm5.RenderGL;
begin
  FCamera.Render;
end;

procedure TForm5.ResizeGL;
begin

end;

{ TSkyboxShader }

class function TSkyboxShader.GetShaderSource: string;
begin
  Result := 'Data\skybox';
end;

end.
