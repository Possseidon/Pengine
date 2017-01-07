unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  OpenGLContext, Shaders, FontManager, GUI, SkyDome, ControlledCamera, Color;

type
  TfrmMain = class(TGLForm)
  private
    // Shader
    FGUIShader: TShader;
    FSkyDomeShader: TShader;

    // Font
    FFont: TBMPFontItem;

    // GUI
    FGUI: TGUI;

    // SkyDome
    FSkyDome: TSkyDome;

    // Camera
    FCamera: TSmoothControlledCamera;

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

procedure TfrmMain.Init;
begin
  VSync := False;
  FPSLimit := 256;

  FGUIShader := TShader.Create;
  FGUIShader.VertexFragmentShaderFromResource('GUI_SHADER');
  FGUIShader.AddAttribute(3, 'vpos');
  FGUIShader.AddAttribute(2, 'vtexcoord');
  FGUIShader.AddAttribute(4, 'vcolor');
  FGUIShader.AddAttribute(2, 'vborderlow');
  FGUIShader.AddAttribute(2, 'vborderhigh');

  FSkyDomeShader := TShader.Create;
  FSkyDomeShader.VertexFragmentShaderFromResource('SKYDOME_SHADER');
  FSkyDomeShader.AddAttribute(3, 'vpos');
  FSkyDomeShader.AddAttribute(1, 'vpitch');

  FSkyDome := TSkyDome.Create(Self, FCamera, FSkyDomeShader);
  FSkyDome.AddStripe(TColorRGB.Create(0.3, 0.5, 0.7), -90);
  FSkyDome.AddStripe(TColorRGB.Create(0.4, 0.6, 0.8), 0);
  FSkyDome.AddStripe(TColorRGB.Create(0.7, 0.8, 0.9), 90);

  FCamera := TSmoothControlledCamera.Create(60, Aspect, 0.1, 100, Self);
  FCamera.AddShader(FSkyDomeShader);
  FCamera.AddVAO(FSkyDome);

  FFont := TBMPFontItem.Create;
  FFont.LoadFromPNGResource('FONT');

  FGUI := TGUI.Create(FGUIShader, Self, FFont);
  FGUI.AddTextureFromResource('STONE_BUTTON', 'button');
  FGUI.AddTextureFromResource('STONE_TEXTFIELD', 'textfield');

  with TGLButton.Create(FGUI) do
  begin
    Caption := 'Hi';
    Height := 0.2;
    Width := 5;
  end;
end;

procedure TfrmMain.Finalize;
begin
  FCamera.Free;
  FSkyDome.Free;
  FGUI.Free;
  FGUIShader.Free;
  FSkyDomeShader.Free;
  FFont.Free;
end;

procedure TfrmMain.ResizeFunc;
begin
  FGUI.Aspect := Aspect;
end;

procedure TfrmMain.UpdateFunc;
begin
  if MustUpdateFPS then
    Caption := Format('ModelEditor: %d', [FPSInt]);

  if Input.KeyPressed('V') then
    VSync := not VSync;

  if Input.KeyPressed(VK_F11) then
    Fullscreen := not Fullscreen;

  FCamera.Update;

  FGUI.Update;
end;

procedure TfrmMain.RenderFunc;
begin
  FCamera.Render;

  FGUI.Render;
end;

end.
