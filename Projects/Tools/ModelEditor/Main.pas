unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  OpenGLContext, Shaders, FontManager, GUI, SkyDome, ControlledCamera, Color, InputHandler, VectorGeometry, Matrix,
  Vcl.StdCtrls;

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
    FButton: TGLButton;
    FText: TGLTextfield;
    FCheckbox: TGLCheckbox;
    FContainer: TGLContainerControl;
    FContainerIn: TGLContainerControl;
    FTrackbarF: TGLTrackbarFixed;
    FTrackbarS: TGLTrackbarSmooth;

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
  FGUI.AddTextureFromResource('STONE_CHECKBOX', 'checkbox');
  FGUI.AddTextureFromResource('STONE_TRACKBAR', 'trackbar');

  FContainer := TGLContainerControl.Create(FGUI);
  FContainer.Size := TGVector2.Create(1, 1);
  FContainer.XOrigin := haLeft;

  FContainerIn := TGLContainerControl.Create(FContainer);
  FContainerIn.Size := TGVector2.Create(1, 1);
  FContainerIn.XOrigin := haLeft;

  FButton := TGLButton.Create(FContainerIn);
  FButton.Caption := 'Hi';
  FButton.Height := 0.2;
  FButton.Width := 1;
  FButton.Depth := -0.5;

  FText := TGLTextfield.Create(FGUI);
  FText.Height := 0.2;
  FText.PosY := 0.5;
  FText.Width := 10;

  FCheckbox := TGLCheckbox.Create(FGUI);
  FCheckbox.PosX := 1;
  FCheckbox.Height := 0.25;

  FTrackbarF := TGLTrackbarFixed.Create(FContainer);
  FTrackbarF.Min := 1;
  FTrackbarF.Max := 5;
  FTrackbarF.Width := 4;
  FTrackbarF.Height := 0.5;
  FTrackbarF.TrackPos := 3;
  FTrackbarF.XOrigin := haLeft;
  FTrackbarF.YOrigin := vaTop;

  FTrackbarS := TGLTrackbarSmooth.Create(FContainer);
  FTrackbarS.Min := 0;
  FTrackbarS.Ticks := 5;
  FTrackbarS.Max := 1;
  FTrackbarS.Width := 4;
  FTrackbarS.Height := 0.5;
  FTrackbarS.TrackPos := 0.5;
  FTrackbarS.XOrigin := haLeft;
  FTrackbarS.YOrigin := vaBottom;

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

  if FButton.Pressed then
  begin
    FText.TextfieldColor := TColorRGB.Rainbow(Random * 6);
    FText.TextColor := not FText.TextfieldColor;
    FButton.Caption := 'Aua';
  end;

  FButton.Pos := FContainerIn.MousePos;

  //FTrackbarF.Height := Sin(Seconds) * 0.15 + 0.4;
  //FTrackbarS.Height := Sin(Seconds) * 0.1 + 0.6;
  //FContainer.Height := Sin(Seconds) + 1;

  FCamera.Update;

  FGUI.Update;
end;

procedure TfrmMain.RenderFunc;
begin
  FCamera.Render;

  FGUI.Render;
end;

end.
