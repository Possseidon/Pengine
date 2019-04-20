program ScribbleBot;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  SettingsDefine in 'SettingsDefine.pas',
  DrawArea in 'DrawArea.pas' {frmDrawArea},
  PaletteArea in 'PaletteArea.pas' {frmPaletteArea};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDrawArea, frmDrawArea);
  Application.CreateForm(TfrmPaletteArea, frmPaletteArea);
  Application.Run;
end.

