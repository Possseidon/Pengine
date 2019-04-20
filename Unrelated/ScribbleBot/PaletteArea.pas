unit PaletteArea;

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

  Pengine.IntMaths,
  
  SettingsDefine;

type
  TfrmPaletteArea = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetSettings: TSettings;
    property Settings: TSettings read GetSettings;

  end;

var
  frmPaletteArea: TfrmPaletteArea;

implementation

uses
  Main;

{$R *.dfm}


procedure TfrmPaletteArea.FormPaint(Sender: TObject);
var
  X, DrawPos: Integer;
begin
  for X := 0 to Settings.Palette.Size.X do
  begin
    DrawPos := Settings.Palette.Area.Width * X div Settings.Palette.Size.X;
    Canvas.MoveTo(DrawPos, 0);
    Canvas.LineTo(DrawPos, ClientHeight);
  end;

  for X := 0 to Settings.Palette.Size.Y do
  begin
    DrawPos := Settings.Palette.Area.Height * X div Settings.Palette.Size.Y;
    Canvas.MoveTo(0, DrawPos);
    Canvas.LineTo(ClientWidth, DrawPos);
  end;
end;

procedure TfrmPaletteArea.FormShow(Sender: TObject);
begin
  Left := Settings.Palette.Area.C1.X - ClientOrigin.X;
  Top := Settings.Palette.Area.C1.Y - ClientOrigin.Y;
  ClientWidth := Settings.Palette.Area.Width;
  ClientHeight := Settings.Palette.Area.Height;
end;

function TfrmPaletteArea.GetSettings: TSettings;
begin
  Result := frmMain.Settings;
end;

end.
