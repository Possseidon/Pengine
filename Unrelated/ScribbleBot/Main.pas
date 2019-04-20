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
  Vcl.StdCtrls,

  SettingsDefine,
  Pengine.IntMaths;

type

  TfrmMain = class(TForm)
    btnEditDrawArea: TButton;
    btnEditPaletteArea: TButton;
    procedure btnEditDrawAreaClick(Sender: TObject);
    procedure btnEditPaletteAreaClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSettings: TSettings;

  public
    property Settings: TSettings read FSettings;

  end;

var
  frmMain: TfrmMain;

implementation

uses
  DrawArea, PaletteArea;

{$R *.dfm}


procedure TfrmMain.btnEditDrawAreaClick(Sender: TObject);
begin
  frmDrawArea.Show;
end;

procedure TfrmMain.btnEditPaletteAreaClick(Sender: TObject);
begin
  frmPaletteArea.Show;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSettings.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSettings := TSettings.Create;
  Settings.DrawArea := IBounds2(200, 500);
  // Settings.Palette.Area := IBounds2(IVec2(200, 550), IVec2(500, 700));
  Settings.Palette.Area := IBounds2(200, 500);
  Settings.Palette.Size := IVec2(5, 2);
end;

end.
