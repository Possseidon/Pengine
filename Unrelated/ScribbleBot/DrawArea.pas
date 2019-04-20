unit DrawArea;

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
  TfrmDrawArea = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    function GetSettings: TSettings;

  protected
    procedure Resize; override;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;

  public
    property Settings: TSettings read GetSettings;

  end;

var
  frmDrawArea: TfrmDrawArea;

implementation

uses
  Main;

{$R *.dfm}

{ TfrmDrawArea }

procedure TfrmDrawArea.FormShow(Sender: TObject);
begin
  Left := Settings.DrawArea.C1.X;
  Top := Settings.DrawArea.C1.Y;
  Width := Settings.DrawArea.Width;
  Height := Settings.DrawArea.Height;
end;

function TfrmDrawArea.GetSettings: TSettings;
begin
  Result := frmMain.Settings;
end;

procedure TfrmDrawArea.Resize;
begin
  inherited;
  Settings.DrawArea := IBounds2(IVec2(Left, Top), IVec2(Left + Width, Top + Height));
end;

procedure TfrmDrawArea.WMMove(var Message: TWMMove);
begin
  inherited;
  Resize;
end;

end.
