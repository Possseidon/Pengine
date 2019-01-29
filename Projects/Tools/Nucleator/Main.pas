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
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ActnList,
  Vcl.Menus,

  Pengine.IntMaths,

  PreviewFrame,
  System.Actions;

type
  TfrmMain = class(TForm)
    frmPreview: TfrmPreview;
    pnlMain: TPanel;
    alActions: TActionList;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    View1: TMenuItem;
    Preview1: TMenuItem;
    actExit: TAction;
    actPreview: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPreviewUpdate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ClientWidth := pnlMain.Width;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actPreviewExecute(Sender: TObject);
begin
  frmPreview.Visible := not frmPreview.Visible;
  if frmPreview.Visible then
    ClientWidth := pnlMain.Width + 300
  else
    ClientWidth := pnlMain.Width;
end;

procedure TfrmMain.actPreviewUpdate(Sender: TObject);
begin
  actPreview.Checked := frmPreview.Visible;
end;

end.
