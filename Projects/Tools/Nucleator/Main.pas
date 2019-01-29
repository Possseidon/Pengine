unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,

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

  ReactorDefine,
  PreviewFrame;

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
var
  Reactor: TReactor;
  P: TIntVector3;
begin
  ClientWidth := pnlMain.Width;

  Reactor := TReactor.Create(IVec3(3, 3, 3));
  Reactor[IVec3(0, 1, 1)] := rbReactorCell;
  Reactor[IVec3(1, 1, 1)] := rbModeratorBlock;  
  Reactor[IVec3(2, 1, 1)] := rbReactorCell;
  Reactor[IVec3(1, 0, 1)] := rbWaterCooler; 
  Reactor[IVec3(1, 2, 1)] := rbWaterCooler; 
  Reactor[IVec3(1, 1, 0)] := rbWaterCooler; 
  Reactor[IVec3(1, 1, 2)] := rbWaterCooler; 
  Reactor[IVec3(2, 2, 1)] := rbRedstoneCooler; 

  ShowMessageFmt('Efficiency: %.1f%%' + sLineBreak + 'HeatFactor: %.1f%%' + sLineBreak + 
    'Power: %.1f rf/t' + sLineBreak + 'Net-Heat: %.1f h/t' + sLineBreak + 'Cooling: %.1f h/t',
    [Reactor.Efficiency * 100, Reactor.HeatFactor * 100, Reactor.PowerGeneration(180), Reactor.HeatGeneration(21.6),
    Reactor.CoolingRate]);

  Reactor.Free;

  Application.Terminate;
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
