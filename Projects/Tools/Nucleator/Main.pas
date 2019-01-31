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
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.WinXCtrls,

  GdiPlus,
  GdiPlusHelpers,

  Pengine.IntMaths,

  SettingsDialog,
  ReactorDefine,
  PreviewFrame,
  ReactorEvolutionDefine;

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
    pmPopulation: TPopupMenu;
    Inspect1: TMenuItem;
    InstactAll1: TMenuItem;
    actSingleStep: TAction;
    actOpen: TAction;
    gbEvolution: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    btnSingleStep: TButton;
    btnStartStop: TButton;
    GroupBox2: TGroupBox;
    lbPopulation: TListBox;
    btnInspect: TButton;
    seGeneration: TSpinEdit;
    edtFitnessWorst: TEdit;
    edtFitnessAverage: TEdit;
    edtFitnessBest: TEdit;
    gbGraph: TGroupBox;
    pbGraph: TPaintBox;
    actSave: TAction;
    actNew: TAction;
    actSaveAs: TAction;
    actNew1: TMenuItem;
    actSave1: TMenuItem;
    actSaveAs1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Label1: TLabel;
    edtEfficiencyWorst: TEdit;
    edtEfficiencyAverage: TEdit;
    edtEfficiencyBest: TEdit;
    Label2: TLabel;
    edtPowerGenerationWorst: TEdit;
    edtPowerGenerationAverage: TEdit;
    edtPowerGenerationBest: TEdit;
    Label3: TLabel;
    edtHeatGenerationWorst: TEdit;
    edtHeatGenerationAverage: TEdit;
    edtHeatGenerationBest: TEdit;
    aiEvolving: TActivityIndicator;
    actCurrentSettings: TAction;
    ShowSettings1: TMenuItem;
    N3: TMenuItem;
    actStartStop: TAction;
    Evolution1: TMenuItem;
    actSingleStep1: TMenuItem;
    actStartStop1: TMenuItem;
    N4: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPreviewUpdate(Sender: TObject);
    procedure actSingleStepExecute(Sender: TObject);
    procedure actSingleStepUpdate(Sender: TObject);
    procedure actStartStopExecute(Sender: TObject);
    procedure actStartStopUpdate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure pbGraphPaint(Sender: TObject);
  private
    FPreviewWidth: Integer;
    FEvolution: TReactorEvolution;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FEvolution.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FPreviewWidth := ClientWidth;
  ClientWidth := pnlMain.Width;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
var
  Settings: TReactorEvolution.ISettings;
begin
  Settings := TReactorEvolution.TSettings.Create;
  frmSettings.Execute(Settings);
  FEvolution := TReactorEvolution.Create(Settings);
end;

procedure TfrmMain.actPreviewExecute(Sender: TObject);
var
  Tmp: Integer;
begin
  frmPreview.Visible := not frmPreview.Visible;
  if frmPreview.Visible then
  begin
    Tmp := FPreviewWidth;
    FPreviewWidth := 0;
    ClientWidth := Tmp;
  end
  else
  begin
    FPreviewWidth := ClientWidth;
    ClientWidth := pnlMain.Width;
  end;
end;

procedure TfrmMain.actPreviewUpdate(Sender: TObject);
begin
  actPreview.Checked := frmPreview.Visible;
end;

procedure TfrmMain.actSingleStepExecute(Sender: TObject);
begin
  // TODO: SingleStep
end;

procedure TfrmMain.actSingleStepUpdate(Sender: TObject);
begin
  actSingleStep.Enabled := FEvolution <> nil;
end;

procedure TfrmMain.actStartStopExecute(Sender: TObject);
begin
  // TODO: Start/Stop
end;

procedure TfrmMain.actStartStopUpdate(Sender: TObject);
begin
  actStartStop.Enabled := FEvolution <> nil;
end;

procedure TfrmMain.FormCanResize(Sender: TObject; var NewWidth, NewHeight:
  Integer; var Resize: Boolean);
begin
  if FPreviewWidth <> 0 then
    NewWidth := pnlMain.Width + Width - ClientWidth;
  Resize := True;
end;

procedure TfrmMain.pbGraphPaint(Sender: TObject);
var
  Graphics: IGPGraphics;
begin
  Graphics := pbGraph.ToGPGraphics;
  // Graphics.FillEllipse(TGPSolidBrush.Create(TGPColor.Black), TGPRectF.Create(0, 0, pbGraph.Width, pbGraph.Height));
end;

end.
