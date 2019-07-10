unit Main;

interface

uses
  System.Classes,
  System.Actions,
  System.Math,
  System.SysUtils,

  Vcl.AppEvnts,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  VclTee.TeeGDIPlus,
  VclTee.Series,
  VclTee.TeEngine,
  VclTee.TeeProcs,
  VclTee.Chart,

  GdiPlus,
  GdiPlusHelpers,

  Pengine.IntMaths,
  Pengine.ICollections,
  Pengine.Utility,
  Pengine.TimeManager,

  SettingsDialog,
  ReactorDefine,
  PreviewFrame,
  ReactorEvolutionDefine;

// Win64 release bug after ~50.000 generations
// Add more info to help with debugging, why the heat doesn't get calculated correctly anymore
// Does it happen without Chart?

// TODO: Didn't I fix this???

type
  TfrmMain = class(TForm)
    alActions: TActionList;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    View1: TMenuItem;
    actExit: TAction;
    actSingleStep: TAction;
    actOpen: TAction;
    gbChart: TGroupBox;
    actSave: TAction;
    actNew: TAction;
    actSaveAs: TAction;
    actNew1: TMenuItem;
    actSave1: TMenuItem;
    actSaveAs1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    actCurrentSettings: TAction;
    ShowSettings1: TMenuItem;
    actStartStop: TAction;
    Evolution1: TMenuItem;
    actSingleStep1: TMenuItem;
    actStartStop1: TMenuItem;
    N4: TMenuItem;
    tcStatistics: TChart;
    seBestValues: TFastLineSeries;
    aeAppEvents: TApplicationEvents;
    pnlTop: TPanel;
    gbEvolution: TGroupBox;
    lbGeneration: TLabel;
    btnSingleStep: TButton;
    btnStartStop: TButton;
    gb3DPreview: TGroupBox;
    frmPreview: TfrmPreview;
    spltChart: TSplitter;
    lvBreakthroughs: TListView;
    edtGeneration: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actSingleStepExecute(Sender: TObject);
    procedure actStartStopExecute(Sender: TObject);
    procedure aeAppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure lvBreakthroughsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvBreakthroughsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure lvBreakthroughsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FEvolution: TReactorEvolution;
    FSortColumn: Integer;
    FEvolutionRunning: Boolean;
    FUpdateTimer: TStopWatch;
    FLoggedChartValues: Integer;

    procedure UpdateGeneration;
    procedure UpdateBreakthroughs;
    procedure UpdateChart;

    procedure ClearChartValues;

    function FormatStat(AValue: Single; Digits: Integer): string;

  public
    property Evolution: TReactorEvolution read FEvolution;

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
var
  Settings: IEvolutionSettings;
begin
  tcStatistics.Zoom.MouseWheel := pmwNormal;

  FSortColumn := 1;
  ClearChartValues;

  Settings := TEvolutionSettings.Create;
  Settings.SetGeneratorFunction(TDefaultGeneratorFunction);
  Settings.SetMutationFunction(TDefaultMutationFunction);
  Settings.SetFitnessFunction(TDefaultFitnessFunction);
  FEvolution := TReactorEvolution.Create(Settings);

  UpdateGeneration;
  UpdateBreakthroughs;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
var
  Settings: IEvolutionSettings;
begin
  Settings := TEvolutionSettings.Create;
  if frmSettings.Execute(Settings) then
  begin
    FEvolution.Free;
    FEvolution := TReactorEvolution.Create(Settings);
    ClearChartValues;
    UpdateGeneration;
    UpdateBreakthroughs;
  end;
end;

procedure TfrmMain.actSingleStepExecute(Sender: TObject);
begin
  FEvolution.Evolve;
  UpdateBreakthroughs;
  UpdateChart;
end;

procedure TfrmMain.actStartStopExecute(Sender: TObject);
begin
  FEvolutionRunning := not FEvolutionRunning;
  if FEvolutionRunning then
  begin
    actStartStop.Caption := 'Stop';
    FUpdateTimer.Start;
  end
  else
  begin
    actStartStop.Caption := 'Start';
    UpdateBreakthroughs;
    UpdateChart;
  end;
end;

procedure TfrmMain.aeAppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  Done := not FEvolutionRunning;
  if Done then
    Exit;

  FEvolution.Evolve;
  UpdateGeneration;
  UpdateBreakthroughs;
  if FUpdateTimer.Time > 0.5 then
  begin
    UpdateChart;
    FUpdateTimer.Start;
  end;
end;

procedure TfrmMain.lvBreakthroughsColumnClick(Sender: TObject; Column: TListColumn);
begin
  if FSortColumn = Column.Index + 1 then
    FSortColumn := -FSortColumn
  else
    FSortColumn := Column.Index + 1;
  lvBreakthroughs.AlphaSort;
end;

procedure TfrmMain.lvBreakthroughsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  A, B: TReactorRating;
begin
  A := TObject(Item1.Data) as TReactorRating;
  B := TObject(Item2.Data) as TReactorRating;
  case Abs(FSortColumn) of
    1:
      Compare := Sign(B.Generation - A.Generation);
    2:
      Compare := Sign(B.Fitness - A.Fitness);
    3:
      Compare := Sign(B.Efficiency - A.Efficiency);
    4:
      Compare := Sign(B.PowerGeneration - A.PowerGeneration);
    5:
      Compare := Sign(A.NetHeatGeneration - B.NetHeatGeneration);
    6:
      Compare := Sign(B.CellCount - A.CellCount);
  end;
  Compare := Sign(FSortColumn) * Compare;
end;

procedure TfrmMain.UpdateBreakthroughs;
var
  I: Integer;
  Reactor: TReactorRating;
  Item: TListItem;
begin
  if FLoggedChartValues = Evolution.ReactorBreakthroughs.Count then
    Exit;

  lvBreakthroughs.Items.BeginUpdate;
  seBestValues.BeginUpdate;

  try
    if FLoggedChartValues <> 0 then
      UpdateChart;

    for I := FLoggedChartValues to FEvolution.ReactorBreakthroughs.MaxIndex do
    begin
      Reactor := FEvolution.ReactorBreakthroughs[I];
      seBestValues.AddXY(Reactor.Generation, Reactor.Fitness);
      seBestValues.AddXY(Reactor.Generation, Reactor.Fitness);

      Item := lvBreakthroughs.Items.Add;
      Item.Data := Reactor;
      Item.Caption := Reactor.Generation.ToString;
      Item.SubItems.Add(FormatStat(Reactor.Fitness, 0));
      Item.SubItems.Add(FormatStat(Reactor.Efficiency, 2));
      Item.SubItems.Add(FormatStat(Reactor.PowerGeneration, 0));
      Item.SubItems.Add(FormatStat(Reactor.NetHeatGeneration, 0));
      Item.SubItems.Add(Reactor.CellCount.ToString);
    end;

    FLoggedChartValues := FEvolution.ReactorBreakthroughs.Count;

  finally
    lvBreakthroughs.Items.EndUpdate;
    seBestValues.EndUpdate;

    tcStatistics.Invalidate;
  end;
end;

procedure TfrmMain.UpdateChart;
begin
  seBestValues.XValue[seBestValues.Count - 1] := Evolution.Generation.Index;
end;

procedure TfrmMain.UpdateGeneration;
begin
  edtGeneration.Text := Evolution.Generation.Index.ToString;
end;

procedure TfrmMain.ClearChartValues;
begin
  FLoggedChartValues := 0;
  lvBreakthroughs.Clear;
  seBestValues.Clear;
end;

function TfrmMain.FormatStat(AValue: Single; Digits: Integer): string;
begin
  Result := Format('%.' + Digits.ToString + 'f', [AValue], FormatSettings.Invariant);
end;

procedure TfrmMain.lvBreakthroughsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not Selected then
    Exit;
  frmPreview.ReactorRating := TObject(Item.Data) as TReactorRating;
end;

end.
