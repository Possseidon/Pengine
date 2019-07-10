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
  Pengine.Collections,
  Pengine.Utility,
  Pengine.TimeManager,

  SettingsDialog,
  ReactorDefine,
  PreviewFrame,
  ReactorEvolutionDefine;

// Win64 release bug after ~50.000 generations
// Add more info to help with debugging, why the heat doesn't get calculated correctly anymore
// Does it happen without Chart?

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
    seMinValues: TFastLineSeries;
    seAvgValues: TFastLineSeries;
    seMaxValues: TFastLineSeries;
    seAllValues: TPointSeries;
    aeAppEvents: TApplicationEvents;
    pnlTop: TPanel;
    gbEvolution: TGroupBox;
    lbGeneration: TLabel;
    lbWorst: TLabel;
    lbAverage: TLabel;
    lbBest: TLabel;
    lbFitness: TLabel;
    lbEfficiency: TLabel;
    lbPowerGeneration: TLabel;
    lbNetHeatGeneration: TLabel;
    btnSingleStep: TButton;
    btnStartStop: TButton;
    gbPopulation: TGroupBox;
    lvPopulation: TListView;
    seGeneration: TSpinEdit;
    edtFitnessWorst: TEdit;
    edtFitnessAverage: TEdit;
    edtFitnessBest: TEdit;
    edtEfficiencyWorst: TEdit;
    edtEfficiencyAverage: TEdit;
    edtEfficiencyBest: TEdit;
    edtPowerGenerationWorst: TEdit;
    edtPowerGenerationAverage: TEdit;
    edtPowerGenerationBest: TEdit;
    edtNetHeatGenerationWorst: TEdit;
    edtNetHeatGenerationAverage: TEdit;
    edtNetHeatGenerationBest: TEdit;
    gb3DPreview: TGroupBox;
    frmPreview: TfrmPreview;
    spltChart: TSplitter;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actSingleStepExecute(Sender: TObject);
    procedure actStartStopExecute(Sender: TObject);
    procedure aeAppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure lvPopulationColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvPopulationCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure lvPopulationSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure seGenerationChange(Sender: TObject);
  private
    FEvolution: TReactorEvolution;
    FSortColumn: Integer;
    FEvolutionRunning: Boolean;
    FUpdateTimer: TStopWatch;
    FLoggedChartValues: Integer;

    procedure UpdateGeneration(AJumpToLast: Boolean);
    procedure UpdateStats;
    procedure UpdatePopulation;
    procedure UpdateChart;

    procedure ClearChartValues;

    function GetCurrentGeneration: TEvolutionGeneration;

    function FormatStat(AValue: Single; Digits: Integer): string;

  public
    property Evolution: TReactorEvolution read FEvolution;
    property CurrentGeneration: TEvolutionGeneration read GetCurrentGeneration;

  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.Generics.Collections;

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

  ClearChartValues;

  Settings := TEvolutionSettings.Create;
  Settings.SetGeneratorFunction(TDefaultGeneratorFunction);
  Settings.SetMutationFunction(TDefaultMutationFunction);
  Settings.SetFitnessFunction(TDefaultFitnessFunction);
  FEvolution := TReactorEvolution.Create(Settings);

  UpdateGeneration(True);
  UpdateChart;
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
    seGeneration.Value := 1;
    UpdateGeneration(True);
    UpdateChart;
  end;
end;

procedure TfrmMain.actSingleStepExecute(Sender: TObject);
begin
  FEvolution.Evolve;
  UpdateGeneration(True);
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
    UpdateGeneration(True);
    UpdateChart;
  end;
end;

procedure TfrmMain.aeAppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  Done := not FEvolutionRunning;
  if Done then
    Exit;

  FEvolution.Evolve;
  if FUpdateTimer.Time > 0.25 then
  begin
    UpdateGeneration(True);
    UpdateChart;
    FUpdateTimer.Start;
  end;
end;

procedure TfrmMain.lvPopulationColumnClick(Sender: TObject; Column: TListColumn);
begin
  if FSortColumn = Column.Index + 1 then
    FSortColumn := -FSortColumn
  else
    FSortColumn := Column.Index + 1;
  lvPopulation.AlphaSort;
end;

procedure TfrmMain.lvPopulationCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  A, B: TReactorRating;
  Settings: IEvolutionSettings;
begin
  A := TObject(Item1.Data) as TReactorRating;
  B := TObject(Item2.Data) as TReactorRating;
  Settings := CurrentGeneration.Settings;
  case Abs(FSortColumn) of
    1:
      Compare := Sign(B.Fitness - A.Fitness);
    2:
      Compare := Sign(B.Efficiency - A.Efficiency);
    3:
      Compare := Sign(B.PowerGeneration - A.PowerGeneration);
    4:
      Compare := Sign(A.NetHeatGeneration - B.NetHeatGeneration);
  end;
  Compare := Sign(FSortColumn) * Compare;
end;

procedure TfrmMain.seGenerationChange(Sender: TObject);
begin
  UpdateGeneration(False);
end;

procedure TfrmMain.UpdateGeneration(AJumpToLast: Boolean);
var
  Value: Integer;
begin
  seGeneration.MaxValue := FEvolution.Generations.Count;
  if AJumpToLast then
    seGeneration.Value := FEvolution.Generations.Count;
  if Integer.TryParse(seGeneration.Text, Value) and InRange(Value, seGeneration.MinValue, seGeneration.MaxValue) then
  begin
    UpdateStats;
    UpdatePopulation;
  end;
end;

procedure TfrmMain.UpdateStats;
var
  Gen: TEvolutionGeneration;
begin
  Gen := CurrentGeneration;
  // Fitness
  edtFitnessWorst.Text := FormatStat(Gen.FitnessStats.Worst, 0);
  edtFitnessAverage.Text := FormatStat(Gen.FitnessStats.Average, 0);
  edtFitnessBest.Text := FormatStat(Gen.FitnessStats.Best, 0);
  // Efficiency
  edtEfficiencyWorst.Text := FormatStat(Gen.EfficiencyStats.Worst, 2);
  edtEfficiencyAverage.Text := FormatStat(Gen.EfficiencyStats.Average, 2);
  edtEfficiencyBest.Text := FormatStat(Gen.EfficiencyStats.Best, 2);
  // Power Generation
  edtPowerGenerationWorst.Text := FormatStat(Gen.PowerGenerationStats.Worst, 0);
  edtPowerGenerationAverage.Text := FormatStat(Gen.PowerGenerationStats.Average, 0);
  edtPowerGenerationBest.Text := FormatStat(Gen.PowerGenerationStats.Best, 0);
  // Net Heat Generation
  edtNetHeatGenerationWorst.Text := FormatStat(Gen.NetHeatGenerationStats.Worst, 0);
  edtNetHeatGenerationAverage.Text := FormatStat(Gen.NetHeatGenerationStats.Average, 0);
  edtNetHeatGenerationBest.Text := FormatStat(Gen.NetHeatGenerationStats.Best, 0);
end;

procedure TfrmMain.UpdatePopulation;
var
  Generation: TEvolutionGeneration;
  ReactorRating: TReactorRating;
  Item: TListItem;
begin
  FSortColumn := 1;
  Generation := CurrentGeneration;
  lvPopulation.Items.BeginUpdate;
  lvPopulation.Items.Clear;
  try
    for ReactorRating in Generation.ReactorRatings do
    begin
      Item := lvPopulation.Items.Add;
      Item.Data := ReactorRating;
      Item.Caption := FormatStat(ReactorRating.Fitness, 0);
      Item.SubItems.Add(FormatStat(ReactorRating.Efficiency, 2));
      Item.SubItems.Add(FormatStat(ReactorRating.PowerGeneration, 0));
      Item.SubItems.Add(FormatStat(ReactorRating.NetHeatGeneration, 0));
    end;

  finally
    lvPopulation.Items.EndUpdate;

  end;
end;

procedure TfrmMain.UpdateChart;
var
  Series: TChartSeries;
  I: Integer;
  Stats: TEvolutionGeneration.TStats;
begin
  for Series in tcStatistics.SeriesList do
    Series.BeginUpdate;

  try
    for I := FLoggedChartValues to FEvolution.Generations.MaxIndex do
    begin
      Stats := FEvolution.Generations[I].FitnessStats;
      seMinValues.AddXY(I + 1, Stats.Worst);
      seAvgValues.AddXY(I + 1, Stats.Average);
      seMaxValues.AddXY(I + 1, Stats.Best);
      // for Reactor in Generation.Reactors do
      // ChartAllValues.AddXY(I, Reactor.Fitness);
    end;
    FLoggedChartValues := FEvolution.Generations.Count;

  finally
    for Series in tcStatistics.SeriesList do
      Series.EndUpdate;

    tcStatistics.Invalidate;

  end;
end;

procedure TfrmMain.ClearChartValues;
begin
  FLoggedChartValues := 0;
  tcStatistics.SeriesList.ClearValues;
end;

function TfrmMain.GetCurrentGeneration: TEvolutionGeneration;
begin
  Result := FEvolution.Generations[seGeneration.Value - 1];
end;

function TfrmMain.FormatStat(AValue: Single; Digits: Integer): string;
begin
  Result := Format('%.' + Digits.ToString + 'f', [AValue], FormatSettings.Invariant);
end;

procedure TfrmMain.lvPopulationSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not Selected then
    Exit;
  frmPreview.ReactorRating := TObject(Item.Data) as TReactorRating;
end;

end.
