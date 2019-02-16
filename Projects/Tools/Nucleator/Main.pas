unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  System.Math,

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

  VclTee.TeeGDIPlus,
  VclTee.TeEngine,
  VclTee.Series,
  VclTee.TeeProcs,
  VclTee.Chart,

  GdiPlus,
  GdiPlusHelpers,

  Pengine.IntMaths,
  Pengine.Collections,
  Pengine.Utility,

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
    lbGeneration: TLabel;
    lbWorst: TLabel;
    lbAverage: TLabel;
    lbBest: TLabel;
    lbFitness: TLabel;
    btnSingleStep: TButton;
    btnStartStop: TButton;
    gbPopulation: TGroupBox;
    lvPopulation: TListView;
    seGeneration: TSpinEdit;
    edtFitnessWorst: TEdit;
    edtFitnessAverage: TEdit;
    edtFitnessBest: TEdit;
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
    lbEfficiency: TLabel;
    edtEfficiencyWorst: TEdit;
    edtEfficiencyAverage: TEdit;
    edtEfficiencyBest: TEdit;
    lbPowerGeneration: TLabel;
    edtPowerGenerationWorst: TEdit;
    edtPowerGenerationAverage: TEdit;
    edtPowerGenerationBest: TEdit;
    lbNetHeatGeneration: TLabel;
    edtNetHeatGenerationWorst: TEdit;
    edtNetHeatGenerationAverage: TEdit;
    edtNetHeatGenerationBest: TEdit;
    aiEvolving: TActivityIndicator;
    actCurrentSettings: TAction;
    ShowSettings1: TMenuItem;
    N3: TMenuItem;
    actStartStop: TAction;
    Evolution1: TMenuItem;
    actSingleStep1: TMenuItem;
    actStartStop1: TMenuItem;
    N4: TMenuItem;
    tcStatistics: TChart;
    Series1: TFastLineSeries;
    Series2: TFastLineSeries;
    Series3: TFastLineSeries;
    Series4: TPointSeries;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPreviewUpdate(Sender: TObject);
    procedure actSingleStepExecute(Sender: TObject);
    procedure actStartStopExecute(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure lvPopulationColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvPopulationCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure seGenerationChange(Sender: TObject);
  private
    FPreviewWidth: Integer;
    FEvolution: TReactorEvolution;
    FSortColumn: Integer;

    procedure UpdateAll;
    procedure UpdateGeneration;
    procedure UpdateStats;
    procedure UpdatePopulation;
    procedure UpdateChart;

    procedure ClearChartValues;

    function GetCurrentGeneration: TEvolutionGeneration;

    function FormatStat(AValue: Single; Digits: Integer): string;
    function GetChartValues(const Index: Integer): TChartSeries;

  public
    property Evolution: TReactorEvolution read FEvolution;
    property CurrentGeneration: TEvolutionGeneration read GetCurrentGeneration;

    property ChartMinValues: TChartSeries index 0 read GetChartValues;
    property ChartAvgValues: TChartSeries index 1 read GetChartValues;
    property ChartMaxValues: TChartSeries index 2 read GetChartValues;
    property ChartAllValues: TChartSeries index 3 read GetChartValues;

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
  FPreviewWidth := ClientWidth;
  ClientWidth := pnlMain.Width;

  tcStatistics.Zoom.MouseWheel := pmwNormal;

  ClearChartValues;

  Settings := TEvolutionSettings.Create;
  Settings.SetGeneratorFunction(TDefaultGeneratorFunction);
  Settings.SetMutationFunction(TDefaultMutationFunction);
  Settings.SetFitnessFunction(TDefaultFitnessFunction);
  FEvolution := TReactorEvolution.Create(Settings);

  UpdateAll;

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
    seGeneration.Value := 1;
    UpdateAll;
  end;
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
  FEvolution.Evolve;
  seGeneration.MaxValue := FEvolution.Generations.Count;
  seGeneration.Value := FEvolution.Generations.Count;
  UpdateAll;
end;

procedure TfrmMain.actStartStopExecute(Sender: TObject);
begin
  // TODO: Start/Stop
end;

procedure TfrmMain.ClearChartValues;
begin
  tcStatistics.SeriesList.ClearValues;
end;

function TfrmMain.FormatStat(AValue: Single; Digits: Integer): string;
begin
  Result := Format('%.' + Digits.ToString + 'f', [AValue], FormatSettings.Invariant);
end;

procedure TfrmMain.FormCanResize(Sender: TObject; var NewWidth, NewHeight:
  Integer; var Resize: Boolean);
begin
  if FPreviewWidth <> 0 then
    NewWidth := pnlMain.Width + Width - ClientWidth;
  Resize := True;
end;

procedure TfrmMain.UpdateChart;
var
  Generation: TEvolutionGeneration;
  I: Integer;
  Stats: TEvolutionGeneration.TStats;
begin
  ClearChartValues;
  I := 1;
  for Generation in FEvolution.Generations do
  begin
    Stats := Generation.FitnessStats;
    ChartMinValues.AddXY(I, Stats.Worst);
    ChartAvgValues.AddXY(I, Stats.Average);
    ChartMaxValues.AddXY(I, Stats.Best);
    //for Reactor in Generation.Reactors do
    //  ChartAllValues.AddXY(I, Reactor.Fitness);
    Inc(I);
  end;
end;

procedure TfrmMain.UpdateGeneration;
begin
  UpdateStats;
  UpdatePopulation;
end;

procedure TfrmMain.UpdateAll;
var
  GenerationCount: Integer;
begin
  GenerationCount := FEvolution.Generations.Count;
  seGeneration.MaxValue := GenerationCount;
  seGeneration.ReadOnly := GenerationCount <= 1;
  UpdateGeneration;
  UpdateChart;
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
  I: Integer;
  Reactor: TRatedReactor;
  Item: TListItem;
begin
  FSortColumn := 1;
  Generation := CurrentGeneration;
  lvPopulation.Items.BeginUpdate;
  lvPopulation.Items.Clear;
  try
    for I := 0 to Generation.Reactors.MaxIndex do
    begin
      Reactor := Generation.Reactors[I];
      Item := lvPopulation.Items.Add;
      Item.Data := Reactor;
      Item.Caption := FormatStat(Reactor.Fitness, 0);
      Item.SubItems.Add(FormatStat(Reactor.Efficiency, 2));
      Item.SubItems.Add(FormatStat(Reactor.PowerGeneration, 0));
      Item.SubItems.Add(FormatStat(Reactor.NetHeatGeneration, 0));
    end;

  finally
    lvPopulation.Items.EndUpdate;

  end;
end;

function TfrmMain.GetChartValues(const Index: Integer): TChartSeries;
begin
  Result := tcStatistics.Series[Index];
end;

function TfrmMain.GetCurrentGeneration: TEvolutionGeneration;
begin
  Result := FEvolution.Generations[seGeneration.Value - 1];
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
  A, B: TRatedReactor;
  Settings: IEvolutionSettings;
begin
  A := TObject(Item1.Data) as TRatedReactor;
  B := TObject(Item2.Data) as TRatedReactor;
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
  UpdateGeneration;
end;

end.
