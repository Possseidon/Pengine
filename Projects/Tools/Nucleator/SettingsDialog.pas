unit SettingsDialog;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.Actions,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.ActnList,
  Vcl.CheckLst,
  Vcl.Menus,
  Vcl.ExtCtrls,

  Pengine.Utility,
  Pengine.ICollections,
  Pengine.IntMaths,

  ReactorDefine,
  ReactorEvolutionDefine;

type

  IHasSettings = interface
    ['{BE872851-764E-4616-B5DB-2C1D62A53147}']
    procedure ShowSettingsDialog;

  end;

  TDefaultGeneratorFunction = class(TGeneratorFunction)
  public type

    TGenerator = class(TReactorGenerator)
    protected
      procedure Generate(AReactor: TReactor; AIndex: Integer); override;

    end;

  public
    class function GetDisplayName: string; override;

    function GetEnumerator: IIterator<TReactor>; override;

  end;

  TDefaultMutationFunction = class(TMutationFunction)
  public type

    TMutator = class(TReactorMutator)
    protected
      procedure Generate(AReactor: TReactor; AIndex: Integer); override;

    end;

  public
    class function GetDisplayName: string; override;

    function GetEnumerator: IIterator<TReactor>; override;

  end;

  TDefaultFitnessFunction = class(TFitnessFunction)
  public
    class function GetDisplayName: string; override;

    function Calculate(AReactor: TReactor; ASettings: IEvolutionSettings): Single; override;

  end;

  TfrmSettings = class(TForm)
    gbReactorBlocks: TGroupBox;
    clbReactorBlocks: TCheckListBox;
    pmReactorBlocks: TPopupMenu;
    EnableAll1: TMenuItem;
    DisableAll1: TMenuItem;
    N1: TMenuItem;
    EnableCoolers1: TMenuItem;
    DisableCoolers1: TMenuItem;
    pnlLeft: TPanel;
    btnGenerate: TButton;
    gbReactor: TGroupBox;
    lbReactorSize: TLabel;
    seReactorSizeX: TSpinEdit;
    seReactorSizeY: TSpinEdit;
    seReactorSizeZ: TSpinEdit;
    gbEvolution: TGroupBox;
    lbPopulationSize: TLabel;
    sePopulationSize: TSpinEdit;
    lbGeneratorFunction: TLabel;
    cbFitnessFunction: TComboBox;
    btnGeneratorFunctionSettings: TButton;
    btnMutationFunctionSettings: TButton;
    cbMutationFunction: TComboBox;
    lbMutationFunction: TLabel;
    lbFuelBasePower: TLabel;
    edtFuelBasePower: TEdit;
    edtFuelBaseHeat: TEdit;
    lbFuelBaseHeat: TLabel;
    lbFitnessFunction: TLabel;
    cbGeneratorFunction: TComboBox;
    btnFitnessFunctionSettings: TButton;
    alSettings: TActionList;
    actShowGeneratorSettings: TAction;
    actShowMutationSettings: TAction;
    actShowFitnessSettings: TAction;
    actDisableAllBlocks: TAction;
    actEnableAllBlocks: TAction;
    actDisableCoolers: TAction;
    actEnabelCoolers: TAction;
    actGenerate: TAction;
    procedure actDisableAllBlocksExecute(Sender: TObject);
    procedure actDisableCoolersExecute(Sender: TObject);
    procedure actEnabelCoolersExecute(Sender: TObject);
    procedure actEnableAllBlocksExecute(Sender: TObject);
    procedure actGenerateExecute(Sender: TObject);
    procedure actGenerateUpdate(Sender: TObject);
    procedure actShowGeneratorSettingsExecute(Sender: TObject);
    procedure actShowGeneratorSettingsUpdate(Sender: TObject);
    procedure cbGeneratorFunctionChange(Sender: TObject);
    procedure clbReactorBlocksClickCheck(Sender: TObject);
    procedure edtFuelValueChange(Sender: TObject);
    procedure edtFuelValueExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sePopulationSizeChange(Sender: TObject);
    procedure seReactorSizeChange(Sender: TObject);
    procedure seReactorSizeExit(Sender: TObject);
  private
    FSettings: IEvolutionSettings;

    procedure LoadSettings;

    procedure InitReactorSize;
    procedure InitReactorBlocks;

    procedure InitFunctions;

    property Settings: IEvolutionSettings read FSettings;

  public
    function Execute(ASettings: IEvolutionSettings): Boolean;

  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.dfm}

{ TfrmInitialization }

procedure TfrmSettings.actDisableAllBlocksExecute(Sender: TObject);
var
  BlockType: TReactor.TBlockType;
begin
  for BlockType in TReactor.BlockTypes do
    clbReactorBlocks.Checked[Ord(BlockType)] := False;
  clbReactorBlocksClickCheck(clbReactorBlocks);
end;

procedure TfrmSettings.actDisableCoolersExecute(Sender: TObject);
var
  BlockType: TReactor.TBlockType;
begin
  for BlockType in TReactor.CoolerTypes do
    clbReactorBlocks.Checked[Ord(BlockType)] := False;
  clbReactorBlocksClickCheck(clbReactorBlocks);
end;

procedure TfrmSettings.actEnabelCoolersExecute(Sender: TObject);
var
  BlockType: TReactor.TBlockType;
begin
  for BlockType in TReactor.CoolerTypes do
    clbReactorBlocks.Checked[Ord(BlockType)] := True;
  clbReactorBlocksClickCheck(clbReactorBlocks);
end;

procedure TfrmSettings.actEnableAllBlocksExecute(Sender: TObject);
var
  BlockType: TReactor.TBlockType;
begin
  for BlockType in TReactor.BlockTypes do
    clbReactorBlocks.Checked[Ord(BlockType)] := True;
  clbReactorBlocksClickCheck(clbReactorBlocks);
end;

procedure TfrmSettings.actGenerateExecute(Sender: TObject);
begin
  ModalResult := mrOk;
  CloseModal;
end;

procedure TfrmSettings.actGenerateUpdate(Sender: TObject);
begin
  actGenerate.Enabled := Settings.BlockTypes <> [];
end;

procedure TfrmSettings.actShowGeneratorSettingsExecute(Sender: TObject);
var
  HasSettings: IHasSettings;
begin
  Assert(Supports(Settings.GeneratorFunction, IHasSettings, HasSettings));
  HasSettings.ShowSettingsDialog;
end;

procedure TfrmSettings.actShowGeneratorSettingsUpdate(Sender: TObject);
begin
  actShowGeneratorSettings.Enabled := Supports(Settings.GeneratorFunction, IHasSettings);
end;

procedure TfrmSettings.cbGeneratorFunctionChange(Sender: TObject);
var
  Selected: TObject;
begin
  Selected := cbGeneratorFunction.Items.Objects[cbGeneratorFunction.ItemIndex];
  Settings.SetGeneratorFunction(TGeneratorFunctionClass(Selected));
end;

procedure TfrmSettings.clbReactorBlocksClickCheck(Sender: TObject);
var
  BlockType: TReactor.TBlockType;
begin
  for BlockType in TReactor.BlockTypes do
    Settings.HasBlockType[BlockType] := clbReactorBlocks.Checked[Ord(BlockType)];
end;

procedure TfrmSettings.edtFuelValueChange(Sender: TObject);
var
  Edit: TEdit;
  Value: Single;
begin
  Edit := Sender as TEdit;
  if Single.TryParse(Edit.Text, Value, FormatSettings.Invariant) then
  begin
    Edit.ParentFont := True;
    case Edit.Tag of
      0:
        Settings.FuelBasePower := Value;
      1:
        Settings.FuelBaseHeat := Value;
    end;
  end
  else
  begin
    Edit.Font.Color := clRed;
  end;
end;

procedure TfrmSettings.edtFuelValueExit(Sender: TObject);
var
  Edit: TEdit;
begin
  Edit := Sender as TEdit;
  Edit.ParentFont := True;
  case Edit.Tag of
    0:
      Edit.Text := PrettyFloat(Settings.FuelBasePower);
    1:
      Edit.Text := PrettyFloat(Settings.FuelBaseHeat);
  end;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  InitReactorSize;
  InitReactorBlocks;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  clbReactorBlocks.ItemIndex := -1;
  seReactorSizeX.SetFocus;
end;

procedure TfrmSettings.LoadSettings;
var
  BlockType: TReactor.TBlockType;
begin
  if Settings.GeneratorFunction = nil then
    Settings.SetGeneratorFunction(TDefaultGeneratorFunction);
  if Settings.MutationFunction = nil then
    Settings.SetMutationFunction(TDefaultMutationFunction);
  if Settings.FitnessFunction = nil then
    Settings.SetFitnessFunction(TDefaultFitnessFunction);

  seReactorSizeX.Value := Settings.ReactorSize.X;
  seReactorSizeY.Value := Settings.ReactorSize.Y;
  seReactorSizeZ.Value := Settings.ReactorSize.Z;
  edtFuelBasePower.Text := PrettyFloat(Settings.FuelBasePower);
  edtFuelBaseHeat.Text := PrettyFloat(Settings.FuelBaseHeat);

  sePopulationSize.Value := Settings.PopulationSize;

  for BlockType in TReactor.BlockTypes do
    clbReactorBlocks.Checked[Ord(BlockType)] := BlockType in Settings.BlockTypes;

  InitFunctions;

end;

procedure TfrmSettings.InitReactorSize;
begin
  seReactorSizeX.MinValue := TReactor.SizeLimits.C1.X;
  seReactorSizeX.MaxValue := TReactor.SizeLimits.C2.X;
  seReactorSizeY.MinValue := TReactor.SizeLimits.C1.Y;
  seReactorSizeY.MaxValue := TReactor.SizeLimits.C2.Y;
  seReactorSizeZ.MinValue := TReactor.SizeLimits.C1.Z;
  seReactorSizeZ.MaxValue := TReactor.SizeLimits.C2.Z;
end;

procedure TfrmSettings.InitReactorBlocks;
var
  Data: TReactor.TBlockData;
begin
  clbReactorBlocks.Items.BeginUpdate;
  clbReactorBlocks.Items.Clear;
  for Data in TReactor.BlockData do
    clbReactorBlocks.Items.Add(Data.DisplayName);
  clbReactorBlocks.Items.EndUpdate;
end;

procedure TfrmSettings.InitFunctions;
var
  FuncClass: TEvolutionFunctionClass;
begin
  cbGeneratorFunction.Items.BeginUpdate;
  cbMutationFunction.Items.BeginUpdate;
  cbFitnessFunction.Items.BeginUpdate;

  cbGeneratorFunction.Clear;
  cbMutationFunction.Clear;
  cbFitnessFunction.Clear;

  try

    for FuncClass in TReactorEvolution.GeneratorFunctions do
    begin
      cbGeneratorFunction.AddItem(FuncClass.GetDisplayName, TObject(FuncClass));
      if FuncClass = Settings.GeneratorFunction.FunctionClass then
        cbGeneratorFunction.ItemIndex := cbGeneratorFunction.Items.Count - 1;
    end;

    for FuncClass in TReactorEvolution.MutationFunctions do
    begin
      cbMutationFunction.AddItem(FuncClass.GetDisplayName, TObject(FuncClass));
      if FuncClass = Settings.MutationFunction.FunctionClass then
        cbMutationFunction.ItemIndex := cbMutationFunction.Items.Count - 1;
    end;

    for FuncClass in TReactorEvolution.FitnessFunctions do
    begin
      cbFitnessFunction.AddItem(FuncClass.GetDisplayName, TObject(FuncClass));
      if FuncClass = Settings.FitnessFunction.FunctionClass then
        cbFitnessFunction.ItemIndex := cbFitnessFunction.Items.Count - 1;
    end;

  finally
    cbGeneratorFunction.Items.EndUpdate;
    cbFitnessFunction.Items.EndUpdate;
    cbMutationFunction.Items.EndUpdate;
  end;
end;

function TfrmSettings.Execute(ASettings: IEvolutionSettings): Boolean;
begin
  FSettings := ASettings.Copy;
  try
    LoadSettings;
    Result := ShowModal = mrOk;
    if Result then
      ASettings.Assign(FSettings);
  finally
    FSettings := nil;
  end;
end;

procedure TfrmSettings.sePopulationSizeChange(Sender: TObject);
begin
  Settings.PopulationSize := sePopulationSize.Value;
end;

procedure TfrmSettings.seReactorSizeChange(Sender: TObject);
var
  SpinEdit: TSpinEdit;
  Tmp: TIntVector3;
begin
  SpinEdit := Sender as TSpinEdit;
  Tmp := Settings.ReactorSize;
  Tmp[TCoordAxis(Ord(caX) + SpinEdit.Tag)] := EnsureRange(SpinEdit.Value, SpinEdit.MinValue, SpinEdit.MaxValue);
  Settings.ReactorSize := Tmp;
end;

procedure TfrmSettings.seReactorSizeExit(Sender: TObject);
var
  SpinEdit: TSpinEdit;
begin
  SpinEdit := Sender as TSpinEdit;
  SpinEdit.Value := Settings.ReactorSize[TCoordAxis(Ord(caX) + SpinEdit.Tag)];
end;

{ TDefaultGeneratorFunction }

class function TDefaultGeneratorFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

function TDefaultGeneratorFunction.GetEnumerator: IIterator<TReactor>;
begin
  Result := TGenerator.Create(Settings);
end;

{ TDefaultMutationFunction }

class function TDefaultMutationFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

function TDefaultMutationFunction.GetEnumerator: IIterator<TReactor>;
begin
  Result := TMutator.Create(ParentGeneration);
end;

{ TDefaultFitnessFunction }

function TDefaultFitnessFunction.Calculate(AReactor: TReactor; ASettings: IEvolutionSettings): Single;
var
  Efficiency, PowerGeneration, NetHeatGeneration, HeatFactor: Single;
begin
  // Result := Max(0, AReactor.PowerGeneration - Power(Max(0, AReactor.NetHeatGeneration), 1.5));
  // Result := AReactor.Efficiency * AReactor.PowerGeneration / (1 + Exp(AReactor.NetHeatGeneration));
  // Result := AReactor.Efficiency * AReactor.PowerGeneration - AReactor.NetHeatGeneration;
  Efficiency := AReactor.Calculation.Efficiency;
  PowerGeneration := AReactor.Calculation.PowerGeneration(ASettings.FuelBasePower);
  NetHeatGeneration := AReactor.Calculation.NetHeatGeneration(ASettings.FuelBaseHeat);
  HeatFactor := Exp(-Sqr((NetHeatGeneration + 0) / 100));
  if NetHeatGeneration > 0 then
    HeatFactor := HeatFactor * 0.8;
  Result := Efficiency * PowerGeneration * HeatFactor;
end;

class function TDefaultFitnessFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

{ TDefaultGeneratorFunction.TGenerator }

procedure TDefaultGeneratorFunction.TGenerator.Generate(AReactor: TReactor; AIndex: Integer);
var
  Pos: TIntVector3;
begin
  for Pos in AReactor.Size do
    AReactor.Blocks[Pos] := Settings.BlockTypeList[Random(Settings.BlockTypeList.Count)];
  AReactor.RemoveInactiveCoolers;
end;

{ TDefaultMutationFunction.TMutator }

procedure TDefaultMutationFunction.TMutator.Generate(AReactor: TReactor; AIndex: Integer);
var
  I: Integer;
  NewType: TReactor.TBlockType;
begin
  AReactor.Assign(ParentGeneration.ReactorRatings[Random(AIndex) div 2].Reactor);
  if AIndex = 0 then
    Exit;
  for I := 0 to 5 * AIndex div Settings.PopulationSize do
  begin
    NewType := Settings.BlockTypeList[Random(Settings.BlockTypeList.Count)];
    AReactor.Blocks[TIntVector3.Random(AReactor.Size)] := NewType;
  end;
  AReactor.RemoveInactiveCoolers;
end;

initialization

TReactorEvolution.RegisterFunction(TDefaultGeneratorFunction);
TReactorEvolution.RegisterFunction(TDefaultMutationFunction);
TReactorEvolution.RegisterFunction(TDefaultFitnessFunction);

end.
