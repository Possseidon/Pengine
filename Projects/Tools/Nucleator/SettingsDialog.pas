unit SettingsDialog;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,

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
  Pengine.Collections,

  ReactorDefine,
  ReactorEvolutionDefine,
  System.Actions;

type

  TDefaultGeneratorFunction = class(TReactorGeneratorFunction)
  public type

    TGenerator = class(TReactorGenerator)
    protected
      procedure Generate(AReactor: TReactor; AIndex: Integer); override;

    end;

  public
    function GetDisplayName: string; override;

    function GetEnumerator: IReactorIterator; override;

  end;

  TDefaultMutationFunction = class(TReactorMutationFunction)
  public
    function GetDisplayName: string; override;

  end;

  TDefaultFitnessFunction = class(TReactorFitnessFunction)
  public
    function GetDisplayName: string; override;

    function Calculate(AReactor: TReactor): Single; override;

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
    seReactorX: TSpinEdit;
    seReactorY: TSpinEdit;
    seReactorZ: TSpinEdit;
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
    procedure actDisableAllBlocksExecute(Sender: TObject);
    procedure actDisableCoolersExecute(Sender: TObject);
    procedure actEnabelCoolersExecute(Sender: TObject);
    procedure actEnableAllBlocksExecute(Sender: TObject);
    procedure actShowGeneratorSettingsExecute(Sender: TObject);
    procedure actShowGeneratorSettingsUpdate(Sender: TObject);
    procedure cbGeneratorFunctionChange(Sender: TObject);
    procedure clbReactorBlocksClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSettings: TReactorEvolution.ISettings;
    FFunctions: TInterfaceArray<IReactorFunction>;

    procedure LoadSettings;

    procedure InitReactorSize;
    procedure InitReactorBlocks;

    procedure InitFunctions;

    property Settings: TReactorEvolution.ISettings read FSettings;

  public
    function Execute(ASettings: TReactorEvolution.ISettings): Boolean;

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

procedure TfrmSettings.actShowGeneratorSettingsExecute(Sender: TObject);
var
  Func: TReactorEvolution.IHasSettingsDialog;
begin
  Assert(Supports(cbGeneratorFunction.Items.Objects
    [cbGeneratorFunction.ItemIndex],
    TReactorEvolution.IHasSettingsDialog, Func));
  Func.ShowSettingsDialog;
end;

procedure TfrmSettings.actShowGeneratorSettingsUpdate(Sender: TObject);
begin
  actShowGeneratorSettings.Enabled := Supports(Settings.GeneratorFunction,
    TReactorEvolution.IHasSettingsDialog);
end;

procedure TfrmSettings.cbGeneratorFunctionChange(Sender: TObject);
var
  Func: TReactorEvolution.IGeneratorFunction;
begin
  Assert(Supports(cbGeneratorFunction.Items.Objects
    [cbGeneratorFunction.ItemIndex],
    TReactorEvolution.IGeneratorFunction, Func));
  Settings.GeneratorFunction := Func;
end;

procedure TfrmSettings.clbReactorBlocksClickCheck(Sender: TObject);
var
  BlockType: TReactor.TBlockType;
begin
  for BlockType in TReactor.BlockTypes do
    Settings.HasBlockType[BlockType] := clbReactorBlocks.Checked[Ord(BlockType)];
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  InitReactorSize;
  InitReactorBlocks;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  clbReactorBlocks.ItemIndex := -1;
  seReactorX.SetFocus;
end;

procedure TfrmSettings.LoadSettings;
var
  BlockType: TReactor.TBlockType;
begin
  if Settings.GeneratorFunction = nil then
    Settings.GeneratorFunction := TDefaultGeneratorFunction.Create(Settings);
  if Settings.MutationFunction = nil then
    Settings.MutationFunction := TDefaultMutationFunction.Create(Settings);
  if Settings.FitnessFunction = nil then
    Settings.FitnessFunction := TDefaultFitnessFunction.Create(Settings);

  seReactorX.Value := Settings.ReactorSize.X;
  seReactorY.Value := Settings.ReactorSize.Y;
  seReactorZ.Value := Settings.ReactorSize.Z;
  edtFuelBasePower.Text := PrettyFloat(Settings.FuelBasePower);
  edtFuelBaseHeat.Text := PrettyFloat(Settings.FuelBaseHeat);

  sePopulationSize.Value := Settings.PopulationSize;

  for BlockType in TReactor.BlockTypes do
    clbReactorBlocks.Checked[Ord(BlockType)] := BlockType in Settings.BlockTypes;

  InitFunctions;

end;

procedure TfrmSettings.InitReactorSize;
begin
  seReactorX.MinValue := TReactor.SizeLimits.C1.X;
  seReactorX.MaxValue := TReactor.SizeLimits.C2.X;
  seReactorX.Value := seReactorX.MinValue;
  seReactorY.MinValue := TReactor.SizeLimits.C1.Y;
  seReactorY.MaxValue := TReactor.SizeLimits.C2.Y;
  seReactorY.Value := seReactorZ.MinValue;
  seReactorZ.MinValue := TReactor.SizeLimits.C1.Z;
  seReactorZ.MaxValue := TReactor.SizeLimits.C2.Z;
  seReactorZ.Value := seReactorZ.MinValue;
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
  FuncClass: TReactorEvolution.TFunctionClass;
  SettingsFunc: TReactorEvolution.IFunction;
  FuncObject: TReactorEvolution.TFunction;
begin
  cbGeneratorFunction.Items.BeginUpdate;
  cbMutationFunction.Items.BeginUpdate;
  cbFitnessFunction.Items.BeginUpdate;

  cbGeneratorFunction.Clear;
  cbMutationFunction.Clear;
  cbFitnessFunction.Clear;

  try

    SettingsFunc := FFunctions.Add(Settings.GeneratorFunction);
    cbGeneratorFunction.AddItem(SettingsFunc.GetDisplayName,
      TObject(SettingsFunc));
    cbGeneratorFunction.ItemIndex := 0;
    for FuncClass in TReactorEvolution.GeneratorFunctions do
    begin
      if TObject(SettingsFunc).ClassType = FuncClass then
        Continue;
      FuncObject := FuncClass.Create(Settings);
      FFunctions.Add(FuncObject);
      cbGeneratorFunction.AddItem(FuncObject.GetDisplayName, FuncObject);
    end;

    SettingsFunc := FFunctions.Add(Settings.MutationFunction);
    cbMutationFunction.AddItem(SettingsFunc.GetDisplayName,
      TObject(SettingsFunc));
    cbMutationFunction.ItemIndex := 0;
    for FuncClass in TReactorEvolution.MutationFunctions do
    begin
      if TObject(SettingsFunc).ClassType = FuncClass then
        Continue;
      FuncObject := FuncClass.Create(Settings);
      FFunctions.Add(FuncObject);
      cbMutationFunction.AddItem(FuncObject.GetDisplayName, FuncObject);
    end;

    SettingsFunc := FFunctions.Add(Settings.FitnessFunction);
    cbFitnessFunction.AddItem(SettingsFunc.GetDisplayName,
      TObject(SettingsFunc));
    cbFitnessFunction.ItemIndex := 0;
    for FuncClass in TReactorEvolution.FitnessFunctions do
    begin
      if TObject(SettingsFunc).ClassType = FuncClass then
        Continue;
      FuncObject := FuncClass.Create(Settings);
      FFunctions.Add(FuncObject);
      cbFitnessFunction.AddItem(FuncObject.GetDisplayName, FuncObject);
    end;

  finally
    cbGeneratorFunction.Items.EndUpdate;
    cbFitnessFunction.Items.EndUpdate;
    cbMutationFunction.Items.EndUpdate;
  end;
end;

function TfrmSettings.Execute(ASettings: TReactorEvolution.ISettings): Boolean;
begin
  FSettings := ASettings.Copy;
  FFunctions := TInterfaceArray<IReactorFunction>.Create;
  try
    LoadSettings;
    Result := ShowModal = mrOk;
    if Result then
      ASettings.Assign(FSettings);
  finally
    FreeAndNil(FFunctions);
    FSettings := nil;
  end;
end;

{ TDefaultGeneratorFunction }

function TDefaultGeneratorFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

function TDefaultGeneratorFunction.GetEnumerator: IReactorIterator;
begin
  Result := TGenerator.Create(Settings);
end;

{ TDefaultMutationFunction }

function TDefaultMutationFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

{ TDefaultFitnessFunction }

function TDefaultFitnessFunction.Calculate(AReactor: TReactor): Single;
begin
  Result :=
    AReactor.PowerGeneration(Settings.FuelBasePower) /
    Max(1, AReactor.HeatGeneration(Settings.FuelBaseHeat) + 11);
end;

function TDefaultFitnessFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

{ TDefaultGeneratorFunction.TGenerator }

procedure TDefaultGeneratorFunction.TGenerator.Generate(AReactor: TReactor; AIndex: Integer);
begin
  
end;

initialization

TReactorEvolution.RegisterFunction(TDefaultGeneratorFunction);
TReactorEvolution.RegisterFunction(TDefaultMutationFunction);
TReactorEvolution.RegisterFunction(TDefaultFitnessFunction);

end.
