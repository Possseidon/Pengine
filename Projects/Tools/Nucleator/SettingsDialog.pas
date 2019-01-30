unit SettingsDialog;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Rtti,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.CheckLst,
  Vcl.Menus,
  Vcl.ExtCtrls,

  Pengine.Utility,
  Pengine.Collections,

  // TODO: Remove
  Pengine.TimeManager,

  ReactorDefine,
  ReactorEvolutionDefine,
  System.Actions,
  Vcl.ActnList;

type

  TDefaultGeneratorFunction = class(TReactorEvolution.TGeneratorFunction)
  public
    function GetDisplayName: string; override;

  end;
                   
  TDefaultMutationFunction = class(TReactorEvolution.TMutationFunction)
  public
    function GetDisplayName: string; override;

  end;

  TDefaultFitnessFunction = class(TReactorEvolution.TFitnessFunction)
  public
    function GetDisplayName: string; override;

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
    procedure actShowGeneratorSettingsExecute(Sender: TObject);
    procedure actShowGeneratorSettingsUpdate(Sender: TObject);
    procedure cbGeneratorFunctionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSettings: TReactorEvolution.ISettings;
    FFunctions: TInterfaceArray<TReactorEvolution.IFunction>;

    procedure LoadSettings;

    procedure InitReactorSize;
    procedure InitFunctions;

    property Settings: TReactorEvolution.ISettings read FSettings;

  public
    procedure Execute(ASettings: TReactorEvolution.ISettings);

  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.dfm}

{ TfrmInitialization }

procedure TfrmSettings.actShowGeneratorSettingsExecute(Sender: TObject);
var
  Func: TReactorEvolution.IHasSettingsDialog;
begin
  Assert(Supports(cbGeneratorFunction.Items.Objects[cbGeneratorFunction.ItemIndex], 
    TReactorEvolution.IHasSettingsDialog, Func));
  Func.ShowSettingsDialog;
end;

procedure TfrmSettings.actShowGeneratorSettingsUpdate(Sender: TObject);
begin
  actShowGeneratorSettings.Enabled := Supports(Settings.GeneratorFunction, TReactorEvolution.IHasSettingsDialog);
end;

procedure TfrmSettings.cbGeneratorFunctionChange(Sender: TObject);
var
  Func: TReactorEvolution.IGeneratorFunction;
begin
  Assert(Supports(cbGeneratorFunction.Items.Objects[cbGeneratorFunction.ItemIndex], 
    TReactorEvolution.IGeneratorFunction, Func));
  Settings.GeneratorFunction := Func;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  InitReactorSize;
end;

procedure TfrmSettings.Execute(ASettings: TReactorEvolution.ISettings);
begin
  FSettings := ASettings.Copy;
  FFunctions := TInterfaceArray<TReactorEvolution.IFunction>.Create;
  try
    LoadSettings;
    if ShowModal = mrOk then
      ASettings.Assign(FSettings);
  finally
    FFunctions.Free;
    FSettings := nil;
  end;
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
    cbGeneratorFunction.AddItem(SettingsFunc.GetDisplayName, TObject(SettingsFunc));
    cbGeneratorFunction.ItemIndex := 0;
    for FuncClass in TReactorEvolution.GeneratorFunctions do
    begin
      if TObject(SettingsFunc).ClassType = FuncClass then
        Continue;
      FuncObject := FuncClass.Create;
      FFunctions.Add(FuncObject);
      cbGeneratorFunction.AddItem(FuncObject.GetDisplayName, FuncObject);
    end;

    SettingsFunc := FFunctions.Add(Settings.MutationFunction);
    cbMutationFunction.AddItem(SettingsFunc.GetDisplayName, TObject(SettingsFunc));
    cbMutationFunction.ItemIndex := 0;
    for FuncClass in TReactorEvolution.MutationFunctions do
    begin
      if TObject(SettingsFunc).ClassType = FuncClass then
        Continue;
      FuncObject := FuncClass.Create;
      FFunctions.Add(FuncObject);
      cbMutationFunction.AddItem(FuncObject.GetDisplayName, FuncObject);
    end;

    SettingsFunc := FFunctions.Add(Settings.FitnessFunction);
    cbFitnessFunction.AddItem(SettingsFunc.GetDisplayName, TObject(SettingsFunc));
    cbFitnessFunction.ItemIndex := 0;
    for FuncClass in TReactorEvolution.FitnessFunctions do
    begin
      if TObject(SettingsFunc).ClassType = FuncClass then
        Continue;
      FuncObject := FuncClass.Create;
      FFunctions.Add(FuncObject);
      cbFitnessFunction.AddItem(FuncObject.GetDisplayName, FuncObject);
    end;

  finally
    cbGeneratorFunction.Items.EndUpdate;
    cbFitnessFunction.Items.EndUpdate;
    cbMutationFunction.Items.EndUpdate;
  end;
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

procedure TfrmSettings.LoadSettings;
begin
  if Settings.GeneratorFunction = nil then
    Settings.GeneratorFunction := TDefaultGeneratorFunction.Create;
  if Settings.MutationFunction = nil then
    Settings.MutationFunction := TDefaultMutationFunction.Create;
  if Settings.FitnessFunction = nil then
    Settings.FitnessFunction := TDefaultFitnessFunction.Create;

  seReactorX.Value := Settings.ReactorSize.X;
  seReactorY.Value := Settings.ReactorSize.Y;
  seReactorZ.Value := Settings.ReactorSize.Z;
  edtFuelBasePower.Text := PrettyFloat(Settings.FuelBasePower);
  edtFuelBaseHeat.Text := PrettyFloat(Settings.FuelBaseHeat);

  sePopulationSize.Value := Settings.PopulationSize;

  InitFunctions;

end;

{ TDefaultGeneratorFunction }

function TDefaultGeneratorFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

{ TDefaultMutationFunction }

function TDefaultMutationFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

{ TDefaultFitnessFunction }

function TDefaultFitnessFunction.GetDisplayName: string;
begin
  Result := 'Default';
end;

initialization

TReactorEvolution.RegisterFunction(TDefaultGeneratorFunction);
TReactorEvolution.RegisterFunction(TDefaultMutationFunction);
TReactorEvolution.RegisterFunction(TDefaultFitnessFunction);

end.
