unit InitializationDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.CheckLst, Vcl.Menus, Vcl.ExtCtrls;

type
  TfrmInitialization = class(TForm)
    gbReactorBlocks: TGroupBox;
    clbReactorBlocks: TCheckListBox;
    pmReactorBlocks: TPopupMenu;
    EnableAll1: TMenuItem;
    DisableAll1: TMenuItem;
    N1: TMenuItem;
    EnableCoolers1: TMenuItem;
    DisableCoolers1: TMenuItem;
    Panel1: TPanel;
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    seReactorX: TSpinEdit;
    seReactorY: TSpinEdit;
    seReactorZ: TSpinEdit;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    sePopulation: TSpinEdit;
    Label3: TLabel;
    cbFitnessFunction: TComboBox;
    btnFitnessFunctionSettings: TButton;
    btnMutationFunctionSettings: TButton;
    cbMutationFunction: TComboBox;
    Label5: TLabel;
    Label4: TLabel;
    edtFuelBasePower: TEdit;
    edtFuelBaseHeat: TEdit;
    Label6: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmInitialization: TfrmInitialization;

implementation

{$R *.dfm}

end.
