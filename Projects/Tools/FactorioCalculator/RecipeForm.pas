unit RecipeForm;

interface

uses
  System.SysUtils,
  System.Classes,

  Vcl.Forms,
  Vcl.Controls,

  GdiPlus,
  GdiPlusHelpers,

  FactoryDefine;

type

  TfrmRecipes = class(TForm)
    procedure FormDeactivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FMachineArray: TMachineArray;

    procedure MachineArrayDestroy;

  public
    procedure Execute(AMachineArray: TMachineArray);

  end;

var
  frmRecipes: TfrmRecipes;

implementation

{$R *.dfm}

procedure TfrmRecipes.Execute(AMachineArray: TMachineArray);
begin
  FMachineArray := AMachineArray;
  FMachineArray.OnDestroy.Add(MachineArrayDestroy);
  Show;
end;

procedure TfrmRecipes.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmRecipes.FormPaint(Sender: TObject);
var
  G: IGPGraphics;
begin
  G := Canvas.ToGPGraphics;
  // TODO: Paint
end;

procedure TfrmRecipes.MachineArrayDestroy;
begin
  FMachineArray := nil;
end;

end.
