unit FactoryFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  GdiPlus,

  Pengine.Vector,

  FactoryDefine;

type
  TfrmFactory = class(TFrame)
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FFactory: TFactory;
    FDragMachineArray: TMachineArray;
    FDragPos: TVector2;

  protected
    procedure PaintWindow(DC: HDC); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Factory: TFactory read FFactory;

  end;

implementation

{$R *.dfm}


uses Main,
  RecipeForm;

{ frmFactory }

constructor TfrmFactory.Create(AOwner: TComponent);
begin
  inherited;
  ControlState := ControlState + [csCustomPaint];
  ControlStyle := ControlStyle + [csOpaque];
  FFactory := TFactory.Create;
  Factory.OnMachineArrayAdd.Add(Invalidate);
  Factory.OnMachineArrayRemove.Add(Invalidate);
  Factory.OnMachineArrayChange.Add(Invalidate);
end;

destructor TfrmFactory.Destroy;
begin
  FFactory.Free;
  inherited;
end;

procedure TfrmFactory.FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
begin
  MachineArray := FFactory.MachineArrayAt(Vec2(X, Y));
  if MachineArray <> nil then
  begin
    if Button = mbLeft then
    begin
      FDragMachineArray := MachineArray;
      FDragPos := Vec2(X, Y);
    end
    else if Button = mbRight then
      frmRecipes.Execute(MachineArray);
  end
  else
  begin
    if Button = mbLeft then
      MachineArray := FFactory.AddMachineArray(Vec2(X, Y), frmMain.Factorio.CraftingMachine['assembling-machine-1']);
  end;
end;

procedure TfrmFactory.FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FDragMachineArray <> nil then
  begin
    FDragMachineArray.Pos := FDragMachineArray.Pos + Vec2(X, Y) - FDragPos;
    FDragPos := Vec2(X, Y);
  end;
end;

procedure TfrmFactory.FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragMachineArray := nil;
end;

procedure TfrmFactory.PaintWindow(DC: HDC);
var
  G: IGPGraphics;
begin
  G := TGPGraphics.Create(DC);
  G.TextRenderingHint := TextRenderingHintAntiAlias;
  Factory.Draw(G);
end;

end.
