unit Main;

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
  Vcl.ExtCtrls,
  Vcl.Menus,

  GdiPlus,
  GdiPlusHelpers,

  Pengine.DebugConsole,
  Pengine.Vector,

  Pengine.Factorio.General,

  FactoryDefine,
  RecipeForm;

type
  TfrmMain = class(TForm)
    pbFactory: TPaintBox;
    mmMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    New1: TMenuItem;
    Saveas1: TMenuItem;
    N2: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbFactoryPaint(Sender: TObject);
  private
    FFactorio: TFactorio;
    FFactory: TFactory;

  public
    property Factorio: TFactorio read FFactorio;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FFactory.Free;
  FFactorio.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  MachineArray: TMachineArray;
begin
  FFactorio := TFactorio.Create;
  FFactory := TFactory.Create;

  MachineArray := FFactory.AddMachineArray(Vec2(50, 50), FFactorio.Find<TFactorio.TAssemblingMachine>('assembling-machine-1'));
  MachineArray.Recipe := FFactorio.Find<TFactorio.TRecipe>('iron-gear-wheel');
  MachineArray.Remove;
end;

procedure TfrmMain.pbFactoryPaint(Sender: TObject);
var
  G: IGPGraphics;
  Font: IGPFont;
  MachineArray: TMachineArray;
  Brush, FontBrush: IGPBrush;
  Pen: IGPPen;
begin
  G := pbFactory.ToGPGraphics;

  Pen := TGPPen.Create($FF7F7FBF, 3);
  Brush := TGPSolidBrush.Create($FFAFAFFF);
  Font := TGPFont.Create('Consolas', 12);
  FontBrush := TGPSolidBrush.Create(TGPColor.DarkBlue);

  for MachineArray in FFactory.MachineArrays do
  begin
    G.FillRectangle(Brush, MachineArray.Pos.X, MachineArray.Pos.Y, 32 * 3 + 8, 32 * 3 + 8);
    G.DrawRectangle(Pen, MachineArray.Pos.X, MachineArray.Pos.Y, 32 * 3 + 8, 32 * 3 + 8);
    G.DrawString(
      Format('x%d', [MachineArray.Count]),
      Font,
      TGPPointF.Create(MachineArray.Pos.X + 4, MachineArray.Pos.Y + 4),
      FontBrush);
    G.DrawImage(
      MachineArray.AssemblingMachine.Icon,
      MachineArray.Pos.X + 30, MachineArray.Pos.Y + 4);
    if MachineArray.Recipe <> nil then
      G.DrawImage(
        MachineArray.Recipe.Icon,
        MachineArray.Pos.X + 68, MachineArray.Pos.Y + 4);
  end;
end;

end.
