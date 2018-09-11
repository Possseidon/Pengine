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

  Pengine.Bitfield,
  Pengine.IntMaths,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.TimeManager,

  MinesweeperDefine,
  Vcl.ExtCtrls;

type

  TfrmMain = class(TForm)
    tmrUpdate: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
  public const

    FieldSize = 40;

  private
    FMinesweeper: TMinesweeper;

    procedure Regenerate;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin                          
  FMinesweeper.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMinesweeper := TMinesweeper.Create(20);
  FMinesweeper.GeneratePercentage(0.5);

  ClientWidth := FieldSize * FMinesweeper.Size.X;
  ClientHeight := FieldSize * FMinesweeper.Size.Y;

end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Regenerate; 
end;

procedure TfrmMain.FormPaint(Sender: TObject);
var
  Pos: TIntVector2;
  Value: Integer;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psClear;
  Canvas.Pen.Width := 1;

  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;

  Canvas.Rectangle(Canvas.ClipRect);

  for Pos in FMinesweeper.Size do
  begin
    if FMinesweeper.IsMine(Pos) then
      Canvas.Brush.Color := clRed
    else
      Canvas.Brush.Color := clLtGray;

    Canvas.Rectangle(Pos.X * FieldSize, Pos.Y * FieldSize, (Pos.X + 1) * FieldSize, (Pos.Y + 1) * FieldSize);

    if not FMinesweeper.IsMine(Pos) then
      Canvas.TextOut(Pos.X * FieldSize + 4, Pos.Y * FieldSize + 3, IntToStr(FMinesweeper.AdjacentMines(Pos)));

  end;
end;

procedure TfrmMain.Regenerate;
begin
  FMinesweeper.GeneratePercentage(0.5);
  Invalidate;
end;

procedure TfrmMain.tmrUpdateTimer(Sender: TObject);
begin                    
  // Regenerate;
end;

end.
