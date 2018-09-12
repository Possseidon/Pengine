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

  Pengine.Bitfield,
  Pengine.IntMaths,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.TimeManager,

  MinesweeperDefine,
  MinesweeperNerualNet, Pengine.NeuralNetwork;

type

  TfrmMain = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  public const

    FieldSize = 20;

  private
    FMinesweeper: TMinesweeper;
    FNet: TMinesweeperNeuralNet;
    FLost: Boolean;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FMinesweeper.Free;
  FNet.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMinesweeper := TMinesweeper.Create(9);

  FNet := TMinesweeperNeuralNet.Create;
  FNet.BeginUpdate;
  FNet.ScanRange := 3;
  FNet.HiddenLayers := 0;
  // FNet.HiddenLayerSize[0] := 10;
  FNet.EndUpdate;
  FNet.Randomize;

  ClientWidth := FieldSize * FMinesweeper.Size.X;
  ClientHeight := FieldSize * FMinesweeper.Size.Y;

end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  BestPos: TIntVector2;
begin
  case Key of
    VK_RETURN:
      begin
        if FLost then
          Exit;

        BestPos := FNet.BestPosition(FMinesweeper);

        if FMinesweeper.Mines = 0 then
          FMinesweeper.Generate(10, BestPos);

        if FMinesweeper.Reveal(BestPos) = rrMine then
          FLost := True;

        Caption := Format('Revealed %s', [BestPos.ToString]);

        Invalidate;
      end;
  end;
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pos: TIntVector2;
begin
  case Button of
    mbLeft:
      begin
        if FLost then
          Exit;

        Pos := IVec2(X, Y) div FieldSize;
        if FMinesweeper.Mines = 0 then
          FMinesweeper.Generate(10, Pos);

        if FMinesweeper.Reveal(Pos) = rrMine then
          FLost := True;

        Invalidate;
      end;
    mbRight:
      begin
        FLost := True;
        FMinesweeper.Clear;
        FMinesweeper.Generate(81 - 4, IVec2(8, 8));
        Invalidate;
      end;
  end;

end;

procedure TfrmMain.FormPaint(Sender: TObject);
var
  Pos: TIntVector2;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psClear;
  Canvas.Pen.Width := 1;

  Canvas.Brush.Color := $FFCFCF;
  Canvas.Brush.Style := bsSolid;

  Canvas.Rectangle(Canvas.ClipRect);

  Canvas.Font.Name := 'Gill Sans Ultra Bold';

  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 3;

  for Pos in FMinesweeper.Size do
  begin
    if FLost and FMinesweeper.IsMine(Pos) then
    begin
      Canvas.Brush.Color := $0000FF;
      Canvas.Pen.Color := $0000BF;
    end
    else if FMinesweeper.IsRevealed(Pos) then
    begin
      Canvas.Brush.Color := $9F7F7F;
      Canvas.Pen.Color := $7F5F5F;
    end
    else
    begin
      Canvas.Brush.Color := $DFAFAF;
      Canvas.Pen.Color := $AF8F8F;
    end;

    Canvas.Brush.Style := bsSolid;
    Canvas.RoundRect(Pos.X * FieldSize + 2, Pos.Y * FieldSize + 2,
      (Pos.X + 1) * FieldSize - 2, (Pos.Y + 1) * FieldSize - 2, 10, 10);

    Canvas.Brush.Style := bsClear;
    if (FMinesweeper.IsRevealed(Pos) or FLost) and not FMinesweeper.IsMine(Pos) then
    begin
      if FMinesweeper.HasAdjacentMines(Pos) then
        Canvas.TextOut(Pos.X * FieldSize + 6, Pos.Y * FieldSize + 1, IntToStr(FMinesweeper.AdjacentMines(Pos)));
    end;

  end;
end;

end.
