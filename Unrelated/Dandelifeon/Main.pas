unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, IntegerMaths, GameOfLifeDefine, Vcl.StdCtrls, Math;

type
  TfrmMain = class(TForm)
    pnlControl: TPanel;
    pbGame: TPaintBox;
    btnSimulate: TButton;
    btnShowStart: TButton;
    lbGenerations: TLabel;
    lbMana: TLabel;
    lbCellCount: TLabel;
    lbManaPerCell: TLabel;
    btnRandom: TButton;
    procedure pbGamePaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbGameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbGameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbGameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnSimulateClick(Sender: TObject);
    procedure btnShowStartClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
  private
    FSimulation: array of TGameOfLife;
    FDisplayGame: TGameOfLife;
    FKillZone: TIntBounds2;
    FBackbuffer: TBitmap;
    FDragging: Boolean;
    FDragState: Boolean;

    FMaxManaPerCell: Single;

    function PaintboxToGame(X, Y: Integer): TIntVector2;
    procedure Simulate;
    procedure SetDisplayGame(const Value: TGameOfLife);

    property DisplayGame: TGameOfLife read FDisplayGame write SetDisplayGame;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnRandomClick(Sender: TObject);
const
  MaxCells = 20;
var
  Pos: TIntVector2;
  PreMax: Single;
  TooMuch: Boolean;
begin
  PreMax := FMaxManaPerCell;
  while PreMax >= FMaxManaPerCell do
  begin
    TooMuch := FSimulation[0].CellCount > MaxCells;
    Pos.Create(Random(25), Random(25));
    if not (Pos in FKillZone) then
    begin
      FSimulation[0][Pos] := not FSimulation[0][Pos] and not TooMuch;
      Simulate;
    end;
  end;
  lbCellCount.Caption := Format('Cell-Count: %d', [FSimulation[0].CellCount]);
  pbGame.Invalidate;
end;

procedure TfrmMain.btnShowStartClick(Sender: TObject);
begin
  DisplayGame := FSimulation[0];
end;

procedure TfrmMain.btnSimulateClick(Sender: TObject);
begin
  Simulate;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FBackbuffer := TBitmap.Create;
  FBackbuffer.SetSize(pbGame.Width, pbGame.Height);
  SetLength(FSimulation, 1);
  FSimulation[0] := TGameOfLife.Create(IVec2(25, 25));
  DisplayGame := FSimulation[0];
  FKillZone := Range2(DisplayGame.Size div 2 - 1, DisplayGame.Size div 2 + 2);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  Simulation: TGameOfLife;
begin
  FBackbuffer.Free;
  for Simulation in FSimulation do
    Simulation.Free;
end;

function TfrmMain.PaintboxToGame(X, Y: Integer): TIntVector2;
begin
  Result := Range2(0, 24).EnsureRange(IVec2(X, Y) div 20);
end;

procedure TfrmMain.pbGameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TIntVector2;
begin
  P := PaintboxToGame(X, Y);
  DisplayGame[P] := not DisplayGame[P];
  FDragState := DisplayGame[P];
  Simulate;
  pbGame.Invalidate;
  FDragging := True;
  lbCellCount.Caption := Format('Cell-Count: %d', [FSimulation[0].CellCount]);
end;

procedure TfrmMain.pbGameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TIntVector2;
begin
  if FDragging then
  begin  
    P := PaintboxToGame(X, Y);
    if DisplayGame[P] <> FDragState then
    begin
      DisplayGame[P] := FDragState;
      Simulate;
      pbGame.Invalidate;
      lbCellCount.Caption := Format('Cell-Count: %d', [FSimulation[0].CellCount]);
    end;
  end;
end;

procedure TfrmMain.pbGameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

procedure TfrmMain.pbGamePaint(Sender: TObject);
var
  Pos: TIntVector2;
begin
  FBackbuffer.Canvas.Brush.Style := bsSolid;
  FBackbuffer.Canvas.Pen.Style := psSolid;
  FBackbuffer.Canvas.Pen.Color := clBlack;
  for Pos in Range2(DisplayGame.Size) do
  begin
    if Pos in FKillZone then
    begin
      if DisplayGame[Pos] then        
        FBackbuffer.Canvas.Brush.Color := $00DFFF
      else
        FBackbuffer.Canvas.Brush.Color := $0000FF;
    end
    else
    begin
      if DisplayGame[Pos] then        
        FBackbuffer.Canvas.Brush.Color := $3FEF7F
      else
        FBackbuffer.Canvas.Brush.Color := $003F00;
    end;
    FBackbuffer.Canvas.Rectangle(Pos.X * 20, Pos.Y * 20, (Pos.X + 1) * 20, (Pos.Y + 1) * 20);  
  end;
  BitBlt(pbGame.Canvas.Handle, 0, 0, FBackbuffer.Width, FBackbuffer.Height, FBackbuffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TfrmMain.SetDisplayGame(const Value: TGameOfLife);
begin
  if FDisplayGame = Value then
    Exit;
  FDisplayGame := Value;
  pbGame.Invalidate;
end;

procedure TfrmMain.Simulate;
var
  I, Mana: Integer;
  BreakFlag: Boolean;
  P: TIntVector2;
  J: Integer;
begin
  Mana := 0;
  for I := 1 to Length(FSimulation) - 1 do
    FSimulation[I].Free;
  SetLength(FSimulation, 1);
  BreakFlag := False;
  for I := 0 to 10000 do
  begin 
    for P in FKillZone do
    begin
      if FSimulation[I][P] then
      begin
        Inc(Mana, Min(I, 100));
        BreakFlag := True;
      end;
    end;
    if BreakFlag then
      Break;    
    SetLength(FSimulation, I + 2);
    FSimulation[I + 1] := FSimulation[I].Step(False);
    for J := 0 to I do
    begin
      if FSimulation[J].Equals(FSimulation[I + 1]) then
      begin    
        lbGenerations.Caption := Format('Generations: %d long loop after %d generations', [I - J + 1, I]);
        lbMana.Caption := 'Mana: 0';
        lbManaPerCell.Caption := 'Mana/Cell: 0';
        Exit;      
      end;
    end;
  end;
  if BreakFlag then
  begin
    lbGenerations.Caption := Format('Generations: %d', [Length(FSimulation) - 1]);
    lbMana.Caption := Format('Mana: %d', [Mana]);
    lbManaPerCell.Caption := Format('Mana/Cell: %f', [Mana / FSimulation[0].CellCount]);
    FMaxManaPerCell := Max(FMaxManaPerCell, Mana / FSimulation[0].CellCount);
  end
  else
  begin
    lbGenerations.Caption := 'Generations: > 10000';
    lbMana.Caption := 'Mana: ???';
    lbManaPerCell.Caption := 'Mana/Cell: ???';
  end;
end;

end.

