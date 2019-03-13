unit GameOfLifeDefine;

interface

uses
  Pengine.BitField, Pengine.IntMaths, SysUtils;

type

  TGameOfLife = class
  private
    FCells: array of TBitField;
    FOutside: Boolean;
    FCellCount: Integer;
    
    function GetSize: TIntVector2;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetCell(APos: TIntVector2): Boolean; 
    procedure SetCell(APos: TIntVector2; const Value: Boolean);
    function GetNeighbours(APos: TIntVector2): Integer;

  public
    constructor Create(ASize: TIntVector2); overload;
    constructor Create(AOther: TGameOfLife); overload;
    destructor Destroy; override;
    
    function Step(AFreeOld: Boolean): TGameOfLife;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Size: TIntVector2 read GetSize;
    property CellCount: Integer read FCellCount;

    property Outside: Boolean read FOutside write FOutside;

    property Cells[APos: TIntVector2]: Boolean read GetCell write SetCell; default;
    property Neighbours[APos: TIntVector2]: Integer read GetNeighbours;
                              
    function Equals(Obj: TObject): Boolean; override;
    
  end;

implementation

{ TGameOfLife }

constructor TGameOfLife.Create(ASize: TIntVector2);
var
  I: Integer;
begin
  if ASize.X < 1 then
    raise Exception.Create('Game of Life width must be greater than zero!');
  if ASize.Y < 1 then
    raise Exception.Create('Game of Life height must be greater than zero!');
  SetLength(FCells, ASize.Y);
  for I := 0 to ASize.Y - 1 do
    FCells[I] := TBitField.Create(ASize.X);
end;

constructor TGameOfLife.Create(AOther: TGameOfLife);
var
  I: Integer;
begin
  SetLength(FCells, AOther.Height);
  for I := 0 to Height - 1 do
  begin
    FCells[I] := TBitField.Create(AOther.Width);  
    FCells[I].Assign(AOther.FCells[I]);
  end;
  FCellCount := AOther.FCellCount;
end;

destructor TGameOfLife.Destroy;
var
  Cells: TBitField;
begin
  for Cells in FCells do
    Cells.Free;
  inherited;
end;

function TGameOfLife.Equals(Obj: TObject): Boolean;
var
  Other: TGameOfLife;
  I: Integer;
begin
  if not (Obj is TGameOfLife) then
    Exit(False);
  Other := TGameOfLife(Obj);
  if FCellCount <> Other.FCellCount then
    Exit(False);
  if Size <> Other.Size then
    Exit(False);
  for I := 0 to Height - 1 do
    if not FCells[I].Equals(Other.FCells[I]) then
      Exit(False);
  Result := True;
end;

function TGameOfLife.GetCell(APos: TIntVector2): Boolean;
begin
  if not (APos in Size) then
    Result := FOutside
  else
    Result := FCells[APos.Y][APos.X];
end;

function TGameOfLife.GetHeight: Integer;
begin
  Result := Length(FCells);
end;

function TGameOfLife.GetNeighbours(APos: TIntVector2): Integer;
var
  Offset: TIntVector2;
begin
  Result := 0;
  for Offset in IBounds1I(-1, 1) do
    if (Offset <> 0) and Cells[APos + Offset] then
      Inc(Result);
end;

function TGameOfLife.GetSize: TIntVector2;
begin
  Result.Create(Width, Height);
end;

function TGameOfLife.GetWidth: Integer;
begin
  Result := FCells[0].Size;
end;

procedure TGameOfLife.SetCell(APos: TIntVector2; const Value: Boolean);
begin
  if Cells[APos] and not Value then
    Dec(FCellCount);
  if not Cells[APos] and Value then
    Inc(FCellCount);
  FCells[APos.Y][APos.X] := Value;
end;

function TGameOfLife.Step(AFreeOld: Boolean): TGameOfLife;
var
  Pos: TIntVector2;
  N: Integer;
begin
  Result := TGameOfLife.Create(Size);
  for Pos in Size do
  begin
    N := Neighbours[Pos];
    if (N = 3) or (N = 2) and Cells[Pos] then
      Result[Pos] := True;
  end;  
  if AFreeOld then
    Free;  
end;

end.

