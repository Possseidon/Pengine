unit Pengine.MC.BlockStructure;

interface

uses
  System.SysUtils,

  Pengine.IntMaths,
  Pengine.EventHandling,

  Pengine.MC.BlockState,
  Pengine.MC.Namespace;

type

  TBlockStructure = class
  public type

    TBlocks = array of array of array of TBlockState;

    TEventInfo = TSenderEventInfo<TBlockStructure>;

    TEvent = TEvent<TEventInfo>;

  private
    FBlocks: TBlocks;
    FOnChanged: TEvent;

    function GetSize: TIntVector3;
    procedure SetSize(const Value: TIntVector3);
    function GetBlock(APos: TIntVector3): TBlockState;

  public
    constructor Create;

    procedure Clear;

    property Size: TIntVector3 read GetSize write SetSize;

    function Exists(APos: TIntVector3): Boolean;
    property Blocks[APos: TIntVector3]: TBlockState read GetBlock; default;
    function SetBlock(APos: TIntVector3; ANSPath: TNSPath): TBlockState;
    procedure RemoveBlock(APos: TIntVector3);

    function OnChanged: TEvent.TAccess;

  end;

implementation

{ TBlockStructure }

procedure TBlockStructure.Clear;
var
  Pos: TIntVector3;
begin
  for Pos in Size do
    RemoveBlock(Pos);
end;

constructor TBlockStructure.Create;
begin
  Size := 1;
end;

function TBlockStructure.Exists(APos: TIntVector3): Boolean;
begin
  Result := Blocks[APos] <> nil;
end;

function TBlockStructure.GetBlock(APos: TIntVector3): TBlockState;
begin
  Result := FBlocks[APos.X, APos.Y, APos.Z];
end;

function TBlockStructure.GetSize: TIntVector3;
begin
  Result.X := Length(FBlocks);
  Result.Y := Length(FBlocks[0]);
  Result.Z := Length(FBlocks[0, 0]);
end;

function TBlockStructure.OnChanged: TEvent.TAccess;
begin
  Result := FOnChanged.Access;
end;

procedure TBlockStructure.RemoveBlock(APos: TIntVector3);
begin
  FreeAndNil(FBlocks[APos.X, APos.Y, APos.Z]);
end;

function TBlockStructure.SetBlock(APos: TIntVector3; ANSPath: TNSPath): TBlockState;
begin
  if Exists(APos) then
    Blocks[APos].Free;
  Result := TBlockState.Create(ANSPath);
  FBlocks[APos.X, APos.Y, APos.Z] := Result;
end;

procedure TBlockStructure.SetSize(const Value: TIntVector3);
begin
  Clear;
  SetLength(FBlocks, Value.X, Value.Y, Value.Z);
end;

end.
