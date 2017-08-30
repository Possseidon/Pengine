unit Pengine.Sorting;

interface

uses
  Classes, SysUtils, IntegerMaths;

type

  { TBinaryTreeSorter }
  {
  TBinaryTreeSorter = class abstract
  protected
    type
      TNode = record
        Left, Right: ^TNode;
        Data: TObject;
      end;
      PNode = ^TNode;

  private
    FRoot: PNode;

    procedure AddToNode(var Node: PNode; Entry: TObject);
    procedure SortedExecuteNode(Node: PNode);

    procedure FreeNode(var Node: PNode);

  protected
    class function Compare(A, B: TObject): Boolean; virtual; abstract;
    class procedure ExecuteEntry(Entry: TObject); virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddToTree(Entry: TObject);

    procedure SortedExecute;
  end;
  }
  { TQuickSorter }

  TQuickSorter = class abstract
  protected
    function Low: Integer; virtual; abstract;
    function High: Integer; virtual; abstract;

    procedure SavePivot(I: Integer); virtual; abstract;
    procedure DiscardPivot; virtual;
    function LessThanPivot(I: Integer): Boolean; virtual; abstract;
    function GreaterThanPivot(I: Integer): Boolean; virtual; abstract;
  
    procedure Swap(A, B: Integer); virtual; abstract;

  public
    function Sort: Boolean;

  end;

implementation

{ TQuickSorter }

procedure TQuickSorter.DiscardPivot;
begin
  // possibly nothing
end;

function TQuickSorter.Sort: Boolean;

  function QuickSortRecursive(iLo, iHi: Integer): Boolean;
  var
    Lo, Hi: Integer;
  begin
    Lo := iLo;
    Hi := iHi;

    SavePivot((Lo + Hi) div 2);

    repeat
      while LessThanPivot(Lo) do
      begin
        Inc(Lo);
        if Lo > iHi then
          Exit(False);
      end;
      while GreaterThanPivot(Hi) do
      begin
        Dec(Hi);
        if Hi < iLo then
          Exit(False);
      end;
      if Lo <= Hi then
      begin
        if Lo <> Hi then
          Swap(Lo, Hi);
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    DiscardPivot;

    Result := True;
    if Hi > iLo then
      Result := QuickSortRecursive(iLo, Hi);
    if Lo < iHi then
      Result := Result and QuickSortRecursive(Lo, iHi);
  end;

var
  L, H: Integer;
begin
  L := Low;
  H := High;
  if H - L > 1 then
    Result := QuickSortRecursive(L, H)
  else
    Result := True;
end;

{ TBinaryTreeSorter }
              {
procedure TBinaryTreeSorter.AddToNode(var Node: PNode; Entry: TObject);
begin
  if Node = nil then
  begin
    Node := AllocMem(SizeOf(TNode));
    Node^.Data := Entry;
  end
  else
  begin
    if Compare(Entry, Node^.Data) then
      AddToNode(Node^.Right, Entry)
    else
      AddToNode(Node^.Left, Entry);
  end;
end;

procedure TBinaryTreeSorter.SortedExecuteNode(Node: PNode);
begin
  if Node^.Left <> nil then
    SortedExecuteNode(Node^.Left);
  ExecuteEntry(Node^.Data);
  if Node^.Right <> nil then
    SortedExecuteNode(Node^.Right);
end;

procedure TBinaryTreeSorter.FreeNode(var Node: PNode);
begin
  if Node^.Left <> nil then
    FreeNode(Node^.Left);
  if Node^.Right <> nil then
    FreeNode(Node^.Right);
  FreeMem(Node);
end;

constructor TBinaryTreeSorter.Create;
begin
  FRoot := AllocMem(SizeOf(TNode));
end;

destructor TBinaryTreeSorter.Destroy;
begin
  FreeNode(FRoot);
  inherited Destroy;
end;

procedure TBinaryTreeSorter.AddToTree(Entry: TObject);
begin
  if FRoot^.Data = nil then
    FRoot^.Data := Entry
  else
    AddToNode(FRoot, Entry);
end;

procedure TBinaryTreeSorter.SortedExecute;
begin
  SortedExecuteNode(FRoot);
end;
              }

end.

