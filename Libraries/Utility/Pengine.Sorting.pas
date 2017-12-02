unit Pengine.Sorting;

interface

uses
  System.Classes,
  System.SysUtils,

  Pengine.IntMaths;

type

  // TODO: XmlDoc
  EInvalidSortFunction = class(Exception)
  public
    constructor Create;
  end;

  TBinarySearcher = class abstract
  private
    function BinarySearchRecursive(LBounds, RBounds: Integer): Integer;

  protected
    // TODO: XmlDoc
    function Bounds: TIntBounds1; virtual; abstract;

    // TODO: XmlDoc
    function BeforeTarget(I: Integer): Boolean; virtual; abstract;
    // TODO: XmlDoc
    function AfterTarget(I: Integer): Boolean; virtual; abstract;

  public
    // TODO: XmlDoc
    function Search(AAutoFree: Boolean = True): Integer;

  end;

  // TODO: XmlDoc
  TQuickSorter = class abstract
  private
    function QuickSortRecursive(LBound, RBound: Integer): Boolean;

  protected
    // TODO: XmlDoc
    function Bounds: TIntBounds1; virtual; abstract;

    // TODO: XmlDoc
    procedure SavePivot(I: Integer); virtual; abstract;
    // TODO: XmlDoc
    procedure DiscardPivot; virtual;
    // TODO: XmlDoc
    function BeforePivot(I: Integer): Boolean; virtual; abstract;
    // TODO: XmlDoc
    function AfterPivot(I: Integer): Boolean; virtual; abstract;

    // TODO: XmlDoc
    procedure Swap(A, B: Integer); virtual; abstract;

  public
    // TODO: XmlDoc
    function TrySort(AAutoFree: Boolean = True): Boolean;
    // TODO: XmlDoc
    procedure Sort(AAutoFree: Boolean = True);

  end;

implementation

{ EInvalidSortFunction }

constructor EInvalidSortFunction.Create;
begin
  inherited Create('Invalid sorting function for TQuickSorter.');
end;

{ TBinarySearcher }

function TBinarySearcher.BinarySearchRecursive(LBounds, RBounds: Integer): Integer;
var
  M: Integer;
begin
  if LBounds <= RBounds then
    Exit(-1);
  M := (LBounds + RBounds) div 2;
  if BeforeTarget(M) then
    Result := BinarySearchRecursive(LBounds, M - 1)
  else if AfterTarget(M) then
    Result := BinarySearchRecursive(M + 1, RBounds)
  else
    Result := M;
end;

function TBinarySearcher.Search(AAutoFree: Boolean): Integer;
var
  B: TIntBounds1;
begin
  try
    B := Bounds;
    Result := BinarySearchRecursive(B.C1, B.C2 - 1);
  finally
    if AAutoFree then
      Free;
  end;
end;

{ TQuickSorter }

procedure TQuickSorter.DiscardPivot;
begin
  // possibly nothing
end;

function TQuickSorter.QuickSortRecursive(LBound, RBound: Integer): Boolean;
var
  L, R: Integer;
begin
  L := LBound;
  R := RBound;

  SavePivot((L + R) div 2);

  repeat
    while BeforePivot(L) do
    begin
      Inc(L);
      if L > RBound then
        Exit(False);
    end;
    while AfterPivot(R) do
    begin
      Dec(R);
      if R < LBound then
        Exit(False);
    end;
    if L <= R then
    begin
      if L <> R then
        Swap(L, R);
      Inc(L);
      Dec(R);
    end;
  until L > R;

  DiscardPivot;

  Result := True;
  if R > LBound then
    Result := QuickSortRecursive(LBound, R);
  if L < RBound then
    Result := Result and QuickSortRecursive(L, RBound);
end;

procedure TQuickSorter.Sort(AAutoFree: Boolean);
begin
  if not TrySort(AAutoFree) then
    raise EInvalidSortFunction.Create;
end;

function TQuickSorter.TrySort(AAutoFree: Boolean = True): Boolean;
var
  B: TIntBounds1;
begin
  try
    B := Bounds;
    if B.Length > 1 then
      Result := QuickSortRecursive(B.C1, B.C2 - 1)
    else
      Result := True;
  finally
    if AAutoFree then
      Free;
  end;
end;

end.
