unit Pengine.Bitfield;

{$PointerMath ON}

interface

uses
  System.SysUtils,

  Pengine.Utility,
  Pengine.IntMaths,
  Pengine.Collections,
  Pengine.CollectionInterfaces;

type

  /// <summary>Raised, if a bitfield-index was out of bounds.</summary>
  EBitfieldRangeError = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if an operation requires bitfields to be of the same size.</summary>
  EBitfieldSizeError = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if an operation requires the bitfield to have at least one true or false bit.</summary>
  EBitfieldEmptyOrFull = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Raised, if a negative size is assigned to a bitfield.</summary>
  EBitfieldNegativeSize = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>Represents a one-dimensional bitfield.</summary>
  TBitfield = class
  public type

    TData = NativeUInt;
    PData = ^TData;

    /// <summary>Iterates over each true or false bit in the bitfield.</summary>
    TIterator = class(TIterator<Integer>)
    private
      FBitfield: TBitfield;
      FCurrent: Integer;
      FFindBits: Boolean;
      FReversed: Boolean;

    public
      /// <summary>Creates and iterator, to iterate over either true of false bits in either direction.</summary>
      constructor Create(ABitfield: TBitfield; AFindBits: Boolean = True; AReversed: Boolean = False);

      /// <returns>The index of the current found bit.</returns>
      function GetCurrent: Integer; override;
      /// <summary>Searches for the next bit.</summary>
      function MoveNext: Boolean; override;

    end;

    TIterateWrapper = record
    private
      FBitfield: TBitfield;
      FFindBits: Boolean;
      FReversed: Boolean;
          
    public
      constructor Create(ABitfield: TBitfield; AFindBits: Boolean = True; AReversed: Boolean = False);
    
      function GetEnumerator: TIterator;
      
    end;
     
  public const

    DataBytes = SizeOf(TData);
    DataBits = DataBytes * 8;
  
  private
    FData: PData;
    FSize: Integer;

    function GetBit(I: Integer): Boolean;
    procedure SetBit(I: Integer; const Value: Boolean);

    procedure SetSize(const Value: Integer);
    function GetDataSetCount: Integer;
    function GetByteSize: Integer;

    function GetOnes: Integer;
    function GetZeros: Integer;

  public
    /// <summary>Creates an empty bitfield.</summary>
    constructor Create; overload;
    /// <summary>Creates a cleared bitfield with a specific size.</summary>
    constructor Create(ABits: Integer); overload;
    destructor Destroy; override;
                          
    /// <summary>Calculates the required count of data sets to store the given amount of bits.</summary>
    class function CalculateDataSetCount(ABits: Integer): Integer;
    /// <summary>Calculates the required size in bytes to store the given amount of bits.</summary>
    class function CalculateByteSize(ABits: Integer): Integer;
    
    /// <summary>Gives access to each bit as boolean.</summary>
    /// <remarks>Default property.</remarks>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    property Bit[I: Integer]: Boolean read GetBit write SetBit; default;

    /// <summary>Sets every bit to true.</summary>
    procedure Fill; overload;
    /// <summary>Sets every bit in the specified range to true.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if either bound is out of bounds.</exception>
    procedure Fill(ARange: TIntBounds1); overload;

    /// <summary>Sets every bit to false.</summary>
    procedure Clear; overload;
    /// <summary>Sets every bit in the specified range to true.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if either bound is out of bounds.</exception>
    procedure Clear(ARange: TIntBounds1); overload;

    /// <summary>Toggles each bit between true and false.</summary>
    procedure Invert; overload;
    /// <summary>Toggles the specified bit between true and false.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    procedure Invert(ABit: Integer); overload;
    /// <summary>Toggles the specified bit between true and false and returns the new state.</summary>
    /// <returns>The new state of the bit.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    function InvertGet(ABit: Integer): Boolean; overload;
    /// <summary>Toggles each bit in the specified range between true and false.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if either bound is out of bounds.</exception>
    procedure Invert(ARange: TIntBounds1); overload;

    /// <returns>A new bitfield, of a bitwise not operation.</returns>
    function Bnot: TBitfield;
    /// <returns>A new bitfield, of a bitwise and operation.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Band(ABitfield: TBitfield): TBitfield;
    /// <returns>A new bitfield, of a bitwise or operation.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Bor(ABitfield: TBitfield): TBitfield;
    /// <returns>A new bitfield, of a bitwise xor operation.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Bxor(ABitfield: TBitfield): TBitfield;

    /// <returns>A copy of the bitfield.</returns>
    function Copy: TBitfield;
    /// <summary>Copies the data from another bitfield.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    procedure Assign(ABitfield: TBitfield);

    /// <summary>The amount of bits, that the bitfield has.</summary>
    /// <remarks>When changing, existing bits preserve their state and new bits are initialized with false.</remarks>
    property Size: Integer read FSize write SetSize;
    /// <summary>The count of data-sets, that the bitfield takes up in acutal storage.</summary>
    property DataSetCount: Integer read GetDataSetCount;
    /// <summary>The size in bytes, that the bitfield takes up in actual storage.</summary>
    property ByteSize: Integer read GetByteSize;
    /// <summary>A pointer to the ByteSize-long data.</summary>
    /// <remarks>For optimization purposes, each data-set is flipped in memory.<p/>
    /// That means, that <c>00101010</c> is actually stored as <c>01010100</c>.</remarks>
    property DataPointer: PData read FData;

    /// <summary>The amount of true bits in the bitfield.</summary>
    property Ones: Integer read GetOnes;
    /// <summary>The amount of false bits in the bitfield.</summary>
    property Zeros: Integer read GetZeros;

    /// <returns>The index of the first true bit.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldEmptyOrFull"/> if there is no true bit.</exception>
    function FirstOne: Integer;
    /// <returns>The index of the last true bit.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldEmptyOrFull"/> if there is no true bit.</exception>
    function LastOne: Integer;
    /// <returns>The index of the first false bit.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldEmptyOrFull"/> if there is no false bit.</exception>
    function FirstZero: Integer;
    /// <returns>The index of the last false bit.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldEmptyOrFull"/> if there is no false bit.</exception>
    function LastZero: Integer;

    /// <returns>True, if any bit in the range is true.</returns>
    function AnyTrue(ABounds: TIntBounds1): Boolean; inline;
    /// <returns>True, if any bit in the range is false.</returns>
    function AnyFalse(ABounds: TIntBounds1): Boolean; inline;
    /// <returns>True, if all bits in the range are true.</returns>
    function AllTrue(ABounds: TIntBounds1): Boolean;
    /// <returns>True, if all bits in the range are false.</returns>
    function AllFalse(ABounds: TIntBounds1): Boolean;

    /// <summary>Finds a row of the same bits.</summary>
    /// <returns>The index of the found position or -1 if no row was found.</returns>
    function Find(AValue: Boolean; ACount: Integer): Integer;  
    
    /// <returns>An iteration wrapper, which can be used with for-in.</returns>
    function Iterate(AFindBits: Boolean = True; AReversed: Boolean = False): TIterateWrapper;
    
    /// <summary>Returns an iterator, which iterates over the indices of all true bits.</summary>
    function GetEnumerator: TIterator;

    /// <returns>A string consisting of ones and zeros.</returns>
    function ToString: string; override;

    /// <returns>True, each bit in both bitfields is equal.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Equals(ABitfield: TBitfield): Boolean; reintroduce; overload; 
    function Equals(Obj: TObject): Boolean; overload; override; 
    
    /// <summary>Checks, if the index is out of bounds.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    procedure RangeCheckException(ABit: Integer); inline;
    /// <returns>True, if the index is not out of bounds.</returns>
    function RangeCheck(ABit: Integer): Boolean; inline;

    /// <summary>Clears the last few bits, which aren't actually part of the bitfield.</summary>
    /// <remarks>This does not change any of the actual data.</remarks>
    procedure ClearExcessBits;

  end;

  /// <summary>Represents a two-dimensional bitfield.</summary>
  /// <remarks>Resizing, but still keeping the data is very quick for this version.<p/>
  /// Operations are slightly faster along the X-axis, as the Y-axis is only an array of simple bitfields.</remarks>
  TBitfieldArray = class
  public type
  
    TData = TObjectArray<TBitfield>;
     
    /// <summary>Iterates over each true or false bit in the bitfield.</summary>
    TIterator = class(TIterator<TIntVector2>)
    private
      FBitfield: TBitfieldArray;
      FCurrent: TIntVector2;
      FFindBits: Boolean;
      FReversed: Boolean;

    public
      /// <summary>Creates and iterator, to iterate over either true of false bits in either direction.</summary>
      constructor Create(ABitfield: TBitfieldArray; AFindBits: Boolean = True; AReversed: Boolean = False);

      /// <returns>The index of the current found bit.</returns>
      function GetCurrent: TIntVector2; override;
      /// <summary>Searches for the next bit.</summary>
      function MoveNext: Boolean; override;

    end;

    TIterateWrapper = record
    private
      FBitfield: TBitfieldArray;
      FFindBits: Boolean;
      FReversed: Boolean;
          
    public
      constructor Create(ABitfield: TBitfieldArray; AFindBits: Boolean = True; AReversed: Boolean = False);
    
      function GetEnumerator: TIterator;
      
    end;
    
  private
    FData: TData;
    FSize: TIntVector2;
    
    function GetBit(ABit: TIntVector2): Boolean;
    procedure SetBit(ABit: TIntVector2; const Value: Boolean);

    procedure SetSize(const Value: TIntVector2);

    function GetOnes: Integer;
    function GetZeros: Integer;

  public
    /// <summary>Creates an empty bitfield.</summary>
    constructor Create; overload;
    /// <summary>Creates a cleared bitfield with a specific size.</summary>
    constructor Create(ASize: TIntVector2); overload;
    destructor Destroy; override;
                    
    /// <summary>Gives access to each bit as boolean.</summary>
    /// <remarks>Default property.</remarks>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    property Bit[ABit: TIntVector2]: Boolean read GetBit write SetBit; default;

    /// <summary>Sets every bit to true.</summary>
    procedure Fill; overload;
    /// <summary>Sets every bit in the specified range to true.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if either bound is out of bounds.</exception>
    procedure Fill(ARange: TIntBounds2); overload;

    /// <summary>Sets every bit to false.</summary>
    procedure Clear; overload;
    /// <summary>Sets every bit in the specified range to true.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if either bound is out of bounds.</exception>
    procedure Clear(ARange: TIntBounds2); overload;

    /// <summary>Toggles each bit between true and false.</summary>
    procedure Invert; overload;
    /// <summary>Toggles the specified bit between true and false.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    procedure Invert(ABit: TIntVector2); overload;
    /// <summary>Toggles the specified bit between true and false and returns the new state.</summary>
    /// <returns>The new state of the bit.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    function InvertGet(ABit: TIntVector2): Boolean; overload;
    /// <summary>Toggles each bit in the specified range between true and false.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if either bound is out of bounds.</exception>
    procedure Invert(ARange: TIntBounds2); overload;

    /// <returns>A new bitfield, of a bitwise not operation.</returns>
    function Bnot: TBitfieldArray;
    /// <returns>A new bitfield, of a bitwise and operation.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Band(ABitfield: TBitfieldArray): TBitfieldArray;
    /// <returns>A new bitfield, of a bitwise or operation.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Bor(ABitfield: TBitfieldArray): TBitfieldArray;
    /// <returns>A new bitfield, of a bitwise xor operation.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Bxor(ABitfield: TBitfieldArray): TBitfieldArray;

    /// <returns>A copy of the bitfield.</returns>
    function Copy: TBitfieldArray;
    /// <summary>Copies the data from another bitfield.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    procedure Assign(ABitfield: TBitfieldArray);

    /// <summary>The amount of bits, that the bitfield has.</summary>
    /// <remarks>When changing, existing bits preserve their state and new bits are initialized with false.</remarks>
    property Size: TIntVector2 read FSize write SetSize;
    
    /// <summary>The amount of true bits in the bitfield.</summary>
    property Ones: Integer read GetOnes;
    /// <summary>The amount of false bits in the bitfield.</summary>
    property Zeros: Integer read GetZeros;
                 
    /// <returns>True, if any bit in the range is true.</returns>
    function AnyTrue(ABounds: TIntBounds2): Boolean; inline;
    /// <returns>True, if any bit in the range is false.</returns>
    function AnyFalse(ABounds: TIntBounds2): Boolean; inline;
    /// <returns>True, if all bits in the range are true.</returns>
    function AllTrue(ABounds: TIntBounds2): Boolean;
    /// <returns>True, if all bits in the range are false.</returns>
    function AllFalse(ABounds: TIntBounds2): Boolean;

    /// <returns>An iteration wrapper, which can be used with for-in.</returns>
    function Iterate(AFindBits: Boolean = True; AReversed: Boolean = False): TIterateWrapper;
    
    /// <summary>Returns an iterator, which iterates over the indices of all true bits.</summary>
    function GetEnumerator: TIterator;

    /// <returns>A multilined string, consisting of ones and zeros.</returns>
    function ToString: string; override;

    /// <returns>True, each bit in both bitfields is equal.</returns>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldSizeError"/> if the bitfields have different sizes.</exception>
    function Equals(ABitfield: TBitfieldArray): Boolean; reintroduce; overload;
    function Equals(Obj: TObject): Boolean; overload; override; 
    
    /// <summary>Checks, if the index is out of bounds.</summary>
    /// <exception><see cref="Pengine.Bitfield|EBitfieldRangeError"/> if the index is out of bounds.</exception>
    procedure RangeCheckException(ABit: TIntVector2); inline;
    /// <returns>True, if the index is not out of bounds.</returns>
    function RangeCheck(ABit: TIntVector2): Boolean; inline;

  end;

implementation

uses
  Math;

{ EBitfieldRangeError }

constructor EBitfieldRangeError.Create;
begin
  inherited Create('The bitfield index is out of bounds.');
end;

{ EBitfieldDifferentSize }

constructor EBitfieldSizeError.Create;
begin
  inherited Create('The operation requires the bitfields to have equal sizes.');
end;

{ EBitfieldEmptyOrFull }

constructor EBitfieldEmptyOrFull.Create;
begin
  inherited Create('The operation requires the bitfield to have at least one true or false bit.');
end;

{ EBitfieldNegativeSize }

constructor EBitfieldNegativeSize.Create;
begin
  inherited Create('The bitfield size cannot be negative.');
end;

{ TBitfield.TIterator }
                        
function TBitfield.TIterator.GetCurrent: Integer;
begin
  Result := FCurrent;
end;

constructor TBitfield.TIterator.Create(ABitfield: TBitfield; AFindBits, AReversed: Boolean);
begin
  FBitfield := ABitfield;
  FFindBits := AFindBits;
  FReversed := AReversed;
  if FReversed then
    FCurrent := FBitfield.Size
  else
    FCurrent := -1;  
end;

function TBitfield.TIterator.MoveNext: Boolean;
begin
  if FReversed then
  begin
    repeat
      Dec(FCurrent);
      if FCurrent = -1 then
        Exit(False);
    until FBitfield[FCurrent] = FFindBits;
  end
  else
  begin
    repeat
      Inc(FCurrent);
      if FCurrent = FBitfield.Size then
        Exit(False);
    until FBitfield[FCurrent] = FFindBits;
  end;
  Result := True;
end;

{ TBitfield.TIterateWrapper }

constructor TBitfield.TIterateWrapper.Create(ABitfield: TBitfield; AFindBits, AReversed: Boolean);
begin
  FBitfield := ABitfield;
  FFindBits := AFindBits;
  FReversed := AReversed;
end;

function TBitfield.TIterateWrapper.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(FBitfield, FFindBits, FReversed);
end;

{ TBitfield }

function TBitfield.GetBit(I: Integer): Boolean;
begin
  RangeCheckException(I);
  Result := ((FData + I div DataBits)^ shr (I mod DataBits)) and 1 = 1;
end;

procedure TBitfield.SetBit(I: Integer; const Value: Boolean);
var
  DataPos: PData;
  Mask: TData;
begin
  RangeCheckException(I);
  DataPos := FData + I div DataBits;
  Mask := TData(1) shl (I mod DataBits);
  if Value then
    DataPos^ := DataPos^ or Mask
  else
    DataPos^ := DataPos^ and not Mask;
end;

procedure TBitfield.SetSize(const Value: Integer);
var
  OldByteSize, NewByteSize: Integer;
begin
  if Value < 0 then
    raise EBitfieldNegativeSize.Create;
  if FSize = Value then
    Exit;
  OldByteSize := ByteSize;
  NewByteSize := CalculateByteSize(Value);
  ClearExcessBits;
  ReallocMem(FData, NewByteSize);
  if NewByteSize > OldByteSize then
    FillChar((FData + FSize div DataBits)^, NewByteSize - OldByteSize, 0);
  FSize := Value;
end;

function TBitfield.GetDataSetCount: Integer;
begin
  Result := CalculateDataSetCount(Size);
end;

function TBitfield.GetByteSize: Integer;
begin
  Result := CalculateByteSize(Size);
end;

function TBitfield.GetOnes: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to DataSetCount - 1 do
    Inc(Result, GetBitCount((FData + I)^));
end;

function TBitfield.GetZeros: Integer;
begin
  Result := Size - Ones;
end;

constructor TBitfield.Create;
begin
  // nothing
end;

constructor TBitfield.Create(ABits: Integer);
begin
  FSize := ABits;
  FData := AllocMem(ByteSize);
end;

destructor TBitfield.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

class function TBitfield.CalculateDataSetCount(ABits: Integer): Integer;
begin
  Result := Ceil(ABits / DataBits);
end;

class function TBitfield.CalculateByteSize(ABits: Integer): Integer;
begin
  Result := CalculateDataSetCount(ABits) * DataBytes;
end;

procedure TBitfield.Fill;
begin
  FillChar(FData^, ByteSize, not 0);
end;

procedure TBitfield.Fill(ARange: TIntBounds1);
var
  First, Last: Integer;
  DataPos: PData;
begin
  RangeCheckException(ARange.C1);
  RangeCheckException(ARange.C2 - 1);
  
  First := Ceil(ARange.C1 / DataBits);
  Last := ARange.C2 div DataBits;  
  if Last > First then
    FillChar((FData + First)^, (Last - First) * DataBytes, not 0);

  if Last >= First then
  begin
    if (ARange.C1 mod DataBits) <> 0 then
    begin
      DataPos := FData + First - 1;
      DataPos^ := DataPos^ or (TData(not 0) shl (ARange.C1 mod DataBits));
    end;  
     
    if (ARange.C2 mod DataBits) <> 0 then
    begin
      DataPos := FData + Last;
      DataPos^ := DataPos^ or (TData(not 0) shr (DataBits - ARange.C2 mod DataBits));
    end;  
  end
  else
  begin
    DataPos := FData + First - 1;
    DataPos^ := DataPos^ or 
      ((TData(not 0) shl (ARange.C1 mod DataBits)) and 
       (TData(not 0) shr (DataBits - ARange.C2 mod DataBits)));
  end;  
end;

function TBitfield.Find(AValue: Boolean; ACount: Integer): Integer;
var
  Found: Boolean;
  I: Integer;
begin
  Result := 0;
  while Result + ACount < Size do
  begin
    Found := True;
    for I := Result to Result + ACount - 1 do
    begin
      if Bit[I] <> AValue then
      begin
        Found := False;
        Result := I + 1;
        Break;
      end;
    end;
    if Found then
      Exit;
  end;
  Exit(-1);
end;

procedure TBitfield.Clear;
begin
  FillChar(FData^, ByteSize, 0);
end;

procedure TBitfield.Clear(ARange: TIntBounds1);
var
  First, Last: Integer;
  DataPos: PData;
begin
  RangeCheckException(ARange.C1);
  RangeCheckException(ARange.C2 - 1);
  
  First := Ceil(ARange.C1 / DataBits);
  Last := ARange.C2 div DataBits;  
  if Last > First then
    FillChar((FData + First)^, (Last - First) * DataBytes, 0);

  if Last >= First then
  begin  
    if (ARange.C1 mod DataBits) <> 0 then
    begin
      DataPos := FData + First - 1;
      DataPos^ := DataPos^ and not (TData(not 0) shl (ARange.C1 mod DataBits));
    end;  
     
    if (ARange.C2 mod DataBits) <> 0 then
    begin
      DataPos := FData + Last;
      DataPos^ := DataPos^ and not (TData(not 0) shr (DataBits - ARange.C2 mod DataBits));
    end;  
  end
  else
  begin
    DataPos := FData + First - 1;
    DataPos^ := DataPos^ and not 
      ((TData(not 0) shl (ARange.C1 mod DataBits)) and 
       (TData(not 0) shr (DataBits - ARange.C2 mod DataBits)));
  end;   
end;

procedure TBitfield.Invert;
var
  I: Integer;
  DataPos: PData;
begin
  for I := 0 to DataSetCount - 1 do
  begin
    DataPos := FData + I;
    DataPos^ := not DataPos^;
  end;
end;

procedure TBitfield.Invert(ABit: Integer);
var
  DataPos: PData;
begin
  DataPos := FData + ABit div DataBits;
  DataPos^ := DataPos^ xor (TData(1) shl (ABit mod DataBits));
end;

function TBitfield.InvertGet(ABit: Integer): Boolean;
begin
  Invert(ABit);
  Result := Bit[ABit];
end;

procedure TBitfield.Invert(ARange: TIntBounds1);
var
  First, Last: Integer;
  DataPos: PData;
  I: Integer;
begin
  RangeCheckException(ARange.C1);
  RangeCheckException(ARange.C2 - 1);
  
  First := Ceil(ARange.C1 / DataBits);
  Last := ARange.C2 div DataBits;  
  for I := First to Last - 1 do
  begin
    DataPos := FData + I;
    DataPos^ := DataPos^ xor TData(not 0);
  end;
  
  if Last >= First then
  begin  
    if (ARange.C1 mod DataBits) <> 0 then
    begin
      DataPos := FData + First - 1;
      DataPos^ := DataPos^ xor (TData(not 0) shl (ARange.C1 mod DataBits));
    end;  
     
    if (ARange.C2 mod DataBits) <> 0 then
    begin
      DataPos := FData + Last;
      DataPos^ := DataPos^ xor (TData(not 0) shr (DataBits - ARange.C2 mod DataBits));
    end;  
  end
  else
  begin
    DataPos := FData + First - 1;
    DataPos^ := DataPos^ xor 
      ((TData(not 0) shl (ARange.C1 mod DataBits)) and 
       (TData(not 0) shr (DataBits - ARange.C2 mod DataBits)));
  end; 
end;

function TBitfield.Bnot: TBitfield;
var
  I: Integer;
begin
  Result := TBitfield.Create(Size);
  for I := 0 to DataSetCount - 1 do
    (Result.FData + I)^ := not (FData + I)^;
end;

function TBitfield.Band(ABitfield: TBitfield): TBitfield;
var
  I: Integer;
begin
  if Size <> ABitfield.Size then
    raise EBitfieldSizeError.Create;
  Result := TBitfield.Create(Size);
  for I := 0 to DataSetCount - 1 do
    (Result.FData + I)^ := (FData + I)^ and (ABitfield.FData + I)^;
end;

function TBitfield.Bor(ABitfield: TBitfield): TBitfield;
var
  I: Integer;
begin             
  if Size <> ABitfield.Size then
    raise EBitfieldSizeError.Create;
  Result := TBitfield.Create(Size);
  for I := 0 to DataSetCount - 1 do
    (Result.FData + I)^ := (FData + I)^ or (ABitfield.FData + I)^;
end;

function TBitfield.Bxor(ABitfield: TBitfield): TBitfield;
var
  I: Integer;
begin              
  if Size <> ABitfield.Size then
    raise EBitfieldSizeError.Create;
  Result := TBitfield.Create(Size);
  for I := 0 to DataSetCount - 1 do
    (Result.FData + I)^ := (FData + I)^ xor (ABitfield.FData + I)^;
end;

function TBitfield.Copy: TBitfield;
begin
  Result := TBitfield.Create(Size);
  Result.Assign(Self);
end;

procedure TBitfield.Assign(ABitfield: TBitfield);
begin
  Size := ABitfield.Size;
  Move(ABitfield.FData^, FData^, ByteSize);
end;

function TBitfield.FirstOne: Integer;
var
  I: Integer;
begin
  for I in Iterate(True, False) do
    Exit(I);
  raise EBitfieldEmptyOrFull.Create;
end;

function TBitfield.LastOne: Integer;
var
  I: Integer;
begin
  for I in Iterate(True, True) do
    Exit(I);                        
  raise EBitfieldEmptyOrFull.Create;
end;

function TBitfield.FirstZero: Integer;
var
  I: Integer;
begin
  for I in Iterate(False, False) do
    Exit(I);                        
  raise EBitfieldEmptyOrFull.Create;
end;

function TBitfield.LastZero: Integer;
var
  I: Integer;
begin
  for I in Iterate(False, True) do
    Exit(I);    
  raise EBitfieldEmptyOrFull.Create;
end;

function TBitfield.AnyTrue(ABounds: TIntBounds1): Boolean;       
begin
  Result := not AllFalse(ABounds);
end;

function TBitfield.AnyFalse(ABounds: TIntBounds1): Boolean;
begin
  Result := not AllTrue(ABounds);
end;

function TBitfield.AllTrue(ABounds: TIntBounds1): Boolean;
var
  I: Integer;
begin
  // TODO: optimize
  for I in ABounds do
    if not Bit[I] then
      Exit(False);
  Result := True;
end;

function TBitfield.AllFalse(ABounds: TIntBounds1): Boolean;
var
  I: Integer;
begin
  // TODO: optimize
  for I in ABounds do
    if Bit[I] then
      Exit(False);
  Result := True;
end;

function TBitfield.Iterate(AFindBits, AReversed: Boolean): TIterateWrapper;
begin
  Result.Create(Self, AFindBits, AReversed);
end;

function TBitfield.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

function TBitfield.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Size - 1 do
  begin
    if Bit[I] then
      Result := Result + '1'
    else
      Result := Result + '0';
  end;
end;

function TBitfield.Equals(ABitfield: TBitfield): Boolean;
var
  I: Integer;
begin
  ClearExcessBits;
  ABitfield.ClearExcessBits;
  for I := 0 to DataSetCount - 1 do
    if (FData + I)^ <> (ABitfield.FData + I)^ then
      Exit(False);
  Result := True;
end;

function TBitfield.Equals(Obj: TObject): Boolean;
begin
  Result := Obj is TBitfield and Equals(TBitfield(Obj));
end;

procedure TBitfield.RangeCheckException(ABit: Integer);
begin
  if not RangeCheck(ABit) then
    raise EBitfieldRangeError.Create;
end;

function TBitfield.RangeCheck(ABit: Integer): Boolean;
begin
  Result := ABit in IBounds1(Size);
end;

procedure TBitfield.ClearExcessBits;
var
  Bits: Integer;
  Mask: TData;
  DataPos: PData;
begin
  Bits := Size mod DataBits;
  if Bits = 0 then
    Exit;
  Mask := (not 0) shl Bits;
  DataPos := FData + Size div DataBits;
  DataPos^ := DataPos^ and Mask;
end;

{ TBitfield2.TIterator }

constructor TBitfieldArray.TIterator.Create(ABitfield: TBitfieldArray; AFindBits, AReversed: Boolean);
begin
  FBitfield := ABitfield;
  FFindBits := AFindBits;
  FReversed := AReversed;
  if FReversed then
  begin
    FCurrent.X := FBitfield.Size.X + 1;
    FCurrent.Y := FBitfield.Size.Y;
  end
  else
    FCurrent := IVec2(0, -1);  
end;

function TBitfieldArray.TIterator.GetCurrent: TIntVector2;
begin
  Result := FCurrent;
end;

function TBitfieldArray.TIterator.MoveNext: Boolean;
begin
  if FReversed then
  begin
    repeat
      FCurrent.X := FCurrent.X - 1;
      if FCurrent.X = -1 then
      begin  
        FCurrent.Y := FCurrent.Y - 1;
        if FCurrent.Y = -1 then
          Exit(False);
      end;
    until FBitfield[FCurrent] = FFindBits;
  end
  else
  begin
    repeat
      FCurrent.X := FCurrent.X + 1;
      if FCurrent.X = FBitfield.Size.X then
      begin
        FCurrent.Y := FCurrent.Y + 1;
        if FCurrent.Y = FBitfield.Size.Y then
          Exit(False);
      end;
    until FBitfield[FCurrent] = FFindBits;
  end;
  Result := True;  
end;

{ TBitfieldArray.TIterateWrapper }

constructor TBitfieldArray.TIterateWrapper.Create(ABitfield: TBitfieldArray; AFindBits, AReversed: Boolean);
begin
  FBitfield := ABitfield;
  FFindBits := AFindBits;
  FReversed := AReversed;
end;

function TBitfieldArray.TIterateWrapper.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(FBitfield, FFindBits, FReversed);
end;
          
{ TBitfieldArray }

function TBitfieldArray.GetBit(ABit: TIntVector2): Boolean;
begin
  RangeCheckException(ABit);
  Result := FData[ABit.Y][ABit.X];
end;

procedure TBitfieldArray.SetBit(ABit: TIntVector2; const Value: Boolean);
begin
  RangeCheckException(ABit);
  FData[ABit.Y][ABit.X] := Value;
end;

procedure TBitfieldArray.SetSize(const Value: TIntVector2);
var
  Bitfield: TBitfield;
begin
  if not (FSize >= 0) then
    raise EBitfieldNegativeSize.Create;
    
  if FSize = Value then
    Exit;
    
  FSize.X := Value.X;
  for Bitfield in FData do
    Bitfield.Size := FSize.X;

  while FSize.Y > Value.Y do
  begin  
    FData.DelLast;
    FSize.Y := FSize.Y - 1;
  end;
  
  while FSize.Y < Value.Y do
  begin
    FData.Add(TBitfield.Create(FSize.X)); 
    FSize.Y := FSize.Y + 1;
  end;

end;

function TBitfieldArray.GetOnes: Integer;
var
  Bitfield: TBitfield;
begin
  Result := 0;
  for Bitfield in FData do
    Inc(Result, Bitfield.Ones);
end;

function TBitfieldArray.GetZeros: Integer;
var
  Bitfield: TBitfield;
begin
  Result := 0;
  for Bitfield in FData do
    Inc(Result, Bitfield.Zeros);
end;

constructor TBitfieldArray.Create;
begin
  FData := TData.Create;
end;

constructor TBitfieldArray.Create(ASize: TIntVector2);
begin
  Create;
  Size := ASize;
end;

destructor TBitfieldArray.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TBitfieldArray.Fill;
var
  Bitfield: TBitfield;
begin
  for Bitfield in FData do
    Bitfield.Fill;
end;

procedure TBitfieldArray.Fill(ARange: TIntBounds2);
var
  Y: Integer;
begin
  RangeCheckException(ARange.C1);
  RangeCheckException(ARange.C2 - 1);
  for Y in ARange.LineY do
    FData[Y].Fill(ARange.LineX);
end;

procedure TBitfieldArray.Clear;
var
  Bitfield: TBitfield;
begin
  for Bitfield in FData do
    Bitfield.Clear;
end;

procedure TBitfieldArray.Clear(ARange: TIntBounds2);
var
  Y: Integer;
begin               
  RangeCheckException(ARange.C1);
  RangeCheckException(ARange.C2 - 1);
  for Y in ARange.LineY do
    FData[Y].Clear(ARange.LineX);
end;

procedure TBitfieldArray.Invert;
var
  Bitfield: TBitfield;
begin
  for Bitfield in FData do
    Bitfield.Invert;
end;

procedure TBitfieldArray.Invert(ABit: TIntVector2);
begin
  Bit[ABit] := not Bit[ABit];
end;

function TBitfieldArray.InvertGet(ABit: TIntVector2): Boolean;
begin
  Invert(ABit);
  Result := Bit[ABit];
end;

procedure TBitfieldArray.Invert(ARange: TIntBounds2);
var
  Y: Integer;
begin          
  RangeCheckException(ARange.C1);
  RangeCheckException(ARange.C2 - 1);
  for Y in ARange.LineY do
    FData[Y].Invert(ARange.LineX);   
end;

function TBitfieldArray.Bnot: TBitfieldArray;
var
  I: Integer;
begin
  Result := TBitfieldArray.Create(0);
  Result.FSize := Size;
  Result.FData.Capacity := Size.Y;
  Result.FData.ForceCount(Size.Y);
  for I := 0 to Size.Y - 1 do
    Result.FData[I] := FData[I].Bnot;
end;

function TBitfieldArray.Band(ABitfield: TBitfieldArray): TBitfieldArray;
var
  I: Integer;
begin
  if Size <> ABitfield.Size then
    raise EBitfieldSizeError.Create;
  Result := TBitfieldArray.Create(0);
  Result.FSize := Size;
  Result.FData.Capacity := Size.Y;
  Result.FData.ForceCount(Size.Y);
  for I := 0 to Size.Y - 1 do
    Result.FData[I] := FData[I].Band(ABitfield.FData[I]);
end;

function TBitfieldArray.Bor(ABitfield: TBitfieldArray): TBitfieldArray;
var
  I: Integer;
begin           
  if Size <> ABitfield.Size then
    raise EBitfieldSizeError.Create;
  Result := TBitfieldArray.Create(0);
  Result.FSize := Size;
  Result.FData.Capacity := Size.Y;
  Result.FData.ForceCount(Size.Y);
  for I := 0 to Size.Y - 1 do
    Result.FData[I] := FData[I].Bor(ABitfield.FData[I]);
end;

function TBitfieldArray.Bxor(ABitfield: TBitfieldArray): TBitfieldArray;
var
  I: Integer;
begin           
  if Size <> ABitfield.Size then
    raise EBitfieldSizeError.Create;
  Result := TBitfieldArray.Create(0);
  Result.FSize := Size;
  Result.FData.Capacity := Size.Y;
  Result.FData.ForceCount(Size.Y);
  for I := 0 to Size.Y - 1 do
    Result.FData[I] := FData[I].Bxor(ABitfield.FData[I]);
end;

function TBitfieldArray.Copy: TBitfieldArray;
begin
  Result := TBitfieldArray.Create(0);
  Result.Assign(Self);
end;

procedure TBitfieldArray.Assign(ABitfield: TBitfieldArray);
var
  I: Integer;
begin
  Size := ABitfield.Size;
  for I := 0 to Size.Y - 1 do
    FData[I].Assign(ABitfield.FData[I]);
end;

function TBitfieldArray.AnyTrue(ABounds: TIntBounds2): Boolean;
begin
  Result := not AllFalse(ABounds);
end;

function TBitfieldArray.AnyFalse(ABounds: TIntBounds2): Boolean;
begin
  Result := not AllTrue(ABounds);
end;

function TBitfieldArray.AllTrue(ABounds: TIntBounds2): Boolean;
var
  Bitfield: TBitfield;
begin
  for Bitfield in FData do
    if not Bitfield.AllTrue(ABounds.LineX) then
      Exit(False);
  Result := True;
end;

function TBitfieldArray.AllFalse(ABounds: TIntBounds2): Boolean;
var
  Bitfield: TBitfield;
begin
  for Bitfield in FData do
    if not Bitfield.AllFalse(ABounds.LineX) then
      Exit(False);
  Result := True;
end;

function TBitfieldArray.Iterate(AFindBits, AReversed: Boolean): TIterateWrapper;
begin
  Result.Create(Self, AFindBits, AReversed);
end;

function TBitfieldArray.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

function TBitfieldArray.ToString: string;
var
  Bitfield: TBitfield;
begin
  Result := '';
  for Bitfield in FData do
    Result := Result + Bitfield.ToString + sLineBreak;
end;

function TBitfieldArray.Equals(ABitfield: TBitfieldArray): Boolean;
var
  I: Integer;
begin
  if Size <> ABitfield.Size then
    raise EBitfieldSizeError.Create;
  for I := 0 to Size.Y - 1 do
    if not FData[I].Equals(ABitfield.FData[I]) then
      Exit(False);
  Result := True;
end;

function TBitfieldArray.Equals(Obj: TObject): Boolean;
begin
  Result := Obj is TBitfieldArray and Equals(TBitfieldArray(Obj));
end;

procedure TBitfieldArray.RangeCheckException(ABit: TIntVector2);
begin
  if not RangeCheck(ABit) then
    raise EBitfieldRangeError.Create;
end;

function TBitfieldArray.RangeCheck(ABit: TIntVector2): Boolean;
begin
  Result := ABit in Size;
end;

end.
