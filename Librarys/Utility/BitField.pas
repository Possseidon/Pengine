unit BitField;

interface

uses
  SysUtils;

type

  { EBitfieldRangeException }

  EBitfieldRangeException = class (Exception)
    constructor Create(AValue, ARange: Integer);
  end;

  { EBitfieldDifferentSize }

  EBitfieldDifferentSize = class (Exception)
    constructor Create(ASize1, ASize2: Integer);
  end;

  { TBitField }

  TBitField = class
  private
    type

      { TIterator }

      TIterator = class
      private
        FBitField: TBitField;
        FCurrent: Integer;
        FReverse: Boolean;
      public
        constructor Create(ABitField: TBitField; AReverse: Boolean);
        property Current: Integer read FCurrent;
        function MoveNext: Boolean;
      end;

  private
    B: array of Byte;
    FSize: Integer;
    FIterReverse: Boolean;
    FOnes: Integer;

    function GetBit(I: Integer): Boolean;
    function GetByteSize: Integer;
    function GetDataPointer: PByte;
    function GetFirstOne: Integer;
    function GetFirstZero: Integer;
    function GetLastOne: Integer;
    function GetLastZero: Integer;
    function GetZeros: Integer;
    procedure SetBit(I: Integer; AValue: Boolean);

    procedure CheckRange(I: Integer);

    property Bit[I: Integer]: Boolean read GetBit write SetBit; default;
  public
    constructor Create; overload;
    constructor Create(Bits: Integer); overload;
    destructor Destroy; override;

    procedure SetSize(Bits: Integer);
    function Toggle(FBit: Integer): Boolean;
    procedure Clear;
    procedure Fill;

    procedure Invert;
    procedure SetBits(ABitField: TBitField);
    procedure ClearBits(ABitField: TBitField);

    procedure NotFrom(ABitField: TBitField);
    procedure OrFrom(ABitField1, ABitField2: TBitField);
    procedure AndFrom(ABitField1, ABitField2: TBitField);
    procedure XOrFrom(ABitField1, ABitField2: TBitField);

    procedure Assign(ABitField: TBitField);

    function IterReversed: TBitField;

    property Size: Integer read FSize;
    property ByteSize: Integer read GetByteSize;
    property DataPointer: PByte read GetDataPointer;
    property Ones: Integer read FOnes;
    property Zeros: Integer read GetZeros;

    property FirstOne: Integer read GetFirstOne;
    property FirstZero: Integer read GetFirstZero;
    property LastOne: Integer read GetLastOne;
    property LastZero: Integer read GetLastZero;

    function GetEnumerator: TIterator;
  end;

implementation

uses
  Math;

{ EBitfieldDifferentSize }

constructor EBitfieldDifferentSize.Create(ASize1, ASize2: Integer);
begin
  CreateFmt('BitFields must have the same size! %d <> %d', [ASize1, ASize2]);
end;

{ TBitField.TIterator }

constructor TBitField.TIterator.Create(ABitField: TBitField; AReverse: Boolean);
begin
  FBitField := ABitField;
  FReverse := AReverse;
  if FReverse then
    FCurrent := FBitField.Size
  else
    FCurrent := -1;
end;

function TBitField.TIterator.MoveNext: Boolean;
begin
  case FReverse of
    False:
      repeat
        Inc(FCurrent);
        if FCurrent = FBitField.Size then
           Exit(False);
      until FBitField[FCurrent];
    else // True
      repeat
        Dec(FCurrent);
        if FCurrent = -1 then
           Exit(False);
      until FBitField[FCurrent];
  end;
  Result := True;
end;

{ EBitfieldRangeException }

constructor EBitfieldRangeException.Create(AValue, ARange: Integer);
begin
  CreateFmt('BitField Index %d out of Range [0 - %d]!', [AValue, ARange]);
end;

{ TBitField }

function TBitField.GetBit(I: Integer): Boolean;
begin
  CheckRange(I);
  Result := B[I div 8] shr (I mod 8) and 1 = 1;
end;

function TBitField.GetByteSize: Integer;
begin
  Result := Length(B);
end;

function TBitField.GetDataPointer: PByte;
begin
  Result := @(B[0]);
end;

function TBitField.GetFirstOne: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Ones = 0 then
     Exit;
  for I := 0 to Size - 1 do
    if Self[I] then
      Exit(I);
end;

function TBitField.GetFirstZero: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Zeros = 0 then
    Exit;
  for I := 0 to Size - 1 do
    if not Self[I] then
      Exit(I);
end;

function TBitField.GetLastOne: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Ones = 0 then
    Exit;
  for I := Size - 1 downto 0 do
    if Self[I] then
      Exit(I);
end;

function TBitField.GetLastZero: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Zeros = 0 then
    Exit;
  for I := Size - 1 downto 0 do
    if not Self[I] then
      Exit(I);
end;

function TBitField.GetZeros: Integer;
begin
  Result := FSize - Ones;
end;

procedure TBitField.SetBit(I: Integer; AValue: Boolean);
var
  Old: Byte;
begin
  CheckRange(I);
  Old := B[I div 8];
  if AValue then // set bit
  begin
    B[I div 8] := 1 shl (I mod 8) or B[I div 8];
    if Old <> B[I div 8] then
      Inc(FOnes);
  end
  else           // clear bit
  begin
    B[I div 8] := not (1 shl (I mod 8)) and B[I div 8];
    if Old <> B[I div 8] then
      Dec(FOnes);
  end;
end;

procedure TBitField.CheckRange(I: Integer);
begin
  if (I < 0) or (I >= FSize) then
    raise EBitfieldRangeException.Create(I, FSize - 1);
end;

constructor TBitField.Create;
begin
  SetSize(0);
end;

constructor TBitField.Create(Bits: Integer);
begin
  SetSize(Bits);
  Clear;
end;

destructor TBitField.Destroy;
begin
  inherited Destroy;
end;

procedure TBitField.SetSize(Bits: Integer);
begin
  SetLength(B, Ceil(Bits / 8));
  FSize := Bits;
end;

function TBitField.Toggle(FBit: Integer): Boolean;
begin
  Bit[FBit] := not Bit[FBit];
  Result := Bit[FBit];
end;

procedure TBitField.Clear;
begin
  if Length(B) > 0 then
     FillChar(B[0], Length(B), 0);
end;

procedure TBitField.Fill;
begin
  if Length(B) > 0 then
     FillChar(B[0], Length(B), $FF);
end;

procedure TBitField.Invert;
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
    Self[I] := not Self[I];
end;

procedure TBitField.SetBits(ABitField: TBitField);
var
  I: Integer;
begin
  for I := 0 to Min(Size, ABitField.Size) - 1 do
    Self[I] := Self[I] or ABitField[I];
end;

procedure TBitField.ClearBits(ABitField: TBitField);
var
  I: Integer;
begin
  for I := 0 to Min(Size, ABitField.Size) - 1 do
    Self[I] := Self[I] and not ABitField[I];
end;

procedure TBitField.NotFrom(ABitField: TBitField);
var
  I: Integer;
begin
  SetSize(ABitField.Size);
  for I := 0 to Size do
    Self[I] := not ABitField[I];
end;

procedure TBitField.OrFrom(ABitField1, ABitField2: TBitField);
var
  I: Integer;
begin
  if ABitField1.Size <> ABitField2.Size then
    raise EBitfieldDifferentSize.Create(ABitfield1.Size, ABitField2.Size);
  SetSize(ABitField1.Size);
  for I := 0 to Size do
    Self[I] := ABitField1[I] or ABitField2[I];
end;

procedure TBitField.AndFrom(ABitField1, ABitField2: TBitField);
var
  I: Integer;
begin
  if ABitField1.Size <> ABitField2.Size then
    raise EBitfieldDifferentSize.Create(ABitfield1.Size, ABitField2.Size);
  SetSize(ABitField1.Size);
  for I := 0 to Size do
    Self[I] := ABitField1[I] and ABitField2[I];
end;

procedure TBitField.XOrFrom(ABitField1, ABitField2: TBitField);
var
  I: Integer;
begin
  if ABitField1.Size <> ABitField2.Size then
    raise EBitfieldDifferentSize.Create(ABitfield1.Size, ABitField2.Size);
  SetSize(ABitField1.Size);
  for I := 0 to Size do
    Self[I] := ABitField1[I] xor ABitField2[I];
end;

procedure TBitField.Assign(ABitField: TBitField);
begin
  SetSize(ABitField.Size);
  Move(ABitField.DataPointer^, DataPointer^, ABitField.ByteSize);
end;

function TBitField.IterReversed: TBitField;
begin
  Self.FIterReverse := True;
  Result := Self;
end;

function TBitField.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self, FIterReverse);
  FIterReverse := False;
end;

end.

