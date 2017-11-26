unit Pengine.AdvancedFileStream;

interface

uses
  Classes, VectorGeometry, SysUtils, BitField, Lists;

type

  { EFileNotFound }

  EFileNotFound = class (EFilerError)
  public
    constructor Create(AFileName: String);
  end;

  TOpenMode = (omWrite, omRead, omAppend);

  { TAdvFileStream }

  TAdvFileStream = class (TFileStream)
  public
    constructor Create(AFilename: String; AOpenMode: TOpenMode);
    function Read(out Buffer; Count: Longint): Longint; reintroduce; overload;

    // Bool
    procedure Write(const AData: Boolean); overload;
    procedure Read(out AResult: Boolean); overload;
    function ReadBoolean: Boolean;

    // SmallInt
    procedure Write(const AData: SmallInt); overload;
    procedure Read(out AResult: SmallInt); overload;
    function ReadSmallInt: SmallInt;

    // Byte
    procedure Write(const AData: Byte); overload;
    procedure Read(out AResult: Byte); overload;
    // ReadByte exists

    // ShortInt
    procedure Write(const AData: ShortInt); overload;
    procedure Read(out AResult: ShortInt); overload;
    function ReadShortInt: ShortInt;

    // Word
    procedure Write(const AData: Word); overload;
    procedure Read(out AResult: Word); overload;
    // ReadWord exists

    // Integer
    procedure Write(const AData: Integer); overload;
    procedure Read(out AResult: Integer); overload;
    function ReadInteger: Integer;

    // Cardinal
    procedure Write(const AData: Cardinal); overload;
    procedure Read(out AResult: Cardinal); overload;
    function ReadCardinal: Cardinal;

    // Int64
    procedure Write(const AData: Int64); overload;
    procedure Read(out AResult: Int64); overload;
    function ReadInt64: Int64;

    // UInt64
    procedure Write(const AData: UInt64); overload;
    procedure Read(out AResult: UInt64); overload;
    function ReadUInt64: UInt64;

    // Single
    procedure Write(const AData: Single); overload;
    procedure Read(out AResult: Single); overload;
    function ReadSingle: Single;

    // Double
    procedure Write(const AData: Double); overload;
    procedure Read(out AResult: Double); overload;
    function ReadDouble: Double;

    // Vector
    procedure Write(const AData: TGVector4); overload;
    procedure Read(out AResult: TGVector4); overload;
    function ReadVector4: TGVector4;

    procedure Write(const AData: TGVector3); overload;
    procedure Read(out AResult: TGVector3); overload;
    function ReadVector3: TGVector3;

    procedure Write(const AData: TGVector2); overload;
    procedure Read(out AResult: TGVector2); overload;
    function ReadVector2: TGVector2;

    // BitField
    procedure Write(ABitField: TBitField); overload;
    procedure Read(ABitField: TBitField); overload;

    // TTagList
    procedure Write(ATagList: TTags); overload;
    procedure Read(ATagList: TTags); overload;

  end;

implementation

{ EFileNotFound }

constructor EFileNotFound.Create(AFileName: String);
begin
  inherited Create('File "' + AFileName + '" not found!');
end;

{ TAdvFileStream }

constructor TAdvFileStream.Create(AFilename: String; AOpenMode: TOpenMode);
var
  Flags: Word;
begin
  case AOpenMode of
    omWrite:
      Flags := fmOpenWrite or fmCreate;
    omRead:
      Flags := fmOpenRead;
    else // omAppend
      Flags := fmOpenReadWrite or fmCreate;
  end;
  if (fmCreate and Flags = 0) and not FileExists(AFilename) then
    raise EFileNotFound.Create(AFileName);

  inherited Create(AFilename, Flags);
end;

function TAdvFileStream.Read(out Buffer; Count: Longint): Longint;
begin
  Result := inherited Read(Buffer{%H-}, Count);
end;

procedure TAdvFileStream.Write(const AData: Boolean);
begin
  Write(AData, SizeOf(Byte));
end;

procedure TAdvFileStream.Read(out AResult: Boolean);
begin
  Read(AResult, SizeOf(Byte));
end;

function TAdvFileStream.ReadBoolean: Boolean;
begin
  Read(Result, SizeOf(Byte));
end;

procedure TAdvFileStream.Write(const AData: SmallInt);
begin
  Write(AData, SizeOf(SmallInt));
end;

procedure TAdvFileStream.Read(out AResult: SmallInt);
begin
  Read(AResult, SizeOf(SmallInt));
end;

function TAdvFileStream.ReadSmallInt: SmallInt;
begin
  Read(Result, SizeOf(SmallInt));
end;

procedure TAdvFileStream.Write(const AData: Byte);
begin
  Write(AData, SizeOf(Byte));
end;

procedure TAdvFileStream.Read(out AResult: Byte);
begin
  Read(AResult, SizeOf(Byte));
end;

procedure TAdvFileStream.Write(const AData: ShortInt);
begin
  Write(AData, SizeOf(ShortInt));
end;

procedure TAdvFileStream.Read(out AResult: ShortInt);
begin
  Read(AResult, SizeOf(ShortInt));
end;

function TAdvFileStream.ReadShortInt: ShortInt;
begin
  Read(Result, SizeOf(ShortInt));
end;

procedure TAdvFileStream.Write(const AData: Word);
begin
  Write(AData, SizeOf(Word));
end;

procedure TAdvFileStream.Read(out AResult: Word);
begin
  Read(AResult, SizeOf(Word));
end;

procedure TAdvFileStream.Write(const AData: Integer);
begin
  Write(AData, SizeOf(Integer));
end;

procedure TAdvFileStream.Read(out AResult: Integer);
begin
  Read(AResult, SizeOf(Integer));
end;

function TAdvFileStream.ReadInteger: Integer;
begin
  Read(Result, SizeOf(Integer));
end;

procedure TAdvFileStream.Write(const AData: Cardinal);
begin
  Write(AData, SizeOf(Cardinal));
end;

procedure TAdvFileStream.Read(out AResult: Cardinal);
begin
  Read(AResult, SizeOf(Cardinal));
end;

function TAdvFileStream.ReadCardinal: Cardinal;
begin
  Read(Result, SizeOf(Cardinal));
end;

procedure TAdvFileStream.Write(const AData: Int64);
begin
  Write(AData, SizeOf(Int64));
end;

procedure TAdvFileStream.Read(out AResult: Int64);
begin
  Read(AResult, SizeOf(Int64));
end;

function TAdvFileStream.ReadInt64: Int64;
begin
  Read(Result, SizeOf(Int64));
end;

procedure TAdvFileStream.Write(const AData: UInt64);
begin
  Write(AData, SizeOf(UInt64));
end;

procedure TAdvFileStream.Read(out AResult: UInt64);
begin
  Read(AResult, SizeOf(UInt64));
end;

function TAdvFileStream.ReadUInt64: UInt64;
begin
  Read(Result, SizeOf(UInt64));
end;

procedure TAdvFileStream.Write(const AData: Single);
begin
  Write(AData, SizeOf(Single));
end;

procedure TAdvFileStream.Read(out AResult: Single);
begin
  Read(AResult, SizeOf(Single));
end;

function TAdvFileStream.ReadSingle: Single;
begin
  Read(Result, SizeOf(Single));
end;

procedure TAdvFileStream.Write(const AData: Double);
begin
  Write(AData, SizeOf(Double));
end;

procedure TAdvFileStream.Read(out AResult: Double);
begin
  Read(AResult, SizeOf(Double));
end;

function TAdvFileStream.ReadDouble: Double;
begin
  Read(Result, SizeOf(Double));
end;

procedure TAdvFileStream.Write(const AData: TGVector4);
begin
  Write(AData, SizeOf(TGVector4));
end;

procedure TAdvFileStream.Read(out AResult: TGVector4);
begin
  Read(AResult, SizeOf(TGVector4));
end;

function TAdvFileStream.ReadVector4: TGVector4;
begin
  Read(Result, SizeOf(TGVector4));
end;

procedure TAdvFileStream.Write(const AData: TGVector3);
begin
  Write(AData, SizeOf(TGVector3));
end;

procedure TAdvFileStream.Read(out AResult: TGVector3);
begin
  Read(AResult, SizeOf(TGVector3));
end;

function TAdvFileStream.ReadVector3: TGVector3;
begin
  Read(Result, SizeOf(TGVector3));
end;

procedure TAdvFileStream.Write(const AData: TGVector2);
begin
  Write(AData, SizeOf(TGVector2));
end;

procedure TAdvFileStream.Read(out AResult: TGVector2);
begin
  Read(AResult, SizeOf(TGVector2));
end;

function TAdvFileStream.ReadVector2: TGVector2;
begin
  Read(Result, SizeOf(TGVector2));
end;

procedure TAdvFileStream.Write(ABitField: TBitField);
begin
  Write(ABitField.Size);
  Write(ABitField.DataPointer^, ABitField.ByteSize);
end;

procedure TAdvFileStream.Read(ABitField: TBitField);
begin
  ABitField.SetSize(ReadInteger);
  Read(ABitField.DataPointer^, ABitField.ByteSize);
end;

procedure TAdvFileStream.Write(ATagList: TTags);
var
  S: String;
begin
  Write(ATagList.Count);
  for S in ATagList do
    WriteAnsiString(S);
end;

procedure TAdvFileStream.Read(ATagList: TTags);
var
  I: Integer;
begin
  ATagList.Clear;
  for I := 0 to ReadCardinal - 1 do
    ATagList[ReadAnsiString] := True;
end;

end.
