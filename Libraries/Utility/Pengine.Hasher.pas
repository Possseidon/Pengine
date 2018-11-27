unit Pengine.Hasher;

interface

uses
  System.SysUtils,

  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Equaller;

type

  THasher<V> = class abstract
  public
    class function Equal(const AValue1, AValue2: V): Boolean; virtual; abstract;    
    class function GetHash(const AValue: V): Cardinal; virtual; abstract;
    class function CanIndex(const AValue: V): Boolean; virtual;
  end;

  /// <summary>A generic, abstract class assistant, to calculate a hash for a specific value type V.</summary>
  /// <remarks>A <see cref="Pengine.ValueEqualler|TEqualler{V}"/> E is required, to counter hash collisions.<p/>
  /// A complex calculation is usually not necessary, as the modulus with a prime is taken by the hash collections.<p/>
  /// The main thing to look out for is, that for same values, <c>xor</c> will always calculate to zero.</remarks>
  THasher<V; E: TEqualler<V>> = class abstract(THasher<V>)
  public      
    class function Equal(const AValue1, AValue2: V): Boolean; override;
  end;

  /// <summary>Directly casts 32-Bit-Pointers to hashes, while taking the <c>xor</c> of low and upper half of
  /// 64-Bit-Pointers.</summary>
  TPointerHasher = class(THasher<Pointer, TPointerEqualler>)
  public
    class function GetHash(const AValue: Pointer): Cardinal; override;
    class function CanIndex(const AValue: Pointer): Boolean; override;
  end;

  /// <summary>Hashes the reference of any descendant of <see cref="System|TObject"/>.</summary>
  /// <remarks>Uses <see cref="Pengine.ValueHasher|TPointerHasher"/> internally.</remarks>
  TRefHasher<V: class> = class(THasher<V, TRefEqualler<V>>)
  public
    class function GetHash(const AValue: V): Cardinal; override;
    class function CanIndex(const AValue: V): Boolean; override;
  end;

  /// <summary>Directly uses the unsigned Integer as hash.</summary>
  TUIntHasher = class(THasher<Cardinal, TUIntEqualler>)
  public
    class function GetHash(const AValue: Cardinal): Cardinal; override;
  end;

  /// <summary>Directly cast from <see cref="System|Integer"/> to the hash.</summary>
  /// <remarks>This means, small negative values turn into huge hashes.</remarks>
  TIntHasher = class(THasher<Integer, TIntEqualler>)
  public
    class function GetHash(const AValue: Integer): Cardinal; override;
  end;

  /// <summary>Takes the <c>xor</c> of both components, after rotating the bits differently for each component.</summary>
  /// <remarks>The bit rotation makes it likely for vectors with same components, to not just calculate to zero.</remarks>
  TIntVector2Hasher = class(THasher<TIntVector2, TIntVector2Equaller>)
  public
    class function GetHash(const AValue: TIntVector2): Cardinal; override;
  end;

  /// <summary>Takes the <c>xor</c> of all components, after rotating the bits differently for each component.</summary>
  /// <remarks>The bit rotation makes it likely for vectors with same components, to not just calculate to zero.</remarks>
  TIntVector3Hasher = class(THasher<TIntVector3, TIntVector3Equaller>)
  public
    class function GetHash(const AValue: TIntVector3): Cardinal; override;
  end;

  /// <summary>Takes the <c>xor</c> of both components.</summary>
  /// <remarks>No further bit rotation is performed, as the bounds are usually different from each other anyway.</remarks>
  TIntBounds1Hasher = class(THasher<TIntBounds1, TIntBounds1Equaller>)
  public
    class function GetHash(const AValue: TIntBounds1): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TIntVector2Hasher"/> and takes the <c>xor</c> of both components.</summary>
  /// <remarks>No further bit rotation is performed, as the bounds are usually different from each other anyway.</remarks>
  TIntBounds2Hasher = class(THasher<TIntBounds2, TIntBounds2Equaller>)
  public
    class function GetHash(const AValue: TIntBounds2): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TIntVector3Hasher"/> and takes the <c>xor</c> of both components.</summary>
  /// <remarks>No further bit rotation is performed, as the bounds are usually different from each other anyway.</remarks>
  TIntBounds3Hasher = class(THasher<TIntBounds3, TIntBounds3Equaller>)
  public
    class function GetHash(const AValue: TIntBounds3): Cardinal; override;
  end;

  /// <summary>Directly uses the existing single bit pattern and converts it to a hash.</summary>
  TSingleHasher = class(THasher<Single, TSingleEqualler>)
  public
    class function GetHash(const AValue: Single): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TSingleHasher"/> and takes the <c>xor</c> of all components, after
  /// rotating the bits differently for each component.</summary>
  /// <remarks>The bit rotation makes it likely for vectors with same components, to not just calculate to zero.</remarks>
  TVector2Hasher = class(THasher<TVector2, TVector2Equaller>)
  public
    class function GetHash(const AValue: TVector2): Cardinal; override;
  end;

  /// <summary>Takes the <c>xor</c> of all components, after rotating the bits differently for each component.</summary>
  /// <remarks>The bit rotation makes it likely for vectors with same components, to not just calculate to zero.</remarks>
  TVector3Hasher = class(THasher<TVector3, TVector3Equaller>)
  public
    class function GetHash(const AValue: TVector3): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TSingleHasher"/> and takes the <c>xor</c> of both components.</summary>
  /// <remarks>No further bit rotation is performed, as the bounds are usually different from each other anyway.</remarks>
  TBounds1Hasher = class(THasher<TBounds1, TBounds1Equaller>)
  public
    class function GetHash(const AValue: TBounds1): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TVector2Hasher"/> and takes the <c>xor</c> of both components.</summary>
  /// <remarks>No further bit rotation is performed, as the bounds are usually different from each other anyway.</remarks>
  TBounds2Hasher = class(THasher<TBounds2, TBounds2Equaller>)
  public
    class function GetHash(const AValue: TBounds2): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TVector3Hasher"/> and takes the <c>xor</c> of both components.</summary>
  /// <remarks>No further bit rotation is performed, as the bounds are usually different from each other anyway.</remarks>
  TBounds3Hasher = class(THasher<TBounds3, TBounds3Equaller>)
  public
    class function GetHash(const AValue: TBounds3): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TVector2Hasher"/> and takes the <c>xor</c> of S and D.</summary>
  /// <remarks>No further bit rotation is performed, as S and D are usually different from each other anyway.</remarks>
  TLine2Hasher = class(THasher<TLine2, TLine2Equaller>)
  public
    class function GetHash(const AValue: TLine2): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TVector3Hasher"/> and takes the <c>xor</c> of S and D.</summary>
  /// <remarks>No further bit rotation is performed, as S and D are usually different from each other anyway.</remarks>
  TLine3Hasher = class(THasher<TLine3, TLine3Equaller>)
  public
    class function GetHash(const AValue: TLine3): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TVector3Hasher"/> and takes the <c>xor</c> of S, D1 and D2.</summary>
  /// <remarks>No further bit rotation is performed, as S, D1 and D2 are usually different from each other anyway.</remarks>
  TPlane3Hasher = class(THasher<TPlane3, TPlane3Equaller>)
  public
    class function GetHash(const AValue: TPlane3): Cardinal; override;
  end;

  /// <summary>Uses <see cref="Pengine.ValueHasher|TSingleHasher"/> and takes the <c>xor</c> of Turn and Pitch.</summary>
  /// <remarks>No further bit rotation is performed, as Turn and Pitch are usually different from each other anyway.</remarks>
  TVectorDirHasher = class(THasher<TVectorDir, TVectorDirEqualler>)
  public
    class function GetHash(const AValue: TVectorDir): Cardinal; override;
  end;

  /// <summary>Calculates a hash using 4 characters in the string and uses them as the 4 bytes in the hash.</summary>
  /// <remarks>The used characters are spreaded equally between the first and last character.<p/>
  /// If the string is shorter than 4 characters, characters will be taken multiple times.</remarks>
  TStringHasher = class(THasher<string, TStringEqualler>)
  public
    class function GetHash(const AValue: string): Cardinal; override;
  end;

  /// <summary>Calculates a hash using 4 characters in the string and uses them as the 4 bytes in the hash.</summary>
  /// <remarks>The used characters are spreaded equally between the first and last character.<p/>
  /// If the string is shorter than 4 characters, characters will be taken multiple times.</remarks>
  TAnsiStringHasher = class(THasher<AnsiString, TAnsiStringEqualler>)
  public
    class function GetHash(const AValue: AnsiString): Cardinal; override;
  end;

  /// <summary>Calulates a hash from any class type.</summary>
  /// <remarks>Internally uses <see cref="Pengine.ValueHasher|TPointerHasher"/>.</remarks>
  TClassHasher = class(THasher<TClass, TClassEqualler>)
  public
    class function GetHash(const AValue: TClass): Cardinal; override;
    class function CanIndex(const AValue: TClass): Boolean; override;
  end;

  TGUIDHasher = class(THasher<TGUID, TGUIDEqualler>)
  public
    class function GetHash(const AValue: TGUID): Cardinal; override;
    class function CanIndex(const AValue: TGUID): Boolean; override;
  end;

  TClassHasher<T> = class(THasher<T, TClassEqualler<T>>)
  public
    class function GetHash(const AValue: T): Cardinal; override;
    class function CanIndex(const AValue: T): Boolean; override;
  end;

  TCharHasher = class(THasher<Char, TCharEqualler>)
  public
    class function GetHash(const AValue: Char): Cardinal; override;
  end;

  // Hashers end, everything else in other units!
  {
  // TODO: XmlDoc
  TClassSet = TValueSet<TClass, TClassHasher>;
  // TODO: XmlDoc
  TClassMap<T> = class(TValueMap<TClass, T, TClassHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TClassMap<T>; reintroduce; inline;
  end;

  TClassRefMap<T: class> = class(TToRefMap<TClass, T, TClassHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TClassRefMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TPointerSet = TValueSet<Pointer, TPointerHasher>;

  // TODO: XmlDoc
  TIntSet = TValueSet<Integer, TIntHasher>;
  // TODO: XmlDoc
  TIntMap<T> = class(TValueMap<Integer, T, TIntHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TUIntSet = TValueSet<Cardinal, TUIntHasher>;
  // TODO: XmlDoc
  TUIntMap<T> = class(TValueMap<Cardinal, T, TUIntHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TUIntMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntVector2Set = TValueSet<TIntVector2, TIntVector2Hasher>;
  // TODO: XmlDoc
  TIntVector2Map<T> = class(TValueMap<TIntVector2, T, TIntVector2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntVector2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntVector3Set = TValueSet<TIntVector3, TIntVector3Hasher>;
  // TODO: XmlDoc
  TIntVector3Map<T> = class(TValueMap<TIntVector3, T, TIntVector3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntVector3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntBounds1Set = TValueSet<TIntBounds1, TIntBounds1Hasher>;
  // TODO: XmlDoc
  TIntBounds1Map<T> = class(TValueMap<TIntBounds1, T, TIntBounds1Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntBounds1Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntBounds2Set = TValueSet<TIntBounds2, TIntBounds2Hasher>;
  // TODO: XmlDoc
  TIntBounds2Map<T> = class(TValueMap<TIntBounds2, T, TIntBounds2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntBounds2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntBounds3Set = TValueSet<TIntBounds3, TIntBounds3Hasher>;
  // TODO: XmlDoc
  TIntBounds3Map<T> = class(TValueMap<TIntBounds3, T, TIntBounds3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntBounds3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TSingleSet = TValueSet<Single, TSingleHasher>;
  // TODO: XmlDoc
  TSingleMap<T> = class(TValueMap<Single, T, TSingleHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TSingleMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TVector2Set = TValueSet<TVector2, TVector2Hasher>;
  // TODO: XmlDoc
  TVector2Map<T> = class(TValueMap<TVector2, T, TVector2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TVector2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TVector3Set = TValueSet<TVector3, TVector3Hasher>;
  // TODO: XmlDoc
  TVector3Map<T> = class(TValueMap<TVector3, T, TVector3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TVector3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TLine2Set = TValueSet<TLine2, TLine2Hasher>;
  // TODO: XmlDoc
  TLine2Map<T> = class(TValueMap<TLine2, T, TLine2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TLine2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TLine3Set = TValueSet<TLine3, TLine3Hasher>;
  // TODO: XmlDoc
  TLine3Map<T> = class(TValueMap<TLine3, T, TLine3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TLine3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TPlane2Set = TValueSet<TPlane2, TPlane2Hasher>;
  // TODO: XmlDoc
  TPlane2Map<T> = class(TValueMap<TPlane2, T, TPlane2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TPlane2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TPlane3Set = TValueSet<TPlane3, TPlane3Hasher>;
  // TODO: XmlDoc
  TPlane3Map<T> = class(TValueMap<TPlane3, T, TPlane3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TPlane3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TVectorDirSet = TValueSet<TVectorDir, TVectorDirHasher>;
  // TODO: XmlDoc
  TVectorDirMap<T> = class(TValueMap<TVectorDir, T, TVectorDirHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TVectorDirMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TStringSet = TValueSet<string, TStringHasher>;
  // TODO: XmlDoc
  TStringMap<T> = class(TValueMap<string, T, TStringHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TStringMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TAnsiStringSet = TValueSet<AnsiString, TAnsiStringHasher>;
  // TODO: XmlDoc
  TAnsiStringMap<T> = class(TValueMap<AnsiString, T, TAnsiStringHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TAnsiStringMap<T>; reintroduce; inline;
  end;
  TAnsiStringRefMap<T: class> = class(TToRefMap<AnsiString, T, TAnsiStringHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TAnsiStringRefMap<T>; reintroduce; inline;
  end;
  }

{ Shorthand overloaded hash functions }

function HashOf(const Value: Pointer): Cardinal; overload; inline;
function HashOf(const Value: Integer): Cardinal; overload; inline;
function HashOf(const Value: Cardinal): Cardinal; overload; inline;
function HashOf(const Value: TIntVector2): Cardinal; overload; inline;
function HashOf(const Value: TIntVector3): Cardinal; overload; inline;
function HashOf(const Value: TIntBounds1): Cardinal; overload; inline;
function HashOf(const Value: TIntBounds2): Cardinal; overload; inline;
function HashOf(const Value: TIntBounds3): Cardinal; overload; inline;
function HashOf(const Value: Single): Cardinal; overload; inline;
function HashOf(const Value: TVector2): Cardinal; overload; inline;
function HashOf(const Value: TVector3): Cardinal; overload; inline;
function HashOf(const Value: TBounds1): Cardinal; overload; inline;
function HashOf(const Value: TBounds2): Cardinal; overload; inline;
function HashOf(const Value: TBounds3): Cardinal; overload; inline;
function HashOf(const Value: string): Cardinal; overload; inline;
function HashOf(const Value: AnsiString): Cardinal; overload; inline;
function HashOf(const Value: TLine2): Cardinal; overload; inline;
function HashOf(const Value: TLine3): Cardinal; overload; inline;
function HashOf(const Value: TPlane3): Cardinal; overload; inline;
function HashOf(const Value: TVectorDir): Cardinal; overload; inline;

implementation

{$IFOPT Q+}{$DEFINE OVERFLOWCHECKSON}{$ENDIF}
{$Q-}

function R(I: Integer; N: Byte): Integer; inline;
begin
  Result := (I shl N) or (I shr (SizeOf(I) * 8 - N));
end;

{$IFDEF OVERFLOWCHECKSON}{$Q+}
{$ENDIF}
           
{ THasher<V, E> }

class function THasher<V>.CanIndex(const AValue: V): Boolean;
begin
  Result := True;
end;
           
class function THasher<V, E>.Equal(const AValue1, AValue2: V): Boolean;
begin
  Result := E.Equal(AValue1, AValue2);
end;

{ TPointerHasher }

class function TPointerHasher.GetHash(const AValue: Pointer): Cardinal;
begin

  {$IFDEF WIN32}

  Result := Cardinal(Pointer(AValue));

  {$ELSE}

  Result := Cardinal(Pointer(AValue)) xor Cardinal(UInt64(Pointer(AValue)) shr 32);

  {$ENDIF}

end;

class function TPointerHasher.CanIndex(const AValue: Pointer): Boolean;
begin
  Result := AValue <> nil;
end;

{ TRefHasher<T> }

class function TRefHasher<V>.GetHash(const AValue: V): Cardinal;
begin
  Result := TPointerHasher.GetHash(Pointer(AValue));
end;

class function TRefHasher<V>.CanIndex(const AValue: V): Boolean;
begin
  Result := TPointerHasher.CanIndex(Pointer(AValue));
end;

{ TUIntHasher }

class function TUIntHasher.GetHash(const AValue: Cardinal): Cardinal;
begin
  Result := AValue;
end;

{ TIntHasher }

class function TIntHasher.GetHash(const AValue: Integer): Cardinal;
begin
  Result := Cardinal(AValue);
end;

{ TIntVector2Hasher }

class function TIntVector2Hasher.GetHash(const AValue: TIntVector2): Cardinal;
begin
  Result := Cardinal(R(AValue.X, 8) xor R(AValue.Y, 24));
end;

{ TIntVector3Hasher }

class function TIntVector3Hasher.GetHash(const AValue: TIntVector3): Cardinal;
begin
  Result := Cardinal(R(AValue.X, 5) xor R(AValue.Y, 16) xor R(AValue.Z, 27));
end;

{ TIntBounds1Hasher }

class function TIntBounds1Hasher.GetHash(const AValue: TIntBounds1): Cardinal;
begin
  Result := Cardinal(AValue.C1) xor Cardinal(AValue.C2);
end;

{ TIntBounds2Hasher }

class function TIntBounds2Hasher.GetHash(const AValue: TIntBounds2): Cardinal;
begin
  Result := HashOf(AValue.C1) xor HashOf(AValue.C2);
end;

{ TIntBounds3Hasher }

class function TIntBounds3Hasher.GetHash(const AValue: TIntBounds3): Cardinal;
begin
  Result := HashOf(AValue.C1) xor HashOf(AValue.C2);
end;

{ TSingleHasher }

class function TSingleHasher.GetHash(const AValue: Single): Cardinal;
begin
  Result := PCardinal(@AValue)^;
end;

{ TVector2Hasher }

class function TVector2Hasher.GetHash(const AValue: TVector2): Cardinal;
begin
  Result := R(HashOf(AValue.X), 8) xor R(HashOf(AValue.Y), 24);
end;

{ TVector3Hasher }

class function TVector3Hasher.GetHash(const AValue: TVector3): Cardinal;
begin
  Result := R(HashOf(AValue.X), 5) xor R(HashOf(AValue.Y), 16) xor R(HashOf(AValue.Z), 27);
end;

{ TBounds1Hasher }

class function TBounds1Hasher.GetHash(const AValue: TBounds1): Cardinal;
begin
  Result := HashOf(AValue.C1) xor HashOf(AValue.C2);
end;

{ TBounds2Hasher }

class function TBounds2Hasher.GetHash(const AValue: TBounds2): Cardinal;
begin
  Result := HashOf(AValue.C1) xor HashOf(AValue.C2);
end;

{ TBounds3Hasher }

class function TBounds3Hasher.GetHash(const AValue: TBounds3): Cardinal;
begin
  Result := HashOf(AValue.C1) xor HashOf(AValue.C2);
end;

{ TLine2Hasher }

class function TLine2Hasher.GetHash(const AValue: TLine2): Cardinal;
begin
  Result := HashOf(AValue.S) xor HashOf(AValue.D);
end;

{ TLine3Hasher }

class function TLine3Hasher.GetHash(const AValue: TLine3): Cardinal;
begin
  Result := HashOf(AValue.S) xor HashOf(AValue.D);
end;

{ TPlane3Hasher }

class function TPlane3Hasher.GetHash(const AValue: TPlane3): Cardinal;
begin
  Result := HashOf(AValue.S) xor HashOf(AValue.DX) xor HashOf(AValue.DY);
end;

{ TVectorDirHasher }

class function TVectorDirHasher.GetHash(const AValue: TVectorDir): Cardinal;
begin
  Result := HashOf(AValue.TurnAngleRad) xor HashOf(AValue.PitchAngleRad);
end;

{ TStringHasher }

class function TStringHasher.GetHash(const AValue: string): Cardinal;
var
  L: Integer;
begin
  L := Length(AValue);
  if L = 0 then
    Exit(0);
  Result :=
    Byte(AValue[L * 0 div 4 + 1]) shl $00 or
    Byte(AValue[L * 1 div 4 + 1]) shl $08 or
    Byte(AValue[L * 2 div 4 + 1]) shl $10 or
    Byte(AValue[L * 3 div 4 + 1]) shl $18;
end;

{ TAnsiStringHasher }

class function TAnsiStringHasher.GetHash(const AValue: AnsiString): Cardinal;
var
  L: Integer;
begin
  L := Length(AValue);
  if L = 0 then
    Exit(0);
  Result :=
    Byte(AValue[L * 0 div 4 + 1]) shl $00 or
    Byte(AValue[L * 1 div 4 + 1]) shl $08 or
    Byte(AValue[L * 2 div 4 + 1]) shl $10 or
    Byte(AValue[L * 3 div 4 + 1]) shl $18;
end;

{ TClassHasher }

class function TClassHasher.GetHash(const AValue: TClass): Cardinal;
begin
  Result := TPointerHasher.GetHash(Pointer(AValue));
end;

class function TClassHasher.CanIndex(const AValue: TClass): Boolean;
begin
  Result := AValue <> nil;
end;

// Wrong unit
(*
{ TUIntMap<T> }

function TUIntMap<T>.Copy(AHashMode: THashMode): TUIntMap<T>;
begin
  Result := TUIntMap<T>(CreateCopy(AHashMode));
end;

{ TIntMap<T> }

function TIntMap<T>.Copy(AHashMode: THashMode): TIntMap<T>;
begin
  Result := TIntMap<T>(CreateCopy(AHashMode));
end;

{ TIntVector2Map<T> }

function TIntVector2Map<T>.Copy(AHashMode: THashMode): TIntVector2Map<T>;
begin
  Result := TIntVector2Map<T>(CreateCopy(AHashMode));
end;

{ TIntVector3Map<T> }

function TIntVector3Map<T>.Copy(AHashMode: THashMode): TIntVector3Map<T>;
begin
  Result := TIntVector3Map<T>(CreateCopy(AHashMode));
end;

{ TIntBounds1Map<T> }

function TIntBounds1Map<T>.Copy(AHashMode: THashMode): TIntBounds1Map<T>;
begin
  Result := TIntBounds1Map<T>(CreateCopy(AHashMode));
end;

{ TIntBounds2Map<T> }

function TIntBounds2Map<T>.Copy(AHashMode: THashMode): TIntBounds2Map<T>;
begin
  Result := TIntBounds2Map<T>(CreateCopy(AHashMode));
end;

{ TIntBounds3Map<T> }

function TIntBounds3Map<T>.Copy(AHashMode: THashMode): TIntBounds3Map<T>;
begin
  Result := TIntBounds3Map<T>(CreateCopy(AHashMode));
end;

{ TSingleMap<T> }

function TSingleMap<T>.Copy(AHashMode: THashMode): TSingleMap<T>;
begin
  Result := TSingleMap<T>(CreateCopy(AHashMode));
end;

{ TVector2Map<T> }

function TVector2Map<T>.Copy(AHashMode: THashMode): TVector2Map<T>;
begin
  Result := TVector2Map<T>(CreateCopy(AHashMode));
end;

{ TVector3Map<T> }

function TVector3Map<T>.Copy(AHashMode: THashMode): TVector3Map<T>;
begin
  Result := TVector3Map<T>(CreateCopy(AHashMode));
end;

{ TLine2Map<T> }

function TLine2Map<T>.Copy(AHashMode: THashMode): TLine2Map<T>;
begin
  Result := TLine2Map<T>(CreateCopy(AHashMode));
end;

{ TLine3Map<T> }

function TLine3Map<T>.Copy(AHashMode: THashMode): TLine3Map<T>;
begin
  Result := TLine3Map<T>(CreateCopy(AHashMode));
end;

{ TPlane2Map<T> }

function TPlane2Map<T>.Copy(AHashMode: THashMode): TPlane2Map<T>;
begin
  Result := TPlane2Map<T>(CreateCopy(AHashMode));
end;

{ TPlane3Map<T> }

function TPlane3Map<T>.Copy(AHashMode: THashMode): TPlane3Map<T>;
begin
  Result := TPlane3Map<T>(CreateCopy(AHashMode));
end;

{ TVectorDirMap<T> }

function TVectorDirMap<T>.Copy(AHashMode: THashMode): TVectorDirMap<T>;
begin
  Result := TVectorDirMap<T>(CreateCopy(AHashMode));
end;

{ TStringMap<T> }

function TStringMap<T>.Copy(AHashMode: THashMode): TStringMap<T>;
begin
  Result := TStringMap<T>(CreateCopy(AHashMode));
end;

{ TAnsiStringMap<T> }

function TAnsiStringMap<T>.Copy(AHashMode: THashMode): TAnsiStringMap<T>;
begin
  Result := TAnsiStringMap<T>(CreateCopy(AHashMode));
end;

{ TClassMap<T> }

function TClassMap<T>.Copy(AHashMode: THashMode): TClassMap<T>;
begin
  Result := TClassMap<T>(CreateCopy(AHashMode));
end;

{ TAnsiStringRefMap<T> }

function TAnsiStringRefMap<T>.Copy(AHashMode: THashMode): TAnsiStringRefMap<T>;
begin
  Result := TAnsiStringRefMap<T>(CreateCopy(AHashMode));
end;

{ TClassToRefMap<T> }

function TClassRefMap<T>.Copy(AHashMode: THashMode): TClassRefMap<T>;
begin
  Result := TClassRefMap<T>(CreateCopy(AHashMode));
end;

*)

{ Shorthand overloaded hash functions }

function HashOf(const Value: Pointer): Cardinal;
begin
  Result := TPointerHasher.GetHash(Value);
end;

function HashOf(const Value: Integer): Cardinal;
begin
  Result := TIntHasher.GetHash(Value);
end;

function HashOf(const Value: Cardinal): Cardinal;
begin
  Result := TUIntHasher.GetHash(Value);
end;

function HashOf(const Value: TIntVector2): Cardinal;
begin
  Result := TIntVector2Hasher.GetHash(Value);
end;

function HashOf(const Value: TIntVector3): Cardinal;
begin
  Result := TIntVector3Hasher.GetHash(Value);
end;

function HashOf(const Value: TIntBounds1): Cardinal;
begin
  Result := TIntBounds1Hasher.GetHash(Value);
end;

function HashOf(const Value: TIntBounds2): Cardinal;
begin
  Result := TIntBounds2Hasher.GetHash(Value);
end;

function HashOf(const Value: TIntBounds3): Cardinal;
begin
  Result := TIntBounds3Hasher.GetHash(Value);
end;

function HashOf(const Value: Single): Cardinal;
begin
  Result := TSingleHasher.GetHash(Value);
end;

function HashOf(const Value: TVector2): Cardinal;
begin
  Result := TVector2Hasher.GetHash(Value);
end;

function HashOf(const Value: TVector3): Cardinal;
begin
  Result := TVector3Hasher.GetHash(Value);
end;

function HashOf(const Value: TBounds1): Cardinal;
begin
  Result := TBounds1Hasher.GetHash(Value);
end;

function HashOf(const Value: TBounds2): Cardinal;
begin
  Result := TBounds2Hasher.GetHash(Value);
end;

function HashOf(const Value: TBounds3): Cardinal;
begin
  Result := TBounds3Hasher.GetHash(Value);
end;

function HashOf(const Value: string): Cardinal;
begin
  Result := TStringHasher.GetHash(Value);
end;

function HashOf(const Value: AnsiString): Cardinal;
begin
  Result := TAnsiStringHasher.GetHash(Value);
end;

function HashOf(const Value: TLine2): Cardinal;
begin
  Result := TLine2Hasher.GetHash(Value);
end;

function HashOf(const Value: TLine3): Cardinal;
begin
  Result := TLine3Hasher.GetHash(Value);
end;

function HashOf(const Value: TPlane3): Cardinal;
begin
  Result := TPlane3Hasher.GetHash(Value);
end;

function HashOf(const Value: TVectorDir): Cardinal;
begin
  Result := TVectorDirHasher.GetHash(Value);
end;

{ TGUIDHasher }

class function TGUIDHasher.CanIndex(const AValue: TGUID): Boolean;
begin
  Result := AValue <> TGUID.Empty;
end;

class function TGUIDHasher.GetHash(const AValue: TGUID): Cardinal;
var
  A: array [0 .. 3] of Cardinal absolute AValue;
begin
  Result := A[0] xor A[1] xor A[2] xor A[3];
end;

{ TClassHasher<T> }

class function TClassHasher<T>.CanIndex(const AValue: T): Boolean;
var
  AsClass: TClass absolute AValue;
begin
  Result := TClassHasher.CanIndex(AsClass);
end;

class function TClassHasher<T>.GetHash(const AValue: T): Cardinal;
var
  AsClass: TClass absolute AValue;
begin
  Result := TClassHasher.GetHash(AsClass);
end;

{ TCharHasher }

class function TCharHasher.GetHash(const AValue: Char): Cardinal;
begin
  Result := Ord(AValue);
end;

end.
