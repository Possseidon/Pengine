unit Pengine.Hasher;

interface

uses
  System.SysUtils,

  Pengine.Collections,
  Pengine.IntMaths,
  Pengine.Vector;

type

  // TODO: XmlDoc
  TPointerHasher = class(TValueHasher<Pointer>)
  public
    class function GetHash(AKey: Pointer): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: Pointer): Boolean; override;
    class function CanIndex(AKey: Pointer): Boolean; override;
  end;

  // TODO: XmlDoc
  TPointerSet = TValueSet<Pointer, TPointerHasher>;

  // TODO: XmlDoc
  TRefHasher<T: class> = class(TValueHasher<T>)
  public
    class function GetHash(AKey: T): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: T): Boolean; override;
    class function CanIndex(AKey: T): Boolean; override;
  end;

  // TODO: XmlDoc
  TRefSet<T: class> = class(TValueSet<T, TRefHasher<T>>)
  private
    FOwnsObjects: Boolean;
    FStoredOwnsObjects: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;
    
  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsObjects: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TRefSet<T>; reintroduce; inline;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;

  end;
  // TODO: XmlDoc
  TRefMap<K: class; V> = class(TValueMap<K, V, TRefHasher<K>>)
  private
    FOwnsKeys: Boolean;
    FStoredOwnsKeys: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsKeys: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TRefMap<K, V>; reintroduce; inline;

    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

  end;
  // TODO: XmlDoc
  TToRefMap<K; V: class; H: TValueHasher<K>> = class abstract(TValueMap<K, V, H>)
  private
    FOwnsValues: Boolean;
    FStoredOwnsValues: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsValues: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TToRefMap<K, V, H>; reintroduce; inline;

    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;

  end;
  // TODO: XmlDoc
  TRefRefMap<K, V: class> = class(TRefMap<K, V>)
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    FStoredOwnsKeys: Boolean;
    FStoredOwnsValues: Boolean;

  protected
    function CreateBucket: THashBase.TBucket; override;

    procedure UnownObjects; override;
    procedure ReownObjects; override;

  public
    constructor Create(AHashMode: THashMode = hmAuto); overload; override;
    constructor Create(AOwnsObjects: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;
    constructor Create(AOwnsKeys, AOwnsValues: Boolean; AHashMode: THashMode = hmAuto); reintroduce; overload;

    function Copy(AHashMode: THashMode = hmAuto): TRefRefMap<K, V>; reintroduce; inline;

    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;

  end;

  // TODO: XmlDoc
  TIntHasher = class(TValueHasher<Integer>)
  public
    class function GetHash(AKey: Integer): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: Integer): Boolean; override;
  end;

  // TODO: XmlDoc
  TIntSet = TValueSet<Integer, TIntHasher>;
  // TODO: XmlDoc
  TIntMap<T> = class(TValueMap<Integer, T, TIntHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TUIntHasher = class(TValueHasher<Cardinal>)
  public
    class function GetHash(AKey: Cardinal): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: Cardinal): Boolean; override;
  end;

  // TODO: XmlDoc
  TUIntSet = TValueSet<Cardinal, TUIntHasher>;
  // TODO: XmlDoc
  TUIntMap<T> = class(TValueMap<Cardinal, T, TUIntHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TUIntMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntVector2Hasher = class(TValueHasher<TIntVector2>)
  public
    class function GetHash(AKey: TIntVector2): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TIntVector2): Boolean; override;
  end;

  // TODO: XmlDoc
  TIntVector2Set = TValueSet<TIntVector2, TIntVector2Hasher>;
  // TODO: XmlDoc
  TIntVector2Map<T> = class(TValueMap<TIntVector2, T, TIntVector2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntVector2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntVector3Hasher = class(TValueHasher<TIntVector3>)
  public
    class function GetHash(AKey: TIntVector3): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TIntVector3): Boolean; override;
  end;

  // TODO: XmlDoc
  TIntVector3Set = TValueSet<TIntVector3, TIntVector3Hasher>;
  // TODO: XmlDoc
  TIntVector3Map<T> = class(TValueMap<TIntVector3, T, TIntVector3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntVector3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntBounds1Hasher = class(TValueHasher<TIntBounds1>)
  public
    class function GetHash(AKey: TIntBounds1): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TIntBounds1): Boolean; override;
  end;

  // TODO: XmlDoc
  TIntBounds1Set = TValueSet<TIntBounds1, TIntBounds1Hasher>;
  // TODO: XmlDoc
  TIntBounds1Map<T> = class(TValueMap<TIntBounds1, T, TIntBounds1Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntBounds1Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntBounds2Hasher = class(TValueHasher<TIntBounds2>)
  public
    class function GetHash(AKey: TIntBounds2): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TIntBounds2): Boolean; override;
  end;

  // TODO: XmlDoc
  TIntBounds2Set = TValueSet<TIntBounds2, TIntBounds2Hasher>;
  // TODO: XmlDoc
  TIntBounds2Map<T> = class(TValueMap<TIntBounds2, T, TIntBounds2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntBounds2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TIntBounds3Hasher = class(TValueHasher<TIntBounds3>)
  public
    class function GetHash(AKey: TIntBounds3): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TIntBounds3): Boolean; override;
  end;

  // TODO: XmlDoc
  TIntBounds3Set = TValueSet<TIntBounds3, TIntBounds3Hasher>;
  // TODO: XmlDoc
  TIntBounds3Map<T> = class(TValueMap<TIntBounds3, T, TIntBounds3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TIntBounds3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TSingleHasher = class(TValueHasher<Single>)
  public
    class function GetHash(AKey: Single): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: Single): Boolean; override;
  end;

  // TODO: XmlDoc
  TSingleSet = TValueSet<Single, TSingleHasher>;
  // TODO: XmlDoc
  TSingleMap<T> = class(TValueMap<Single, T, TSingleHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TSingleMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TVector2Hasher = class(TValueHasher<TVector2>)
  public
    class function GetHash(AKey: TVector2): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TVector2): Boolean; override;
  end;

  // TODO: XmlDoc
  TVector2Set = TValueSet<TVector2, TVector2Hasher>;
  // TODO: XmlDoc
  TVector2Map<T> = class(TValueMap<TVector2, T, TVector2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TVector2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TVector3Hasher = class(TValueHasher<TVector3>)
  public
    class function GetHash(AKey: TVector3): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TVector3): Boolean; override;
  end;

  // TODO: XmlDoc
  TVector3Set = TValueSet<TVector3, TVector3Hasher>;
  // TODO: XmlDoc
  TVector3Map<T> = class(TValueMap<TVector3, T, TVector3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TVector3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TBounds1Hasher = class(TValueHasher<TBounds1>)
  public
    class function GetHash(AKey: TBounds1): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TBounds1): Boolean; override;
  end;

  // TODO: XmlDoc
  TBounds2Hasher = class(TValueHasher<TBounds2>)
  public
    class function GetHash(AKey: TBounds2): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TBounds2): Boolean; override;
  end;

  // TODO: XmlDoc
  TBounds3Hasher = class(TValueHasher<TBounds3>)
  public
    class function GetHash(AKey: TBounds3): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TBounds3): Boolean; override;
  end;

  // TODO: XmlDoc
  TLine2Hasher = class(TValueHasher<TLine2>)
  public
    class function GetHash(AKey: TLine2): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TLine2): Boolean; override;
  end;

  // TODO: XmlDoc
  TLine2Set = TValueSet<TLine2, TLine2Hasher>;
  // TODO: XmlDoc
  TLine2Map<T> = class(TValueMap<TLine2, T, TLine2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TLine2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TLine3Hasher = class(TValueHasher<TLine3>)
  public
    class function GetHash(AKey: TLine3): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TLine3): Boolean; override;
  end;

  // TODO: XmlDoc
  TLine3Set = TValueSet<TLine3, TLine3Hasher>;
  // TODO: XmlDoc
  TLine3Map<T> = class(TValueMap<TLine3, T, TLine3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TLine3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TPlane2Hasher = class(TValueHasher<TPlane2>)
  public
    class function GetHash(AKey: TPlane2): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TPlane2): Boolean; override;
  end;

  // TODO: XmlDoc
  TPlane2Set = TValueSet<TPlane2, TPlane2Hasher>;
  // TODO: XmlDoc
  TPlane2Map<T> = class(TValueMap<TPlane2, T, TPlane2Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TPlane2Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TPlane3Hasher = class(TValueHasher<TPlane3>)
  public
    class function GetHash(AKey: TPlane3): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TPlane3): Boolean; override;
  end;

  // TODO: XmlDoc
  TPlane3Set = TValueSet<TPlane3, TPlane3Hasher>;
  // TODO: XmlDoc
  TPlane3Map<T> = class(TValueMap<TPlane3, T, TPlane3Hasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TPlane3Map<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TVectorDirHasher = class(TValueHasher<TVectorDir>)
  public
    class function GetHash(AKey: TVectorDir): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TVectorDir): Boolean; override;
  end;

  // TODO: XmlDoc
  TVectorDirSet = TValueSet<TVectorDir, TVectorDirHasher>;
  // TODO: XmlDoc
  TVectorDirMap<T> = class(TValueMap<TVectorDir, T, TVectorDirHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TVectorDirMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TStringHasher = class(TValueHasher<string>)
  public
    class function GetHash(AKey: string): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: string): Boolean; override;
    class function CanIndex(AKey: string): Boolean; override;
  end;

  // TODO: XmlDoc
  TStringSet = TValueSet<string, TStringHasher>;
  // TODO: XmlDoc
  TStringMap<T> = class(TValueMap<string, T, TStringHasher>)
  public
    function Copy(AHashMode: THashMode = hmAuto): TStringMap<T>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TAnsiStringHasher = class(TValueHasher<AnsiString>)
  public
    class function GetHash(AKey: AnsiString): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: AnsiString): Boolean; override;
    class function CanIndex(AKey: AnsiString): Boolean; override;
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

  // TODO: XmlDoc
  TClassHasher = class(TValueHasher<TClass>)
  public
    class function GetHash(AKey: TClass): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: TClass): Boolean; override;
    class function CanIndex(AKey: TClass): Boolean; override;
  end;

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
function HashOf(const Value: TPlane2): Cardinal; overload; inline;
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

{ TIntHasher }

class function TIntHasher.GetHash(AKey: Integer): Cardinal;
begin
  Result := Cardinal(R(AKey, 16));
end;

class function TIntHasher.KeysEqual(AKey1, AKey2: Integer): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TUIntHasher }

class function TUIntHasher.GetHash(AKey: Cardinal): Cardinal;
begin
  Result := R(AKey, 16);
end;

class function TUIntHasher.KeysEqual(AKey1, AKey2: Cardinal): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TIntVector2Hasher }

class function TIntVector2Hasher.GetHash(AKey: TIntVector2): Cardinal;
begin
  Result := Cardinal(R(AKey.X, 8) xor R(AKey.Y, 24));
end;

class function TIntVector2Hasher.KeysEqual(AKey1, AKey2: TIntVector2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TIntVector3Hasher }

class function TIntVector3Hasher.GetHash(AKey: TIntVector3): Cardinal;
begin
  Result := Cardinal(R(AKey.X, 5) xor R(AKey.Y, 16) xor R(AKey.Z, 27));
end;

class function TIntVector3Hasher.KeysEqual(AKey1, AKey2: TIntVector3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TIntBounds1Hasher }

class function TIntBounds1Hasher.GetHash(AKey: TIntBounds1): Cardinal;
begin
  Result := Cardinal(R(AKey.C1, 8) xor R(AKey.C2, 24));
end;

class function TIntBounds1Hasher.KeysEqual(AKey1, AKey2: TIntBounds1): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TIntBounds2Hasher }

class function TIntBounds2Hasher.GetHash(AKey: TIntBounds2): Cardinal;
begin
  Result := HashOf(AKey.C1) xor HashOf(AKey.C2);
end;

class function TIntBounds2Hasher.KeysEqual(AKey1, AKey2: TIntBounds2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TIntBounds3Hasher }

class function TIntBounds3Hasher.GetHash(AKey: TIntBounds3): Cardinal;
begin
  Result := HashOf(AKey.C1) xor HashOf(AKey.C2);
end;

class function TIntBounds3Hasher.KeysEqual(AKey1, AKey2: TIntBounds3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TSingleHasher }

class function TSingleHasher.GetHash(AKey: Single): Cardinal;
begin
  Result := PCardinal(@AKey)^;
end;

class function TSingleHasher.KeysEqual(AKey1, AKey2: Single): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TVector2Hasher }

class function TVector2Hasher.GetHash(AKey: TVector2): Cardinal;
begin
  Result := HashOf(AKey.X) xor HashOf(AKey.Y);
end;

class function TVector2Hasher.KeysEqual(AKey1, AKey2: TVector2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TVector3Hasher }

class function TVector3Hasher.GetHash(AKey: TVector3): Cardinal;
begin
  Result := HashOf(AKey.X) xor HashOf(AKey.Y) xor HashOf(AKey.Z);
end;

class function TVector3Hasher.KeysEqual(AKey1, AKey2: TVector3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TLine2Hasher }

class function TLine2Hasher.GetHash(AKey: TLine2): Cardinal;
begin
  Result := HashOf(AKey.S) xor HashOf(AKey.D);
end;

class function TLine2Hasher.KeysEqual(AKey1, AKey2: TLine2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TLine3Hasher }

class function TLine3Hasher.GetHash(AKey: TLine3): Cardinal;
begin
  Result := HashOf(AKey.S) xor HashOf(AKey.D);
end;

class function TLine3Hasher.KeysEqual(AKey1, AKey2: TLine3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TPlane2Hasher }

class function TPlane2Hasher.GetHash(AKey: TPlane2): Cardinal;
begin
  Result := HashOf(AKey.S) xor HashOf(AKey.D1) xor HashOf(AKey.D2);
end;

class function TPlane2Hasher.KeysEqual(AKey1, AKey2: TPlane2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TPlane3Hasher }

class function TPlane3Hasher.GetHash(AKey: TPlane3): Cardinal;
begin
  Result := HashOf(AKey.S) xor HashOf(AKey.D1) xor HashOf(AKey.D2);
end;

class function TPlane3Hasher.KeysEqual(AKey1, AKey2: TPlane3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TVectorDirHasher }

class function TVectorDirHasher.GetHash(AKey: TVectorDir): Cardinal;
begin
  Result := HashOf(AKey.TurnAngleRad) xor HashOf(AKey.PitchAngleRad);
end;

class function TVectorDirHasher.KeysEqual(AKey1, AKey2: TVectorDir): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TStringHasher }

class function TStringHasher.GetHash(AKey: string): Cardinal;
var
  L: Integer;
begin
  L := Length(AKey);
  Result :=
    Byte(AKey[L * 0 div 4 + 1]) shl $00 or
    Byte(AKey[L * 1 div 4 + 1]) shl $08 or
    Byte(AKey[L * 2 div 4 + 1]) shl $10 or
    Byte(AKey[L * 3 div 4 + 1]) shl $18;
end;

class function TStringHasher.KeysEqual(AKey1, AKey2: string): Boolean;
begin
  Result := AKey1 = AKey2;
end;

class function TStringHasher.CanIndex(AKey: string): Boolean;
begin
  Result := AKey <> '';
end;

{ TAnsiStringHasher }

class function TAnsiStringHasher.GetHash(AKey: AnsiString): Cardinal;
var
  L: Integer;
begin
  L := Length(AKey);
  Result :=
    Byte(AKey[L * 0 div 4 + 1]) shl $00 or
    Byte(AKey[L * 1 div 4 + 1]) shl $08 or
    Byte(AKey[L * 2 div 4 + 1]) shl $10 or
    Byte(AKey[L * 3 div 4 + 1]) shl $18;
end;

class function TAnsiStringHasher.KeysEqual(AKey1, AKey2: AnsiString): Boolean;
begin
  Result := AKey1 = AKey2;
end;

class function TAnsiStringHasher.CanIndex(AKey: AnsiString): Boolean;
begin
  Result := AKey <> '';
end;

{ TRefHasher<T> }

class function TRefHasher<T>.GetHash(AKey: T): Cardinal;
begin
  Result := TPointerHasher.GetHash(Pointer(AKey));
end;

class function TRefHasher<T>.KeysEqual(AKey1, AKey2: T): Boolean;
begin
  Result := TPointerHasher.KeysEqual(Pointer(AKey1), Pointer(AKey2));
end;

class function TRefHasher<T>.CanIndex(AKey: T): Boolean;
begin
  Result := TPointerHasher.CanIndex(Pointer(AKey));
end;

{ TIntMap<T> }

function TIntMap<T>.Copy(AHashMode: THashMode): TIntMap<T>;
begin
  Result := TIntMap<T>(CreateCopy(AHashMode));
end;

{ TUIntMap<T> }

function TUIntMap<T>.Copy(AHashMode: THashMode): TUIntMap<T>;
begin
  Result := TUIntMap<T>(CreateCopy(AHashMode));
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

function TRefSet<T>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefArrayOwnLinked<T>.Create(@FOwnsObjects, 4, 2);
end;

procedure TRefSet<T>.UnownObjects;
begin
  FStoredOwnsObjects := OwnsObjects;
  OwnsObjects := False;
end;

procedure TRefSet<T>.ReownObjects;
begin
  OwnsObjects := FStoredOwnsObjects;
end;

constructor TRefSet<T>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TRefSet<T>.Create(AOwnsObjects: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsObjects := AOwnsObjects;
end;

{ TRefSet<T> }

function TRefSet<T>.Copy(AHashMode: THashMode): TRefSet<T>;
begin
  Result := TRefSet<T>(CreateCopy(AHashMode));
end;

function TRefMap<K, V>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefPairArrayOwnLinked<K, V>.Create(@FOwnsKeys, 4, 2);
end;

procedure TRefMap<K, V>.UnownObjects;
begin
  FStoredOwnsKeys := OwnsKeys;
  OwnsKeys := False;
end;

procedure TRefMap<K, V>.ReownObjects;
begin
  OwnsKeys := FStoredOwnsKeys;
end;

constructor TRefMap<K, V>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TRefMap<K, V>.Create(AOwnsKeys: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsKeys := AOwnsKeys;
end;

{ TRefMap<K, V> }

function TRefMap<K, V>.Copy(AHashMode: THashMode): TRefMap<K, V>;
begin
  Result := TRefMap<K, V>(CreateCopy(AHashMode));
end;

{ TClassHasher }

class function TClassHasher.GetHash(AKey: TClass): Cardinal;
begin

  {$IFDEF WIN32}

  Result := Cardinal(Pointer(AKey));

  {$ELSE}

  Result := Cardinal(Pointer(AKey)) xor Cardinal(NativeUInt(Pointer(AKey)) shr 32);

  {$ENDIF}

end;

class function TClassHasher.KeysEqual(AKey1, AKey2: TClass): Boolean;
begin
  Result := AKey1 = AKey2;
end;

class function TClassHasher.CanIndex(AKey: TClass): Boolean;
begin
  Result := AKey <> nil;
end;

{ TClassMap<T> }

function TClassMap<T>.Copy(AHashMode: THashMode): TClassMap<T>;
begin
  Result := TClassMap<T>(CreateCopy(AHashMode));
end;

function TRefRefMap<K, V>.CreateBucket: THashBase.TBucket;
begin
  Result := TRefRefPairArrayOwnLinked<K, V>.Create(@FOwnsKeys, @FOwnsValues, 4, 2);
end;

procedure TRefRefMap<K, V>.UnownObjects;
begin
  FStoredOwnsKeys := OwnsKeys;
  FStoredOwnsValues := OwnsValues;
  OwnsKeys := False;
  OwnsValues := False;
end;

procedure TRefRefMap<K, V>.ReownObjects;
begin
  OwnsKeys := FStoredOwnsKeys;
  OwnsValues := FStoredOwnsValues;
end;

constructor TRefRefMap<K, V>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TRefRefMap<K, V>.Create(AOwnsObjects: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsKeys := AOwnsObjects;
  OwnsValues := AOwnsObjects;
end;

constructor TRefRefMap<K, V>.Create(AOwnsKeys, AOwnsValues: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsKeys := AOwnsKeys;
  OwnsValues := AOwnsValues;
end;

{ TRefRefMap<K, V> }

function TRefRefMap<K, V>.Copy(AHashMode: THashMode): TRefRefMap<K, V>;
begin
  Result := TRefRefMap<K, V>(CreateCopy(AHashMode));
end;

function TToRefMap<K, V, H>.CreateBucket: THashBase.TBucket;
begin
  Result := TToRefPairArrayOwnLinked<K, V>.Create(@FOwnsValues, 4, 2);
end;

procedure TToRefMap<K, V, H>.UnownObjects;
begin
  FStoredOwnsValues := OwnsValues;
  OwnsValues := False;
end;

procedure TToRefMap<K, V, H>.ReownObjects;
begin
  OwnsValues := FStoredOwnsValues;
end;

constructor TToRefMap<K, V, H>.Create(AHashMode: THashMode);
begin
  inherited;
end;

constructor TToRefMap<K, V, H>.Create(AOwnsValues: Boolean; AHashMode: THashMode);
begin
  inherited Create(AHashMode);
  OwnsValues := AOwnsValues;
end;

{ TToRefMap<K, V, H> }

function TToRefMap<K, V, H>.Copy(AHashMode: THashMode): TToRefMap<K, V, H>;
begin
  Result := TToRefMap<K, V, H>(CreateCopy(AHashMode));
end;

{ TAnsiStringRefMap<T> }

function TAnsiStringRefMap<T>.Copy(AHashMode: THashMode): TAnsiStringRefMap<T>;
begin
  Result := TAnsiStringRefMap<T>(CreateCopy(AHashMode));
end;

{ TPointerHasher }

class function TPointerHasher.CanIndex(AKey: Pointer): Boolean;
begin
  Result := AKey <> nil;
end;

class function TPointerHasher.GetHash(AKey: Pointer): Cardinal;
begin

  {$IFDEF WIN32}

  Result := Cardinal(Pointer(AKey));

  {$ELSE}

  Result := Cardinal(Pointer(AKey)) xor Cardinal(NativeUInt(Pointer(AKey)) shr 32);

  {$ENDIF}

end;

class function TPointerHasher.KeysEqual(AKey1, AKey2: Pointer): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TClassToRefMap<T> }

function TClassRefMap<T>.Copy(AHashMode: THashMode): TClassRefMap<T>;
begin
  Result := TClassRefMap<T>(CreateCopy(AHashMode));
end;

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

function HashOf(const Value: TPlane2): Cardinal;
begin
  Result := TPlane2Hasher.GetHash(Value);
end;

function HashOf(const Value: TPlane3): Cardinal;
begin
  Result := TPlane3Hasher.GetHash(Value);
end;

function HashOf(const Value: TVectorDir): Cardinal;
begin
  Result := TVectorDirHasher.GetHash(Value);
end;

{ TBounds1Hasher }

class function TBounds1Hasher.GetHash(AKey: TBounds1): Cardinal;
begin
  Result := HashOf(AKey.C1) xor HashOf(AKey.C2);
end;

class function TBounds1Hasher.KeysEqual(AKey1, AKey2: TBounds1): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TBounds2Hasher }

class function TBounds2Hasher.GetHash(AKey: TBounds2): Cardinal;
begin
  Result := HashOf(AKey.C1) xor HashOf(AKey.C2);
end;

class function TBounds2Hasher.KeysEqual(AKey1, AKey2: TBounds2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TBounds3Hasher }

class function TBounds3Hasher.GetHash(AKey: TBounds3): Cardinal;
begin
  Result := HashOf(AKey.C1) xor HashOf(AKey.C2);
end;

class function TBounds3Hasher.KeysEqual(AKey1, AKey2: TBounds3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

end.
