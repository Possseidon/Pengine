unit Pengine.Hasher;

interface

uses
  System.SysUtils,

  Pengine.Collections,
  Pengine.IntMaths,
  Pengine.Vector;

type

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
    function Copy(AAutoRehash: Boolean = True): TIntMap<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TUIntMap<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TIntVector2Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TIntVector3Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TIntBounds1Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TIntBounds2Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TIntBounds3Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TSingleMap<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TVector2Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TVector3Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TLine2Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TLine3Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TPlane2Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TPlane3Map<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TVectorDirMap<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TStringMap<T>; reintroduce; inline;
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
    function Copy(AAutoRehash: Boolean = True): TAnsiStringMap<T>; reintroduce; inline;
  end;
  // TODO: XmlDoc
  TAnsiStringObjectMap<T: class> = class(TAnsiStringMap<T>)
  protected
    function CreateBucket: THashBase.TBucket; override;
    function CreateCopy(AAutoRehash: Boolean = True): THashBase; override;
  end;

  // TODO: XmlDoc
  TRefHasher<T: class> = class(TValueHasher<T>)
  public
    class function GetHash(AKey: T): Cardinal; override;
    class function KeysEqual(AKey1, AKey2: T): Boolean; override;
    class function CanIndex(AKey: T): Boolean; override;
  end;

  // TODO: XmlDoc
  TRefSet<T: class> = class(TValueSet<T, TRefHasher<T>>)
  public
    function Copy(AAutoRehash: Boolean = True): TRefSet<T>; reintroduce; inline;
  end;
  // TODO: XmlDoc
  TRefMap<K: class; V> = class(TValueMap<K, V, TRefHasher<K>>)
  public
    function Copy(AAutoRehash: Boolean = True): TRefMap<K, V>; reintroduce; inline;
  end;
  // TODO: XmlDoc
  TRefRefMap<K, V: class> = class(TRefMap<K, V>)
  public
    function Copy(AAutoRehash: Boolean = True): TRefRefMap<K, V>; reintroduce; inline;
  end;

  // TODO: XmlDoc
  TObjectSet<T: class> = class(TRefSet<T>)
  protected
    function CreateBucket: THashBase.TBucket; override;
    function CreateCopy(AAutoRehash: Boolean): THashBase; override;
  end;
  // TODO: XmlDoc
  TObjectMap<K: class; V> = class(TRefMap<K, V>)
  protected
    function CreateBucket: THashBase.TBucket; override;
    function CreateCopy(AAutoRehash: Boolean): THashBase; override;
  end;
  // TODO: XmlDoc
  TObjectRefMap<K, V: class> = class(TObjectMap<K, V>)
  protected
    function CreateCopy(AAutoRehash: Boolean): THashBase; override;
  end;

  // TODO: XmlDoc
  TObjectObjectMap<K, V: class> = class(TObjectRefMap<K, V>)
  protected
    function CreateBucket: THashBase.TBucket; override;
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
    function Copy(AAutoRehash: Boolean = True): TClassMap<T>; reintroduce; inline;
  end;

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
  Result := TIntVector2Hasher.GetHash(AKey.C1) xor TIntVector2Hasher.GetHash(AKey.C2);
end;

class function TIntBounds2Hasher.KeysEqual(AKey1, AKey2: TIntBounds2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TIntBounds3Hasher }

class function TIntBounds3Hasher.GetHash(AKey: TIntBounds3): Cardinal;
begin
  Result := TIntVector3Hasher.GetHash(AKey.C1) xor TIntVector3Hasher.GetHash(AKey.C2);
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
  Result := TSingleHasher.GetHash(AKey.X) xor TSingleHasher.GetHash(AKey.Y);
end;

class function TVector2Hasher.KeysEqual(AKey1, AKey2: TVector2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TVector3Hasher }

class function TVector3Hasher.GetHash(AKey: TVector3): Cardinal;
begin
  Result := TSingleHasher.GetHash(AKey.X) xor TSingleHasher.GetHash(AKey.Y) xor TSingleHasher.GetHash(AKey.Z);
end;

class function TVector3Hasher.KeysEqual(AKey1, AKey2: TVector3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TLine2Hasher }

class function TLine2Hasher.GetHash(AKey: TLine2): Cardinal;
begin
  Result := TVector2Hasher.GetHash(AKey.S) xor TVector2Hasher.GetHash(AKey.D);
end;

class function TLine2Hasher.KeysEqual(AKey1, AKey2: TLine2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TLine3Hasher }

class function TLine3Hasher.GetHash(AKey: TLine3): Cardinal;
begin
  Result := TVector3Hasher.GetHash(AKey.S) xor TVector3Hasher.GetHash(AKey.D);
end;

class function TLine3Hasher.KeysEqual(AKey1, AKey2: TLine3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TPlane2Hasher }

class function TPlane2Hasher.GetHash(AKey: TPlane2): Cardinal;
begin
  Result := TVector2Hasher.GetHash(AKey.S) xor TVector2Hasher.GetHash(AKey.D1) xor TVector2Hasher.GetHash(AKey.D2);
end;

class function TPlane2Hasher.KeysEqual(AKey1, AKey2: TPlane2): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TPlane3Hasher }

class function TPlane3Hasher.GetHash(AKey: TPlane3): Cardinal;
begin
  Result := TVector3Hasher.GetHash(AKey.S) xor TVector3Hasher.GetHash(AKey.D1) xor TVector3Hasher.GetHash(AKey.D2);
end;

class function TPlane3Hasher.KeysEqual(AKey1, AKey2: TPlane3): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TVectorDirHasher }

class function TVectorDirHasher.GetHash(AKey: TVectorDir): Cardinal;
begin
  Result := TSingleHasher.GetHash(AKey.TurnAngleRad) xor TSingleHasher.GetHash(AKey.PitchAngleRad);
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

class function TRefHasher<T>.GetHash(AKey: T): Cardinal;
begin

  {$IFDEF WIN32}

  Result := Cardinal(Pointer(AKey));

  {$ELSE}

  Result := Cardinal(Pointer(AKey)) xor Cardinal(NativeUInt(Pointer(AKey)) shr 32);

  {$ENDIF}

end;

class function TRefHasher<T>.KeysEqual(AKey1, AKey2: T): Boolean;
begin
  Result := AKey1 = AKey2;
end;

{ TRefHasher<T> }

class function TRefHasher<T>.CanIndex(AKey: T): Boolean;
begin
  Result := AKey <> nil;
end;

{ TObjectSet<T> }

function TObjectSet<T>.CreateCopy(AAutoRehash: Boolean): THashBase;
begin
  Result := TRefSet<T>.Create(AAutoRehash);
  CopyTo(Result);
end;

function TObjectSet<T>.CreateBucket: THashBase.TBucket;
begin
  Result := TObjectArray<T>.Create(4, 2);
end;

{ TObjectMap<K, V> }

function TObjectMap<K, V>.CreateBucket: THashBase.TBucket;
begin
  Result := TObjectPairArray<K, V>.Create(4, 2);
end;

function TObjectMap<K, V>.CreateCopy(AAutoRehash: Boolean): THashBase;
begin
  Result := TRefMap<K, V>.Create(AAutoRehash);
  CopyTo(Result);
end;

{ TIntMap<T> }

function TIntMap<T>.Copy(AAutoRehash: Boolean): TIntMap<T>;
begin
  Result := TIntMap<T>(CreateCopy(AAutoRehash));
end;

{ TUIntMap<T> }

function TUIntMap<T>.Copy(AAutoRehash: Boolean): TUIntMap<T>;
begin
  Result := TUIntMap<T>(CreateCopy(AAutoRehash));
end;

{ TIntVector2Map<T> }

function TIntVector2Map<T>.Copy(AAutoRehash: Boolean): TIntVector2Map<T>;
begin
  Result := TIntVector2Map<T>(CreateCopy(AAutoRehash));
end;

{ TIntVector3Map<T> }

function TIntVector3Map<T>.Copy(AAutoRehash: Boolean): TIntVector3Map<T>;
begin
  Result := TIntVector3Map<T>(CreateCopy(AAutoRehash));
end;

{ TIntBounds1Map<T> }

function TIntBounds1Map<T>.Copy(AAutoRehash: Boolean): TIntBounds1Map<T>;
begin
  Result := TIntBounds1Map<T>(CreateCopy(AAutoRehash));
end;

{ TIntBounds2Map<T> }

function TIntBounds2Map<T>.Copy(AAutoRehash: Boolean): TIntBounds2Map<T>;
begin
  Result := TIntBounds2Map<T>(CreateCopy(AAutoRehash));
end;

{ TIntBounds3Map<T> }

function TIntBounds3Map<T>.Copy(AAutoRehash: Boolean): TIntBounds3Map<T>;
begin
  Result := TIntBounds3Map<T>(CreateCopy(AAutoRehash));
end;

{ TSingleMap<T> }

function TSingleMap<T>.Copy(AAutoRehash: Boolean): TSingleMap<T>;
begin
  Result := TSingleMap<T>(CreateCopy(AAutoRehash));
end;

{ TVector2Map<T> }

function TVector2Map<T>.Copy(AAutoRehash: Boolean): TVector2Map<T>;
begin
  Result := TVector2Map<T>(CreateCopy(AAutoRehash));
end;

{ TVector3Map<T> }

function TVector3Map<T>.Copy(AAutoRehash: Boolean): TVector3Map<T>;
begin
  Result := TVector3Map<T>(CreateCopy(AAutoRehash));
end;

{ TLine2Map<T> }

function TLine2Map<T>.Copy(AAutoRehash: Boolean): TLine2Map<T>;
begin
  Result := TLine2Map<T>(CreateCopy(AAutoRehash));
end;

{ TLine3Map<T> }

function TLine3Map<T>.Copy(AAutoRehash: Boolean): TLine3Map<T>;
begin
  Result := TLine3Map<T>(CreateCopy(AAutoRehash));
end;

{ TPlane2Map<T> }

function TPlane2Map<T>.Copy(AAutoRehash: Boolean): TPlane2Map<T>;
begin
  Result := TPlane2Map<T>(CreateCopy(AAutoRehash));
end;

{ TPlane3Map<T> }

function TPlane3Map<T>.Copy(AAutoRehash: Boolean): TPlane3Map<T>;
begin
  Result := TPlane3Map<T>(CreateCopy(AAutoRehash));
end;

{ TVectorDirMap<T> }

function TVectorDirMap<T>.Copy(AAutoRehash: Boolean): TVectorDirMap<T>;
begin
  Result := TVectorDirMap<T>(CreateCopy(AAutoRehash));
end;

{ TStringMap<T> }

function TStringMap<T>.Copy(AAutoRehash: Boolean): TStringMap<T>;
begin
  Result := TStringMap<T>(CreateCopy(AAutoRehash));
end;

{ TAnsiStringMap<T> }

function TAnsiStringMap<T>.Copy(AAutoRehash: Boolean): TAnsiStringMap<T>;
begin
  Result := TAnsiStringMap<T>(CreateCopy(AAutoRehash));
end;

{ TRefSet<T> }

function TRefSet<T>.Copy(AAutoRehash: Boolean): TRefSet<T>;
begin
  Result := TRefSet<T>(CreateCopy(AAutoRehash));
end;

{ TRefMap<K, V> }

function TRefMap<K, V>.Copy(AAutoRehash: Boolean): TRefMap<K, V>;
begin
  Result := TRefMap<K, V>(CreateCopy(AAutoRehash));
end;

{ TAnsiStringObjectMap<T> }

function TAnsiStringObjectMap<T>.CreateBucket: THashBase.TBucket;
begin
   Result := TToObjectPairArray<AnsiString, T>.Create(4, 2);
end;

function TAnsiStringObjectMap<T>.CreateCopy(AAutoRehash: Boolean): THashBase;
begin
  Result := TAnsiStringMap<T>.Create(AAutoRehash);
  CopyTo(Result);
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

function TClassMap<T>.Copy(AAutoRehash: Boolean): TClassMap<T>;
begin
  Result := TClassMap<T>(CreateCopy(AAutoRehash));
end;

{ TObjectObjectMap<K, V> }

function TObjectObjectMap<K, V>.CreateBucket: THashBase.TBucket;
begin
  Result := TObjectObjectPairArray<K, V>.Create(4, 2);
end;

{ TRefRefMap<K, V> }

function TRefRefMap<K, V>.Copy(AAutoRehash: Boolean): TRefRefMap<K, V>;
begin
  Result := TRefRefMap<K, V>(CreateCopy(AAutoRehash));
end;

{ TObjectRefMap<K, V> }

function TObjectRefMap<K, V>.CreateCopy(AAutoRehash: Boolean): THashBase;
begin
  Result := TRefRefMap<K, V>.Create(AAutoRehash);
  CopyTo(Result);
end;

end.
