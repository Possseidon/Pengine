unit Pengine.Equaller;

interface

uses
  System.SysUtils,

  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Color;

type

  /// <summary>A generic, abstract class assistant, to test a specific value type V for equality.</summary>
  TEqualler<T> = class abstract
  public
    class function Equal(const AValue1, AValue2: T): Boolean; virtual; abstract;
  end;

  TBoolEqualler = class(TEqualler<Boolean>)
  public
    class function Equal(const AValue1, AValue2: Boolean): Boolean; override;
  end;

  TPointerEqualler = class(TEqualler<Pointer>)
  public
    class function Equal(const AValue1, AValue2: Pointer): Boolean; override;
  end;

  TRefEqualler<T: class> = class(TEqualler<T>)
  public
    class function Equal(const AValue1, AValue2: T): Boolean; override;
  end;

  TUIntEqualler = class(TEqualler<Cardinal>)
  public
    class function Equal(const AValue1, AValue2: Cardinal): Boolean; override;
  end;

  TIntEqualler = class(TEqualler<Integer>)
  public
    class function Equal(const AValue1, AValue2: Integer): Boolean; override;
  end;

  TIntVector2Equaller = class(TEqualler<TIntVector2>)
  public
    class function Equal(const AValue1, AValue2: TIntVector2): Boolean; override;
  end;

  TIntVector3Equaller = class(TEqualler<TIntVector3>)
  public
    class function Equal(const AValue1, AValue2: TIntVector3): Boolean; override;
  end;

  TIntBounds1Equaller = class(TEqualler<TIntBounds1>)
  public
    class function Equal(const AValue1, AValue2: TIntBounds1): Boolean; override;
  end;

  TIntBounds2Equaller = class(TEqualler<TIntBounds2>)
  public
    class function Equal(const AValue1, AValue2: TIntBounds2): Boolean; override;
  end;

  TIntBounds3Equaller = class(TEqualler<TIntBounds3>)
  public
    class function Equal(const AValue1, AValue2: TIntBounds3): Boolean; override;
  end;

  TSingleEqualler = class(TEqualler<Single>)
  public
    class function Equal(const AValue1, AValue2: Single): Boolean; override;
  end;

  TVector2Equaller = class(TEqualler<TVector2>)
  public
    class function Equal(const AValue1, AValue2: TVector2): Boolean; override;
  end;

  TVector3Equaller = class(TEqualler<TVector3>)
  public
    class function Equal(const AValue1, AValue2: TVector3): Boolean; override;
  end;

  TBounds1Equaller = class(TEqualler<TBounds1>)
  public
    class function Equal(const AValue1, AValue2: TBounds1): Boolean; override;
  end;

  TBounds2Equaller = class(TEqualler<TBounds2>)
  public
    class function Equal(const AValue1, AValue2: TBounds2): Boolean; override;
  end;

  TBounds3Equaller = class(TEqualler<TBounds3>)
  public
    class function Equal(const AValue1, AValue2: TBounds3): Boolean; override;
  end;

  TLine2Equaller = class(TEqualler<TLine2>)
  public
    class function Equal(const AValue1, AValue2: TLine2): Boolean; override;
  end;

  TLine3Equaller = class(TEqualler<TLine3>)
  public
    class function Equal(const AValue1, AValue2: TLine3): Boolean; override;
  end;

  TPlane3Equaller = class(TEqualler<TPlane3>)
  public
    class function Equal(const AValue1, AValue2: TPlane3): Boolean; override;
  end;

  TVectorDirEqualler = class(TEqualler<TVectorDir>)
  public
    class function Equal(const AValue1, AValue2: TVectorDir): Boolean; override;
  end;

  TColorRGBEqualler = class(TEqualler<TColorRGB>)
  public
    class function Equal(const AValue1, AValue2: TColorRGB): Boolean; override;
  end;

  TColorRGBAEqualler = class(TEqualler<TColorRGBA>)
  public
    class function Equal(const AValue1, AValue2: TColorRGBA): Boolean; override;
  end;

  TStringEqualler = class(TEqualler<string>)
  public
    class function Equal(const AValue1, AValue2: string): Boolean; override;
  end;

  TAnsiStringEqualler = class(TEqualler<AnsiString>)
  public
    class function Equal(const AValue1, AValue2: AnsiString): Boolean; override;
  end;

  TClassEqualler = class(TEqualler<TClass>)
  public
    class function Equal(const AValue1, AValue2: TClass): Boolean; override;
  end;

  TGUIDEqualler = class(TEqualler<TGUID>)
  public
    class function Equal(const AValue1, AValue2: TGUID): Boolean; override;
  end;

  TClassEqualler<T> = class(TEqualler<T>)
  public
    class function Equal(const AValue1, AValue2: T): Boolean; override;
  end;

  /// <remarks>/!\ Not type checked! You could give this any type, but should only give it an enum.</remarks>
  TEnumEqualler<T> = class(TEqualler<T>)
  public
    class function Equal(const AValue1, AValue2: T): Boolean; override;
  end;

  TCharEqualler = class(TEqualler<Char>)
  public
    class function Equal(const AValue1, AValue2: Char): Boolean; override;
  end;

implementation

{ TBoolEqualler }

class function TBoolEqualler.Equal(const AValue1, AValue2: Boolean): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TPointerEqualler }

class function TPointerEqualler.Equal(const AValue1, AValue2: Pointer): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TRefEqualler<T> }

class function TRefEqualler<T>.Equal(const AValue1, AValue2: T): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TUIntEqualler }

class function TUIntEqualler.Equal(const AValue1, AValue2: Cardinal): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TIntEqualler }

class function TIntEqualler.Equal(const AValue1, AValue2: Integer): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TIntVector2Equaller }

class function TIntVector2Equaller.Equal(const AValue1, AValue2: TIntVector2): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TIntVector3Equaller }

class function TIntVector3Equaller.Equal(const AValue1, AValue2: TIntVector3): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TIntBounds1Equaller }

class function TIntBounds1Equaller.Equal(const AValue1, AValue2: TIntBounds1): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TIntBounds2Equaller }

class function TIntBounds2Equaller.Equal(const AValue1, AValue2: TIntBounds2): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TIntBounds3Equaller }

class function TIntBounds3Equaller.Equal(const AValue1, AValue2: TIntBounds3): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TSingleEqualler }

class function TSingleEqualler.Equal(const AValue1, AValue2: Single): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TVector2Equaller }

class function TVector2Equaller.Equal(const AValue1, AValue2: TVector2): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TVector3Equaller }

class function TVector3Equaller.Equal(const AValue1, AValue2: TVector3): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TBounds1Equaller }

class function TBounds1Equaller.Equal(const AValue1, AValue2: TBounds1): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TBounds2Equaller }

class function TBounds2Equaller.Equal(const AValue1, AValue2: TBounds2): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TBounds3Equaller }

class function TBounds3Equaller.Equal(const AValue1, AValue2: TBounds3): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TLine2Equaller }

class function TLine2Equaller.Equal(const AValue1, AValue2: TLine2): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TLine3Equaller }

class function TLine3Equaller.Equal(const AValue1, AValue2: TLine3): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TPlane3Equaller }

class function TPlane3Equaller.Equal(const AValue1, AValue2: TPlane3): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TVectorDirEqualler }

class function TVectorDirEqualler.Equal(const AValue1, AValue2: TVectorDir): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TColorRGBEqualler }

class function TColorRGBEqualler.Equal(const AValue1, AValue2: TColorRGB): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TColorRGBAEqualler }

class function TColorRGBAEqualler.Equal(const AValue1, AValue2: TColorRGBA): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TStringEqualler }

class function TStringEqualler.Equal(const AValue1, AValue2: string): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TAnsiStringEqualler }

class function TAnsiStringEqualler.Equal(const AValue1, AValue2: AnsiString): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TClassEqualler }

class function TClassEqualler.Equal(const AValue1, AValue2: TClass): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TEnumEqualler<T> }

class function TEnumEqualler<T>.Equal(const AValue1, AValue2: T): Boolean;
begin
  Result := CompareMem(@AValue1, @AValue2, SizeOf(T));
end;

{ TGUIDEqualler }

class function TGUIDEqualler.Equal(const AValue1, AValue2: TGUID): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TClassEqualler<T> }

class function TClassEqualler<T>.Equal(const AValue1, AValue2: T): Boolean;
var
  AsClass1: TClass absolute AValue1;
  AsClass2: TClass absolute AValue2;
begin
  Result := AsClass1 = AsClass2;
end;

{ TCharEqualler }

class function TCharEqualler.Equal(const AValue1, AValue2: Char): Boolean;
begin
  Result := AValue1 = AValue2;
end;

end.
