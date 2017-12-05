unit Pengine.ValueEqualler;

interface

uses
  Pengine.IntMaths,
  Pengine.Vector;

type

  TValueEqualler = class abstract;
  TValueEquallerClass = class of TValueEqualler;

  /// <summary>A generic, abstract class assistant, to test a specific value type V for equality.</summary>
  TValueEqualler<T> = class abstract(TValueEqualler)
    class function Equal(const AValue1, AValue2: T): Boolean; virtual; abstract;
  end;


  TPointerEqualler = class(TValueEqualler<Pointer>)
  public
    class function Equal(const AValue1, AValue2: Pointer): Boolean; override;
  end;

  TRefEqualler<T: class> = class(TValueEqualler<T>)
  public
    class function Equal(const AValue1, AValue2: T): Boolean; override;
  end;

  TUIntEqualler = class(TValueEqualler<Cardinal>)
  public
    class function Equal(const AValue1, AValue2: Cardinal): Boolean; override;
  end;

  TIntEqualler = class(TValueEqualler<Integer>)
  public
    class function Equal(const AValue1, AValue2: Integer): Boolean; override;
  end;

  TIntVector2Equaller = class(TValueEqualler<TIntVector2>)
  public
    class function Equal(const AValue1, AValue2: TIntVector2): Boolean; override;
  end;

  TIntVector3Equaller = class(TValueEqualler<TIntVector3>)
  public
    class function Equal(const AValue1, AValue2: TIntVector3): Boolean; override;
  end;

  TIntBounds1Equaller = class(TValueEqualler<TIntBounds1>)
  public
    class function Equal(const AValue1, AValue2: TIntBounds1): Boolean; override;
  end;

  TIntBounds2Equaller = class(TValueEqualler<TIntBounds2>)
  public
    class function Equal(const AValue1, AValue2: TIntBounds2): Boolean; override;
  end;

  TIntBounds3Equaller = class(TValueEqualler<TIntBounds3>)
  public
    class function Equal(const AValue1, AValue2: TIntBounds3): Boolean; override;
  end;

  TSingleEqualler = class(TValueEqualler<Single>)
  public
    class function Equal(const AValue1, AValue2: Single): Boolean; override;
  end;

  TVector2Equaller = class(TValueEqualler<TVector2>)
  public
    class function Equal(const AValue1, AValue2: TVector2): Boolean; override;
  end;

  TVector3Equaller = class(TValueEqualler<TVector3>)
  public
    class function Equal(const AValue1, AValue2: TVector3): Boolean; override;
  end;

  TBounds1Equaller = class(TValueEqualler<TBounds1>)
  public
    class function Equal(const AValue1, AValue2: TBounds1): Boolean; override;
  end;

  TBounds2Equaller = class(TValueEqualler<TBounds2>)
  public
    class function Equal(const AValue1, AValue2: TBounds2): Boolean; override;
  end;

  TBounds3Equaller = class(TValueEqualler<TBounds3>)
  public
    class function Equal(const AValue1, AValue2: TBounds3): Boolean; override;
  end;

  TLine2Equaller = class(TValueEqualler<TLine2>)
  public
    class function Equal(const AValue1, AValue2: TLine2): Boolean; override;
  end;

  TLine3Equaller = class(TValueEqualler<TLine3>)
  public
    class function Equal(const AValue1, AValue2: TLine3): Boolean; override;
  end;

  TPlane2Equaller = class(TValueEqualler<TPlane2>)
  public
    class function Equal(const AValue1, AValue2: TPlane2): Boolean; override;
  end;

  TPlane3Equaller = class(TValueEqualler<TPlane3>)
  public
    class function Equal(const AValue1, AValue2: TPlane3): Boolean; override;
  end;

  TVectorDirEqualler = class(TValueEqualler<TVectorDir>)
  public
    class function Equal(const AValue1, AValue2: TVectorDir): Boolean; override;
  end;

  TStringEqualler = class(TValueEqualler<string>)
  public
    class function Equal(const AValue1, AValue2: string): Boolean; override;
  end;

  TAnsiStringEqualler = class(TValueEqualler<AnsiString>)
  public
    class function Equal(const AValue1, AValue2: AnsiString): Boolean; override;
  end;

  TClassEqualler = class(TValueEqualler<TClass>)
  public
    class function Equal(const AValue1, AValue2: TClass): Boolean; override;
  end;

implementation

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

{ TPlane2Equaller }

class function TPlane2Equaller.Equal(const AValue1, AValue2: TPlane2): Boolean;
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

end.
