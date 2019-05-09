unit Pengine.MC.Namespace;

interface

uses
  System.SysUtils,

  Pengine.Equaller,
  Pengine.Hasher;

type

  /// <summary>A namespace path in the form <c>namespace:path</c>.</summary>
  TNSPath = record
  public const

    DefaultNamespace = 'minecraft';

    NamespaceSeparator = ':';

  private
    FNamespace: string;
    FPath: string;

    function GetNamespace: string;
    procedure SetNamespace(const Value: string);

  public
    constructor Create(APath: string); overload;
    constructor Create(ANamespace, APath: string); overload;

    class function Empty: TNSPath; static;

    property Namespace: string read GetNamespace write SetNamespace;
    property Path: string read FPath write FPath;

    function IsEmpty: Boolean;

    function Format(AShowDefaultNamespace: Boolean = True): string;

    class operator Implicit(ANSPath: TNSPath): string; overload;
    class operator Implicit(AString: string): TNSPath; overload;

    class operator GreaterThan(A, B: TNSPath): Boolean; inline;
    class operator LessThan(A, B: TNSPath): Boolean; inline;
    class operator GreaterThanOrEqual(A, B: TNSPath): Boolean; inline;
    class operator LessThanOrEqual(A, B: TNSPath): Boolean; inline;

    class operator Equal(A, B: TNSPath): Boolean; inline;
    class operator NotEqual(A, B: TNSPath): Boolean; inline;

  end;

  TNSPathEqualler = class(TEqualler<TNSPath>)
  public
    class function Equal(const AValue1, AValue2: TNSPath): Boolean; override;

  end;

  TNSPathHasher = class(THasher<TNSPath, TNSPathEqualler>)
  public
    class function GetHash(const AValue: TNSPath): Cardinal; override;

  end;

function NSPath(APath: string): TNSPath; overload;
function NSPath(ANamespace, APath: string): TNSPath; overload;

implementation

function NSPath(APath: string): TNSPath;
begin
  Result.Create(APath);
end;

function NSPath(ANamespace, APath: string): TNSPath;
begin
  Result.Create(ANamespace, APath);
end;

{ TNSPath }

constructor TNSPath.Create(ANamespace, APath: string);
begin
  Namespace := ANamespace;
  Path := APath;
end;

constructor TNSPath.Create(APath: string);
begin
  Path := APath;
end;

function TNSPath.IsEmpty: Boolean;
begin
  Result := Path.IsEmpty;
end;

class function TNSPath.Empty: TNSPath;
begin
  Result.Create('');
end;

class operator TNSPath.Equal(A, B: TNSPath): Boolean;
begin
  Result := A.Format = B.Format;
end;

function TNSPath.Format(AShowDefaultNamespace: Boolean): string;
begin
  if not AShowDefaultNamespace and FNamespace.IsEmpty then
    Result := Path
  else
    Result := Namespace + NamespaceSeparator + Path;
end;

function TNSPath.GetNamespace: string;
begin
  if FNamespace.IsEmpty then
    Result := DefaultNamespace
  else
    Result := FNamespace;
end;

class operator TNSPath.GreaterThan(A, B: TNSPath): Boolean;
begin
  Result := A.Format > B.Format;
end;

class operator TNSPath.GreaterThanOrEqual(A, B: TNSPath): Boolean;
begin
  Result := A.Format >= B.Format;
end;

class operator TNSPath.Implicit(AString: string): TNSPath;
var
  SplitPos: Integer;
begin
  SplitPos := AString.IndexOf(':');
  if SplitPos <> -1 then
    Result.Namespace := AString.Substring(0, SplitPos);
  Result.Path := AString.Substring(SplitPos + 1);
end;

class operator TNSPath.LessThan(A, B: TNSPath): Boolean;
begin
  Result := A.Format < B.Format;
end;

class operator TNSPath.LessThanOrEqual(A, B: TNSPath): Boolean;
begin
  Result := A.Format <= B.Format;
end;

class operator TNSPath.NotEqual(A, B: TNSPath): Boolean;
begin
  Result := not(A = B);
end;

class operator TNSPath.Implicit(ANSPath: TNSPath): string;
begin
  Result := ANSPath.Format;
end;

procedure TNSPath.SetNamespace(const Value: string);
begin
  if Value = DefaultNamespace then
    FNamespace := ''
  else
    FNamespace := Value;
end;

{ TNSPathEqualler }

class function TNSPathEqualler.Equal(const AValue1, AValue2: TNSPath): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TNSPathHasher }

class function TNSPathHasher.GetHash(const AValue: TNSPath): Cardinal;
begin
  Result := HashOf(AValue.Namespace) xor HashOf(AValue.Path);
end;

end.
