unit Pengine.Utility;

interface

uses
  Winapi.Windows,

  System.SysUtils,

  Pengine.Interfaces;

type

  EOptWrapperNoValue = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>A wrapper, to make a reference out of any type.</summary>
  TRef<T> = class
  public
    Value: T;

    constructor Create(AValue: T);

  end;

  /// <summary>A wrapper, to make any type optional.</summary>
  TOpt<T> = record
  public type

    PReader = ^TReader;
    TReader = record
    private
      function GetHasValue: Boolean;
      function GetValue: T;

    public
      property HasValue: Boolean read GetHasValue;
      property Value: T read GetValue;

    end;

  private
    FHasValue: IInterface;
    FValue: T;

    function GetValue: T;
    procedure SetValue(const Value: T);

  public
    /// <summary>Creates a new object with value.</summary>
    constructor Create(AValue: T); overload;

    /// <returns>Wether there currently is a valid value.</returns>
    function HasValue: Boolean;
    /// <summary>The value.</summary>
    /// <exception><see cref="Pengine.Collections|EOptWrapperNoValue"/> if there is no value.</exception>
    property Value: T read GetValue write SetValue;

    /// <summary>Removes the value.</summary>
    procedure Clear;

    function Reader: TReader;

    class operator Implicit(AValue: T): TOpt<T>;
    class operator Implicit(AOpt: TOpt<T>): T;
    class operator LogicalOr(AOpt: TOpt<T>; ADefault: T): T;

    class operator Equal(A, B: TOpt<T>): Boolean;
    class operator NotEqual(A, B: TOpt<T>): Boolean;

  end;

  /// <summary>Wraps an optional owned object, which can be changed to a new instance publicly.</summary>
  /// <remarks>Useful to replace expensive object assignments with simply recreating a new object.</remarks>
  TOwned<T: class> = record
  private
    FValue: IOwningInterface;

    procedure SetValue(const Value: T);
    function GetValue: T;

  public
    /// <summary>Initialized the owned object with the given instance.</summary>
    constructor Create(AValue: T); overload;

    /// <returns>True, if an object is currently owned.</returns>
    function HasValue: Boolean;
    /// <summary>The current object.</summary>
    property Value: T read GetValue write SetValue;
    /// <summary>Frees the current object and sets the reference to nil.</summary>
    procedure Reset;
    /// <returns>The current owned object and resets itself, without freeing said object.</returns>
    function Own: T;

    class operator Implicit(AValue: T): TOwned<T>;
    class operator Implicit(AValue: TOwned<T>): T;

  end;

function GetBitCount(num: NativeUInt): Integer;

function PrettyFloat(AValue: Single): string; overload;
function PrettyFloat(AValue: Double): string; overload;

function ContainsOnly(AText: string; ASet: TSysCharSet): Boolean;

function ExpandEnvVars(AText: string): string;

implementation

function GetBitCount(num: NativeUInt): Integer;
asm
  {$IFDEF CPUX64}
  POPCNT    rax, num
  {$ELSE}
  POPCNT    eax, num
  {$ENDIF}
end;

function PrettyFloat(AValue: Single): string;
begin
  Result := AValue.ToString(ffGeneral, 7, 0, TFormatSettings.Invariant)
end;

function PrettyFloat(AValue: Double): string;
begin
  Result := AValue.ToString(ffGeneral, 15, 0, TFormatSettings.Invariant)
end;

function ContainsOnly(AText: string; ASet: TSysCharSet): Boolean;
var
  C: Char;
begin
  for C in AText do
    if not CharInSet(C, ASet) then
      Exit(False);
  Result := True;
end;

function ExpandEnvVars(AText: string): string;
var
  Len: Cardinal;
begin
  if AText.IsEmpty then
    Exit('');
  Len := ExpandEnvironmentStrings(PChar(AText), nil, 0);
  SetLength(Result, Len);
  ExpandEnvironmentStrings(PChar(AText), PChar(Result), Len);
  Result := TrimRight(Result);
end;

{ EOptWrapperNoValue }

constructor EOptWrapperNoValue.Create;
begin
  inherited Create('The optional wrapper does not have a value.');
end;

{ TRef<T> }

constructor TRef<T>.Create(AValue: T);
begin
  Value := AValue;
end;

{ TOpt<T> }

function TOpt<T>.GetValue: T;
begin
  if not HasValue then
    raise EOptWrapperNoValue.Create;
  Result := FValue;
end;

function TOpt<T>.HasValue: Boolean;
begin
  Result := FHasValue <> nil;
end;

class operator TOpt<T>.Implicit(AOpt: TOpt<T>): T;
begin
  Result := AOpt.Value;
end;

class operator TOpt<T>.Implicit(AValue: T): TOpt<T>;
begin
  Result.Value := AValue;
end;

class operator TOpt<T>.LogicalOr(AOpt: TOpt<T>; ADefault: T): T;
begin
  if AOpt.HasValue then
    Exit(AOpt.Value);
  Result := ADefault;
end;

class operator TOpt<T>.NotEqual(A, B: TOpt<T>): Boolean;
begin
  Result := not (A = B);
end;

function TOpt<T>.Reader: TReader;
begin
  Result := PReader(@Self)^;
end;

procedure TOpt<T>.SetValue(const Value: T);
begin
  FHasValue := DummyInterface;
  FValue := Value;
end;

constructor TOpt<T>.Create(AValue: T);
begin
  Value := AValue;
end;

class operator TOpt<T>.Equal(A, B: TOpt<T>): Boolean;
begin
  Result := A.HasValue = B.HasValue and (not A.HasValue or CompareMem(@A.FValue, @B.FValue, SizeOf(T)));
end;

procedure TOpt<T>.Clear;
begin
  FHasValue := nil;
end;

{ TOpt<T>.TReader }

function TOpt<T>.TReader.GetHasValue: Boolean;
begin
  Result := TOpt<T>(Pointer(@Self)^).HasValue;
end;

function TOpt<T>.TReader.GetValue: T;
begin
  Result := TOpt<T>(Pointer(@Self)^).Value;
end;

{ TOwned<T> }

constructor TOwned<T>.Create(AValue: T);
begin
  Value := AValue;
end;

function TOwned<T>.GetValue: T;
begin
  Result := T(FValue.Value);
end;

function TOwned<T>.HasValue: Boolean;
begin
  Result := FValue <> nil;
end;

class operator TOwned<T>.Implicit(AValue: TOwned<T>): T;
begin
  Result := AValue.Value;
end;

class operator TOwned<T>.Implicit(AValue: T): TOwned<T>;
begin
  Result.Value := AValue;
end;

procedure TOwned<T>.Reset;
begin
  FreeAndNil(FValue);
end;

procedure TOwned<T>.SetValue(const Value: T);
begin
  if Value = nil then
    FValue := nil
  else
    FValue := TOwningInterface.Create(Value);
end;

function TOwned<T>.Own: T;
begin
  Result := T(FValue.Own);
end;

end.
