unit Pengine.Utility;

interface

uses
  System.SysUtils;

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
  TOpt<T> = class
  public type

    TReader = class
    private
      function GetHasValue: Boolean;
      function GetValue: T;

    public
      property HasValue: Boolean read GetHasValue;
      property Value: T read GetValue;

    end;

  private
    FHasValue: Boolean;
    FValue: T;

    function GetValue: T;
    procedure SetValue(const Value: T);

  public
    /// <summary>Creates a new object without value.</summary>
    constructor Create; overload;
    /// <summary>Creates a new object with value.</summary>
    constructor Create(AValue: T); overload;

    /// <summary>Wether there currently is a valid value.</summary>
    property HasValue: Boolean read FHasValue;
    /// <summary>The value.</summary>
    /// <exception><see cref="Pengine.Collections|EOptWrapperNoValue"/> if there is no value.</exception>
    property Value: T read GetValue write SetValue;

    /// <summary>Removes the value.</summary>
    procedure Clear;

    function Reader: TReader;

    function Copy: TOpt<T>;

  end;

  /// <summary>A wrapper, that allows value types of any size.</summary>
  TOptRef<T> = class
  private
    FValueRef: TRef<T>;

    function GetHasValue: Boolean;

    function GetValue: T;
    procedure SetValue(const Value: T);

  public
    /// <summary>Creates a new object without value.</summary>
    constructor Create; overload;
    /// <summary>Creates a new object with value.</summary>
    constructor Create(AValue: T); overload;
    destructor Destroy; override;

    /// <summary>Wether there currently is a valid value.</summary>
    property HasValue: Boolean read GetHasValue;
    /// <summary>The value.</summary>
    /// <exception><see cref="Pengine.Collections|EOptWrapperNoValue"/> if there is no value.</exception>
    property Value: T read GetValue write SetValue;

    /// <summary>Removes the value.</summary>
    procedure Clear;

  end;

function GetBitCount(num: NativeUInt): Integer;

function PrettyFloat(AValue: Single): string; overload;
function PrettyFloat(AValue: Double): string; overload;

function ContainsOnly(AText: string; ASet: TSysCharSet): Boolean;

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

{ EOptWrapperNoValue }

constructor EOptWrapperNoValue.Create;
begin
  inherited Create('The optional wrapper does not have a wrapper.');
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

function TOpt<T>.Reader: TReader;
begin
  Result := TReader(Self);
end;

procedure TOpt<T>.SetValue(const Value: T);
begin
  FHasValue := True;
  FValue := Value;
end;

constructor TOpt<T>.Create;
begin
  // nothing
end;

function TOpt<T>.Copy: TOpt<T>;
begin
  if HasValue then
    Result := TOpt<T>.Create(Value)
  else
    Result := TOpt<T>.Create;
end;

constructor TOpt<T>.Create(AValue: T);
begin
  Value := AValue;
end;

procedure TOpt<T>.Clear;
begin
  FHasValue := False;
end;

{ TOptRef<T> }

function TOptRef<T>.GetHasValue: Boolean;
begin
  Result := FValueRef <> nil;
end;

function TOptRef<T>.GetValue: T;
begin
  if not HasValue then
    raise EOptWrapperNoValue.Create;
  Result := FValueRef.Value;
end;

procedure TOptRef<T>.SetValue(const Value: T);
begin
  if FValueRef = nil then
    FValueRef := TRef<T>.Create(Value)
  else
    FValueRef.Value := Value;
end;

constructor TOptRef<T>.Create;
begin
  // nothing
end;

constructor TOptRef<T>.Create(AValue: T);
begin
  Value := AValue;
end;

destructor TOptRef<T>.Destroy;
begin
  FValueRef.Free;
  inherited;
end;

procedure TOptRef<T>.Clear;
begin
  FValueRef := nil;
end;

{ TOpt<T>.TReader }

function TOpt<T>.TReader.GetHasValue: Boolean;
begin
  Result := TOpt<T>(Self).HasValue;
end;

function TOpt<T>.TReader.GetValue: T;
begin
  Result := TOpt<T>(Self).Value;
end;

end.
