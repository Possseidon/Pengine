unit Pengine.Formatting;

interface

type

  IFormatter = interface
    function Format: string;

  end;

  IFormatter<T> = interface(IFormatter)
    function GetValue: T;
    procedure SetValue(AValue: T);

    property Value: T read GetValue write SetValue;
    function Format(AValue: T): string; overload;

  end;

  TFormatter<T> = class(TInterfacedObject, IFormatter<T>)
  private
    FValue: T;

    function GetValue: T;
    procedure SetValue(AValue: T);

  public
    constructor Create; overload; virtual;
    constructor Create(AValue: T); overload;

    property Value: T read GetValue write SetValue;

    function Format: string; overload; virtual; abstract;
    function Format(AValue: T): string; overload;

  end;

implementation

{ TFormatter<T> }

constructor TFormatter<T>.Create;
begin
  // nothing by default
end;

constructor TFormatter<T>.Create(AValue: T);
begin
  Create;
  FValue := AValue;
end;

function TFormatter<T>.Format(AValue: T): string;
begin
  FValue := AValue;
  Result := Format;
end;

function TFormatter<T>.GetValue: T;
begin
  Result := FValue;
end;

procedure TFormatter<T>.SetValue(AValue: T);
begin
  FValue := AValue;
end;

end.
