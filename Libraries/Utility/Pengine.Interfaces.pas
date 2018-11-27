unit Pengine.Interfaces;

interface

type

  /// <summary>
  /// A replacement for <see cref="System|TInterfacedObject"/>, which is not reference counted.
  /// </summary>
  TInterfaceBase = class(TObject, IInterface)
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; inline; stdcall;
    function _Release: Integer; inline; stdcall;

  end;

  IOwningInterface = interface
    function GetValue: TObject;
    property Value: TObject read GetValue;
    function Own: TObject;

  end;

  TOwningInterface = class(TInterfacedObject, IOwningInterface)
  private
    FValue: TObject;

    function GetValue: TObject;

  public
    constructor Create(AValue: TObject);
    destructor Destroy; override;

    property Value: TObject read GetValue;
    function Own: TObject;

  end;

var
  DummyInterface: TInterfaceBase;

implementation

{ TInterfaceBase }

function TInterfaceBase.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TInterfaceBase._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfaceBase._Release: Integer;
begin
  Result := -1;
end;

{ TOwningInterface }

constructor TOwningInterface.Create(AValue: TObject);
begin
  FValue := AValue;
end;

destructor TOwningInterface.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TOwningInterface.GetValue: TObject;
begin
  Result := FValue;
end;

function TOwningInterface.Own: TObject;
begin
  Result := FValue;
  FValue := nil;
end;

initialization

DummyInterface := TInterfaceBase.Create;

finalization

DummyInterface.Free;

end.
