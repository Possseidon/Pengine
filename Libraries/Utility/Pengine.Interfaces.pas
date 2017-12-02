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

end.
