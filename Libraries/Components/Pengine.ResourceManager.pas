unit Pengine.ResourceManager;

interface

uses
{$IFNDEF DebugConsole}
  Dialogs, UITypes,
{$ENDIF}
  Shaders, TextureManager, VAOManager, GLEnums, VectorGeometry, Lists, SysUtils, DebugConsoleDefine;

type

  { TResourceBase }

  TResourceBase = class abstract
  private
    class var
      FData: TClassRefMap<TObject>;

    class procedure Load; virtual; abstract;
    class procedure Unload; virtual; abstract;

  public
    class constructor Create;
    class destructor Destroy;

  end;

  TResourceClass = class of TResourceBase;

  { TResource }

  TResource<T: class> = class abstract(TResourceBase)
  private
    class procedure Load; override;
    class procedure Unload; override;

  protected
    class function CreateData: T; virtual; abstract;

    /// <summary>Call this in the class constructor of each non-abstract sub-class</summary>
    class procedure AddToResourceManager;

  public

    /// <summary>Query the Data behind the Resource</summary>
    class function Data: T;

  end;

  { TResourceParameter }

  TResourceParameter = class abstract
  private
    FRefCount: Integer;

  protected
    function EqualTo(AOther: TResourceParameter): Boolean; virtual; abstract;
    function GetHash(ARange: Cardinal): Cardinal; virtual; abstract;

  public
    constructor Create; virtual;

  end;

  TResourceParameterMap<T: class> = class(TObjectObjectMap<TResourceParameter, T>)
  protected
    function GetKeyHash(AKey: TResourceParameter): Cardinal; override;
    class function CantIndex(AKey: TResourceParameter): Boolean; override;
    class function KeysEqual(AKey1, AKey2: TResourceParameter): Boolean; override;
  end;

  { TParamResoruce<T> }

  /// <summary>
  /// An alternative to TResource<T>, which can contain Parameters.
  /// Create a new Resource of this type, using Make and Release it after use.
  /// </summary>
  TParamResource<T: class; P: TResourceParameter> = class abstract
  private
    class var
      FData: TResourceParameterMap<T>;

  protected
    class procedure CreateData(var AData: T; AParam: P); virtual; abstract;

  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>Get a reference to a parametrized resource</summary>
    class function Make(AParams: P): T; overload;
    /// <summary>Get a reference to a default parametrized resource</summary>
    class function Make: T; overload;

    /// <summary>Release the with make obtained reference to a resource</summary>
    class procedure Release(AParams: P); overload;
    /// <summary>Release the with make obtained reference to a resource</summary>
    class procedure Release; overload;
  end;

  { TResourceManager }

  TResourceManager = class
  private
    class procedure Add(AResourceClass: TResourceClass);
{$IFDEF DEBUG}
    class procedure ShowUnfreedParamResources;
{$ENDIF}
  private
  class var
    FResourceClasses: TGenericArray<TResourceClass>;
    FUnloadResourceClasses: TGenericArray<TResourceClass>;
    FUnfreedParamResources: TRefSet<TResourceParameter>;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure Init;
    class procedure Finalize;

  end;

  { TShaderResource }

  TShaderResource = class(TResource<TShader>)
  protected
    class function GetShaderSource: string; virtual; abstract;
    class function GetAttributeOrder: TShaderAttributeOrder; virtual; abstract;

    class function CreateData: TShader; override;
  end;

implementation

{ TResourceBase }

class constructor TResourceBase.Create;
begin
  FData := TClassRefMap<TObject>.Create;
end;

class destructor TResourceBase.Destroy;
begin
  FData.Free;
end;

{ TResource<T> }

class procedure TResource<T>.Load;
begin
  Data;
end;

class procedure TResource<T>.Unload;
begin
  Data.Free;
end;

class procedure TResource<T>.AddToResourceManager;
begin
  TResourceManager.Add(Self);
end;

class function TResource<T>.Data: T;
begin
  Result := T(FData.GetOrNil(Self));
  if Result = nil then
  begin
    TResourceManager.FUnloadResourceClasses.Add(Self);
    Result := CreateData;
    FData[Self] := Result;
  end;
end;

{ TResourceParameter }

constructor TResourceParameter.Create;
begin
  // nothing by default
end;

{ TResourceParameterRefMap }

function TResourceParameterMap<T>.GetKeyHash(AKey: TResourceParameter): Cardinal;
begin
  Result := AKey.GetHash(InternalSize);
end;

class function TResourceParameterMap<T>.CantIndex(AKey: TResourceParameter): Boolean;
begin
  Result := AKey = nil;
end;

class function TResourceParameterMap<T>.KeysEqual(AKey1, AKey2: TResourceParameter): Boolean;
begin
  Result := AKey1.EqualTo(AKey2);
end;

{ TParamResource<T, P> }

class constructor TParamResource<T, P>.Create;
begin
  FData := TResourceParameterMap<T>.Create;
end;

class destructor TParamResource<T, P>.Destroy;
var
  Data: TPair<TResourceParameter, T>;
begin
  for Data in FData do
    TResourceManager.FUnfreedParamResources.Add(Data.Key);

  FData.Free;
end;

class function TParamResource<T, P>.Make(AParams: P): T;
var
  ActualKey: TResourceParameter;
begin
  if FData.Get(AParams, Result) then
  begin
    ActualKey := FData.ActualKey(AParams);
    Inc(ActualKey.FRefCount);
    if ActualKey <> TResourceParameter(AParams) then
      AParams.Free;
  end
  else
  begin
    CreateData(Result, AParams);
    FData[AParams] := Result;
    Inc(AParams.FRefCount)
  end;
end;

class function TParamResource<T, P>.Make: T;
var
  Params: P;
begin
  Params := P.Create;
  Make(Params);
end;

class procedure TParamResource<T, P>.Release(AParams: P);
var
  ActualKey: TResourceParameter;
begin
  ActualKey := FData.ActualKey(AParams);
  Dec(ActualKey.FRefCount);
  if ActualKey <> TResourceParameter(AParams) then
    AParams.Free;
  if ActualKey.FRefCount = 0 then
    FData.Del(ActualKey);
end;

class procedure TParamResource<T, P>.Release;
begin
  Release(P.Create);
end;

{ TResourceManager }

class constructor TResourceManager.Create;
begin
  FResourceClasses := TGenericArray<TResourceClass>.Create;
end;

class destructor TResourceManager.Destroy;
begin
  FUnloadResourceClasses.Free;
{$IFDEF DEBUG}
  ShowUnfreedParamResources;
{$ENDIF}
  FUnfreedParamResources.Free;
end;

class procedure TResourceManager.Init;
var
  ResourceClass: TResourceClass;
begin
  FUnloadResourceClasses := TGenericArray<TResourceClass>.Create;
  for ResourceClass in FResourceClasses do
    ResourceClass.Load;
  FResourceClasses.Free;
end;

class procedure TResourceManager.Finalize;
var
  ResourceClass: TResourceClass;
begin
  for ResourceClass in FUnloadResourceClasses do
    ResourceClass.Unload;
  FUnfreedParamResources := TRefSet<TResourceParameter>.Create;
end;

{$IFDEF DEBUG}
class procedure TResourceManager.ShowUnfreedParamResources;
var
  ParamResource: TResourceParameter;
  ErrorString: string;
begin
  if FUnfreedParamResources.Count > 0 then
  begin
    ErrorString := 'Following Resource';
    if FUnfreedParamResources.Count > 1 then
      ErrorString := ErrorString + 's';
    ErrorString := ErrorString + ' did not get released:' + sLineBreak;

    for ParamResource in FUnfreedParamResources do
    begin
      ErrorString := ErrorString + Format(
        '- %s: %d reference',
        [ParamResource.ClassName, ParamResource.FRefCount]);
      if ParamResource.FRefCount > 1 then
        ErrorString := ErrorString + 's';
      ErrorString := ErrorString + sLineBreak;
    end;
{$IFDEF CONSOLE}
    DebugWriteLine(sLineBreak + ErrorString);
{$ELSE}
    MessageDlg(ErrorString, mtError, [mbOk], 0);
{$ENDIF}
  end;
end;
{$ENDIF}

class procedure TResourceManager.Add(AResourceClass: TResourceClass);
begin
  FResourceClasses.Add(AResourceClass);
end;

{ TShaderResource }

class function TShaderResource.CreateData: TShader;
begin
  Result := TShader.Create;
  Result.LoadFromFile(GetShaderSource);
  Result.SetAttributeOrder(GetAttributeOrder);
end;

end.
