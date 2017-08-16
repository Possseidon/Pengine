unit ResourceManager;

interface

uses
  Shaders, TextureManager, VAOManager, GLEnums, VectorGeometry, Lists, SysUtils, DebugConsoleDefine;

type

  { TResourceBase }

  TResourceBase = class abstract
  private class var
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
  protected
    function EqualTo(AOther: TResourceParameter): Boolean; virtual; abstract;
    function GetHash(ARange: Integer): Integer; virtual; abstract;

  public
    constructor Create; virtual;

  end;

  TResourceParameterRefMap<T> = class(TMap<TResourceParameter, T>)
  protected
    function GetKeyHash(AKey: TResourceParameter): Integer; override;
    class function CantIndex(AKey: TResourceParameter): Boolean; override;
    class function KeysEqual(AKey1, AKey2: TResourceParameter): Boolean; override;
  end;

  TResourceParameterMap<T> = class(TResourceParameterRefMap<T>)
  protected
    procedure FreeKey(AKey: TResourceParameter); override;
  end;

  { TParamResoruce<T> }

  // TODO: Change this, so that it won't create two instances for same parameters
  //       by adding an "Equals" and a reference counter for each parameter and such
  //       (create HashMap<TParam, TParamRes>, add refcounter to TParamRes and Destroy -> Release)

  /// <summary>
  /// An alternative to TResource<T>, which can contain Parameters.
  /// Create a new Resource of this type, using Make and Free it after use.
  /// </summary>
  TParamResource<T: class; P: TResourceParameter> = class abstract
  private class var
    FData: TResourceParameterMap<T>;
    FRefCounts: TResourceParameterRefMap<Integer>;

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

  private class var
    FResourceClasses: TGenericArray<TResourceClass>;
    FUnloadResourceClasses: TGenericArray<TResourceClass>;

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

function TResourceParameterRefMap<T>.GetKeyHash(AKey: TResourceParameter): Integer;
begin
  Result := AKey.GetHash(InternalSize);
end;

class function TResourceParameterRefMap<T>.CantIndex(AKey: TResourceParameter): Boolean;
begin
  Result := AKey = nil;
end;

class function TResourceParameterRefMap<T>.KeysEqual(AKey1, AKey2: TResourceParameter): Boolean;
begin
  Result := AKey1.EqualTo(AKey2);
end;

{ TResourceParameterMap }

procedure TResourceParameterMap<T>.FreeKey(AKey: TResourceParameter);
begin
  AKey.Free;
end;

{ TParamResource<T, P> }

class constructor TParamResource<T, P>.Create;
begin
  FData := TResourceParameterMap<T>.Create;
  FRefCounts := TResourceParameterRefMap<Integer>.Create;
end;

class destructor TParamResource<T, P>.Destroy;
begin
  FRefCounts.Free;
  FData.Free;
end;

class function TParamResource<T, P>.Make(var AParams: P): T;
begin
  if not FData.Get(AParams, Result) then
  begin
    CreateData(Result, AParams);
    FData[AParams] := Result;
    FRefCounts[AParams] := 1;
  end
  else
  begin
    FRefCounts[AParams] := FRefCounts[AParams] + 1;
    AParams.Free;
    AParams :=
    // add function to get the pointer to the actual key object and use that to do stuff..
  end;
end;

class function TParamResource<T, P>.Make: T;
begin
  Result := Make(P.Create);
end;

class procedure TParamResource<T, P>.Release(AParams: P);
var
  RefCount: Integer;
begin
  RefCount := FRefCounts[AParams] - 1;
  if RefCount = 0 then
  begin
    FData[AParams].Free;
    FData.Del(AParams);
    FRefCounts.Del(AParams);
  end
  else
    FRefCounts[AParams] := RefCount;
  AParams.Free;
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
end;

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
