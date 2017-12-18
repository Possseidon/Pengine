unit Pengine.ResourceManager;

interface

uses

  {$IFNDEF DebugConsole}

  Vcl.Dialogs,
  System.UITypes,

  {$ENDIF}

  System.SysUtils,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Equaller,
  Pengine.DebugConsole;

type

  { TResourceBase }

  TResourceBase = class abstract
  public type
    TDataMap = TToRefMap<TClass, TObject, TClassHasher>;

  private
    class var
      FData: TDataMap;

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

    /// <summary>Call this in the class constructor of each non-abstract sub-class.</summary>
    class procedure AddToResourceManager;

  public
    /// <summary>Query the Data behind the Resource</summary>
    class function Data: T;

  end;

  TParamResource = class(TObject);

  TParamResourceClass = class of TParamResource;

  TResourceParameter = class abstract
  private
    FRefCount: Integer;
    FResourceClass: TParamResourceClass;

  protected
    function Equals(AOther: TResourceParameter): Boolean; reintroduce; virtual;
    function GetHash: Cardinal; virtual;

  end;

  TResourceParameterEqualler = class(TEqualler<TResourceParameter>)
    class function Equal(const AValue1, AValue2: TResourceParameter): Boolean; override;
  end;

  TResourceParameterHasher = class(THasher<TResourceParameter, TResourceParameterEqualler>)
  public
    class function GetHash(const AValue: TResourceParameter): Cardinal; override;
    class function CanIndex(const AValue: TResourceParameter): Boolean; override;
  end;

  TResourceParameterMap<T> = class(TMap<TResourceParameter, T, TResourceParameterHasher>)
  protected
    function CreateCopy(AHashMode: THashMode): THashBase; override;
  end;

  /// <summary>
  /// An alternative to TResource<T>, which can contain Parameters.
  /// Create a new Resource of this type using Make and Release it after use.
  /// </summary>
  TParamResource<T: class; P: TResourceParameter> = class abstract(TParamResource)
  private
    class var
      FData: TResourceParameterMap<T>;

  protected
    class function CreateData(AParam: P): T; virtual; abstract;

  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>Get a reference to a parametrized resource</summary>
    class function Make(AParams: P; AFreeParam: Boolean = True): T; overload;

    /// <summary>Release the with make obtained reference to a resource</summary>
    class procedure Release(AParams: P); overload;
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
    FResourceClasses: TArray<TResourceClass>;
    FUnloadResourceClasses: TArray<TResourceClass>;
    FUnfreedParamResources: TRefSet<TResourceParameter>;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure Init;
    class procedure Finalize;

  end;

implementation

{ TResourceBase }

class constructor TResourceBase.Create;
begin
  FData := TDataMap.Create;
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
  if not FData.Get(Self, TObject(Result)) then   
  begin
    Result := CreateData;
    FData[Self] := Result;
    TResourceManager.FUnloadResourceClasses.Add(Self);
  end;
end;

{ TResourceParameterRefMap }

function TResourceParameterMap<T>.CreateCopy(AHashMode: THashMode): THashBase;
begin
  raise ENotSupportedException.Create('A resource parameter map cannot be copied.');
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

class function TParamResource<T, P>.Make(AParams: P; AFreeParam: Boolean = True): T;
var
  ActualKey: TResourceParameter;
begin
  AParams.FResourceClass := Self;
  if FData.Get(AParams, Result) then
  begin
    ActualKey := FData.ActualKeys[AParams];
    Inc(ActualKey.FRefCount);
    if AFreeParam and (ActualKey <> TResourceParameter(AParams)) then
      AParams.Free;
  end
  else
  begin
    Result := CreateData(AParams);
    FData[AParams] := Result;
    Inc(AParams.FRefCount)
  end;
end;

class procedure TParamResource<T, P>.Release(AParams: P);
var
  Key: TResourceParameter;
begin
  Key := FData.ActualKeys[AParams];
  Dec(Key.FRefCount);
  if Key <> TResourceParameter(AParams) then
    AParams.Free;
  if Key.FRefCount = 0 then
    FData.Del(Key);
end;

{ TResourceManager }

class constructor TResourceManager.Create;
begin
  FResourceClasses := TArray<TResourceClass>.Create;
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
  FUnloadResourceClasses := TArray<TResourceClass>.Create;
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

{ TResourceParameterHasher }

class function TResourceParameterHasher.CanIndex(const AValue: TResourceParameter): Boolean;
begin
  Result := AValue <> nil;
end;

class function TResourceParameterHasher.GetHash(const AValue: TResourceParameter): Cardinal;
begin
  Result := AValue.GetHash;
end;

{ TResourceParameterEqualler }

class function TResourceParameterEqualler.Equal(const AValue1, AValue2: TResourceParameter): Boolean;
begin
  Result := AValue1.Equals(AValue2);
end;

{ TResourceParameter }

function TResourceParameter.Equals(AOther: TResourceParameter): Boolean;
begin
  Result := FResourceClass = AOther.FResourceClass;
end;

function TResourceParameter.GetHash: Cardinal;
begin
  Result := HashOf(FResourceClass);
end;

end.
