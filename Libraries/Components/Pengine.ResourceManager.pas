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
    FResourceClass: TParamResourceClass;

  protected
    constructor Create; virtual;

    function CreateCopy: TResourceParameter;

    function Equals(AOther: TResourceParameter): Boolean; reintroduce; virtual;
    function GetHash: Cardinal; virtual;

  public
    procedure Assign(AResourceParameter: TResourceParameter); virtual;
    function Copy: TResourceParameter;

  end;

  TResourceParameterClass = class of TResourceParameter;

  TResourceParameterEntry = class
  private
    FRefCount: Integer;

  public
    constructor Create;

    class function GetTypeString: string; virtual; abstract;

    procedure AddRef;
    procedure DelRef;

    property RefCount: Integer read FRefCount;

  end;

  TResourceParameterEntry<T: class> = class(TResourceParameterEntry)
  private
    FData: T;

  public
    constructor Create(AData: T);
    destructor Destroy; override;

    class function GetTypeString: string; override;

    property Data: T read FData;

  end;

  TResourceParameterEqualler = class(TEqualler<TResourceParameter>)
    class function Equal(const AValue1, AValue2: TResourceParameter): Boolean; override;
  end;

  TResourceParameterHasher = class(THasher<TResourceParameter, TResourceParameterEqualler>)
  public
    class function GetHash(const AValue: TResourceParameter): Cardinal; override;
    class function CanIndex(const AValue: TResourceParameter): Boolean; override;
  end;

  TResourceParameterMap<T: class> = class(TObjectObjectMap<TResourceParameter, TResourceParameterEntry<T>, TResourceParameterHasher>);

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
    class procedure ReleaseReferences(AParam: P); virtual;

  public
    class constructor Create;
    class destructor Destroy;

    /// <summary>Get a reference to a parametrized resource</summary>
    class function Make(AParams: P; AFreeParam: Boolean = True): T; overload;

    /// <summary>Release the with make obtained reference to a resource</summary>
    class procedure Release(AParams: P; AFreeParam: Boolean = True); overload;
  end;

  { TResourceManager }

  TResourceManager = class
  public type

    TResourceClasses = TArray<TResourceClass>;
    TResourceParameterEntrySet = TRefSet<TResourceParameterEntry>;

  private
    class procedure Add(AResourceClass: TResourceClass);
    {$IFDEF DEBUG}
    class procedure ShowUnfreedParamResources;
    {$ENDIF}
  private
    class var
      FResourceClasses: TResourceClasses;
      FUnloadResourceClasses: TResourceClasses;
      FUnfreedParamResources: TResourceParameterEntrySet;

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

{ TParamResource<T, P> }

class constructor TParamResource<T, P>.Create;
begin
  FData := TResourceParameterMap<T>.Create;
end;

class destructor TParamResource<T, P>.Destroy;
var
  Data: TPair<TResourceParameter, TResourceParameterEntry<T>>;
begin
  for Data in FData do
    TResourceManager.FUnfreedParamResources.Add(Data.Value);

  FData.Free;
end;

class function TParamResource<T, P>.Make(AParams: P; AFreeParam: Boolean = True): T;
var
  ActualKey: TResourceParameter;
  ResourceEntry: TResourceParameterEntry<T>;
begin
  AParams.FResourceClass := Self;
  if FData.Get(AParams, ResourceEntry) then
  begin
    ResourceEntry.AddRef;
    Result := ResourceEntry.Data;
  end
  else
  begin
    Result := CreateData(AParams);
    FData[AParams.Copy] := TResourceParameterEntry<T>.Create(Result);
  end;
  if AFreeParam then
    AParams.Free;
end;

class procedure TParamResource<T, P>.Release(AParams: P; AFreeParam: Boolean = True);
var
  Data: TResourceParameterEntry<T>;
begin
  AParams.FResourceClass := Self;
  Data := FData[AParams];
  Data.DelRef;
  if Data.RefCount = 0 then
  begin
    ReleaseReferences(AParams);
    FData.Del(AParams);
  end;
  if AFreeParam then
    AParams.Free;
end;

class procedure TParamResource<T, P>.ReleaseReferences(AParam: P);
begin
  // nothing
end;

{ TResourceManager }

class constructor TResourceManager.Create;
begin
  FResourceClasses := TResourceClasses.Create;
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
  FUnloadResourceClasses := TResourceClasses.Create;
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
  FUnfreedParamResources := TResourceParameterEntrySet.Create;
end;

{$IFDEF DEBUG}

class procedure TResourceManager.ShowUnfreedParamResources;
var
  ResourceEntry: TResourceParameterEntry;
  ErrorString: string;
begin
  if FUnfreedParamResources.Count > 0 then
  begin
    ErrorString := 'Following Resource';
    if FUnfreedParamResources.Count > 1 then
      ErrorString := ErrorString + 's';
    ErrorString := ErrorString + ' did not get released:' + sLineBreak;

    for ResourceEntry in FUnfreedParamResources do
    begin
      ErrorString := ErrorString + Format(
        '- %s: %d reference',
        [ResourceEntry.GetTypeString, ResourceEntry.FRefCount]);
      if ResourceEntry.FRefCount > 1 then
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

procedure TResourceParameter.Assign(AResourceParameter: TResourceParameter);
begin
  FResourceClass := AResourceParameter.FResourceClass;
end;

function TResourceParameter.Copy: TResourceParameter;
begin
  Result := CreateCopy;
end;

constructor TResourceParameter.Create;
begin
  // nothing
end;

function TResourceParameter.CreateCopy: TResourceParameter;
begin
  Result := TResourceParameterClass(ClassType).Create;
  Result.Assign(Self);
end;

function TResourceParameter.Equals(AOther: TResourceParameter): Boolean;
begin
  Result := FResourceClass = AOther.FResourceClass;
end;

function TResourceParameter.GetHash: Cardinal;
begin
  Result := HashOf(FResourceClass);
end;

{ TResourceParameterEntry }

constructor TResourceParameterEntry.Create;
begin
  FRefCount := 1;
end;

procedure TResourceParameterEntry.AddRef;
begin
  Inc(FRefCount);
end;

procedure TResourceParameterEntry.DelRef;
begin
  Dec(FRefCount);
end;

{ TResourceParameterEntry<T> }

constructor TResourceParameterEntry<T>.Create(AData: T);
begin
  inherited Create;
  FData := AData;
end;

destructor TResourceParameterEntry<T>.Destroy;
begin
  FData.Free;
  inherited;
end;

class function TResourceParameterEntry<T>.GetTypeString: string;
begin
  Result := T.ClassName;
end;

end.
