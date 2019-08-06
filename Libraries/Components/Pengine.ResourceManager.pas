unit Pengine.ResourceManager;

interface

uses
  System.SysUtils,

  Pengine.ICollections;

type

  TResourceManager = class;

  IResource = interface
  end;

  IResource<T> = interface(IResource)
    function GetData: T;

    property Data: T read GetData;

  end;

  TResourceClass = class of TResource;

  TResource = class(TInterfacedObject)
  private
    FResourceManager: TResourceManager;

  public
    constructor Create(AResourceManager: TResourceManager);

  end;

  TResource<T: class> = class(TResource, IResource<T>)
  private
    FData: T;

    // IResource<T>
    function GetData: T;

  protected
    function CreateData: T; virtual; abstract;

  public
    constructor Create(AResourceManager: TResourceManager);
    destructor Destroy; override;

    class function Get: IResource<T>; overload;
    class function Get(AResourceManager: TResourceManager): IResource<T>; overload;

    // IResource<T>
    property Data: T read GetData;

  end;

  TParamResource<T: class; P> = class(TResource, IResource<T>)
  private
    FData: T;
    FParam: P;

    // IResource<T>
    function GetData: T;

  protected
    function CreateData(AParam: P): T; virtual; abstract;

  public
    constructor Create(AParam: P; AResourceManager: TResourceManager);
    destructor Destroy; override;

    class function Get(AParam: P): IResource<T>; overload;
    class function Get(AParam: P; AResourceManager: TResourceManager): IResource<T>; overload;

    // IResource<T>
    property Data: T read GetData;

  end;

  IParamResourceKey = interface
    function Equals(AOther: IParamResourceKey): Boolean;
    function Hash: Cardinal;

  end;

  TParamResourceKey = class(TInterfacedObject);

  TParamResourceKey<P> = class(TParamResourceKey, IParamResourceKey)
  private
    FResourceClass: TResourceClass;
    FParam: P;

  public
    constructor Create(AResourceClass: TResourceClass; AParam: P);

    function Equals(AOther: IParamResourceKey): Boolean; reintroduce;
    function Hash: Cardinal;

  end;

  TResourceManager = class
  private
    // Uses Pointer instead of IResource to avoid reference counting
    // Requires cast to real class type anyway
    FResources: IMap<TResourceClass, Pointer>;
    FParamResources: IMap<IParamResourceKey, Pointer>;

  public
    constructor Create;

  end;

var
  ResourceManager: TResourceManager;

implementation

{ TResource<T> }

function TResource<T>.GetData: T;
begin
  Result := FData;
end;

constructor TResource<T>.Create;
begin
  inherited;
  FData := CreateData;
end;

destructor TResource<T>.Destroy;
begin
  FData.Free;
  FResourceManager.FResources.Remove(TResourceClass(ClassType));
  inherited;
end;

class function TResource<T>.Get: IResource<T>;
begin
  Result := Get(ResourceManager);
end;

class function TResource<T>.Get(AResourceManager: TResourceManager): IResource<T>;
var
  ResPointer: Pointer;
begin
  if AResourceManager.FResources.Get(Self, ResPointer) then
    Exit(IResource<T>(ResPointer));
  Result := Self.Create(AResourceManager);
  AResourceManager.FResources[Self] := Result;
end;

{ TParamResource<T, P> }

function TParamResource<T, P>.GetData: T;
begin
  Result := FData;
end;

constructor TParamResource<T, P>.Create(AParam: P; AResourceManager: TResourceManager);
begin
  inherited Create(AResourceManager);
  FData := CreateData(AParam);
  FParam := AParam;
end;

destructor TParamResource<T, P>.Destroy;
begin
  FData.Free;
  FResourceManager.FParamResources.Remove(TParamResourceKey<P>.Create(TResourceClass(ClassType), FParam));
  inherited;
end;

class function TParamResource<T, P>.Get(AParam: P): IResource<T>;
begin
  Result := Get(AParam, ResourceManager);
end;

class function TParamResource<T, P>.Get(AParam: P; AResourceManager: TResourceManager): IResource<T>;
var
  ResPointer: Pointer;
  ParamResourceKey: IParamResourceKey;
begin
  ParamResourceKey := TParamResourceKey<P>.Create(Self, AParam);
  if AResourceManager.FParamResources.Get(ParamResourceKey, ResPointer) then
    Exit(IResource<T>(ResPointer));
  Result := Self.Create(AParam, AResourceManager);
  AResourceManager.FParamResources[ParamResourceKey] := Result;
end;

{ TResourceManager }

constructor TResourceManager.Create;
begin
  FResources := TMap<TResourceClass, Pointer>.Create;
  FParamResources := TMap<IParamResourceKey, Pointer>.Create;
  FParamResources.EquateKey := function(A, B: IParamResourceKey): Boolean
    begin
      Result := A.Equals(B);
    end;
  FParamResources.HashKey := function(AParamResourceKey: IParamResourceKey): Cardinal
    begin
      Result := AParamResourceKey.Hash;
    end;
end;

{ TResource }

constructor TResource.Create(AResourceManager: TResourceManager);
begin
  FResourceManager := AResourceManager;
end;

{ TParamResourceKey<P> }

constructor TParamResourceKey<P>.Create(AResourceClass: TResourceClass; AParam: P);
begin
  FResourceClass := AResourceClass;
  FParam := AParam;
end;

function TParamResourceKey<P>.Equals(AOther: IParamResourceKey): Boolean;
var
  OtherTyped: TParamResourceKey<P>;
begin
  if TObject(AOther).ClassType <> TParamResourceKey<P> then
    Exit(False);
  OtherTyped := TParamResourceKey<P>(AOther);
  // TODO: Add a way to use a custom parameter equate function
  Result := (OtherTyped.FResourceClass = FResourceClass) and TDefault<P>.Equate(OtherTyped.FParam, FParam);
end;

function TParamResourceKey<P>.Hash: Cardinal;
begin
  Result := TDefault<TResourceClass>.Hash(FResourceClass) xor TDefault<P>.Hash(FParam);
end;

initialization

ResourceManager := TResourceManager.Create;

finalization

ResourceManager.Free;

end.
