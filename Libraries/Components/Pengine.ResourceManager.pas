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

    class function EquateParam(A, B: P): Boolean; virtual;
    class function HashParam(AParam: P): Cardinal; virtual;

    // IResource<T>
    property Data: T read GetData;

  end;

  IParamResourceKey = interface
    function Equals(AOther: IParamResourceKey): Boolean;
    function Hash: Cardinal;

  end;

  TParamResourceKey = class(TInterfacedObject);

  TParamResourceKey<T: class; P> = class(TParamResourceKey, IParamResourceKey)
  public type

    TDummyParamResource = class(TParamResource<T, P>);

    TDummyResourceClass = class of TDummyParamResource;

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
  FResourceManager.FParamResources.Remove(TParamResourceKey<T, P>.Create(TResourceClass(ClassType), FParam));
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
  ParamResourceKey := TParamResourceKey<T, P>.Create(Self, AParam);
  if AResourceManager.FParamResources.Get(ParamResourceKey, ResPointer) then
    Exit(IResource<T>(ResPointer));
  Result := Self.Create(AParam, AResourceManager);
  AResourceManager.FParamResources[ParamResourceKey] := Result;
end;

class function TParamResource<T, P>.EquateParam(A, B: P): Boolean;
begin
  Result := TDefault.Equate<P>(A, B);
end;

class function TParamResource<T, P>.HashParam(AParam: P): Cardinal;
begin
  Result := TDefault.Hash<P>(AParam);
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

{ TParamResourceKey<T, P> }

constructor TParamResourceKey<T, P>.Create(AResourceClass: TResourceClass; AParam: P);
begin
  FResourceClass := AResourceClass;
  FParam := AParam;
end;

function TParamResourceKey<T, P>.Equals(AOther: IParamResourceKey): Boolean;
var
  OtherTyped: TParamResourceKey<T, P>;
begin
  if TObject(AOther).ClassType <> TParamResourceKey<T, P> then
    Exit(False);
  OtherTyped := TParamResourceKey<T, P>(AOther);
  Result := (OtherTyped.FResourceClass = FResourceClass);
  Result := Result and TDummyResourceClass(FResourceClass).EquateParam(OtherTyped.FParam, FParam);
end;

function TParamResourceKey<T, P>.Hash: Cardinal;
begin
  Result := TDefault.Hash(FResourceClass) xor TDummyResourceClass(FResourceClass).HashParam(FParam);
end;

initialization

ResourceManager := TResourceManager.Create;

finalization

ResourceManager.Free;

end.
