unit Pengine.MC.Datapack;

interface

uses
  Winapi.Windows,

  System.IOUtils,
  System.SysUtils,
  System.Types,
  System.Math,

  Pengine.CollectionInterfaces,
  Pengine.ObservableCollections,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.EventHandling,

  Pengine.MC.General,
  Pengine.MC.Brigadier, Winapi.ShellAPI;

type

  EDatapack = class(Exception);

  TDatapackBase = class
  protected
    function GetFullPath: TFileName; virtual; abstract;
    function GetNamespacePath: string; virtual;

    function GetName: string; virtual; abstract;
    procedure SetName(const Value: string); virtual;

    function GetDisplayName: string; virtual;
    procedure SetDisplayName(const Value: string); virtual;
                                     
    class function MoveToRecycleBin(APath: TFileName): Boolean; static;

  public
    procedure AfterConstruction; override;

    /// <summary>The full file path to the object.</summary>
    property FullPath: TFileName read GetFullPath;

    function NameEditable: Boolean; virtual;
    property Name: string read GetName write SetName;
    property DisplayName: string read GetDisplayName write SetDisplayName;

    /// <returns>Wether NamespacePath is available on the current object.</returns>
    function HasNamespacePath: Boolean; virtual;
    /// <summary>The namespace identifier or namespace itself.</summary>
    property NamespacePath: string read GetNamespacePath;

    procedure Update; virtual;

    procedure Delete; virtual; abstract;

  end;

  TDatapack = class(TDatapackBase)
  public type

    TData = class;
    TDirectory = class;
    TNamespace = class;

    TFileSystemEntry = class(TDatapackBase)
    public type

      TEventInfo = TSenderEventInfo<TFileSystemEntry>;

      TEvent = TEvent<TEventInfo>;

    private
      FData: TData;
      FParent: TDirectory;
      FName: string;

      function GetNamespace: TNamespace;
      function GetDatapack: TDatapack;

    protected
      FOnRename: TEvent;
      FOnRemove: TEvent;

      function GetFullPath: TFileName; override;

      function GetName: string; override;
      procedure SetName(const Value: string); override;

    public
      constructor Create(AParent: TDirectory; AName: string);

      property Data: TData read FData;
      property Namespace: TNamespace read GetNamespace;
      property Datapack: TDatapack read GetDatapack;

      function NameEditable: Boolean; override;

      property Parent: TDirectory read FParent;
                                        
      function OnRename: TEvent.TAccess;

    end;

    TFile = class(TFileSystemEntry)
    public type

      TEventInfo = TSenderEventInfo<TFile>;

      TEvent = TEvent<TEventInfo>;

    private
      FTimestamp: TDateTime;
      FFileExists: Boolean;
      FOnChange: TEvent;
      FOnFileRemoved: TEvent;
      FOnRemove: TEvent;

      function GetTimestamp: TDateTime;

      function GetFileExists: Boolean;

    protected
      function GetFullPath: TFileName; override;
      function GetNamespacePath: string; override;

      function GetDisplayName: string; override;
      procedure SetDisplayName(const Value: string); override;

    public
      procedure BeforeDestruction; override;

      function HasNamespacePath: Boolean; override;

      property FileExists: Boolean read GetFileExists;
      procedure Update; override;
      procedure Delete; override;
      
      function OnChange: TEvent.TAccess;
      function OnFileRemoved: TEvent.TAccess;
      function OnRemove: TEvent.TAccess;

    end;

    TDirectory = class(TFileSystemEntry)
    public type

      TEventInfo = TSenderEventInfo<TDirectory>;

      TEvent = TEvent<TEventInfo>;

      TEntries = TObservableToObjectMap<string, TFileSystemEntry, TStringHasher>;

    private
      FEntries: TEntries;
      FRename: Boolean;
      FOnAddEntry: TFileSystemEntry.TEvent;
      FOnRemoveEntry: TFileSystemEntry.TEvent;

      function GetEntries: TEntries.TReader;

      procedure AddEntryEvents(AEntry: TFileSystemEntry);
      procedure RemoveEntryEvents(AEntry: TFileSystemEntry);

      procedure ChildEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
      procedure ChildEntryRemove(AInfo: TFileSystemEntry.TEventInfo);

      procedure EntryAdd(AInfo: TEntries.TAddEventInfo);
      procedure EntryRemove(AInfo: TEntries.TRemoveEventInfo);
      procedure EntryValueChange(AInfo: TEntries.TValueChangeEventInfo);

      procedure ChildRename(AEntry: TFileSystemEntry; ANewName: string);

    protected
      function GetNamespacePath: string; override;

    public
      constructor Create(AParent: TDirectory; AName: string);
      destructor Destroy; override;

      property Entries: TEntries.TReader read GetEntries;

      function HasNamespacePath: Boolean; override;

      function AddDirectory(AName: string): TDirectory;
      function AddFile(AName: string): TFile;
      procedure Remove(AName: string);

      procedure Update; override;
      procedure Delete; override;
      
      function OnAddEntry: TFileSystemEntry.TEvent.TAccess;
      function OnRemoveEntry: TFileSystemEntry.TEvent.TAccess;

    end;

    TRootDirectory = class(TDirectory)
    protected
      function GetNamespacePath: string; override;
      function GetFullPath: TFileName; override;

    public
      constructor Create(AData: TData; AName: string = '');

      procedure Delete; override;
      
    end;

    TDataType = (
      dtAdvancement,
      dtFunction,
      dtLootTable,
      dtRecipe,
      dtStructure,
      dtTag
      );

    TData = class(TDatapackBase)
    public type

      TEventInfo = TSenderEventInfo<TData>;

      TEvent = TEvent<TEventInfo>;

    private
      FNamespace: TNamespace;

    protected
      FOnAddFileSystemEntry: TFileSystemEntry.TEvent;
      FOnRemoveFileSystemEntry: TFileSystemEntry.TEvent;

      function GetFullPath: TFileName; override;
      function GetName: string; override;
      function GetDisplayName: string; override;

    public
      constructor Create(ANamespace: TNamespace); virtual;

      class function GetDisplayNameNormal: string;
      class function GetDisplayNamePlural: string;
      class function GetFileExtension: string;
      class function GetFolderName: string;
      class function GetType: TDataType; virtual; abstract;

      property Namespace: TNamespace read FNamespace;

      function OnAddFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
      function OnRemoveFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
      
      procedure Delete; override;

    end;

    TDataSimple = class(TData)
    private
      FDirectory: TRootDirectory;

      procedure FileSystemEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
      procedure FileSystemEntryRemove(AInfo: TFileSystemEntry.TEventInfo);

    public
      constructor Create(ANamespace: TNamespace); override;
      destructor Destroy; override;

      property Directory: TRootDirectory read FDirectory;

      procedure Update; override;      
      
    end;

    TDataMulti = class(TData)
    private
      // TODO

    protected

    public
      procedure Update; override;

    end;

    TDataClass = class of TData;

    TNamespace = class(TDatapackBase)
    public type

      TEventInfo = TSenderEventInfo<TNamespace>;

      TEvent = TEvent<TEventInfo>;

      TDataArray = array [TDataType] of TData;

    private
      FDatapack: TDatapack;
      FName: string;
      FData: TDataArray;
      FOnEnableData: TData.TEvent;
      FOnDisableData: TData.TEvent;
      FOnAddFileSystemEntry: TFileSystemEntry.TEvent;
      FOnRemoveFileSystemEntry: TFileSystemEntry.TEvent;

      function GetData(AType: TDataType): TData; overload;
      function GetData(ADataClass: TDataClass): TData; overload;

      procedure AddDataEvents(AData: TData);
      procedure RemoveDataEvents(AData: TData);

      procedure DataEnable(AData: TData);
      procedure DataDisable(AData: TData);
      procedure FileSystemEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
      procedure FileSystemEntryRemove(AInfo: TFileSystemEntry.TEventInfo);

    protected
      function GetNamespacePath: string; override;
      function GetFullPath: TFileName; override;
      function GetName: string; override;
      procedure SetName(const Value: string); override;

    public
      constructor Create(ADatapack: TDatapack; AName: string);
      destructor Destroy; override;

      property Datapack: TDatapack read FDatapack;

      function HasNamespacePath: Boolean; override;
      function NameEditable: Boolean; override;

      function DataEnabled(AType: TDataType): Boolean;
      property Data[AType: TDataType]: TData read GetData; default;
      property Data[ADataClass: TDataClass]: TData read GetData; default;
      function Get<T: TData>: T;

      procedure EnableData(AType: TDataType);
      procedure DisableData(AType: TDataType);

      procedure Update; override;    
      procedure Delete; override;

      function OnEnableData: TData.TEvent.TAccess;
      function OnDisableData: TData.TEvent.TAccess;
      function OnAddFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
      function OnRemoveFileSystemEntry: TFileSystemEntry.TEvent.TAccess;

    end;

    TNamespaces = TObservableToObjectMap<string, TNamespace, TStringHasher>;

  public const

    NamespaceFolderName = 'data';

  private
    FPath: TFileName;
    FNamespaces: TNamespaces;
    FOnAddNamespace: TNamespace.TEvent;
    FOnRemoveNamespace: TNamespace.TEvent;
    FOnEnableData: TData.TEvent;
    FOnDisableData: TData.TEvent;
    FOnAddFileSystemEntry: TFileSystemEntry.TEvent;
    FOnRemoveFileSystemEntry: TFileSystemEntry.TEvent;

    function GetNamespaces: TNamespaces.TReader;
    function GetDataPath: TFileName;

    procedure AddNamespaceEvents(ANamespace: TNamespace);
    procedure RemoveNamespaceEvents(ANamespace: TNamespace);

    procedure NamespaceAdd(AInfo: TNamespaces.TAddEventInfo);
    procedure NamespaceRemove(AInfo: TNamespaces.TRemoveEventInfo);
    procedure DataEnable(AInfo: TData.TEventInfo);
    procedure DataDisable(AInfo: TData.TEventInfo);
    procedure FileSystemEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
    procedure FileSystemEntryRemove(AInfo: TFileSystemEntry.TEventInfo);

    class procedure VerifyName(AName: string); static;

  protected
    function GetFullPath: TFileName; override;
    function GetName: string; override;

  public
    constructor Create(APath: TFileName);
    destructor Destroy; override;

    property Namespaces: TNamespaces.TReader read GetNamespaces;
    function AddNamespace(AName: string): TNamespace;
    procedure RemoveNamespace(AName: string);

    /// <summary>The full path to the <c>pack.mcmeta</c>.</summary>
    property FilePath: TFileName read FPath;
    /// <summary>The path to the folder containing the namespaces.</summary>
    property DataPath: TFileName read GetDataPath;

    procedure Update; override;
    procedure Delete; override;

    function OnAddNamespace: TNamespace.TEvent.TAccess;
    function OnRemoveNamespace: TNamespace.TEvent.TAccess;
    function OnEnableData: TData.TEvent.TAccess;
    function OnDisableData: TData.TEvent.TAccess;
    function OnAddFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
    function OnRemoveFileSystemEntry: TFileSystemEntry.TEvent.TAccess;

  end;

  TDPAdvancements = class(TDatapack.TDataSimple)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPFunctions = class(TDatapack.TDataSimple)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPLootTables = class(TDatapack.TDataSimple)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPRecipes = class(TDatapack.TDataSimple)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPStructures = class(TDatapack.TDataSimple)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPTags = class(TDatapack.TDataMulti)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

const

  DPClasses: array [TDatapack.TDataType] of TDatapack.TDataClass = (
    TDPAdvancements,
    TDPFunctions,
    TDPLootTables,
    TDPRecipes,
    TDPStructures,
    TDPTags
    );

  DPFolderNames: array [TDatapack.TDataType] of string = (
    'advancements',
    'functions',
    'loot_tables',
    'recipes',
    'structures',
    'tags'
    );

  DPDisplayNames: array [TDatapack.TDataType] of string = (
    'Advancement',
    'Function',
    'Loot Table',
    'Recipe',
    'Structure',
    'Tag'
    );

  DPDisplayNamesPlural: array [TDatapack.TDataType] of string = (
    'Advancements',
    'Functions',
    'Loot Tables',
    'Recipes',
    'Structures',
    'Tags'
    );

  DPFileExtensions: array [TDatapack.TDataType] of string = (
    '.json',
    '.mcfunction',
    '.json',
    '.json',
    '.nbt',
    '.json'
    );

implementation

{ TDatapack.TNamespace }

procedure TDatapack.TNamespace.AddDataEvents(AData: TData);
begin
  AData.OnAddFileSystemEntry.Add(FileSystemEntryAdd);
  AData.OnRemoveFileSystemEntry.Add(FileSystemEntryRemove);
end;

constructor TDatapack.TNamespace.Create(ADatapack: TDatapack; AName: string);
begin
  FDatapack := ADatapack;
  FName := AName;
end;

procedure TDatapack.TNamespace.DataDisable(AData: TData);
begin
  RemoveDataEvents(AData);
  FOnDisableData.Execute(TData.TEventInfo.Create(AData));
end;

procedure TDatapack.TNamespace.DataEnable(AData: TData);
begin
  AddDataEvents(AData);
  FOnEnableData.Execute(TData.TEventInfo.Create(AData));
end;

function TDatapack.TNamespace.DataEnabled(AType: TDataType): Boolean;
begin
  Result := FData[AType] <> nil;
end;

procedure TDatapack.TNamespace.Delete;
begin
  Datapack.RemoveNamespace(Name);
end;

destructor TDatapack.TNamespace.Destroy;
var
  DataType: TDataType;
begin
  for DataType := Low(TDataType) to High(TDataType) do
    FData[DataType].Free;
  inherited;
end;

procedure TDatapack.TNamespace.DisableData(AType: TDataType);
begin
  FOnDisableData.Execute(TData.TEventInfo.Create(FData[AType]));
  RemoveDataEvents(FData[AType]);
  FreeAndNil(FData[AType]);
end;

procedure TDatapack.TNamespace.EnableData(AType: TDataType);
begin
  if DataEnabled(AType) then
    Exit;
  FData[AType] := DPClasses[AType].Create(Self);
  AddDataEvents(FData[AType]);
  System.IOUtils.TDirectory.CreateDirectory(FData[AType].FullPath);
  FOnEnableData.Execute(TData.TEventInfo.Create(FData[AType]));
end;

procedure TDatapack.TNamespace.FileSystemEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnAddFileSystemEntry.Execute(AInfo, False);
end;

procedure TDatapack.TNamespace.FileSystemEntryRemove(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnRemoveFileSystemEntry.Execute(AInfo, False);
end;

function TDatapack.TNamespace.Get<T>: T;
begin
  Result := T(FData[T.GetType]);
end;

function TDatapack.TNamespace.GetData(ADataClass: TDataClass): TData;
begin
  Result := FData[ADataClass.GetType];
end;

function TDatapack.TNamespace.GetData(AType: TDataType): TData;
begin
  Result := FData[AType];
end;

function TDatapack.TNamespace.GetFullPath: TFileName;
begin
  Result := TPath.Combine(Datapack.DataPath, Name);
end;

function TDatapack.TNamespace.GetName: string;
begin
  Result := FName;
end;

function TDatapack.TNamespace.GetNamespacePath: string;
begin
  Result := Name;
end;

function TDatapack.TNamespace.HasNamespacePath: Boolean;
begin
  Result := True;
end;

function TDatapack.TNamespace.NameEditable: Boolean;
begin
  Result := True;
end;

function TDatapack.TNamespace.OnAddFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnAddFileSystemEntry.Access;
end;

function TDatapack.TNamespace.OnDisableData: TData.TEvent.TAccess;
begin
  Result := FOnDisableData.Access;
end;

function TDatapack.TNamespace.OnEnableData: TData.TEvent.TAccess;
begin
  Result := FOnEnableData.Access;
end;

function TDatapack.TNamespace.OnRemoveFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnRemoveFileSystemEntry.Access;
end;

procedure TDatapack.TNamespace.RemoveDataEvents(AData: TData);
begin
  AData.OnAddFileSystemEntry.Remove(FileSystemEntryAdd);
  AData.OnRemoveFileSystemEntry.Remove(FileSystemEntryRemove);
end;

procedure TDatapack.TNamespace.SetName(const Value: string);
var
  OldName: string;
  OldPath: TFileName;
begin
  if Name = Value then
    Exit;
  VerifyName(Value);
  OldPath := FullPath;
  OldName := Name;
  FName := Value;
  if not RenameFile(OldPath, FullPath) then
  begin
    FName := OldName;
    RaiseLastOSError;
  end;
end;

procedure TDatapack.TNamespace.Update;
var
  T: TDataType;
  Path: string;
begin
  for T := Low(TDataType) to High(TDataType) do
  begin
    Path := TPath.Combine(FullPath, DPClasses[T].GetFolderName);
    if not System.IOUtils.TDirectory.Exists(Path) then
    begin
      if FData[T] <> nil then
      begin
        DataDisable(FData[T]);
        FreeAndNil(FData[T]);
      end;
    end
    else
    begin
      if Data[T] = nil then
      begin
        FData[T] := DPClasses[T].Create(Self);
        DataEnable(FData[T]);
      end
      else
        FData[T].Update;
    end;
  end;
end;

{ TDatapack }

function TDatapack.AddNamespace(AName: string): TNamespace;
begin
  if FNamespaces.KeyExists(AName) then
    raise EDatapack.Create('This namespace exists already.');

  Result := TNamespace.Create(Self, AName);

  try
    System.IOUtils.TDirectory.CreateDirectory(Result.FullPath);
  except
    Result.Free;
    raise;
  end;

  FNamespaces[AName] := Result;
end;

procedure TDatapack.AddNamespaceEvents(ANamespace: TNamespace);
begin
  ANamespace.OnEnableData.Add(DataEnable);
  ANamespace.OnDisableData.Add(DataDisable);
  ANamespace.OnAddFileSystemEntry.Add(FileSystemEntryAdd);
  ANamespace.OnRemoveFileSystemEntry.Add(FileSystemEntryRemove);
end;

constructor TDatapack.Create(APath: TFileName);
begin
  FPath := APath;
  FNamespaces := TNamespaces.Create;
  FNamespaces.OnAdd.Add(NamespaceAdd);
  FNamespaces.OnRemove.Add(NamespaceRemove);
end;

procedure TDatapack.RemoveNamespace(AName: string);
var
  Namespace: TNamespace;
begin
  if not FNamespaces.Get(AName, Namespace) then
    raise EDatapack.Create('The namespace to remove could not be found.');

  if MoveToRecycleBin(Namespace.FullPath) then
    FNamespaces.Remove(AName);
end;

procedure TDatapack.RemoveNamespaceEvents(ANamespace: TNamespace);
begin
  ANamespace.OnEnableData.Remove(DataEnable);
  ANamespace.OnDisableData.Remove(DataDisable);
end;

procedure TDatapack.DataDisable(AInfo: TData.TEventInfo);
begin
  FOnDisableData.Execute(AInfo, False);
end;

procedure TDatapack.DataEnable(AInfo: TData.TEventInfo);
begin
  FOnEnableData.Execute(AInfo, False);
end;

procedure TDatapack.Delete;
begin
  raise ENotImplemented.Create('TDatapack.Delete');
end;

destructor TDatapack.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

procedure TDatapack.FileSystemEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnAddFileSystemEntry.Execute(AInfo, False);
end;

procedure TDatapack.FileSystemEntryRemove(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnRemoveFileSystemEntry.Execute(AInfo, False);
end;

function TDatapack.GetFullPath: TFileName;
begin
  Result := ExtractFilePath(FilePath);
end;

function TDatapack.GetDataPath: TFileName;
begin
  Result := TPath.Combine(FullPath, NamespaceFolderName);
end;

function TDatapack.GetName: string;
begin
  Result := ExtractFileName(FullPath);
end;

function TDatapack.GetNamespaces: TNamespaces.TReader;
begin
  Result := FNamespaces.Reader;
end;

procedure TDatapack.NamespaceAdd(AInfo: TNamespaces.TAddEventInfo);
begin
  AddNamespaceEvents(AInfo.Value);
  FOnAddNamespace.Execute(TNamespace.TEventInfo.Create(AInfo.Value));
end;

procedure TDatapack.NamespaceRemove(AInfo: TNamespaces.TRemoveEventInfo);
begin
  RemoveNamespaceEvents(AInfo.Value);
  FOnRemoveNamespace.Execute(TNamespace.TEventInfo.Create(AInfo.Value));
end;

function TDatapack.OnAddFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnAddFileSystemEntry.Access;
end;

function TDatapack.OnAddNamespace: TNamespace.TEvent.TAccess;
begin
  Result := FOnAddNamespace.Access;
end;

function TDatapack.OnDisableData: TData.TEvent.TAccess;
begin
  Result := FOnDisableData.Access;
end;

function TDatapack.OnEnableData: TData.TEvent.TAccess;
begin
  Result := FOnEnableData.Access;
end;

function TDatapack.OnRemoveFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnRemoveFileSystemEntry.Access;
end;

function TDatapack.OnRemoveNamespace: TNamespace.TEvent.TAccess;
begin
  Result := FOnRemoveNamespace.Access;
end;

procedure TDatapack.Update;
var
  CurrentNamespaces: TSet<string, TStringHasher>;
  NamespaceFullPath, NamespaceName: string;
begin
  CurrentNamespaces := FNamespaces.KeySet(hmManual); // don't rehash when empty

  try
    for NamespaceFullPath in System.IOUtils.TDirectory.GetDirectories(DataPath) do
    begin
      NamespaceName := ExtractFileName(NamespaceFullPath);
      if not CurrentNamespaces.TryRemove(NamespaceName) then
        FNamespaces[NamespaceName] := TNamespace.Create(Self, NamespaceName)
      else
        FNamespaces[NamespaceName].Update;
    end;

    for NamespaceName in CurrentNamespaces do
      FNamespaces.Remove(NamespaceName);

  finally
    CurrentNamespaces.Free;

  end;
end;

class procedure TDatapack.VerifyName(AName: string);
var
  C: Char;
begin
  if AName.IsEmpty then
    raise EDatapack.Create('The name cannot be empty.');

  for C in AName do
    if not CharInSet(C, NamespaceChars) then
      raise EDatapack.CreateFmt('The name can only contain %s, but contains "%s".', [NamespaceCharsText, C]);
end;

{ TDatapack.TData }

constructor TDatapack.TData.Create(ANamespace: TNamespace);
begin
  FNamespace := ANamespace;
end;

procedure TDatapack.TData.Delete;
begin
  if MoveToRecycleBin(FullPath) then
    Namespace.DisableData(GetType);
end;

function TDatapack.TData.GetDisplayName: string;
begin
  Result := GetDisplayNamePlural;
end;

class function TDatapack.TData.GetDisplayNameNormal: string;
begin
  Result := DPDisplayNames[GetType];
end;

class function TDatapack.TData.GetDisplayNamePlural: string;
begin
  Result := DPDisplayNamesPlural[GetType];
end;

class function TDatapack.TData.GetFileExtension: string;
begin
  Result := DPFileExtensions[GetType];
end;

class function TDatapack.TData.GetFolderName: string;
begin
  Result := DPFolderNames[GetType];
end;

function TDatapack.TData.GetFullPath: TFileName;
begin
  Result := TPath.Combine(Namespace.FullPath, GetFolderName);
end;

function TDatapack.TData.GetName: string;
begin
  Result := GetDisplayName;
end;

function TDatapack.TData.OnAddFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnAddFileSystemEntry.Access;
end;

function TDatapack.TData.OnRemoveFileSystemEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnRemoveFileSystemEntry.Access;
end;

{ TDPAdvancements }

class function TDPAdvancements.GetType: TDatapack.TDataType;
begin
  Result := dtAdvancement;
end;

{ TDPFunctions }

class function TDPFunctions.GetType: TDatapack.TDataType;
begin
  Result := dtFunction;
end;

{ TDPLootTables }

class function TDPLootTables.GetType: TDatapack.TDataType;
begin
  Result := dtLootTable;
end;

{ TDPRecipes }

class function TDPRecipes.GetType: TDatapack.TDataType;
begin
  Result := dtRecipe;
end;

{ TDPStructures }

class function TDPStructures.GetType: TDatapack.TDataType;
begin
  Result := dtStructure;
end;

{ TDPTags }

class function TDPTags.GetType: TDatapack.TDataType;
begin
  Result := dtTag;
end;

{ TDatapack.TFileSystemEntry }

constructor TDatapack.TFileSystemEntry.Create(AParent: TDirectory; AName: string);
begin
  FParent := AParent;
  if Parent <> nil then
    FData := Parent.Data;
  FName := AName;
end;

function TDatapack.TFileSystemEntry.GetDatapack: TDatapack;
begin
  Result := Namespace.Datapack;
end;

function TDatapack.TFileSystemEntry.GetFullPath: TFileName;
begin
  Result := TPath.Combine(Parent.FullPath, Name);
end;

function TDatapack.TFileSystemEntry.GetName: string;
begin
  Result := FName;
end;

function TDatapack.TFileSystemEntry.GetNamespace: TNamespace;
begin
  Result := Data.Namespace;
end;

function TDatapack.TFileSystemEntry.NameEditable: Boolean;
begin
  Result := True;
end;

function TDatapack.TFileSystemEntry.OnRename: TEvent.TAccess;
begin
  Result := FOnRename.Access;
end;

procedure TDatapack.TFileSystemEntry.SetName(const Value: string);
var
  OldName: string;
  OldPath: TFileName;
  FileExists: Boolean;
begin
  if Name = Value then
    Exit;
  VerifyName(Value);
  OldName := Name;
  OldPath := FullPath;
  Parent.ChildRename(Self, Value);
  FileExists := not(Self is TFile) or (Self is TFile) and TFile(Self).FileExists;
  FName := Value;
  if FileExists and not RenameFile(OldPath, FullPath) then
  begin
    FName := OldName;
    RaiseLastOSError;
  end;
  FOnRename.Execute(TEventInfo.Create(Self));
end;

{ TDatapack.TDirectory }

function TDatapack.TDirectory.AddDirectory(AName: string): TDirectory;
begin
  Result := TDirectory.Create(Self, AName);
  System.IOUtils.TDirectory.CreateDirectory(Result.FullPath);
  FEntries[AName] := Result;
end;

procedure TDatapack.TDirectory.AddEntryEvents(AEntry: TFileSystemEntry);
begin
  if AEntry is TDirectory then
  begin
    TDirectory(AEntry).OnAddEntry.Add(ChildEntryAdd);
    TDirectory(AEntry).OnRemoveEntry.Add(ChildEntryRemove);
  end;
end;

function TDatapack.TDirectory.AddFile(AName: string): TFile;
begin
  Result := TFile.Create(Self, AName);
  FEntries[AName] := Result;
end;

procedure TDatapack.TDirectory.ChildEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnAddEntry.Execute(AInfo, False);
end;

procedure TDatapack.TDirectory.ChildEntryRemove(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnRemoveEntry.Execute(AInfo, False);
end;

procedure TDatapack.TDirectory.ChildRename(AEntry: TFileSystemEntry; ANewName: string);
begin
  if Entries.KeyExists(ANewName) then
    raise EDatapack.CreateFmt('Directory already contains entry with name "%s".', [ANewName]);
  FRename := True;
  FEntries.OwnsValues := False;
  FEntries[ANewName] := FEntries[AEntry.Name];
  FEntries.Remove(AEntry.Name);
  FEntries.OwnsValues := True;
  FRename := False;
end;

constructor TDatapack.TDirectory.Create(AParent: TDirectory; AName: string);
begin
  inherited;
  FEntries := TEntries.Create;
  FEntries.OnAdd.Add(EntryAdd);
  FEntries.OnRemove.Add(EntryRemove);
  FEntries.OnValueChange.Add(EntryValueChange);
end;

procedure TDatapack.TDirectory.Delete;
begin
  if MoveToRecycleBin(FullPath) then
    Parent.Remove(Name);
end;

destructor TDatapack.TDirectory.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TDatapack.TDirectory.EntryAdd(AInfo: TEntries.TAddEventInfo);
begin
  if FRename then
    Exit;
  AddEntryEvents(AInfo.Value);
  FOnAddEntry.Execute(TFileSystemEntry.TEventInfo.Create(AInfo.Value));
end;

procedure TDatapack.TDirectory.EntryRemove(AInfo: TEntries.TRemoveEventInfo);
begin
  if FRename then
    Exit;
  RemoveEntryEvents(AInfo.Value);
  FOnRemoveEntry.Execute(TFileSystemEntry.TEventInfo.Create(AInfo.Value));
end;

procedure TDatapack.TDirectory.EntryValueChange(AInfo: TEntries.TValueChangeEventInfo);
begin
  RemoveEntryEvents(AInfo.OldValue);
  FOnRemoveEntry.Execute(TFileSystemEntry.TEventInfo.Create(AInfo.OldValue));
  AddEntryEvents(AInfo.NewValue);
  FOnAddEntry.Execute(TFileSystemEntry.TEventInfo.Create(AInfo.NewValue));
end;

function TDatapack.TDirectory.GetEntries: TEntries.TReader;
begin
  Result := FEntries.Reader;
end;

function TDatapack.TDirectory.GetNamespacePath: string;
begin
  Result := Parent.NamespacePath + Name + '/';
end;

function TDatapack.TDirectory.HasNamespacePath: Boolean;
begin
  Result := True;
end;

function TDatapack.TDirectory.OnAddEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnAddEntry.Access;
end;

function TDatapack.TDirectory.OnRemoveEntry: TFileSystemEntry.TEvent.TAccess;
begin
  Result := FOnRemoveEntry.Access;
end;

procedure TDatapack.TDirectory.Remove(AName: string);
begin
  ;
  FEntries.Remove(AName);
end;

procedure TDatapack.TDirectory.RemoveEntryEvents(AEntry: TFileSystemEntry);
begin
  if AEntry is TDirectory then
  begin
    TDirectory(AEntry).OnAddEntry.Remove(ChildEntryAdd);
    TDirectory(AEntry).OnRemoveEntry.Remove(ChildEntryRemove);
  end;
end;

procedure TDatapack.TDirectory.Update;
var
  CurrentEntries: TSet<string, TStringHasher>;
  DataFullPath, DataName: string;
  Entry: TFileSystemEntry;
  IsDir, Add: Boolean;
begin
  CurrentEntries := Entries.KeySet;

  try
    if not System.IOUtils.TDirectory.Exists(FullPath) then
      Exit;

    for DataFullPath in System.IOUtils.TDirectory.GetFileSystemEntries(FullPath) do
    begin
      DataName := ExtractFileName(DataFullPath);

      IsDir := DirectoryExists(DataFullPath);

      if CurrentEntries.TryRemove(DataName) then
        Entry := Entries[DataName]
      else
        Entry := nil;

      Add := False;
      if IsDir then
      begin
        if not(Entry is TDirectory) then
        begin
          Entry := TDirectory.Create(Self, DataName);
          Add := True;
        end;
      end
      else if not(Entry is TFile) then
      begin
        Entry := TFile.Create(Self, DataName);
        Add := True;
      end;

      if Add then
        FEntries[DataName] := Entry
      else
        Entry.Update;
    end;

    for DataName in CurrentEntries do
      FEntries.Remove(DataName);

  finally
    CurrentEntries.Free;
  end;
end;

{ TDatapack.TFile }

procedure TDatapack.TFile.BeforeDestruction;
begin
  FOnRemove.Execute(TEventInfo.Create(Self));
end;

function TDatapack.TFile.GetFileExists: Boolean;
begin
  Update;
  Result := FFileExists;
end;

function TDatapack.TFile.GetFullPath: TFileName;
begin
  Result := TPath.Combine(Parent.FullPath, Name);
end;

procedure TDatapack.TFile.Delete;
begin
  if FileExists then
  begin
    if MoveToRecycleBin(FullPath) then
      Parent.Remove(Name);
  end
  else
    Parent.Remove(Name);
end;

function TDatapack.TFile.GetDisplayName: string;
begin
  Result := ChangeFileExt(Name, '');
end;

function TDatapack.TFile.GetNamespacePath: string;
begin
  Result := Parent.NamespacePath + DisplayName;
end;

function TDatapack.TFile.GetTimestamp: TDateTime;
begin
  Result := System.IOUtils.TFile.GetLastWriteTime(FullPath);
end;

function TDatapack.TFile.HasNamespacePath: Boolean;
begin
  Result := True;
end;

function TDatapack.TFile.OnChange: TEvent.TAccess;
begin
  Result := FOnChange.Access;
end;

function TDatapack.TFile.OnFileRemoved: TEvent.TAccess;
begin
  Result := FOnFileRemoved.Access;
end;

function TDatapack.TFile.OnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

procedure TDatapack.TFile.SetDisplayName(const Value: string);
begin
  if Name = Value then
    Exit;
  VerifyName(Value);
  Name := Value + ExtractFileExt(Name);
end;

procedure TDatapack.TFile.Update;
var
  NewTimestamp: TDateTime;
  FileExistsNow: Boolean;
begin
  FileExistsNow := System.IOUtils.TFile.Exists(FullPath);

  if FFileExists and not FileExistsNow then
  begin
    FFileExists := False;
    FOnFileRemoved.Execute(TEventInfo.Create(Self));
  end;

  if FileExistsNow then
  begin
    FFileExists := True;
    NewTimestamp := GetTimestamp;
    if FTimestamp = NewTimestamp then
      Exit;
    FTimestamp := NewTimestamp;
    FOnChange.Execute(TEventInfo.Create(Self));
  end;
end;

{ TDatapack.TRootDirectory }

constructor TDatapack.TRootDirectory.Create(AData: TData; AName: string);
begin
  inherited Create(nil, AName);
  FData := AData;
end;

procedure TDatapack.TRootDirectory.Delete;
begin
  if Data is TDataMulti then
    raise ENotImplemented.Create('TDataMulti(Data).Remove(Self);');
end;

function TDatapack.TRootDirectory.GetFullPath: TFileName;
begin
  if Name.IsEmpty then
    Result := Data.FullPath
  else
    Result := TPath.Combine(Namespace.FullPath, Name);
end;

function TDatapack.TRootDirectory.GetNamespacePath: string;
begin
  Result := Namespace.NamespacePath + ':';
end;

{ TDatapack.TDataSimple }

constructor TDatapack.TDataSimple.Create(ANamespace: TNamespace);
begin
  inherited;
  FDirectory := TRootDirectory.Create(Self);
  FDirectory.OnAddEntry.Add(FileSystemEntryAdd);
  FDirectory.OnRemoveEntry.Add(FileSystemEntryRemove);
end;

destructor TDatapack.TDataSimple.Destroy;
begin
  FDirectory.Free;
  inherited;
end;

procedure TDatapack.TDataSimple.FileSystemEntryAdd(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnAddFileSystemEntry.Execute(AInfo, False);
end;

procedure TDatapack.TDataSimple.FileSystemEntryRemove(AInfo: TFileSystemEntry.TEventInfo);
begin
  FOnRemoveFileSystemEntry.Execute(AInfo, False);
end;

procedure TDatapack.TDataSimple.Update;
begin
  Directory.Update;
end;

{ TDatapackBase }

procedure TDatapackBase.AfterConstruction;
begin
  Update;
end;

function TDatapackBase.GetDisplayName: string;
begin
  Result := Name;
end;

function TDatapackBase.GetNamespacePath: string;
begin
  raise ENotSupportedException.Create('Namespacepath not available.');
end;

function TDatapackBase.HasNamespacePath: Boolean;
begin
  Result := False;
end;

class function TDatapackBase.MoveToRecycleBin(APath: TFileName): Boolean;
var
  FOS: TSHFileOpStruct;
  ErrorCode: Integer;
begin
  ZeroMemory(@FOS, SizeOf(TSHFileOpStruct));
  FOS.wFunc := FO_DELETE;
  FOS.pFrom := PChar(APath + #0);
  FOS.fFlags := FOF_ALLOWUNDO;
  ErrorCode := SHFileOperation(FOS);
  Result := ErrorCode = 0;
end;

function TDatapackBase.NameEditable: Boolean;
begin
  Result := False;
end;

procedure TDatapackBase.SetDisplayName(const Value: string);
begin
  Name := Value;
end;

procedure TDatapackBase.SetName(const Value: string);
begin
  raise ENotSupportedException.Create('Name not editable.');
end;

procedure TDatapackBase.Update;
begin
  // nothing by default
end;

{ TDatapack.TDataMulti }

procedure TDatapack.TDataMulti.Update;
begin
  // TODO: TDatapack.TDataMulti.Update
end;

end.
