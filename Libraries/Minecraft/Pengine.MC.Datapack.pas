unit Pengine.MC.Datapack;

interface

uses
  System.IOUtils,
  System.SysUtils,
  System.Types,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher;

type

  TDatapack = class
  public type

    TDataType = (
      dtAdvancement,
      dtFunction,
      dtLootTable,
      dtRecipe,
      dtStructure,
      dtTag
      );

    TNamespace = class;
      
    TData = class abstract
    public type

      TDirectories = TToObjectMap<string, TData, TStringHasher>;
      TFiles = TSet<string, TStringHasher>;

    private
      FNamespace: TNamespace;
      FParent: TData;
      FName: string;
      FDirectories: TDirectories;
      FFiles: TFiles;

      function GetDirectories: TDirectories.TReader;
      function GetFiles: TFiles.TReader;
      function GetFullPath: string;
      function GetFullName: string;
      function GetFullNameNoNamespace: string;

    public
      constructor Create(ANamespace: TNamespace); overload; virtual;
      constructor Create(ANamespace: TNamespace; AParent: TData; AName: string); overload; virtual;
      destructor Destroy; override;
      
      class function GetDisplayName: string; 
      class function GetDisplayNamePlural: string; 
      class function GetFileExtension: string;
      class function GetFolderName: string; 
      class function GetType: TDataType; virtual; abstract;

      procedure ScanChanges; virtual;

      property Name: string read FName;
      property Namespace: TNamespace read FNamespace;
      property Parent: TData read FParent;
      property FullName: string read GetFullName;
      property FullPath: string read GetFullPath;

      function FullPathTo(AName: string): string;
      function FullNameTo(AName: string): string;

      property Directories: TDirectories.TReader read GetDirectories;
      property Files: TFiles.TReader read GetFiles;

    end;

    TDataClass = class of TData;

    TNamespace = class
    public type

      TDataArray = array [TDataType] of TData;

    private
      FDatapack: TDatapack;
      FName: string;
      FData: TDataArray;

      function GetData(AType: TDataType): TData;
      function GetFullPath: string;

    public
      constructor Create(ADatapack: TDatapack; AName: string);
      destructor Destroy; override;
      
      property Datapack: TDatapack read FDatapack;
      property Name: string read FName;
      property FullPath: string read GetFullPath;

      property Data[AType: TDataType]: TData read GetData;
      function Get<T: TData>: T;

      procedure ScanChanges;

    end;

    TNamespaces = TToObjectMap<string, TNamespace, TStringHasher>;

  public const

    NamespaceFolderName = 'data';

  private
    FPath: TFileName;
    FNamespaces: TNamespaces;

    function GetNamespaces: TNamespaces.TReader;
    function GetNamespacePath: TFileName;
    function GetRootPath: TFileName;

  public
    constructor Create(APath: TFileName);
    destructor Destroy; override;

    procedure ScanChanges;

    property Namespaces: TNamespaces.TReader read GetNamespaces;
    function AddNamespace(AName: string): TNamespace;
    procedure RemoveNamespace(AName: string);

    property FilePath: TFileName read FPath;
    property RootPath: TFileName read GetRootPath;
    property NamespacePath: TFileName read GetNamespacePath;

  end;

  TDPAdvancements = class(TDatapack.TData)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPFunctions = class(TDatapack.TData)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPLootTables = class(TDatapack.TData)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPRecipes = class(TDatapack.TData)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPStructures = class(TDatapack.TData)
  public
    class function GetType: TDatapack.TDataType; override;

  end;

  TDPTags = class(TDatapack.TData)
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

constructor TDatapack.TNamespace.Create(ADatapack: TDatapack; AName: string);
begin
  FDatapack := ADatapack;
  FName := AName;
  ScanChanges;
end;

destructor TDatapack.TNamespace.Destroy;
var
  DataType: TDataType;
begin
  for DataType := Low(TDataType) to High(TDataType) do
    FData[DataType].Free;
  inherited;
end;

function TDatapack.TNamespace.Get<T>: T;
begin
  Result := T(FData[T.GetType]);
end;

function TDatapack.TNamespace.GetData(AType: TDataType): TData;
begin
  Result := FData[AType];
end;

function TDatapack.TNamespace.GetFullPath: string;
begin
  Result := TPath.Combine(Datapack.NamespacePath, Name);
end;

procedure TDatapack.TNamespace.ScanChanges;
var
  T: TDataType;
  Path: string;
begin
  for T := Low(TDataType) to High(TDataType) do
  begin
    Path := TPath.Combine(FullPath, DPClasses[T].GetFolderName);
    if not TDirectory.Exists(Path) then
      FreeAndNil(FData[T])
    else if Data[T] = nil then
      FData[T] := DPClasses[T].Create(Self);
  end;                                 
end;

{ TDatapack }

function TDatapack.AddNamespace(AName: string): TNamespace;
begin
  Result := TNamespace.Create(Self, AName);
  FNamespaces[AName] := Result;
end;

constructor TDatapack.Create(APath: TFileName);
begin
  FPath := APath;
  FNamespaces := TNamespaces.Create;
  ScanChanges;
end;

procedure TDatapack.RemoveNamespace(AName: string);
begin
  FNamespaces.Remove(AName);
end;

destructor TDatapack.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

function TDatapack.GetNamespacePath: TFileName;
begin
  Result := TPath.Combine(ExtractFilePath(FilePath), NamespaceFolderName);
end;

function TDatapack.GetNamespaces: TNamespaces.TReader;
begin
  Result := FNamespaces.Reader;
end;

function TDatapack.GetRootPath: TFileName;
begin
  Result := ExtractFilePath(FilePath);
end;

procedure TDatapack.ScanChanges;
var
  CurrentNamespaces: TSet<string, TStringHasher>;
  NamespaceFullPath, Namespace: string;
begin
  // A set of all currently known namespaces
  CurrentNamespaces := Namespaces.KeySet;

  // Scan for existing namespaces
  for NamespaceFullPath in TDirectory.GetDirectories(NamespacePath) do
  begin
    Namespace := ExtractFileName(NamespaceFullPath);
    // Remove each existing namespace from the set, so in the end we are left with namespaces that do not exist anymore
    if CurrentNamespaces.TryRemove(Namespace) then
    begin
      // If the namespace did exist, scan for data changes
      FNamespaces[Namespace].ScanChanges;
    end
    else
    begin
      // If the namespace did not exist, add it to the list of known namespaces
      FNamespaces[Namespace] := TNamespace.Create(Self, Namespace);
    end;
  end;

  for NamespaceFullPath in CurrentNamespaces do
    FNamespaces.Remove(NamespaceFullPath);

  CurrentNamespaces.Free;
end;

{ TDatapack.TData }

constructor TDatapack.TData.Create(ANamespace: TNamespace);
begin
  FNamespace := ANamespace;
  FDirectories := TDirectories.Create;
  FFiles := TFiles.Create;
  ScanChanges;
end;

constructor TDatapack.TData.Create(ANamespace: TNamespace; AParent: TData; AName: string);
begin
  FParent := AParent;
  FName := AName;
  Create(ANamespace);
end;

destructor TDatapack.TData.Destroy;
begin
  FDirectories.Free;
  FFiles.Free;
  inherited;
end;

function TDatapack.TData.FullNameTo(AName: string): string;
begin
  Result := FullName + ChangeFileExt(AName, '');
end;

function TDatapack.TData.FullPathTo(AName: string): string;
begin
  Result := TPath.Combine(FullPath, AName);
end;

function TDatapack.TData.GetDirectories: TDirectories.TReader;
begin
  Result := FDirectories.Reader;
end;

class function TDatapack.TData.GetDisplayName: string;
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

function TDatapack.TData.GetFiles: TFiles.TReader;
begin
  Result := FFiles.Reader;
end;

class function TDatapack.TData.GetFolderName: string;
begin                               
  Result := DPFolderNames[GetType];
end;

function TDatapack.TData.GetFullName: string;
begin
  Result := Namespace.Name + ':' + GetFullNameNoNamespace;
end;

function TDatapack.TData.GetFullNameNoNamespace: string;
begin
  if Parent = nil then
    Result := ''
  else
    Result := Parent.GetFullNameNoNamespace + Name + '/';
end;

function TDatapack.TData.GetFullPath: string;
begin
  if Parent <> nil then
    Result := TPath.Combine(Parent.FullPath, Name)
  else
    Result := TPath.Combine(Namespace.FullPath, GetFolderName);
end;

procedure TDatapack.TData.ScanChanges;
var
  CurrentDirectories, CurrentFiles: TSet<string, TStringHasher>;
  DataFullPath, DataName: string;
begin
  CurrentDirectories := FDirectories.KeySet;    
  CurrentFiles := FFiles.Copy;
  
  for DataFullPath in TDirectory.GetFileSystemEntries(FullPath) do
  begin                                
    DataName := ExtractFileName(DataFullPath);

    if DirectoryExists(DataFullPath) then
    begin
      if CurrentDirectories.TryRemove(DataName) then
        FDirectories[DataName].ScanChanges
      else
        FDirectories[DataName] := TDataClass(Self.ClassType).Create(Namespace, Self, DataName);
    end
    else if ExtractFileExt(DataName) = GetFileExtension then
    begin
      if not CurrentFiles.TryRemove(DataFullPath) then
        FFiles[DataName] := True;
    end;
  end;

  for DataName in CurrentDirectories do
    FDirectories.Remove(DataName);

  for DataName in CurrentFiles do
    FFiles.Remove(DataName);

  CurrentDirectories.Free;
  CurrentFiles.Free;
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

end.
