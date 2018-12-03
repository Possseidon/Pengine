unit Pengine.MC.Item;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.Types,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Settings,
  Pengine.Parsing,
  Pengine.Utility,
  Pengine.JSON,

  Pengine.MC.Namespace,
  Pengine.MC.NBT,
  Pengine.MC.General,
  Pengine.IntMaths;

type

  // TODO: Some really horrible code duplicate with block (and fluid) tags

  /// <summary>An item type.</summary>
  TItemType = class
  private
    FNSPath: TNSPath;

  public
    constructor Create(ANSPath: TNSPath);

    property NSPath: TNSPath read FNSPath;

  end;

  TItemTypes = TRefArray<TItemType>;

  /// <summary>A collection of all item types.</summary>
  TItemTypeCollection = class
  public type

    TMap = TToObjectMap<TNSPath, TItemType, TNSPathHasher>;

  private
    FMap: TMap;
    FOrder: TItemTypes;
    FSorted: TItemTypes;

    function GetOrder: TItemTypes.TReader;
    function GetSorted: TItemTypes.TReader;
    function GetCount: Integer;

  public
    constructor Create(AJObject: TJObject);
    destructor Destroy; override;

    function Exists(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out AItemType: TItemType): Boolean;

    property Count: Integer read GetCount;

    /// <summary>All item types as found in the file.</summary>
    property Order: TItemTypes.TReader read GetOrder;
    /// <summary>All item types sorted alphabetically.</summary>
    property Sorted: TItemTypes.TReader read GetSorted;

  end;

  /// <summary>Loads available items from a file.</summary>
  TItemSettings = class(TSettings)
  public const

    DefaultPath = 'Data\reports\items.json';

  private
    FItems: TItemTypeCollection;
    FPath: string;
    FNamespacePrefix: Boolean;

    procedure SetPath(const Value: string);

  protected
    procedure DoReload; override;

    class function GetNameForVersion(AVersion: Integer): string; override;

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property NamespacePrefix: Boolean read FNamespacePrefix write FNamespacePrefix;

    property Items: TItemTypeCollection read FItems;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

  /// <summary>
  /// <p>An item stack, defined by namespace identifier and optional NBT.</p>
  /// <p>Example: <c>minecraft:iron_sword{Damage:50s}</c></p>
  /// </summary>
  /// <remarks>Despite being called "stack" this does NOT contain an amount.</remarks>
  TItemStack = class
  public type

    IParser = IObjectParser<TItemStack>;

    /// <summary>Parses a whole item stack.</summary>
    TParser = class(TObjectParser<TItemStack>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TItemSuggestions = class(TParseSuggestionsGenerated<TParser>)
    protected
      procedure Generate; override;

    end;

  private
    FNSPath: TNSPath;
    FNBT: TOwned<TNBTCompound>;

  public
    constructor Create(ANSPath: TNSPath); overload;

    class function Parser: IParser;

    property NSPath: TNSPath read FNSPath write FNSPath;

    property NBT: TOwned<TNBTCompound> read FNBT;

    function Format: string; virtual;

  end;

  TItemTagCollection = class;

  /// <summary>An item tag, which maps to various items or other item tags.</summary>
  TItemTag = class
  private
    FReplace: Boolean;
    FNSPath: TNSPath;
    FItemTypes: TItemTypes;
    FSorted: TItemTypes;

    function GetItemTypes: TItemTypes.TReader;
    function GetSorted: TItemTypes.TReader;

  public
    constructor Create(AItemTags: TItemTagCollection; ANSPath: TNSPath; AJObject: TJObject);
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath;

    property ItemTypes: TItemTypes.TReader read GetItemTypes;
    property Sorted: TItemTypes.TReader read GetSorted;

  end;

  TItemTags = TRefArray<TItemTag>;

  /// <summary>A collection of all item tags.</summary>
  TItemTagCollection = class
  public type

    TMap = TToObjectMap<TNSPath, TItemTag, TNSPathHasher>;

  private
    FItemTypes: TItemTypeCollection;
    FMap: TMap;
    FTags: TItemTags;
    FPath: string;

    function GetTags: TItemTags.TReader;

    function Load(AFileName: TFileName): TItemTag;
    function LoadFromName(ANSPath: TNSPath): TItemTag;

  public
    constructor Create(AItemTypes: TItemTypeCollection; APath: string);
    destructor Destroy; override;

    property Path: string read FPath;

    property ItemTypes: TItemTypeCollection read FItemTypes;

    function Exists(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out AItemTag: TItemTag): Boolean;

    /// <summary>All item tags sorted alphabetically.</summary>
    property Tags: TItemTags.TReader read GetTags;

  end;

  /// <summary>Loads available item tags from a directory.</summary>
  TItemTagSettings = class(TSettings)
  public const

    DefaultPath = 'Data\tags\items';

  private
    FItemTags: TItemTagCollection;
    FPath: string;
    FNamespacePrefix: Boolean;

    procedure SetPath(const Value: string);

  protected
    procedure DoReload; override;

    class function GetNameForVersion(AVersion: Integer): string; override;

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property NamespacePrefix: Boolean read FNamespacePrefix write FNamespacePrefix;

    property ItemTags: TItemTagCollection read FItemTags;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

  /// <summary>
  /// <p>An item tag defined by namespace identifier and optional NBT.</p>
  /// <p>Example: <c>#minecraft:banners{Patterns:[]}</c></p>
  /// </summary>
  TItemStackTag = class(TItemStack)
  public type

    IParser = IObjectParser<TItemStackTag>;

    /// <summary>Parses a whole item state.</summary>
    TParser = class(TObjectParser<TItemStackTag>, IParser)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TItemTagSuggestions = class(TParseSuggestionsGenerated<TParser>)
    protected
      procedure Generate; override;

    end;

  public
    class function Parser: TParser;

    function Format: string; override;

  end;

  TItemSlotClass = class of TItemSlot;

  TItemSlot = class
  public type

    TType = (
      stArmor,
      stContainer,
      stEnderchest,
      stHorse,
      stHotbar,
      stInventory,
      stVillager,
      stWeapon
      );

    IParser = IObjectParser<TItemSlot>;

    TParser = class(TObjectParser<TItemSlot>, IParser)
    public type

      TNameSuggestions = class(TParseSuggestionsSimple<TParser>)
      public
        class function GetCount: Integer; override;
        class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

      end;

      TSubNameSuggestions = class(TParseSuggestionsGenerated<TParser>)
      private
        FSlotClass: TItemSlotClass;

      protected
        procedure Generate; override;

      public
        constructor Create(ASlotClass: TItemSlotClass);

      end;

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

  private
    FHasIndex: Boolean;
    FIndex: Integer;
    FUseNameIndex: Boolean;

  public
    constructor Create;

    class function Parser: IParser;

    class function GetType: TType; virtual; abstract;
    class function GetName: string;
    class function GetTypeFromName(AName: string; out AType: TType): Boolean;
    class function GetIndexCount: Integer; virtual;
    class function GetSubNameCount: Integer; virtual;
    class function GetSubName(AIndex: Integer): string; virtual;
    class function GetIndexFromSubName(AName: string): Integer;
    class function IndexRequired: Boolean; virtual;

    procedure ResetIndex;
    procedure SetIndex(AIndex: Integer);
    procedure SetNameIndex(AIndex: Integer);

    function Format: string;

  end;

  TItemSlotArmor = class(TItemSlot)
  public const

    SubNames: array [0 .. 3] of string = (
      'head',
      'chest',
      'legs',
      'feet'
      );

  public
    class function GetType: TItemSlot.TType; override;
    class function GetSubNameCount: Integer; override;
    class function GetSubName(AIndex: Integer): string; override;

  end;

  TItemSlotContainer = class(TItemSlot)
  public
    class function GetType: TItemSlot.TType; override;
    class function GetIndexCount: Integer; override;

  end;

  TItemSlotEnderchest = class(TItemSlot)
  public
    class function GetType: TItemSlot.TType; override;
    class function GetIndexCount: Integer; override;

  end;

  TItemSlotHorse = class(TItemSlot)
  public const

    SubNames: array [0 .. 2] of string = (
      'armor',
      'chest',
      'saddle'
      );

  public
    class function GetType: TItemSlot.TType; override;
    class function GetIndexCount: Integer; override;
    class function GetSubNameCount: Integer; override;
    class function GetSubName(AIndex: Integer): string; override;

  end;

  TItemSlotHotbar = class(TItemSlot)
  public
    class function GetType: TItemSlot.TType; override;
    class function GetIndexCount: Integer; override;

  end;

  TItemSlotInventory = class(TItemSlot)
  public
    class function GetType: TItemSlot.TType; override;
    class function GetIndexCount: Integer; override;

  end;

  TItemSlotVillager = class(TItemSlot)
  public
    class function GetType: TItemSlot.TType; override;
    class function GetIndexCount: Integer; override;

  end;

  TItemSlotWeapon = class(TItemSlot)
  public const

    SubNames: array [0 .. 1] of string = (
      'mainhand',
      'offhand'
      );

  public
    class function GetType: TItemSlot.TType; override;
    class function GetSubNameCount: Integer; override;
    class function GetSubName(AIndex: Integer): string; override;
    class function IndexRequired: Boolean; override;

  end;

const

  ItemSlotClasses: array [TItemSlot.TType] of TItemSlotClass = (
    TItemSlotArmor,
    TItemSlotContainer,
    TItemSlotEnderchest,
    TItemSlotHorse,
    TItemSlotHotbar,
    TItemSlotInventory,
    TItemSlotVillager,
    TItemSlotWeapon
    );

  ItemSlotNames: array [TItemSlot.TType] of string = (
    'armor',
    'container',
    'enderchest',
    'horse',
    'hotbar',
    'inventory',
    'villager',
    'weapon'
    );

implementation

{ TItemTypeCollection }

constructor TItemTypeCollection.Create(AJObject: TJObject);
var
  JPair: TJPair;
  ItemState: TItemType;
begin
  FMap := TMap.Create;
  FOrder := TItemTypes.Create;

  for JPair in AJObject do
  begin
    ItemState := TItemType.Create(JPair.Key);
    FMap[ItemState.NSPath] := ItemState;
    FOrder.Add(ItemState);
  end;

  FSorted := FOrder.Copy;
  FSorted.Sort(
    function(A, B: TItemType): Boolean
    begin
      Result := A.NSPath < B.NSPath;
    end);
end;

destructor TItemTypeCollection.Destroy;
begin
  FMap.Free;
  FOrder.Free;
  FSorted.Free;
  inherited;
end;

function TItemTypeCollection.Exists(ANSPath: TNSPath): Boolean;
begin
  Result := FMap.KeyExists(ANSPath);
end;

function TItemTypeCollection.Get(ANSPath: TNSPath; out AItemType: TItemType): Boolean;
begin
  Result := FMap.Get(ANSPath, AItemType);
end;

function TItemTypeCollection.GetCount: Integer;
begin
  Result := FOrder.Count;
end;

function TItemTypeCollection.GetOrder: TItemTypes.TReader;
begin
  Result := FOrder.Reader;
end;

function TItemTypeCollection.GetSorted: TItemTypes.TReader;
begin
  Result := FSorted.Reader;
end;

{ TItemSettings }

procedure TItemSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  inherited;
  with ASerializer do
  begin
    Define('path', FPath);
    Define('namespace_prefix', FNamespacePrefix);
  end;
end;

destructor TItemSettings.Destroy;
begin
  FItems.Free;
  inherited;
end;

class function TItemSettings.GetDescription: string;
begin
  Result := 'Path configuration for items in items.json file.';
end;

class function TItemSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_items';
end;

class function TItemSettings.GetTitle: string;
begin
  Result := 'Items';
end;

procedure TItemSettings.DoReload;
var
  ItemsText: string;
  JItems: TJObject;
begin
  FreeAndNil(FItems);

  if TFile.Exists(Path) then
  begin
    ItemsText := TFile.ReadAllText(Path);
    JItems := TJObject.Parse(ItemsText);
  end
  else
    JItems := TJObject.Create;

  try
    FItems := TItemTypeCollection.Create(JItems);
  finally
    JItems.Free;
  end;
end;

procedure TItemSettings.SetDefaults;
begin
  FPath := DefaultPath;
  FNamespacePrefix := True;
end;

procedure TItemSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

{ TItemType }

constructor TItemType.Create(ANSPath: TNSPath);
begin
  FNSPath := ANSPath;
end;

{ TItemStack.TParser }

class function TItemStack.TParser.GetResultName: string;
begin
  Result := 'Item-Stack';
end;

function TItemStack.TParser.Parse: Boolean;
var
  Marker: TLogMarker;
  NSPathString: string;
  NSPath: TNSPath;
  ItemType: TItemType;
  ItemExists: Boolean;
begin
  Marker := GetMarker;

  BeginSuggestions(TItemSuggestions.Create);

  NSPathString := ReadWhile(NamespacePathChars);

  EndSuggestions;

  if NSPathString.IsEmpty then
    Exit(False);
  NSPath := NSPathString;

  ItemExists := RootSettingsG.Get<TItemSettings>.Items.Get(NSPath, ItemType);
  if not ItemExists then
    Log(Marker, '"%s" is not a valid item.', [NSPath.Format]);

  ParseResult := TItemStack.Create(NSPath);

  ParseResult.NBT.Value := TNBTCompound.Parser.Optional(Info);

  Result := True;
end;

{ TItemStack.TItemSuggestions }

procedure TItemStack.TItemSuggestions.Generate;
var
  Item: TItemType;
  Settings: TItemSettings;
begin
  Settings := RootSettingsG.Get<TItemSettings>;
  for Item in Settings.Items.Order do
    AddSuggestion(ParseSuggestion(Item.NSPath.Format(False), Item.NSPath.Format(False)));
  AddSuggestion(ParseSuggestion(TNSPath.Empty, TNSPath.Empty));
  for Item in Settings.Items.Order do
    AddSuggestion(ParseSuggestion(Item.NSPath, Item.NSPath));
end;

{ TItemStack }

constructor TItemStack.Create(ANSPath: TNSPath);
begin
  FNSPath := ANSPath;
end;

function TItemStack.Format: string;
begin
  Result := NSPath.Format(RootSettingsG.Get<TItemSettings>.NamespacePrefix);
  if NBT.HasValue and not NBT.Value.Empty then
    Result := Result + NBT.Value.Format;
end;

class function TItemStack.Parser: IParser;
begin
  Result := TParser.Create;
end;

{ TItemTag }

constructor TItemTag.Create(AItemTags: TItemTagCollection; ANSPath: TNSPath; AJObject: TJObject);
var
  JValue: TJValue;
  Value: string;
  ItemType: TItemType;
begin
  FNSPath := ANSPath;
  FItemTypes := TItemTypes.Create;
  FReplace := AJObject['replace'].AsBool;
  for JValue in AJObject['values'].AsArray do
  begin
    Value := JValue.AsString;
    if Value.StartsWith('#') then
    begin
      Value := Value.Substring(1);
      FItemTypes.Add(AItemTags.LoadFromName(Value).ItemTypes.GetEnumerator);
    end
    else
    begin
      if AItemTags.ItemTypes.Get(Value, ItemType) then
        FItemTypes.Add(ItemType);
    end;
  end;
  FSorted := ItemTypes.Copy;
  FSorted.Sort(
    function(A, B: TItemType): Boolean
    begin
      Result := A.NSPath < B.NSPath;
    end
    );
end;

destructor TItemTag.Destroy;
begin
  FSorted.Free;
  FItemTypes.Free;
  inherited;
end;

function TItemTag.GetItemTypes: TItemTypes.TReader;
begin
  Result := FItemTypes.Reader;
end;

function TItemTag.GetSorted: TItemTypes.TReader;
begin
  Result := FSorted.Reader;
end;

{ TItemTagCollection }

constructor TItemTagCollection.Create(AItemTypes: TItemTypeCollection; APath: string);
var
  FileName: TFileName;
begin
  FPath := APath;
  FItemTypes := AItemTypes;
  FMap := TMap.Create;
  FTags := TItemTags.Create;

  for FileName in TDirectory.GetFiles(APath, '*.json') do
    Load(FileName);

  FTags.Sort(
    function(A, B: TItemTag): Boolean
    begin
      Result := A.NSPath < B.NSPath;
    end);
end;

destructor TItemTagCollection.Destroy;
begin
  FMap.Free;
  FTags.Free;
  inherited;
end;

function TItemTagCollection.Exists(ANSPath: TNSPath): Boolean;
begin
  Result := FMap.KeyExists(ANSPath);
end;

function TItemTagCollection.Get(ANSPath: TNSPath; out AItemTag: TItemTag): Boolean;
begin
  Result := FMap.Get(ANSPath, AItemTag);
end;

function TItemTagCollection.GetTags: TItemTags.TReader;
begin
  Result := FTags.Reader;
end;

function TItemTagCollection.Load(AFileName: TFileName): TItemTag;
var
  NSPath: TNSPath;
  JObject: TJObject;
begin
  NSPath := ChangeFileExt(ExtractFileName(AFileName), '');
  if Get(NSPath, Result) then
    Exit;
  JObject := TJObject.Parse(TFile.ReadAllText(AFileName));
  try
    Result := TItemTag.Create(Self, NSPath, JObject);
    FMap[NSPath] := Result;
    FTags.Add(Result);
  finally
    JObject.Free;
  end;
end;

function TItemTagCollection.LoadFromName(ANSPath: TNSPath): TItemTag;
begin
  Result := Load(TPath.Combine(Path, ANSPath.Format(False) + '.json'));
end;

{ TItemTagSettings }

procedure TItemTagSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  inherited;
  with ASerializer do
  begin
    Define('path', FPath);
    Define('namespace_prefix', FNamespacePrefix);
  end;
end;

destructor TItemTagSettings.Destroy;
begin
  FItemTags.Free;
  inherited;
end;

class function TItemTagSettings.GetDescription: string;
begin
  Result := 'Path configuration for item tags folder.';
end;

class function TItemTagSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_itemtags';
end;

class function TItemTagSettings.GetTitle: string;
begin
  Result := 'Item-Tags';
end;

procedure TItemTagSettings.DoReload;
begin
  FItemTags.Free;
  FItemTags := TItemTagCollection.Create(Root.Get<TItemSettings>.Items, Path);
end;

procedure TItemTagSettings.SetDefaults;
begin
  FPath := DefaultPath;
  FNamespacePrefix := True;
end;

procedure TItemTagSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

{ TItemStackTag }

function TItemStackTag.Format: string;
begin
  Result := '#' + NSPath.Format(RootSettingsG.Get<TItemTagSettings>.NamespacePrefix);
  if NBT.HasValue and not NBT.Value.Empty then
    Result := Result + NBT.Value.Format;
end;

class function TItemStackTag.Parser: TParser;
begin
  Result := TParser.Create;
end;

{ TItemStackTag.TParser }

class function TItemStackTag.TParser.GetResultName: string;
begin
  Result := 'Item-Tag';
end;

function TItemStackTag.TParser.Parse: Boolean;
var
  NSPathString: string;
  NSPath: TNSPath;
  Marker: TLogMarker;
  TagExists: Boolean;
  ItemTag: TItemTag;
begin
  Marker := GetMarker;

  if not StartsWith('#') then
    Exit(False);

  BeginSuggestions(TItemTagSuggestions.Create);

  NSPathString := ReadWhile(NamespacePathChars);

  EndSuggestions;

  if NSPathString.IsEmpty then
  begin
    Log(1, 'Expected item tag.');
    Exit(True);
  end;
  NSPath := NSPathString;

  ParseResult := TItemStackTag.Create(NSPath);
  TagExists := RootSettingsG.Get<TItemTagSettings>.ItemTags.Get(NSPath, ItemTag);
  if not TagExists then
    Log(Marker, '"%s" is not a valid item tag.', [NSPath.Format]);

  ParseResult.NBT.Value := TNBTCompound.Parser.Optional(Info);

  Result := True;
end;

{ TItemStackTag.TItemTagSuggestions }

procedure TItemStackTag.TItemTagSuggestions.Generate;
var
  Tag: TItemTag;
  Settings: TItemTagSettings;
begin
  Settings := RootSettingsG.Get<TItemTagSettings>;
  for Tag in Settings.ItemTags.Tags do
    AddSuggestion(ParseSuggestion(Tag.NSPath.Format(False), Tag.NSPath.Format(False)));
  AddSuggestion(ParseSuggestion(TNSPath.Empty, TNSPath.Empty));
  for Tag in Settings.ItemTags.Tags do
    AddSuggestion(ParseSuggestion(Tag.NSPath, Tag.NSPath));
end;

{ TItemSlot }

constructor TItemSlot.Create;
begin
  ResetIndex;
end;

function TItemSlot.Format: string;
begin
  if not FHasIndex then
    Exit(GetName);
  if FUseNameIndex then
    Result := GetName + '.' + GetSubName(FIndex)
  else
    Result := GetName + '.' + FIndex.ToString;
end;

class function TItemSlot.GetIndexCount: Integer;
begin
  Result := 0;
end;

class function TItemSlot.GetIndexFromSubName(AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to GetSubNameCount - 1 do
    if AName = GetSubName(I) then
      Exit(I);
  Result := -1;
end;

class function TItemSlot.GetName: string;
begin
  Result := ItemSlotNames[GetType];
end;

class function TItemSlot.GetSubName(AIndex: Integer): string;
begin
  Assert(False, 'No Slot-SubNames specified.');
end;

class function TItemSlot.GetSubNameCount: Integer;
begin
  Result := 0;
end;

class function TItemSlot.GetTypeFromName(AName: string; out AType: TType): Boolean;
var
  SlotType: TType;
begin
  for SlotType := Low(TType) to High(TType) do
    if AName = ItemSlotNames[SlotType] then
    begin
      AType := SlotType;
      Exit(True);
    end;
  Result := False;
end;

class function TItemSlot.IndexRequired: Boolean;
begin
  Result := True;
end;

class function TItemSlot.Parser: IParser;
begin
  Result := TParser.Create;
end;

procedure TItemSlot.ResetIndex;
begin
  FHasIndex := False;
end;

procedure TItemSlot.SetIndex(AIndex: Integer);
begin
  FHasIndex := True;
  FUseNameIndex := False;
  FIndex := AIndex;
end;

procedure TItemSlot.SetNameIndex(AIndex: Integer);
begin
  FHasIndex := True;
  FUseNameIndex := True;
  FIndex := AIndex;
end;

{ TItemSlotArmor }

class function TItemSlotArmor.GetSubName(AIndex: Integer): string;
begin
  Result := SubNames[AIndex];
end;

class function TItemSlotArmor.GetSubNameCount: Integer;
begin
  Result := Length(SubNames);
end;

class function TItemSlotArmor.GetType: TItemSlot.TType;
begin
  Result := stArmor;
end;

{ TItemSlotContainer }

class function TItemSlotContainer.GetIndexCount: Integer;
begin
  Result := 54;
end;

class function TItemSlotContainer.GetType: TItemSlot.TType;
begin
  Result := stContainer;
end;

{ TItemSlotEnderchest }

class function TItemSlotEnderchest.GetIndexCount: Integer;
begin
  Result := 27;
end;

class function TItemSlotEnderchest.GetType: TItemSlot.TType;
begin
  Result := stEnderchest;
end;

{ TItemSlotHorse }

class function TItemSlotHorse.GetIndexCount: Integer;
begin
  Result := 15;
end;

class function TItemSlotHorse.GetSubName(AIndex: Integer): string;
begin
  Result := SubNames[AIndex];
end;

class function TItemSlotHorse.GetSubNameCount: Integer;
begin
  Result := Length(SubNames);
end;

class function TItemSlotHorse.GetType: TItemSlot.TType;
begin
  Result := stHorse;
end;

{ TItemSlotHotbar }

class function TItemSlotHotbar.GetIndexCount: Integer;
begin
  Result := 9;
end;

class function TItemSlotHotbar.GetType: TItemSlot.TType;
begin
  Result := stHotbar;
end;

{ TItemSlotInventory }

class function TItemSlotInventory.GetIndexCount: Integer;
begin
  Result := 27;
end;

class function TItemSlotInventory.GetType: TItemSlot.TType;
begin
  Result := stInventory;
end;

{ TItemSlotVillager }

class function TItemSlotVillager.GetIndexCount: Integer;
begin
  Result := 8;
end;

class function TItemSlotVillager.GetType: TItemSlot.TType;
begin
  Result := stVillager;
end;

{ TItemSlotWeapon }

class function TItemSlotWeapon.GetSubName(AIndex: Integer): string;
begin
  Result := SubNames[AIndex];
end;

class function TItemSlotWeapon.GetSubNameCount: Integer;
begin
  Result := Length(SubNames);
end;

class function TItemSlotWeapon.GetType: TItemSlot.TType;
begin
  Result := stWeapon;
end;

class function TItemSlotWeapon.IndexRequired: Boolean;
begin
  Result := False;
end;

{ TItemSlot.TParser }

class function TItemSlot.TParser.GetResultName: string;
begin
  Result := 'Item-Slot';
end;

function TItemSlot.TParser.Parse: Boolean;
var
  Name: string;
  SlotType: TItemSlot.TType;
  SlotClass: TItemSlotClass;
  Marker, SubNameMarker: TLogMarker;
  Index: Integer;
begin
  Marker := GetMarker;
  BeginSuggestions(TNameSuggestions);
  Name := ReadWhile(IdentChars - ['.']);
  EndSuggestions;

  if not TItemSlot.GetTypeFromName(Name, SlotType) then
    Exit(False);
  SlotClass := ItemSlotClasses[SlotType];

  ParseResult := SlotClass.Create;
  if not StartsWith('.') then
  begin
    if SlotClass.IndexRequired then
      Log(Marker, 'Slot requires index.');
    Exit(True);
  end;

  SubNameMarker := GetMarker;
  BeginSuggestions(TSubNameSuggestions.Create(SlotClass));
  Name := ReadWhile(IdentChars);
  EndSuggestions;

  if Name.IsEmpty then
    Log(Marker, 'Slot index expected.', elFatal);

  if TryStrToInt(Name, Index) then
  begin
    if SlotClass.GetIndexCount = 0 then
      Log(Marker, 'Slot-Type "%s" does not have any numerical indices.', [SlotClass.GetName], elFatal)
    else if not(Index in IBounds1(SlotClass.GetIndexCount)) then
      Log(Marker, 'Index must be in range %s.', [IBounds1(SlotClass.GetIndexCount).ToString]);
    ParseResult.SetIndex(Index);
    Exit(True);
  end;

  Index := SlotClass.GetIndexFromSubName(Name);
  if Index <> -1 then
    ParseResult.SetNameIndex(Index)
  else
    Log(Marker, 'Invalid slot index.', elFatal);

  Result := True;
end;

{ TItemSlot.TParser.TNameSuggestions }

class function TItemSlot.TParser.TNameSuggestions.GetCount: Integer;
begin
  Result := Length(ItemSlotNames);
end;

class function TItemSlot.TParser.TNameSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := ItemSlotNames[TType(AIndex)];
end;

{ TItemSlot.TParser.TSubNameSuggestions }

constructor TItemSlot.TParser.TSubNameSuggestions.Create(ASlotClass: TItemSlotClass);
begin
  inherited Create;
  FSlotClass := ASlotClass;
end;

procedure TItemSlot.TParser.TSubNameSuggestions.Generate;
var
  Index: Integer;
begin
  for Index := 0 to FSlotClass.GetSubNameCount - 1 do
    AddSuggestion(FSlotClass.GetSubName(Index));
  for Index := 0 to FSlotClass.GetIndexCount - 1 do
    AddSuggestion(Index.ToString);
end;

end.
