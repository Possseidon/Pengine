unit Pengine.MC.Item;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Settings,
  Pengine.Parser,
  Pengine.Utility,
  Pengine.JSON,

  Pengine.MC.Namespace,
  Pengine.MC.NBT,
  Pengine.MC.General;

type

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

  public
    constructor Create(AJObject: TJObject);
    destructor Destroy; override;

    function Exists(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out AItemType: TItemType): Boolean;

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

    procedure SetPath(const Value: string);

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property Items: TItemTypeCollection read FItems;

    procedure Reload;

  end;

  /// <summary>
  /// <p>An item stack, defined by namespace identifier and optional NBT.</p>
  /// <p>Example: <c>minecraft:iron_sword{Damage:50s}</c></p>
  /// </summary>
  /// <remarks>Despite being called "stack" this does NOT contain an amount.</remarks>
  TItemStack = class
  public type

    /// <summary>Parses a whole item stack.</summary>
    TParser = class(TObjectParserWithSettings<TItemStack>)
    private
      FSettings: TItemSettings;

    protected
      function Parse: Boolean; override;
      procedure InitSettings; override;

      property Settings: TItemSettings read FSettings;

    public
      class function GetResultName: string; override;

    end;

    TItemSuggestions = class(TParseSuggestionsGenerated<TParser>)
    private
      FSettings: TItemSettings;

    protected
      procedure Generate; override;

    public
      constructor Create(ASettings: TItemSettings);

      property Settings: TItemSettings read FSettings;

    end;

  private
    FNSPath: TNSPath;
    FNBT: TOwned<TNBTCompound>;

  public
    constructor Create; overload;
    constructor Create(ANSPath: TNSPath); overload;
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath write FNSPath;

    property NBT: TOwned<TNBTCompound> read FNBT;

    function Format(AShowDefaultNamespace: Boolean = True): string; virtual;

  end;

  {
    TItemTagCollection = class;

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

    TItemTagSettings = class
    public const

    DefaultPath = 'Data\data\minecraft\tags\items';

    private
    FItemTags: TItemTagCollection;
    FPath: string;

    procedure SetPath(const Value: string);

    public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property ItemTags: TItemTagCollection read FItemTags;

    procedure Reload;

    end;

    TItemStackTag = class
    public type

    /// <summary>Parses a whole item state.</summary>
    TParser = class(TObjectParserWithSettings<TItemStackTag>)
    private
    FSettings: TItemTagSettings;

    protected
    function Parse: Boolean; override;
    procedure InitSettings; override;

    property Settings: TItemTagSettings read FSettings;

    public
    class function GetResultName: string; override;

    end;

    TItemTagSuggestions = class(TParseSuggestionsGenerated<TParser>)
    private
    FSettings: TItemTagSettings;

    protected
    procedure Generate; override;

    public
    constructor Create(ASettings: TItemTagSettings);

    property Settings: TItemTagSettings read FSettings;

    end;

    public
    function Format(AShowDefaultNamespace: Boolean = True): string; override;

    end;
  }
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

function TItemTypeCollection.GetOrder: TItemTypes.TReader;
begin
  Result := FOrder.Reader;
end;

function TItemTypeCollection.GetSorted: TItemTypes.TReader;
begin
  Result := FSorted.Reader;
end;

{ TItemSettings }

destructor TItemSettings.Destroy;
begin
  FItems.Free;
  inherited;
end;

class function TItemSettings.GetDescription: string;
begin
  Result := 'Path configuration for items in items.json file.';
end;

class function TItemSettings.GetTitle: string;
begin
  Result := 'Items';
end;

procedure TItemSettings.Reload;
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
  Path := DefaultPath;
end;

procedure TItemSettings.SetPath(const Value: string);
begin
  if Path = Value then
    Exit;
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

procedure TItemStack.TParser.InitSettings;
begin
  FSettings := AllSettings.Sub<TItemSettings>;
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

  BeginSuggestions(TItemSuggestions.Create(Settings));

  NSPathString := ReadWhile(NamespacePathChars);

  EndSuggestions;

  if NSPathString.IsEmpty then
    Exit(False);
  NSPath := NSPathString;

  ItemExists := Settings.Items.Get(NSPath, ItemType);
  if not ItemExists then
    Log(Marker, '"%s" is not a valid item.', [NSPath.Format]);

  SetParseResult(TItemStack.Create(NSPath));

  ParseResult.NBT.Put(TNBTParserCompound.Optional(Info, omReturnNil));

  Result := True;
end;

{ TItemStack.TItemSuggestions }

constructor TItemStack.TItemSuggestions.Create(ASettings: TItemSettings);
begin
  FSettings := ASettings;
end;

procedure TItemStack.TItemSuggestions.Generate;
var
  Item: TItemType;
begin
  for Item in Settings.Items.Order do
    AddSuggestion(ParseSuggestion(Item.NSPath.Format(False), Item.NSPath.Format(False)));
  AddSuggestion(ParseSuggestion(TNSPath.Empty, TNSPath.Empty));
  for Item in Settings.Items.Order do
    AddSuggestion(ParseSuggestion(Item.NSPath, Item.NSPath));
end;

{ TItemStack }

constructor TItemStack.Create;
begin
  FNBT := TOwned<TNBTCompound>.Create;
end;

constructor TItemStack.Create(ANSPath: TNSPath);
begin
  Create;
  FNSPath := ANSPath;
end;

destructor TItemStack.Destroy;
begin
  FNBT.Free;
  inherited;
end;

function TItemStack.Format(AShowDefaultNamespace: Boolean): string;
begin
  Result := NSPath.Format(AShowDefaultNamespace);
  if NBT.HasValue and not NBT.Value.Empty then
    Result := Result + NBT.Value.Format;
end;

end.
