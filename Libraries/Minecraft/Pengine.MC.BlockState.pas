unit Pengine.MC.BlockState;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.Types,

  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.Parser,
  Pengine.Utility,
  Pengine.Settings,
  Pengine.JSON,

  Pengine.MC.Namespace,
  Pengine.MC.NBT,
  Pengine.MC.General;

type

  // TODO: Some really horrible code duplicate with item (and fluid) tags

  /// <summary>A block type with all its possible properties and their respective values.</summary>
  TBlockType = class
  public type

    /// <summary>A single block property and all its possible values.</summary>
    TProperty = class
    public type

      TValues = TArray<string>;

    private
      FName: string;
      FValues: TValues;

      function GetValues: TValues.TReader;

    public
      constructor Create(AJPair: TJPair);
      destructor Destroy; override;

      property Name: string read FName;
      property Values: TValues.TReader read GetValues;
      function Exists(AValue: string): Boolean;

    end;

    TMap = TToObjectMap<string, TProperty, TStringHasher>;
    TProperties = TRefArray<TProperty>;

  private
    FNSPath: TNSPath;
    FMap: TMap;
    FProperties: TProperties;

    function GetProperties: TProperties.TReader;

  public
    constructor Create(AJPair: TJPair);
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath;

    function PropertyExists(AName: string): Boolean;
    function GetProperty(AName: string; out AProperty: TProperty): Boolean;

    property Properties: TProperties.TReader read GetProperties;

  end;

  TBlockTypes = TRefArray<TBlockType>;

  /// <summary>A collection of all block types.</summary>
  TBlockTypeCollection = class
  public type

    TMap = TToObjectMap<TNSPath, TBlockType, TNSPathHasher>;

  private
    FMap: TMap;
    FOrder: TBlockTypes;
    FSorted: TBlockTypes;

    function GetOrder: TBlockTypes.TReader;
    function GetSorted: TBlockTypes.TReader;

  public
    constructor Create(AJObject: TJObject);
    destructor Destroy; override;

    function Exists(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out ABlockType: TBlockType): Boolean;

    /// <summary>All block types as found in the file.</summary>
    property Order: TBlockTypes.TReader read GetOrder;
    /// <summary>All block types sorted alphabetically.</summary>
    property Sorted: TBlockTypes.TReader read GetSorted;

  end;

  /// <summary>Loads available blocks from a file.</summary>
  TBlockSettings = class(TSettings)
  public const

    DefaultPath = 'Data\reports\blocks.json';

  private
    FBlocks: TBlockTypeCollection;
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

    property Blocks: TBlockTypeCollection read FBlocks;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

  /// <summary>
  /// <p>A block state defined by namespace identifier and optional Properties and NBT.</p>
  /// <p>Example: <c>minecraft:chest[facing=south]{Inventory:[]}</c></p>
  /// </summary>
  TBlockState = class
  public type

    /// <summary>Parses a whole block state.</summary>
    TParser = class(TObjectParser<TBlockState>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TBlockSuggestions = class(TParseSuggestionsGenerated<TParser>)
    protected
      procedure Generate; override;

    end;

    /// <summary>A value for a property of a block state.</summary>
    TPropertyValue = class
    private
      FProperty: string;
      FValue: string;

    public
      constructor Create(AProperty, AValue: string);

      property Prop: string read FProperty write FProperty;
      property Value: string read FValue write FValue;

      function Format: string;

    end;

    TProperties = class(TObjectArray<TPropertyValue>)
    public
      function Format: string;

    end;

    /// <summary>Parses the properties of a block state.</summary>
    /// <remarks>Context specific, on which properties are available for a certain block or a whole block tag.</remarks>
    TPropertiesParser = class(TObjectParser<TProperties>)
    private
      FBlockTypes: TBlockTypes.TReader;

    protected
      function Parse: Boolean; override;

    public
      constructor Create(AInfo: TParseInfo; ABlockTypes: TBlockTypes.TReader; ARequired: Boolean);

      class function Optional(AInfo: TParseInfo; ABlockTypes: TBlockTypes.TReader): TProperties; reintroduce;

      class function GetResultName: string; override;

    end;

    TPropertySuggestions = class(TParseSuggestionsGenerated)
    private
      FBlockTypes: TBlockTypes;

    protected
      procedure Generate; override;

    public
      constructor Create(ABlockTypes: TBlockTypes);
      destructor Destroy; override;

      function GetTitle: string; override;

    end;

    TPropertyValueSuggestions = class(TParseSuggestions)
    private
      FProperty: TBlockType.TProperty;

    public
      constructor Create(AProperty: TBlockType.TProperty);

      function GetTitle: string; override;
      function GetCount: Integer; override;
      function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  private
    FNSPath: TNSPath;
    FProperties: TOwned<TProperties>;
    FNBT: TOwned<TNBTCompound>;

  public
    constructor Create; overload;
    constructor Create(ANSPath: TNSPath); overload;
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath write FNSPath;

    property Properties: TOwned<TProperties> read FProperties;
    property NBT: TOwned<TNBTCompound> read FNBT;

    function Format: string; virtual;

  end;

  TBlockTagCollection = class;

  /// <summary>A block tag, which maps to various blocks or other block tags.</summary>
  TBlockTag = class
  private
    FReplace: Boolean;
    FNSPath: TNSPath;
    FBlockTypes: TBlockTypes;
    FSorted: TBlockTypes;

    function GetBlockTypes: TBlockTypes.TReader;
    function GetSorted: TBlockTypes.TReader;

  public
    constructor Create(ABlockTags: TBlockTagCollection; ANSPath: TNSPath; AJObject: TJObject);
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath;

    property BlockTypes: TBlockTypes.TReader read GetBlockTypes;
    property Sorted: TBlockTypes.TReader read GetSorted;

  end;

  TBlockTags = TRefArray<TBlockTag>;

  /// <summary>A collection of all block tags.</summary>
  TBlockTagCollection = class
  public type

    TMap = TToObjectMap<TNSPath, TBlockTag, TNSPathHasher>;

  private
    FBlockTypes: TBlockTypeCollection;
    FMap: TMap;
    FTags: TBlockTags;
    FPath: string;

    function GetTags: TBlockTags.TReader;

    function Load(AFileName: TFileName): TBlockTag;
    function LoadFromName(ANSPath: TNSPath): TBlockTag;

  public
    constructor Create(ABlockTypes: TBlockTypeCollection; APath: string);
    destructor Destroy; override;

    property Path: string read FPath;

    property BlockTypes: TBlockTypeCollection read FBlockTypes;

    function Exists(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out ABlockTag: TBlockTag): Boolean;

    /// <summary>All block tags sorted alphabetically.</summary>
    property Tags: TBlockTags.TReader read GetTags;

  end;

  /// <summary>Loads available block tags from a directory.</summary>
  TBlockTagSettings = class(TSettings)
  public const

    DefaultPath = 'Data\data\minecraft\tags\blocks';

  private
    FBlockTags: TBlockTagCollection;
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

    property BlockTags: TBlockTagCollection read FBlockTags;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

  /// <summary>
  /// <p>A block tag defined by namespace identifier and optional Properties and NBT.</p>
  /// <p>Example: <c>#minecraft:log[axis=x]{}</c></p>
  /// </summary>
  TBlockStateTag = class(TBlockState)
  public type

    /// <summary>Parses a whole block state.</summary>
    TParser = class(TObjectParser<TBlockStateTag>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TBlockTagSuggestions = class(TParseSuggestionsGenerated<TParser>)
    protected
      procedure Generate; override;

    end;

  public
    function Format: string; override;

  end;

implementation

{ TBlockType.TProperty }

constructor TBlockType.TProperty.Create(AJPair: TJPair);
var
  JValue: TJValue;
begin
  FName := AJPair.Key;
  FValues := TValues.Create;
  for JValue in AJPair.AsArray do
    FValues.Add(JValue.AsString);
end;

destructor TBlockType.TProperty.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TBlockType.TProperty.Exists(AValue: string): Boolean;
var
  Value: string;
begin
  for Value in Values do
    if Value = AValue then
      Exit(True);
  Result := False;
end;

function TBlockType.TProperty.GetValues: TValues.TReader;
begin
  Result := FValues.Reader;
end;

{ TBlockTypeCollection }

constructor TBlockTypeCollection.Create(AJObject: TJObject);
var
  JPair: TJPair;
  BlockState: TBlockType;
begin
  FMap := TMap.Create;
  FOrder := TBlockTypes.Create;

  for JPair in AJObject do
  begin
    BlockState := TBlockType.Create(JPair);
    FMap[BlockState.NSPath] := BlockState;
    FOrder.Add(BlockState);
  end;

  FSorted := FOrder.Copy;
  FSorted.Sort(
    function(A, B: TBlockType): Boolean
    begin
      Result := A.NSPath < B.NSPath;
    end);
end;

destructor TBlockTypeCollection.Destroy;
begin
  FMap.Free;
  FOrder.Free;
  FSorted.Free;
  inherited;
end;

function TBlockTypeCollection.Exists(ANSPath: TNSPath): Boolean;
begin
  Result := FMap.KeyExists(ANSPath);
end;

function TBlockTypeCollection.Get(ANSPath: TNSPath; out ABlockType: TBlockType): Boolean;
begin
  Result := FMap.Get(ANSPath, ABlockType);
end;

function TBlockTypeCollection.GetOrder: TBlockTypes.TReader;
begin
  Result := FOrder.Reader;
end;

function TBlockTypeCollection.GetSorted: TBlockTypes.TReader;
begin
  Result := FSorted.Reader;
end;

{ TBlockType }

constructor TBlockType.Create(AJPair: TJPair);
var
  JPair: TJPair;
  Prop: TProperty;
  JProperties: TJObject;
begin
  FNSPath := AJPair.Key;
  FMap := TMap.Create;
  FProperties := TProperties.Create;
  if AJPair.AsObject.Get<TJObject>('properties', JProperties) then
  begin
    for JPair in JProperties do
    begin
      Prop := TProperty.Create(JPair);
      FMap[Prop.Name] := Prop;
      FProperties.Add(Prop);
    end;
  end;
end;

destructor TBlockType.Destroy;
begin
  FMap.Free;
  FProperties.Free;
  inherited;
end;

function TBlockType.PropertyExists(AName: string): Boolean;
begin
  Result := FMap.KeyExists(AName);
end;

function TBlockType.GetProperty(AName: string; out AProperty: TProperty): Boolean;
begin
  Result := FMap.Get(AName, AProperty);
end;

function TBlockType.GetProperties: TProperties.TReader;
begin
  Result := FProperties.Reader;
end;

{ TBlockState }

constructor TBlockState.Create(ANSPath: TNSPath);
begin
  Create;
  FNSPath := ANSPath;
end;

constructor TBlockState.Create;
begin
  FProperties := TOwned<TProperties>.Create;
  FNBT := TOwned<TNBTCompound>.Create;
end;

destructor TBlockState.Destroy;
begin
  FProperties.Free;
  FNBT.Free;
  inherited;
end;

function TBlockState.Format: string;
begin
  Result := NSPath.Format(RootSettingsG.Get<TBlockSettings>.NamespacePrefix);
  if Properties.HasValue and not Properties.Value.Empty then
    Result := Result + Properties.Value.Format;
  if NBT.HasValue and not NBT.Value.Empty then
    Result := Result + NBT.Value.Format;
end;

{ TBlockState.TParser }

class function TBlockState.TParser.GetResultName: string;
begin
  Result := 'Block-State';
end;

function TBlockState.TParser.Parse: Boolean;
var
  Marker: TLogMarker;
  NSPathString: string;
  NSPath: TNSPath;
  BlockType: TBlockType;
  BlockExists: Boolean;
  Blocks: TBlockTypes;
begin
  Marker := GetMarker;

  BeginSuggestions(TBlockSuggestions.Create);

  NSPathString := ReadWhile(NamespacePathChars);

  EndSuggestions;

  if NSPathString.IsEmpty then
    Exit(False);
  NSPath := NSPathString;

  BlockExists := RootSettingsG.Get<TBlockSettings>.Blocks.Get(NSPath, BlockType);
  if not BlockExists then
    Log(Marker, '"%s" is not a valid block.', [NSPath.Format]);

  SetParseResult(TBlockState.Create(NSPath));

  ParseResult.NBT.Put(TNBTCompound.TParser.Optional(Info, omReturnNil));

  Blocks := TBlockTypes.Create;
  try
    if BlockExists then
      Blocks.Add(BlockType);
    ParseResult.Properties.Put(TPropertiesParser.Optional(Info, Blocks.Reader));
  finally
    Blocks.Free;
  end;

  if not ParseResult.NBT.HasValue then
    ParseResult.NBT.Put(TNBTCompound.TParser.Optional(Info, omReturnNil));

  Result := True;
end;

{ TBlockState.TPropertyValue }

constructor TBlockState.TPropertyValue.Create(AProperty, AValue: string);
begin
  FProperty := AProperty;
  FValue := AValue;
end;

function TBlockState.TPropertyValue.Format: string;
begin
  Result := Prop + '=' + Value;
end;

{ TBlockSettings }

procedure TBlockSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  inherited;
  with ASerializer do
  begin
    Define('path', FPath);
    Define('namespace_prefix', FNamespacePrefix);
  end;
end;

destructor TBlockSettings.Destroy;
begin
  FBlocks.Free;
  inherited;
end;

class function TBlockSettings.GetDescription: string;
begin
  Result := 'Path configuration for block states in blocks.json file.';
end;

class function TBlockSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_blocks';
end;

class function TBlockSettings.GetTitle: string;
begin
  Result := 'Blocks';
end;

procedure TBlockSettings.DoReload;
var
  BlocksText: string;
  BlocksJ: TJObject;
begin
  FreeAndNil(FBlocks);

  if TFile.Exists(Path) then
  begin
    BlocksText := TFile.ReadAllText(Path);
    BlocksJ := TJObject.Parse(BlocksText);
  end
  else
  begin
    BlocksJ := TJObject.Create;
  end;

  try
    FBlocks := TBlockTypeCollection.Create(BlocksJ);
  finally
    BlocksJ.Free;
  end;
end;

procedure TBlockSettings.SetDefaults;
begin
  Path := DefaultPath;
  NamespacePrefix := True;
end;

procedure TBlockSettings.SetPath(const Value: string);
begin
  if Path = Value then
    Exit;
  FPath := Value;
  Reload;
end;

{ TBlockState.TBlockSuggestions }

procedure TBlockState.TBlockSuggestions.Generate;
var
  Block: TBlockType;
  Settings: TBlockSettings;
begin
  Settings := RootSettingsG.Get<TBlockSettings>;
  for Block in Settings.Blocks.Order do
    AddSuggestion(ParseSuggestion(Block.NSPath.Format(False), Block.NSPath.Format(False)));
  AddSuggestion(ParseSuggestion(TNSPath.Empty, TNSPath.Empty));
  for Block in Settings.Blocks.Order do
    AddSuggestion(ParseSuggestion(Block.NSPath, Block.NSPath));
end;

{ TBlockState.TPropertySuggestions }

constructor TBlockState.TPropertySuggestions.Create(ABlockTypes: TBlockTypes);
begin
  FBlockTypes := ABlockTypes;
end;

destructor TBlockState.TPropertySuggestions.Destroy;
begin
  FBlockTypes.Free;
  inherited;
end;

procedure TBlockState.TPropertySuggestions.Generate;
var
  BlockType: TBlockType;
  Prop: TBlockType.TProperty;
begin
  for BlockType in FBlockTypes do
    for Prop in BlockType.Properties do
      AddUniqueSuggestion(Prop.Name);
end;

function TBlockState.TPropertySuggestions.GetTitle: string;
begin
  Result := 'Block-Property';
end;

{ TBlockState.TPropertyValueSuggestions }

constructor TBlockState.TPropertyValueSuggestions.Create(AProperty: TBlockType.TProperty);
begin
  FProperty := AProperty;
end;

function TBlockState.TPropertyValueSuggestions.GetCount: Integer;
begin
  Result := FProperty.Values.Count;
end;

function TBlockState.TPropertyValueSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := FProperty.Values[AIndex];
end;

function TBlockState.TPropertyValueSuggestions.GetTitle: string;
begin
  Result := 'Property-Value';
end;

{ TBlockState.TPropertiesParser }

constructor TBlockState.TPropertiesParser.Create(AInfo: TParseInfo; ABlockTypes: TBlockTypes.TReader;
ARequired: Boolean);
begin
  FBlockTypes := ABlockTypes;
  inherited Create(AInfo, ARequired);
end;

class function TBlockState.TPropertiesParser.GetResultName: string;
begin
  Result := 'Block-Properties';
end;

class function TBlockState.TPropertiesParser.Optional(AInfo: TParseInfo; ABlockTypes: TBlockTypes.TReader): TProperties;
begin
  Result := Create(AInfo, ABlockTypes, False).GetOptional;
end;

function TBlockState.TPropertiesParser.Parse: Boolean;
var
  PropertyName: string;
  PropertyValue: string;
  BlockType: TBlockType;
  Prop: TBlockType.TProperty;
  HasBlockTypes: Boolean;
begin
  if not StartsWith('[') then
    Exit(False);

  SetParseResult(TProperties.Create);

  HasBlockTypes := (FBlockTypes <> nil) and not FBlockTypes.Empty;
  if HasBlockTypes then
    BeginSuggestions(TPropertySuggestions.Create(FBlockTypes.Copy));

  SkipWhitespace;
  if StartsWith(']', False) then
  begin
    EndSuggestions;
    Advance;
    Exit(True);
  end;

  while True do
  begin
    PropertyName := ReadWhile(IdentChars);
    EndSuggestions;

    if PropertyName.IsEmpty then
    begin
      Log(1, 'Expected block state property name.', elFatal);
      Exit(True);
    end;

    Prop := nil;
    if HasBlockTypes then
    begin
      for BlockType in FBlockTypes do
        if BlockType.GetProperty(PropertyName, Prop) then
          Break;
      if Prop = nil then
        Log(-PropertyName.Length, '"%s" is not a valid property.', [PropertyName]);
    end;

    SkipWhitespace;

    if not StartsWith('=') then
      raise EParseError.Create('Expected "=".');

    if Prop <> nil then
      BeginSuggestions(TPropertyValueSuggestions.Create(Prop));

    SkipWhitespace;

    PropertyValue := ReadWhile(IdentChars);

    EndSuggestions;

    if PropertyValue.IsEmpty then
      Log(1, 'Expected value for property.')
    else if (Prop <> nil) and not Prop.Exists(PropertyValue) then
      Log(-PropertyValue.Length, '"%s" is not a valid value for "%s".', [PropertyValue, PropertyName]);

    ParseResult.Add(TPropertyValue.Create(PropertyName, PropertyValue));

    SkipWhitespace;

    if StartsWith(']') then
      Break;

    if not StartsWith(',') then
      raise EParseError.Create('Expected "]" or ",".');

    if HasBlockTypes then
      BeginSuggestions(TPropertySuggestions.Create(FBlockTypes.Copy));

    SkipWhitespace;
  end;

  Result := True;
end;

{ TBlockState.TProperties }

function TBlockState.TProperties.Format: string;
var
  I: Integer;
begin
  with TStringBuilder.Create('[') do
  begin
    if not Empty then
      Append(Items[0].Format);
    for I := 1 to MaxIndex do
    begin
      Append(', ');
      Append(Items[I].Format);
    end;
    Append(']');
    Result := ToString;
    Free;
  end;
end;

{ TBlockStateTag }

function TBlockStateTag.Format: string;
begin
  Result := '#' + NSPath.Format(RootSettingsG.Get<TBlockTagSettings>.NamespacePrefix);
  if Properties.HasValue and not Properties.Value.Empty then
    Result := Result + Properties.Value.Format;
  if NBT.HasValue and not NBT.Value.Empty then
    Result := Result + NBT.Value.Format;
end;

{ TBlockTag }

constructor TBlockTag.Create(ABlockTags: TBlockTagCollection; ANSPath: TNSPath; AJObject: TJObject);
var
  JValue: TJValue;
  Value: string;
  BlockType: TBlockType;
begin
  FNSPath := ANSPath;
  FBlockTypes := TBlockTypes.Create;
  FReplace := AJObject['replace'].AsBool;
  for JValue in AJObject['values'].AsArray do
  begin
    Value := JValue.AsString;
    if Value.StartsWith('#') then
    begin
      Value := Value.Substring(1);
      FBlockTypes.Add(ABlockTags.LoadFromName(Value).BlockTypes.GetEnumerator);
    end
    else
    begin
      if ABlockTags.BlockTypes.Get(Value, BlockType) then
        FBlockTypes.Add(BlockType);
    end;
  end;
  FSorted := BlockTypes.Copy;
  FSorted.Sort(
    function(A, B: TBlockType): Boolean
    begin
      Result := A.NSPath < B.NSPath;
    end
    );
end;

destructor TBlockTag.Destroy;
begin
  FSorted.Free;
  FBlockTypes.Free;
  inherited;
end;

function TBlockTag.GetBlockTypes: TBlockTypes.TReader;
begin
  Result := FBlockTypes.Reader;
end;

function TBlockTag.GetSorted: TBlockTypes.TReader;
begin
  Result := FSorted.Reader;
end;

{ TBlockTagCollection }

constructor TBlockTagCollection.Create(ABlockTypes: TBlockTypeCollection; APath: string);
var
  FileName: TFileName;
begin
  FPath := APath;
  FBlockTypes := ABlockTypes;
  FMap := TMap.Create;
  FTags := TBlockTags.Create;

  for FileName in TDirectory.GetFiles(APath, '*.json') do
    Load(FileName);

  FTags.Sort(
    function(A, B: TBlockTag): Boolean
    begin
      Result := A.NSPath < B.NSPath;
    end);
end;

destructor TBlockTagCollection.Destroy;
begin
  FMap.Free;
  FTags.Free;
  inherited;
end;

function TBlockTagCollection.Exists(ANSPath: TNSPath): Boolean;
begin
  Result := FMap.KeyExists(ANSPath);
end;

function TBlockTagCollection.Get(ANSPath: TNSPath; out ABlockTag: TBlockTag): Boolean;
begin
  Result := FMap.Get(ANSPath, ABlockTag);
end;

function TBlockTagCollection.GetTags: TBlockTags.TReader;
begin
  Result := FTags.Reader;
end;

function TBlockTagCollection.Load(AFileName: TFileName): TBlockTag;
var
  NSPath: TNSPath;
  JObject: TJObject;
begin
  NSPath := ChangeFileExt(ExtractFileName(AFileName), '');
  if Get(NSPath, Result) then
    Exit;
  JObject := TJObject.Parse(TFile.ReadAllText(AFileName));
  try
    Result := TBlockTag.Create(Self, NSPath, JObject);
    FMap[NSPath] := Result;
    FTags.Add(Result);
  finally
    JObject.Free;
  end;
end;

function TBlockTagCollection.LoadFromName(ANSPath: TNSPath): TBlockTag;
begin
  Result := Load(TPath.Combine(Path, ANSPath.Format(False) + '.json'));
end;

{ TBlockTagSettings }

procedure TBlockTagSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  inherited;
  with ASerializer do
  begin
    Define('path', FPath);
    Define('namespace_prefix', FNamespacePrefix);
  end;
end;

destructor TBlockTagSettings.Destroy;
begin
  FBlockTags.Free;
  inherited;
end;

class function TBlockTagSettings.GetDescription: string;
begin
  Result := 'Path configuration for block tags folder.';
end;

class function TBlockTagSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_blocktags';
end;

class function TBlockTagSettings.GetTitle: string;
begin
  Result := 'Block-Tags';
end;

procedure TBlockTagSettings.DoReload;
begin
  FBlockTags.Free;
  FBlockTags := TBlockTagCollection.Create(Root.Get<TBlockSettings>.Blocks, Path);
end;

procedure TBlockTagSettings.SetDefaults;
begin
  Path := DefaultPath;
  NamespacePrefix := True;
end;

procedure TBlockTagSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

{ TBlockStateTag.TParser }

class function TBlockStateTag.TParser.GetResultName: string;
begin
  Result := 'Block Tag';
end;

function TBlockStateTag.TParser.Parse: Boolean;
var
  NSPathString: string;
  NSPath: TNSPath;
  Marker: TLogMarker;
  TagExists: Boolean;
  BlockTag: TBlockTag;
  BlockTypes: TBlockTypes.TReader;
begin
  Marker := GetMarker;

  if not StartsWith('#') then
    Exit(False);

  BeginSuggestions(TBlockTagSuggestions.Create);

  NSPathString := ReadWhile(NamespacePathChars);

  EndSuggestions;

  if NSPathString.IsEmpty then
  begin
    Log(1, 'Expected block tag.');
    Exit(True);
  end;
  NSPath := NSPathString;

  SetParseResult(TBlockStateTag.Create(NSPath));
  TagExists := RootSettingsG.Get<TBlockTagSettings>.BlockTags.Get(NSPath, BlockTag);
  if not TagExists then
    Log(Marker, '"%s" is not a valid block tag.', [NSPath.Format]);

  ParseResult.NBT.Put(TNBTCompound.TParser.Optional(Info, omReturnNil));

  if BlockTag <> nil then
    BlockTypes := BlockTag.BlockTypes
  else
    BlockTypes := nil;

  ParseResult.Properties.Put(TPropertiesParser.Optional(Info, BlockTypes));

  if not ParseResult.NBT.HasValue then
    ParseResult.NBT.Put(TNBTCompound.TParser.Optional(Info, omReturnNil));

  Result := True;
end;

{ TBlockStateTag.TBlockTagSuggestions }

procedure TBlockStateTag.TBlockTagSuggestions.Generate;
var
  Settings: TBlockTagSettings;
  Tag: TBlockTag;
begin
  Settings := RootSettingsG.Get<TBlockTagSettings>;
  for Tag in Settings.BlockTags.Tags do
    AddSuggestion(ParseSuggestion(Tag.NSPath.Format(False), Tag.NSPath.Format(False)));
  AddSuggestion(ParseSuggestion(TNSPath.Empty, TNSPath.Empty));
  for Tag in Settings.BlockTags.Tags do
    AddSuggestion(ParseSuggestion(Tag.NSPath, Tag.NSPath));
end;

end.
