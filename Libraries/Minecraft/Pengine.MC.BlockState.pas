unit Pengine.MC.BlockState;

interface

uses
  System.SysUtils,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,

  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.Parser,
  Pengine.Settings,

  Pengine.MC.Namespace,
  Pengine.MC.NBT,
  Pengine.MC.General;

type

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
      constructor Create(AJSONPair: TJSONPair);
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

    function GetOrder: TProperties.TReader;

  public
    constructor Create(AJSONPair: TJSONPair);
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath;

    function PropertyExists(AName: string): Boolean;
    function GetProperty(AName: string; out AProperty: TProperty): Boolean;

    property Properties: TProperties.TReader read GetOrder;

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
    constructor Create(AJSONObject: TJSONObject);
    destructor Destroy; override;

    function Exists(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out ABlockProperties: TBlockType): Boolean;

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

    procedure SetPath(const Value: string);

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property Blocks: TBlockTypeCollection read FBlocks;

    procedure Reload;

  end;

  /// <summary>
  /// <p>A block state defined by namespace identifier and optional Properties and NBT.</p>
  /// <p>Example: <c>minecraft:chest[facing=south]{Inventory:[]}</c></p>
  /// </summary>
  TBlockState = class
  public type

    /// <summary>Parses a whole block state.</summary>
    TParser = class(TObjectParserWithSettings<TBlockState>)
    private
      FSettings: TBlockSettings;

    protected
      function Parse: Boolean; override;
      procedure InitSettings; override;

      property Settings: TBlockSettings read FSettings;

    public
      class function GetResultName: string; override;

    end;

    TBlockSuggestions = class(TParseSuggestionsGenerated<TParser>)
    private
      FSettings: TBlockSettings;

    protected
      procedure Generate; override;

    public
      constructor Create(ASettings: TBlockSettings);

      property Settings: TBlockSettings read FSettings;

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

    /// <summary>Parses the property-part of a block state.</summary>
    /// <remarks>Context specific, on which properties are available for a certain block or a whole block tag.</remarks>
    TPropertiesParser = class(TRefParser<TProperties>)
    private
      FBlockTypes: TBlockTypes.TReader;

    protected
      function Parse: Boolean; override;

    public
      constructor Create(AInfo: TParseInfo; ABlockTypes: TBlockTypes.TReader; ARequired: Boolean);

      class function GetResultName: string; override;

    end;

    TPropertySuggestions = class(TParseSuggestionsGenerated)
    private
      FBlockTypes: TBlockTypes.TReader;

    protected
      procedure Generate; override;

    public
      constructor Create(ABlockTypes: TBlockTypes.TReader);

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
    FProperties: TProperties;
    FNBT: TNBTCompound;

    procedure SetNBT(const Value: TNBTCompound);

  public
    constructor Create; overload;
    constructor Create(ANSPath: TNSPath); overload;
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath write FNSPath;
    property Properties: TProperties read FProperties;
    property NBT: TNBTCompound read FNBT write SetNBT;

    function Format(AShowDefaultNamespace: Boolean = True): string;

  end;

  // TODO: Tags

implementation

{ TBlockState.TProperty }

constructor TBlockType.TProperty.Create(AJSONPair: TJSONPair);
var
  JSONValue: TJSONValue;
begin
  FName := AJSONPair.JsonString.Value;
  FValues := TValues.Create;
  for JSONValue in AJSONPair.JSONValue as TJSONArray do
    FValues.Add((JSONValue as TJSONString).Value);
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

{ TBlockStates }

constructor TBlockTypeCollection.Create(AJSONObject: TJSONObject);
var
  JSONPair: TJSONPair;
  BlockState: TBlockType;
begin
  FMap := TMap.Create;
  FOrder := TBlockTypes.Create;

  for JSONPair in AJSONObject do
  begin
    BlockState := TBlockType.Create(JSONPair);
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

function TBlockTypeCollection.Get(ANSPath: TNSPath; out ABlockProperties: TBlockType): Boolean;
begin
  Result := FMap.Get(ANSPath, ABlockProperties);
end;

function TBlockTypeCollection.GetOrder: TBlockTypes.TReader;
begin
  Result := FOrder.Reader;
end;

function TBlockTypeCollection.GetSorted: TBlockTypes.TReader;
begin
  Result := FSorted.Reader;
end;

{ TBlockState }

constructor TBlockType.Create(AJSONPair: TJSONPair);
var
  JSONPair: TJSONPair;
  Prop: TProperty;
  JSONProperties: TJSONObject;
begin
  FNSPath := AJSONPair.JsonString.Value;
  FMap := TMap.Create;
  FProperties := TProperties.Create;
  if AJSONPair.JSONValue.TryGetValue<TJSONObject>('properties', JSONProperties) then
  begin
    for JSONPair in JSONProperties do
    begin
      Prop := TProperty.Create(JSONPair);
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

function TBlockType.GetOrder: TProperties.TReader;
begin
  Result := FProperties.Reader;
end;

{ TBlockState }

constructor TBlockState.Create(ANSPath: TNSPath);
begin
  FNSPath := ANSPath;
  FProperties := TProperties.Create;
end;

constructor TBlockState.Create;
begin
  FProperties := TProperties.Create;
end;

destructor TBlockState.Destroy;
begin
  FProperties.Free;
  FNBT.Free;
  inherited;
end;

function TBlockState.Format(AShowDefaultNamespace: Boolean): string;
var
  I: Integer;
begin
  Result := NSPath.Format(AShowDefaultNamespace);
  if not Properties.Empty then
  begin
    Result := Result + '[' + Properties.First.Format;
    for I := 1 to Properties.MaxIndex do
      Result := Result + ', ' + Properties[I].Format;
    Result := Result + ']';
  end;
  if (NBT <> nil) and not NBT.Empty then
    Result := Result + NBT.Format;
end;

procedure TBlockState.SetNBT(const Value: TNBTCompound);
begin
  FNBT.Free;
  FNBT := Value;
end;

{ TBlockState.TParser }

class function TBlockState.TParser.GetResultName: string;
begin
  Result := 'Block-State';
end;

procedure TBlockState.TParser.InitSettings;
begin
  FSettings := AllSettings.Sub<TBlockSettings>;
end;

function TBlockState.TParser.Parse: Boolean;
var
  Marker, PropertiesStart: TLogMarker;
  NSPathString: string;
  NSPath: TNSPath;
  Properties: TBlockType;
  PropertyName, PropertyValue: string;
  Prop: TBlockType.TProperty;
  BlockExists: Boolean;
begin
  Marker := GetMarker;
  BeginSuggestions(TBlockSuggestions.Create(Settings));

  NSPathString := ReadWhile(NamespacePathChars);

  EndSuggestions;

  if NSPathString.IsEmpty then
    Exit(False);
  NSPath := NSPathString;

  BlockExists := Settings.Blocks.Get(NSPath, Properties);
  if not BlockExists then
    Log(Marker, '"%s" is not a valid block.', [NSPath.Format]);

  SetParseResult(TBlockState.Create(NSPath));

  ParseResult.NBT := TNBTParserCompound.Optional(Info, omReturnNil);

  // Properties
  PropertiesStart := GetMarker;

  if ParseResult.NBT = nil then
    ParseResult.NBT := TNBTParserCompound.Optional(Info, omReturnNil);

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

destructor TBlockSettings.Destroy;
begin
  FBlocks.Free;
  inherited;
end;

class function TBlockSettings.GetDescription: string;
begin
  Result := 'Path configuration for block states in blocks.json file.';
end;

class function TBlockSettings.GetTitle: string;
begin
  Result := 'Blocks';
end;

procedure TBlockSettings.Reload;
var
  BlocksText: string;
  BlocksJSON: TJSONObject;
begin
  FBlocks.Free;
  if TFile.Exists(Path) then
  begin
    BlocksText := TFile.ReadAllText(Path);
    BlocksJSON := TJSONObject.ParseJSONValue(BlocksText) as TJSONObject;
  end
  else
    BlocksJSON := TJSONObject.Create;
  try
    FBlocks := TBlockTypeCollection.Create(BlocksJSON);
  finally
    BlocksJSON.Free;
  end;
end;

procedure TBlockSettings.SetDefaults;
begin
  Path := DefaultPath;
end;

procedure TBlockSettings.SetPath(const Value: string);
begin
  if Path = Value then
    Exit;
  FPath := Value;
  Reload;
end;

{ TBlockState.TBlockSuggestions }

constructor TBlockState.TBlockSuggestions.Create(ASettings: TBlockSettings);
begin
  FSettings := ASettings;
  inherited Create;
end;

procedure TBlockState.TBlockSuggestions.Generate;
var
  Block: TBlockType;
begin
  for Block in Settings.Blocks.Order do
    AddSuggestion(ParseSuggestion(Block.NSPath.Format(False), Block.NSPath.Format(False)));
  for Block in Settings.Blocks.Order do
    AddSuggestion(ParseSuggestion(Block.NSPath, Block.NSPath));
end;

{ TBlockState.TPropertySuggestions }

constructor TBlockState.TPropertySuggestions.Create(ABlockTypes: TBlockTypes.TReader);
begin
  FBlockTypes := ABlockTypes;
end;

procedure TBlockState.TPropertySuggestions.Generate;
var
  BlockType: TBlockType;
  Prop: TBlockType.TProperty;
begin
  for BlockType in FBlockTypes do
    for Prop in BlockType.Properties do
      AddSuggestion(Prop.Name);
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

function TBlockState.TPropertiesParser.Parse: Boolean;
var
  PropertyName: string;
  PropertyValue: string;
  BlockExists: Boolean;
  BlockType: TBlockType;
  Prop: TBlockType.TProperty;
begin
  if not StartsWith('[') then
    Exit(False);

  if not FBlockTypes.Empty then
    BeginSuggestions(TPropertySuggestions.Create(FBlockTypes));

  SkipWhitespace;
  if not StartsWith(']') then
  begin
    while True do
    begin
      PropertyName := ReadWhile(IdentChars);
      EndSuggestions;

      if PropertyName.IsEmpty then
      begin
        Log(1, 'Expected block state property name.');
      end
      else if BlockExists then
      begin
        Prop := nil;
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

      ParseObject.Add(TPropertyValue.Create(PropertyName, PropertyValue));

      SkipWhitespace;

      if StartsWith(']') then
        Break;

      if not StartsWith(',') then
        raise EParseError.Create('Expected "]" or ",".');

      if not FBlockTypes.Empty then
        BeginSuggestions(TPropertySuggestions.Create(FBlockTypes));

      SkipWhitespace;
    end;
  end;
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
    Result := ToString;
    Free;
  end;
end;

end.
