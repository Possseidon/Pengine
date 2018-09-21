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

  /// <summary>A collection of all possible properties and their values.</summary>
  TBlockProperties = class
  public type

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
    TOrder = TRefArray<TProperty>;

  private
    FNSPath: TNSPath;
    FMap: TMap;
    FOrder: TOrder;

    function GetOrder: TOrder.TReader;

  public
    constructor Create(AJSONPair: TJSONPair);
    destructor Destroy; override;

    property NSPath: TNSPath read FNSPath;

    function Exists(AName: string): Boolean;
    function Get(AName: string; out AProperty: TProperty): Boolean;

    property Order: TOrder.TReader read GetOrder;

  end;

  /// <summary>All available blocks.</summary>
  TBlocks = class
  public type

    TMap = TToObjectMap<TNSPath, TBlockProperties, TNSPathHasher>;
    TOrder = TRefArray<TBlockProperties>;

  private
    FMap: TMap;
    FOrder: TOrder;
    FSorted: TOrder;

    function GetOrder: TOrder.TReader;
    function GetSorted: TOrder.TReader;

  public
    constructor Create(AJSONObject: TJSONObject);
    destructor Destroy; override;

    function Exists(ANSPath: TNSPath): Boolean;
    function Get(ANSPath: TNSPath; out ABlockProperties: TBlockProperties): Boolean;

    /// <summary>A read-only array of the order, as found in the file.</summary>
    property Order: TOrder.TReader read GetOrder;
    /// <summary>An alpha-sorted version of the same array.</summary>
    property Sorted: TOrder.TReader read GetSorted;

  end;

  /// <summary>Loads available blocks from a file.</summary>
  TBlockSettings = class(TSettings)
  public const

    DefaultPath = 'Data\reports\blocks.json';

  private
    FBlocks: TBlocks;
    FPath: string;

    procedure SetPath(const Value: string);

  public
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;
    property Blocks: TBlocks read FBlocks;

    procedure Reload;

  end;

  /// <summary>A parserable and formattable blockstate with properties and NBT.</summary>
  TBlockState = class
  public type

    /// <summary>A parser for </summary>
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

    TProperties = TObjectArray<TPropertyValue>;

    TPropertiesParser = class(TRefParser<TProperties>)
    public type

      TBlockList = TRefArray<TBlockState>;

    private
      FBlocks: TBlockList.TReader;

    protected
      function Parse: Boolean; override;

    public
      constructor Create(AInfo: TParseInfo; ABlocks: TBlockList.TReader; ARequired: Boolean);

      class function GetResultName: string; override;

    end;

    TPropertySuggestions = class(TParseSuggestions)
    private
      FProperties: TBlockProperties;

    public
      constructor Create(AProperties: TBlockProperties);

      function GetTitle: string; override;      
      function GetCount: Integer; override;
      function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

    TPropertyValueSuggestions = class(TParseSuggestions)
    private
      FProperty: TBlockProperties.TProperty;

    public
      constructor Create(AProperty: TBlockProperties.TProperty);

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

constructor TBlockProperties.TProperty.Create(AJSONPair: TJSONPair);
var
  JSONValue: TJSONValue;
begin
  FName := AJSONPair.JsonString.Value;
  FValues := TValues.Create;
  for JSONValue in AJSONPair.JSONValue as TJSONArray do
    FValues.Add((JSONValue as TJSONString).Value);
end;

destructor TBlockProperties.TProperty.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TBlockProperties.TProperty.Exists(AValue: string): Boolean;
var
  Value: string;
begin
  for Value in Values do
    if Value = AValue then
      Exit(True);
  Result := False;
end;

function TBlockProperties.TProperty.GetValues: TValues.TReader;
begin
  Result := FValues.Reader;
end;

{ TBlockStates }

constructor TBlocks.Create(AJSONObject: TJSONObject);
var
  JSONPair: TJSONPair;
  BlockState: TBlockProperties;
begin
  FMap := TMap.Create;
  FOrder := TOrder.Create;

  for JSONPair in AJSONObject do
  begin
    BlockState := TBlockProperties.Create(JSONPair);
    FMap[BlockState.NSPath] := BlockState;
    FOrder.Add(BlockState);
  end;

  FSorted := FOrder.Copy;
  FSorted.Sort(
    function(A, B: TBlockProperties): Boolean
    begin
      Result := A.NSPath < B.NSPath;
    end);
end;

destructor TBlocks.Destroy;
begin
  FMap.Free;
  FOrder.Free;
  FSorted.Free;
  inherited;
end;

function TBlocks.Exists(ANSPath: TNSPath): Boolean;
begin
  Result := FMap.KeyExists(ANSPath);
end;

function TBlocks.Get(ANSPath: TNSPath; out ABlockProperties: TBlockProperties): Boolean;
begin
  Result := FMap.Get(ANSPath, ABlockProperties);
end;

function TBlocks.GetOrder: TOrder.TReader;
begin
  Result := FOrder.Reader;
end;

function TBlocks.GetSorted: TOrder.TReader;
begin
  Result := FSorted.Reader;
end;

{ TBlockState }

constructor TBlockProperties.Create(AJSONPair: TJSONPair);
var
  JSONPair: TJSONPair;
  Prop: TProperty;
  JSONProperties: TJSONObject;
begin
  FNSPath := AJSONPair.JsonString.Value;
  FMap := TMap.Create;
  FOrder := TOrder.Create;
  if AJSONPair.JSONValue.TryGetValue<TJSONObject>('properties', JSONProperties) then
  begin
    for JSONPair in JSONProperties do
    begin
      Prop := TProperty.Create(JSONPair);
      FMap[Prop.Name] := Prop;
      FOrder.Add(Prop);
    end;
  end;
end;

destructor TBlockProperties.Destroy;
begin
  FMap.Free;
  FOrder.Free;
  inherited;
end;

function TBlockProperties.Exists(AName: string): Boolean;
begin
  Result := FMap.KeyExists(AName);
end;

function TBlockProperties.Get(AName: string; out AProperty: TProperty): Boolean;
begin
  Result := FMap.Get(AName, AProperty);
end;

function TBlockProperties.GetOrder: TOrder.TReader;
begin
  Result := FOrder.Reader;
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
  Properties: TBlockProperties;
  PropertyName, PropertyValue: string;
  Prop: TBlockProperties.TProperty;
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
    FBlocks := TBlocks.Create(BlocksJSON);
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
  Block: TBlockProperties;
begin
  for Block in Settings.Blocks.Order do
    AddSuggestion(ParseSuggestion(Block.NSPath.Format(False), Block.NSPath.Format(False)));
  for Block in Settings.Blocks.Order do
    AddSuggestion(ParseSuggestion(Block.NSPath, Block.NSPath));
end;

{ TBlockState.TPropertySuggestions }

constructor TBlockState.TPropertySuggestions.Create(AProperties: TBlockProperties);
begin
  FProperties := AProperties;
end;

function TBlockState.TPropertySuggestions.GetCount: Integer;
begin
  Result := FProperties.Order.Count;
end;

function TBlockState.TPropertySuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := FProperties.Order[AIndex].Name;
end;

function TBlockState.TPropertySuggestions.GetTitle: string;
begin
  Result := 'Block-Property';
end;

{ TBlockState.TPropertyValueSuggestions }

constructor TBlockState.TPropertyValueSuggestions.Create(AProperty: TBlockProperties.TProperty);
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

constructor TBlockState.TPropertiesParser.Create(AInfo: TParseInfo; ABlocks: TBlockList.TReader; ARequired: Boolean);
begin
  FBlocks := ABlocks;
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
begin
  if not StartsWith('[') then
    Exit(False);

  if not FBlocks.Empty then
    BeginSuggestions(TPropertySuggestions.Create(FBlocks));
  SkipWhitespace;
  if not StartsWith(']') then
  begin
    while True do
    begin
      PropertyName := ReadWhile(IdentChars);
      EndSuggestions;

      Prop := nil;
      if PropertyName.IsEmpty then
        Log(1, 'Expected block state property name.')
      else if BlockExists and not Properties.Get(PropertyName, Prop) then
        Log(-PropertyName.Length, '"%s" is not a valid property for "%s".', [PropertyName, NSPath.Format]);

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

      ParseResult.Properties.Add(TPropertyValue.Create(PropertyName, PropertyValue));

      SkipWhitespace;

      if StartsWith(']') then
        Break;

      if not StartsWith(',') then
        raise EParseError.Create('Expected "]" or ",".');

      if BlockExists then
        BeginSuggestions(TPropertySuggestions.Create(Properties));

      SkipWhitespace;
    end;
  end;
end;

end.
