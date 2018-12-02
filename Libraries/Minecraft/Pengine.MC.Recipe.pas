unit Pengine.MC.Recipe;

interface

uses
  System.SysUtils,
  System.Math,

  Pengine.JSON,
  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.IntMaths,

  Pengine.MC.Item,
  Pengine.Settings;

type

  ERecipeError = class(Exception);

  /// <summary>A base class for all kinds of minecraft-recipes.</summary>
  TRecipe = class
  public type

    TType = (
      rtShapeless,
      rtShaped,
      rtSmelt,
      rtSpecial
      );

    /// <summary>A list of valid ingredients for one input.</summary>
    TIngredient = class
    public type

      TItems = TRefArray<TItemType>;

      TTags = TRefArray<TItemTag>;

    private
      FRecipe: TRecipe;
      FItems: TItems;
      FTags: TTags;

    public
      constructor Create(ARecipe: TRecipe); overload;
      constructor Create(ARecipe: TRecipe; AJValue: TJValue); overload;
      destructor Destroy; override;

      property Recipe: TRecipe read FRecipe;

      property Items: TItems read FItems;
      property Tags: TTags read FTags;

      procedure Load(AJValue: TJValue);
      function Save: TJValue;

    end;

    /// <summary>An item type and optional count representing the result of a recipe.</summary>
    TResult = class
    private
      FRecipe: TRecipe;
      FItem: TItemType;
      FCount: Integer;

    public
      constructor Create(ARecipe: TRecipe);

      property Recipe: TRecipe read FRecipe;

      property Item: TItemType read FItem write FItem;
      property Count: Integer read FCount write FCount;

      procedure Load(AJValue: TJValue);
      function Save: TJValue;

    end;

  private
    FItems: TItemTypeCollection;
    FTags: TItemTagCollection;

  public
    constructor Create; overload; virtual;
    constructor Create(AJObject: TJObject); overload;

    class function GetType: TType; virtual; abstract;
    class function GetName: string;
    class function GetDisplayName: string;

    class function CreateTyped(AJObject: TJObject): TRecipe;

    procedure Load(AJObject: TJObject); virtual;
    function Save: TJObject; virtual;

    property Items: TItemTypeCollection read FItems;
    property Tags: TItemTagCollection read FTags;

  end;

  /// <summary>A base class for recipes performed in a crafting table.</summary>
  TRecipeCrafting = class(TRecipe)
  public const

    /// <summary>The size of the crafting table grid.</summary>
    GridSize: TIntVector2 = (X: 3; Y: 3);

    /// <summary>The amount of items, which can be put in a crafting table.</summary>
    MaxCount = 3 * 3;

  private
    FGroup: string;
    FResult: TRecipe.TResult;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Load(AJObject: TJObject); override;
    function Save: TJObject; override;

    property Group: string read FGroup write FGroup;

    property Result: TRecipe.TResult read FResult;

  end;

  /// <summary>A crafting recipe for which only the ingredients put not the positioning is relevant.</summary>
  TRecipeShapeless = class(TRecipeCrafting)
  public type

    TIngredients = TObjectArray<TRecipe.TIngredient>;

  private
    FIngredients: TIngredients;

    function GetIngredient: TIngredients.TReader;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetType: TRecipe.TType; override;

    procedure Load(AJObject: TJObject); override;
    function Save: TJObject; override;

    /// <summary>A list of all ingredients.</summary>
    property Ingredients: TIngredients.TReader read GetIngredient;
    function AddIngredient: TRecipe.TIngredient;

  end;

  /// <summary>A classic crafting recipe where both item type and positioning are relevant.</summary>
  TRecipeShaped = class(TRecipeCrafting)
  public type

    TKey = Char;

    TPattern = array of array of TKey;

    TKeys = TToObjectPairArray<TKey, TRecipe.TIngredient>;

  private
    FPattern: TPattern;
    FKeys: TKeys;

    function GetPatternSize: TIntVector2;
    procedure SetPatternSize(const Value: TIntVector2);
    function GetPattern(APos: TIntVector2): TKey;
    procedure SetPattern(APos: TIntVector2; const Value: TKey);
    function GetKeys: TKeys.TReader;
    function GetIngredient(APos: TIntVector2): TRecipe.TIngredient;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetType: TRecipe.TType; override;

    procedure Load(AJObject: TJObject); override;
    function Save: TJObject; override;

    /// <summary>The size of the pattern.</summary>
    property PatternSize: TIntVector2 read GetPatternSize write SetPatternSize;
    /// <summary>Which key is used at a specified position.</summary>
    property Pattern[APos: TIntVector2]: TKey read GetPattern write SetPattern;

    /// <summary>A map specifying which key corresponds to which ingredient.</summary>
    property Keys: TKeys.TReader read GetKeys;
    /// <summary>Gets an ingredient key and returns nil, if it doesn't exist.</summary>
    function GetKey(AKey: TKey): TRecipe.TIngredient;
    /// <summary>Adds a new ingredient key.</summary>
    function AddKey(AKey: TKey): TRecipe.TIngredient;
    /// <summary>Removes an existing ingredient key.</summary>
    procedure RemoveKey(AKey: TKey);
    /// <returns>True, if the given key exists.</returns>
    function KeyExists(AKey: TKey): Boolean;

    /// <summary>Generates an unused key by using the first unused uppercase letter.</summary>
    function GenerateFreeKey: TKey; overload;
    /// <summary>Generates an unused key from the given key list.</summary>
    function GenerateFreeKey(AKeyList: System.TArray<TKey>): TKey; overload;

    /// <summary>The ingredient at a certain position in the crafting grid.</summary>
    property Ingredients[APos: TIntVector2]: TRecipe.TIngredient read GetIngredient;

  end;

  /// <summary>A recipe for furnace smelting.</summary>
  TRecipeSmelting = class(TRecipe)
  private
    FIngredient: TRecipe.TIngredient;
    FResult: TItemType;
    FExperience: Single;
    FCookingTime: Integer;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetType: TRecipe.TType; override;

    procedure Load(AJObject: TJObject); override;
    function Save: TJObject; override;

    property Ingredient: TRecipe.TIngredient read FIngredient;
    property Result: TItemType read FResult;
    property Experience: Single read FExperience write FExperience;
    property CookingTime: Integer read FCookingTime write FCookingTime;

  end;

  /// <summary>A special type of recipe, with hard-coded behavior and no parameters.</summary>
  TRecipeSpecial = class(TRecipe)
  private
    FName: string;

  public
    class function GetType: TRecipe.TType; override;

    procedure Load(AJObject: TJObject); override;

    property Name: string read FName write FName;

  end;

  TRecipeClass = class of TRecipe;

const

  RecipeNames: array [TRecipe.TType] of string = (
    'crafting_shapeless',
    'crafting_shaped',
    'smelting',
    ''
    );

  RecipeDisplayNames: array [TRecipe.TType] of string = (
    'Shapeless-Crafting',
    'Shaped-Crafting',
    'Smelting',
    '[Special]'
  );

  RecipeClasses: array [TRecipe.TType] of TRecipeClass = (
    TRecipeShapeless,
    TRecipeShaped,
    TRecipeSmelting,
    TRecipeSpecial
    );

implementation

{ TRecipe.TIngredient }

constructor TRecipe.TIngredient.Create(ARecipe: TRecipe);
begin
  FRecipe := ARecipe;
  FItems := TItems.Create;
  FTags := TTags.Create;
end;

constructor TRecipe.TIngredient.Create(ARecipe: TRecipe; AJValue: TJValue);
begin
  Create(ARecipe);
  Load(AJValue);
end;

destructor TRecipe.TIngredient.Destroy;
begin
  FItems.Free;
  FTags.Free;
  inherited;
end;

procedure TRecipe.TIngredient.Load(AJValue: TJValue);

  procedure LoadSingleEntry(AJObject: TJObject);
  var
    JName: TJString;
    Item: TItemType;
    Tag: TItemTag;
  begin
    if AJObject.Get('item', JName) then
    begin
      if not Recipe.Items.Get(JName.Text, Item) then
        raise ERecipeError.CreateFmt('Unknown item: %s', [JName.Text]);
      Items.Add(Item);
    end
    else if AJObject.Get('tag', JName) then
    begin
      if not Recipe.Tags.Get(JName.Text, Tag) then
        raise ERecipeError.CreateFmt('Unknown tag: %s', [JName.Text]);
      Tags.Add(Tag);
    end;
  end;

var
  Item: TJValue;
begin
  if AJValue.IsObject then
    LoadSingleEntry(AJValue)
  else if AJValue.IsArray then
    for Item in TJArray(AJValue.Value) do
      LoadSingleEntry(Item)
  else
    raise ERecipeError.Create('Ingredient must be a JSON-Object or a JSON-Array of such JSON-Objects.');
end;

function TRecipe.TIngredient.Save: TJValue;
var
  I: Integer;
begin
  if Items.Count + Tags.Count = 1 then
  begin
    Result := TJObject.Create;
    if Tags.Empty then
      Result['item'] := Items.First.NSPath.Format
    else
      Result['tag'] := Tags.First.NSPath.Format;
    Exit;
  end;

  Result := TJArray.Create;
  for I := 0 to Items.MaxIndex do
    Result.AddObject['item'] := Items[I].NSPath.Format;
  for I := 0 to Tags.MaxIndex do
    Result.AddObject['tag'] := Tags[I].NSPath.Format;
end;

{ TRecipeShaped }

function TRecipeShaped.GetPatternSize: TIntVector2;
begin
  Result.X := Length(FPattern);
  if Result.X > 0 then
    Result.Y := Length(FPattern[0])
  else
    Result.Y := 0;
end;

procedure TRecipeShaped.SetPatternSize(const Value: TIntVector2);
begin
  if PatternSize = Value then
    Exit;
  SetLength(FPattern, Value.X, Value.Y);
end;

function TRecipeShaped.GetPattern(APos: TIntVector2): TKey;
begin
  Result := FPattern[APos.X, APos.Y];
end;

function TRecipeShaped.Save: TJObject;
var
  I, J: Integer;
  JPattern: TJArray;
  PatternLine: string;
  JKeys: TJObject;
  Pair: TKeys.TPair;
begin
  Result := inherited;

  JPattern := Result.AddArray('pattern');
  for I := 0 to PatternSize.Y - 1 do
  begin
    PatternLine := '';
    for J := 0 to PatternSize.X - 1 do
      PatternLine := PatternLine + FPattern[I, J];
    JPattern.Add(PatternLine);
  end;

  JKeys := Result.AddObject('key'); 
  for Pair in Keys do
    JKeys[Pair.Key] := Pair.Value.Save;
end;

procedure TRecipeShaped.SetPattern(APos: TIntVector2; const Value: TKey);
begin
  FPattern[APos.X, APos.Y] := Value;
end;

function TRecipeShaped.GetKey(AKey: TKey): TRecipe.TIngredient;
var
  I: Integer;
begin
  for I := 0 to Keys.MaxIndex do
    if Keys[I].Key = AKey then
      Exit(Keys[I].Value);
  Result := nil;
end;

function TRecipeShaped.GetKeys: TKeys.TReader;
begin
  Result := FKeys.Reader;
end;

function TRecipeShaped.GetIngredient(APos: TIntVector2): TRecipe.TIngredient;
begin
  if Pattern[APos] = ' ' then
    Exit(nil);
  Result := GetKey(Pattern[APos]);
end;

constructor TRecipeShaped.Create;
begin
  inherited;
  FKeys := TKeys.Create;
end;

destructor TRecipeShaped.Destroy;
begin
  FKeys.Free;
  inherited;
end;

class function TRecipeShaped.GetType: TRecipe.TType;
begin
  Result := rtShaped;
end;

function TRecipeShaped.KeyExists(AKey: TKey): Boolean;
begin
  Result := GetKey(AKey) <> nil;
end;

procedure TRecipeShaped.Load(AJObject: TJObject);
var
  JPair: TJPair;
  JPatternLine: TJValue;
  JPattern: TJArray;
  Width: Integer;
  Ingredient: TIngredient;
  P: TIntVector2;
  Line: string;
begin
  inherited;

  JPattern := AJObject['pattern'].AsArray;
  Width := 0;
  for JPatternLine in JPattern do
    Width := Max(Width, JPatternLine.AsString.Length);
  PatternSize := IVec2(Width, JPattern.Count);
  for P in PatternSize do
  begin
    Line := JPattern[P.Y].AsString;
    if P.X >= Line.Length then
      Pattern[P] := ' '
    else
      Pattern[P] := Line[P.X + 1];
  end;

  FKeys.Clear;
  for JPair in AJObject['key'].AsObject do
  begin
    if JPair.Key.Length > 1 then
      raise ERecipeError.Create('Ingredient keys can only be a single char.');
    Ingredient := AddKey(JPair.Key[1]);
    Ingredient.Load(JPair.Value);
  end;
end;

function TRecipeShaped.AddKey(AKey: TKey): TRecipe.TIngredient;
begin
  if GetKey(AKey) <> nil then
    raise ERecipeError.Create('Ingredient key exists already.');
  Result := TIngredient.Create(Self);
  FKeys.Add(TKeys.TPair.Create(AKey, Result));
end;

procedure TRecipeShaped.RemoveKey(AKey: TKey);
var
  I: Integer;
begin
  for I := 0 to Keys.MaxIndex do
    if Keys[I].Key = AKey then
      FKeys.RemoveAt(I);
  raise ERecipeError.Create('Ingredient key doesn''t exist.');
end;

function TRecipeShaped.GenerateFreeKey: TKey;
begin
  Result := 'A';
  while GetKey('A') <> nil do
    Inc(Result);
end;

function TRecipeShaped.GenerateFreeKey(AKeyList: System.TArray<TKey>): TKey;
var
  I: Integer;
begin
  for I := 0 to Length(AKeyList) - 1 do
    if not KeyExists(AKeyList[I]) then
      Exit(AKeyList[I]);
  raise ERecipeError.Create('Could not generate a free ingredient key with the given list.');
end;

{ TRecipe }

constructor TRecipe.Create;
begin
  FItems := RootSettingsG.Get<TItemSettings>.Items;
  FTags := RootSettingsG.Get<TItemTagSettings>.ItemTags;
end;

constructor TRecipe.Create(AJObject: TJObject);
begin
  Create;
  Load(AJObject);
end;

class function TRecipe.CreateTyped(AJObject: TJObject): TRecipe;
var
  TypeName: string;
  RecipeType: TType;
begin
  TypeName := AJObject['type'];
  for RecipeType := Low(TType) to Pred(rtSpecial) do
    if RecipeNames[RecipeType] = TypeName then
      Exit(RecipeClasses[RecipeType].Create(AJObject));
  Result := TRecipeSpecial.Create(AJObject);
end;

class function TRecipe.GetDisplayName: string;
begin
  Result := RecipeDisplayNames[GetType];
end;

class function TRecipe.GetName: string;
begin
  Result := RecipeNames[GetType];
end;

procedure TRecipe.Load(AJObject: TJObject);
begin
  // nothing by default
end;

function TRecipe.Save: TJObject;
begin
  Result := TJObject.Create;
  Result['type'] := GetName;
end;

{ TRecipeShapeless }

function TRecipeShapeless.AddIngredient: TRecipe.TIngredient;
begin
  Result := FIngredients.Add(TIngredient.Create(Self));
end;

constructor TRecipeShapeless.Create;
begin
  inherited;
  FIngredients := TIngredients.Create;
end;

destructor TRecipeShapeless.Destroy;
begin
  FIngredients.Free;
  inherited;
end;

function TRecipeShapeless.GetIngredient: TIngredients.TReader;
begin
  Result := FIngredients.Reader;
end;

class function TRecipeShapeless.GetType: TRecipe.TType;
begin
  Result := rtShapeless;
end;

procedure TRecipeShapeless.Load(AJObject: TJObject);
var
  JIngredient: TJValue;
begin
  inherited;
  FIngredients.Clear;
  for JIngredient in AJObject['ingredients'].AsArray do
    FIngredients.Add(TIngredient.Create(Self, JIngredient));
end;

function TRecipeShapeless.Save: TJObject;
var
  JIngredients: TJArray;
  Ingredient: TIngredient;
begin
  Result := inherited;
  JIngredients := Result.AddArray('ingredients');
  for Ingredient in Ingredients do
    JIngredients.Add(Ingredient.Save);
end;

{ TRecipeSmelting }

constructor TRecipeSmelting.Create;
begin
  inherited;
  FIngredient := TIngredient.Create(Self);
end;

destructor TRecipeSmelting.Destroy;
begin
  FIngredient.Free;
  inherited;
end;

class function TRecipeSmelting.GetType: TRecipe.TType;
begin
  Result := rtSmelt;
end;

procedure TRecipeSmelting.Load(AJObject: TJObject);
begin
  inherited;
  Ingredient.Load(AJObject['ingredient']);
  if not Items.Get(AJObject['result'].AsString, FResult) then
    raise ERecipeError.Create('Unknown ingredient item.');
  Experience := AJObject['experience'];
  CookingTime := AJObject['cookingtime'];
end;

function TRecipeSmelting.Save: TJObject;
begin
  Result := inherited;
  Result['ingredient'] := Ingredient.Save;
  Result['experience'] := Experience;
  Result['cookingtime'] := CookingTime;
end;

{ TRecipeSpecial }

class function TRecipeSpecial.GetType: TRecipe.TType;
begin
  Result := rtSpecial;
end;

procedure TRecipeSpecial.Load(AJObject: TJObject);
begin
  inherited;
  FName := AJObject['type'];
end;

{ TRecipe.TResult }

constructor TRecipe.TResult.Create(ARecipe: TRecipe);
begin
  FRecipe := ARecipe;
end;

procedure TRecipe.TResult.Load(AJValue: TJValue);
var
  ItemName: string;
begin
  if AJValue.IsString then
  begin
    ItemName := AJValue;
    Count := 1;
  end
  else
  begin
    ItemName := AJValue['item'];
    Count := AJValue['count'] or 1;
  end;

  if not Recipe.Items.Get(ItemName, FItem) then
    raise ERecipeError.CreateFmt('Unknown item type: %s', [ItemName]);
end;

function TRecipe.TResult.Save: TJValue;
begin
  if Count = 1 then
    Exit(TJString.Create(Item.NSPath));
  Result := TJObject.Create;
  Result['item'] := TJString.Create(Item.NSPath.Format);
  Result['count'] := Count;
end;

{ TRecipeCrafting }

constructor TRecipeCrafting.Create;
begin
  inherited;
  FResult := TResult.Create(Self);
end;

destructor TRecipeCrafting.Destroy;
begin
  FResult.Free;
  inherited;
end;

procedure TRecipeCrafting.Load(AJObject: TJObject);
begin
  inherited;
  Group := AJObject['group'] or '';
  Result.Load(AJObject['result']);
end;

function TRecipeCrafting.Save: TJObject;
begin
  Result := inherited;
  if Group <> '' then
    Result['group'] := Group;
  Result['result'] := Self.Result.Save;
end;

end.
