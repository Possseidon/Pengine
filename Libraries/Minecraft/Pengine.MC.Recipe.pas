unit Pengine.MC.Recipe;

interface

uses
  System.SysUtils,

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
      constructor Create(ARecipe: TRecipe);
      destructor Destroy; override;

      property Recipe: TRecipe read FRecipe;

      property Items: TItems read FItems;
      property Tags: TTags read FTags;

      procedure Load(AJValue: TJValue);

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
    constructor Create(AJObjct: TJObject); overload;

    class function GetType: TType; virtual; abstract;
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

  end;

  /// <summary>A crafting recipe for which only the ingredients put not the positioning is relevant.</summary>
  TRecipeShapeless = class(TRecipeCrafting)
  public type

    TIngredients = TObjectArray<TRecipe.TIngredient>;

  private
    FIngredients: TIngredients;

  public
    class function GetType: TRecipe.TType; override;

    property Ingredients: TIngredients read FIngredients;

  end;

  /// <summary>A classic crafting recipe where both item type and positioning are relevant.</summary>
  TRecipeShaped = class(TRecipeCrafting)
  public type

    TKey = Char;

    TShape = array of array of TKey;

    TKeys = TToObjectMap<TKey, TRecipe.TIngredient, TCharHasher>;

  private
    FShape: TShape;
    FKeys: TKeys;

    function GetIngredient(APos: TIntVector2): TRecipe.TIngredient;
    function GetShape(APos: TIntVector2): TKey;
    function GetShapeSize: TIntVector2;
    procedure SetIngredient(APos: TIntVector2; const Value: TRecipe.TIngredient);
    procedure SetShape(APos: TIntVector2; const Value: TKey);
    procedure SetShapeSize(const Value: TIntVector2);

  public
    constructor Create;
    destructor Destroy; override;

    class function GetType: TRecipe.TType; override;

    /// <summary>The size of the shape.</summary>
    property ShapeSize: TIntVector2 read GetShapeSize write SetShapeSize;
    /// <summary>Which key is used at a specified position.</summary>
    property Shape[APos: TIntVector2]: TKey read GetShape write SetShape;

    /// <summary>A map specifying which key corresponds to which ingredient.</summary>
    property Keys: TKeys read FKeys;

    /// <summary>The ingredient at a certain position in the crafting grid.</summary>
    /// <remarks>Setting an item, which is not yet in the key map will generate a new key.</remarks>
    property Ingredients[APos: TIntVector2]: TRecipe.TIngredient read GetIngredient write SetIngredient;

    /// <summary>Generates an unused key by using the first unused uppercase letter.</summary>
    function GenerateFreeKey: TKey; overload;
    /// <summary>Generates an unused key from the given key list.</summary>
    function GenerateFreeKey(AKeyList: System.TArray<TKey>): TKey; overload;

  end;

  /// <summary>A recipe for furnace smelting.</summary>
  TRecipeSmelting = class(TRecipe)
  private

  public
    class function GetType: TRecipe.TType; override;

  end;

  /// <summary>A special type of recipe, with hard-coded behavior and no parameters.</summary>
  TRecipeSpecial = class(TRecipe)
  private
    FName: string;

  public
    class function GetType: TRecipe.TType; override;

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
    JName := AJObject['item'].Cast<TJString>;
    if JName.Exists then
    begin
      if not Recipe.Items.Get(JName.AsString, Item) then
        raise ERecipeError.CreateFmt('Unknown item: %s', [JName.AsString]);
      Items.Add(Item);
    end;

    JName := AJObject['tag'].Cast<TJString>;
    if JName.Exists then
    begin
      if not Recipe.Tags.Get(JName.AsString, Tag) then
        raise ERecipeError.CreateFmt('Unknown tag: %s', [JName.AsString]);
      Tags.Add(Tag);
    end;
  end;

begin
  if AJValue is TJObject then
  begin
    LoadSingleEntry(TJObject(AJValue));
  end
  else if AJValue is TJArray then
  begin

  end;

end;

{ TRecipeShaped }

constructor TRecipeShaped.Create;
begin

end;

destructor TRecipeShaped.Destroy;
begin

  inherited;
end;

function TRecipeShaped.GenerateFreeKey(AKeyList: System.TArray<TKey>): TKey;
begin

end;

function TRecipeShaped.GenerateFreeKey: TKey;
begin

end;

function TRecipeShaped.GetIngredient(APos: TIntVector2): TRecipe.TIngredient;
begin

end;

function TRecipeShaped.GetShape(APos: TIntVector2): TKey;
begin

end;

function TRecipeShaped.GetShapeSize: TIntVector2;
begin

end;

class function TRecipeShaped.GetType: TRecipe.TType;
begin
  Result := rtShaped;
end;

procedure TRecipeShaped.SetIngredient(APos: TIntVector2; const Value: TRecipe.TIngredient);
begin

end;

procedure TRecipeShaped.SetShape(APos: TIntVector2; const Value: TKey);
begin

end;

procedure TRecipeShaped.SetShapeSize(const Value: TIntVector2);
begin

end;

{ TRecipe }

constructor TRecipe.Create;
begin
  FItems := RootSettingsG.Get<TItemSettings>.Items;
  FTags := RootSettingsG.Get<TItemTagSettings>.ItemTags;
end;

constructor TRecipe.Create(AJObjct: TJObject);
begin

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
  Result := TRecipeSpecial.Create;
end;

procedure TRecipe.Load(AJObject: TJObject);
begin
  // nothing by default
end;

function TRecipe.Save: TJObject;
begin
  Result := TJObject.Create;
  Result['type'] := RecipeNames[GetType];
end;

{ TRecipeShapeless }

class function TRecipeShapeless.GetType: TRecipe.TType;
begin
  Result := rtShapeless;
end;

{ TRecipeSmelting }

class function TRecipeSmelting.GetType: TRecipe.TType;
begin
  Result := rtSmelt;
end;

{ TRecipeSpecial }

class function TRecipeSpecial.GetType: TRecipe.TType;
begin
  Result := rtSpecial;
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
  if AJValue is TJString then
  begin
    ItemName := AJValue.AsString;
    Count := 1;
  end
  else
  begin
    ItemName := AJValue['item'].AsString;
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

end.
