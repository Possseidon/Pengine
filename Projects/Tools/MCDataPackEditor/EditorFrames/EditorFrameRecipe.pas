unit EditorFrameRecipe;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GdiPlus,
  GdiPlusHelpers,

  Pengine.JSON,
  Pengine.Settings,
  Pengine.IntMaths,

  Pengine.MC.Recipe,
  Pengine.MC.ItemIcons,
  Pengine.MC.Item,

  DatapackView;

type

  TfrmEditorRecipes = class(TFrame)
    cbRecipeType: TComboBox;
    Label1: TLabel;
    edtGroup: TEdit;
    Label2: TLabel;
    pbRecipe: TPaintBox;
    procedure cbRecipeTypeChange(Sender: TObject);
    procedure pbRecipePaint(Sender: TObject);
  private const

    ImageScale = 4;

  private
    FRecipe: TRecipe;
    FBackgroundImage: IGPBitmap;
    FIcons: TItemIconSettings.TIcons.TReader;

    function LoadImage(AName: string): IGPBitmap;

    function GetRecipeType: TRecipe.TType;
    procedure SetRecipeType(const Value: TRecipe.TType);

    procedure InitRecipeTypeBox;

    function GetIcon(AIngredients: TRecipe.TIngredient): IGPBitmap;
    
    procedure RenderShaped(G: IGPGraphics);
    procedure RenderShapeless(G: IGPGraphics);
    procedure RenderSmelt(G: IGPGraphics);
    procedure RenderSpecial(G: IGPGraphics);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Recipe: TRecipe read FRecipe;
    property RecipeType: TRecipe.TType read GetRecipeType write SetRecipeType;

  end;

  TEditorRecipes = class(TEditor)
  protected
    procedure SaveProc; override;
    procedure LoadProc; override;
    procedure ModifiedChanged; override;

  public
    class function GetFrameClass: TFrameClass; override;

    function Frame: TfrmEditorRecipes;

  end;

implementation

{$R *.dfm}

{ TEditorRecipes }

function TEditorRecipes.Frame: TfrmEditorRecipes;
begin
  Result := TfrmEditorRecipes(inherited Frame);
end;

class function TEditorRecipes.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorRecipes;
end;

procedure TEditorRecipes.LoadProc;
var
  JObject: TJObject;
begin
  JObject := TJObject.CreateFromFile(NodeData.FullPath);
  try
    Frame.Recipe.Load(JObject);

  finally
    JObject.Free;

  end;
end;

procedure TEditorRecipes.ModifiedChanged;
begin
  // TODO: What
end;

procedure TEditorRecipes.SaveProc;
begin
  // TODO: Save
end;

{ TfrmEditorRecipes }

constructor TfrmEditorRecipes.Create(AOwner: TComponent);
begin
  inherited;
  InitRecipeTypeBox;
  RecipeType := rtShaped;
  FIcons := RootSettingsG.Get<TItemIconSettings>.Icons;
end;

destructor TfrmEditorRecipes.Destroy;
begin
  FRecipe.Free;
  inherited;
end;

procedure TfrmEditorRecipes.cbRecipeTypeChange(Sender: TObject);
begin
  RecipeType := TRecipe.TType(cbRecipeType.ItemIndex);
end;

function TfrmEditorRecipes.GetIcon(AIngredients: TRecipe.TIngredient): IGPBitmap;
var
  Item: TItemType;
begin
  if AIngredients.Items.Empty then
    Item := AIngredients.Tags.First.ItemTypes.First
  else
    Item := AIngredients.Items.First;
  Result := FIcons[Item];
end;

function TfrmEditorRecipes.GetRecipeType: TRecipe.TType;
begin
  Result := FRecipe.GetType;
end;

procedure TfrmEditorRecipes.InitRecipeTypeBox;
var
  RecipeClass: TRecipeClass;
begin
  cbRecipeType.Items.BeginUpdate;
  try
    cbRecipeType.Clear;
    for RecipeClass in RecipeClasses do
      cbRecipeType.Items.Add(RecipeClass.GetDisplayName);

  finally
    cbRecipeType.Items.EndUpdate;

  end;
end;

function TfrmEditorRecipes.LoadImage(AName: string): IGPBitmap;
begin
  Result := TGPBitmap.FromFile(TPath.Combine('Data\assets\textures\gui', AName));
end;

procedure TfrmEditorRecipes.pbRecipePaint(Sender: TObject);
var
  G: IGPGraphics;
begin
  G := pbRecipe.ToGPGraphics;

  // Background
  G.PixelOffsetMode := PixelOffsetModeHalf;
  G.InterpolationMode := InterpolationModeNearestNeighbor;
  if FBackgroundImage <> nil then
    G.DrawImage(FBackgroundImage, 0, 0, FBackgroundImage.Width * ImageScale, FBackgroundImage.Height * ImageScale);
  
  // Items
  case Recipe.GetType of
    rtShapeless:
      RenderShapeless(G);
    rtShaped:
      RenderShaped(G);
    rtSmelt:
      RenderSmelt(G);
    rtSpecial:
      RenderSpecial(G);
  end;
end;

procedure TfrmEditorRecipes.RenderShaped(G: IGPGraphics);
const
  Origin: TIntVector2 = (X: 10; Y: 10);   
  Size: TIntVector2 = (X: 16; Y: 16);
  Space: TintVector2 = (X: 2; Y: 2);    
  
var
  Rec: TRecipeShaped;
  PatternPos, DrawPos: TIntVector2;
  Icon: IGPBitmap;
  Ingredient: TRecipe.TIngredient;
begin
  Rec := TRecipeShaped(Recipe);

  for PatternPos in Rec.PatternSize do
  begin
    Ingredient := Rec.Ingredients[PatternPos];
    if Ingredient = nil then
      Continue;
    Icon := GetIcon(Ingredient);
    DrawPos := Origin * ImageScale + PatternPos * (Size + Space) * ImageScale;
    G.DrawImage(Icon, DrawPos.X, DrawPos.Y, Size.X * ImageScale, Size.Y * ImageScale);
  end;
end;

procedure TfrmEditorRecipes.RenderShapeless(G: IGPGraphics);
var
  Rec: TRecipeShapeless;
begin
  Rec := TRecipeShapeless(Recipe);
end;

procedure TfrmEditorRecipes.RenderSmelt(G: IGPGraphics);
var
  Rec: TRecipeSmelting;
begin
  Rec := TRecipeSmelting(Recipe);
end;

procedure TfrmEditorRecipes.RenderSpecial(G: IGPGraphics);
var
  Rec: TRecipeSpecial;
begin
  Rec := TRecipeSpecial(Recipe);
end;

procedure TfrmEditorRecipes.SetRecipeType(const Value: TRecipe.TType);
begin
  FRecipe.Free;
  // TODO: Ask for confirmation and copy stuff over if possible
  FRecipe := RecipeClasses[Value].Create;
  cbRecipeType.ItemIndex := Ord(Value);

  if Recipe is TRecipeCrafting then
    FBackgroundImage := LoadImage('crafting.png')
  else if Recipe is TRecipeSmelting then
    FBackgroundImage := LoadImage('smelting.png')
  else
    FBackgroundImage := nil;

  edtGroup.Enabled := Recipe is TRecipeCrafting;

  pbRecipe.Invalidate;
end;

end.
