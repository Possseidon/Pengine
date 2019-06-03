unit RecipeForm;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,

  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.ExtCtrls,

  GdiPlus,
  GdiPlusHelpers,

  Pengine.ICollections,
  Pengine.IntMaths,
  Pengine.Utility,

  Pengine.Factorio.General,

  FactoryDefine;

type

  TBoxDrawType = (
    dtDefault,
    dtSelected
    );

  TfrmRecipes = class(TForm)
    gbCraftingMachineType: TGroupBox;
    pbCraftingMachine: TPaintBox;
    pnlMachineArray: TPanel;
    lbCount: TLabel;
    lbPerformance: TLabel;
    lbPerformanceUnit: TLabel;
    seCount: TSpinEdit;
    edtPerformance: TEdit;
    gbRecipe: TGroupBox;
    pbRecipe: TPaintBox;
    pbGroup: TPaintBox;
    lbMachineName: TLabel;
    lbGroupName: TLabel;
    lbRecipeName: TLabel;
    btnRemove: TButton;
    tmrResize: TTimer;
    procedure btnRemoveClick(Sender: TObject);
    procedure edtPerformanceChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure pbCraftingMachineMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbCraftingMachinePaint(Sender: TObject);
    procedure pbGroupPaint(Sender: TObject);
    procedure pbGroupMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbRecipeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbRecipePaint(Sender: TObject);
    procedure seCountChange(Sender: TObject);
    procedure tmrResizeTimer(Sender: TObject);
  private
    FMachineArray: TMachineArray;
    FTargetWidth: Integer;
    FTargetHeight: Integer;
    FSelectedGroup: TFactorio.TItemGroup;

    procedure DrawBox(G: IGPGraphics; X, Y, Size: Integer; AType: TBoxDrawType);

    procedure MachineArrayChange;
    procedure MachineArrayDestroy;

    function GetFactorio: TFactorio;

    function Groups: IIterate<TFactorio.TItemGroup>;
    function Subgroups: IIterate<TFactorio.TItemSubgroup>;
    function Recipes: IIterate<IIterate<TFactorio.TRecipe>>;

    function CraftingMachinesWidth: Integer;
    function GroupWidth: Integer;
    function RecipeWidth: Integer;
    function RecipeHeight: Integer;

    procedure UpdateSize;
    procedure UpdateGroupName;
    procedure UpdateRecipeName;

    procedure MakeOpaque(AControl: TControl);

  public
    constructor Create(AOwner: TComponent); override;

    procedure Execute(AMachineArray: TMachineArray);

    property Factorio: TFactorio read GetFactorio;

  end;

var
  frmRecipes: TfrmRecipes;

implementation

{$R *.dfm}


uses Main;

function TfrmRecipes.CraftingMachinesWidth: Integer;
begin
  Result := Factorio.CraftingMachine.Count * 38;
end;

constructor TfrmRecipes.Create(AOwner: TComponent);
begin
  inherited;
  MakeOpaque(gbCraftingMachineType);
  MakeOpaque(gbRecipe);
end;

procedure TfrmRecipes.btnRemoveClick(Sender: TObject);
begin
  FMachineArray.Remove;
end;

procedure TfrmRecipes.DrawBox(G: IGPGraphics; X, Y, Size: Integer; AType: TBoxDrawType);
const
  Colors: array [TBoxDrawType] of record Pen, Gradient1, Gradient2: Cardinal end = (
    (Pen: $FF000000; Gradient1: $FFBBBBBB; Gradient2: $FFEEEEEE),
    (Pen: $FFDD2222; Gradient1: $FF8888BB; Gradient2: $FFBBBBEE));
var
  Rect: TGPRect;
  Pen: IGPPen;
  Brush: IGPBrush;
begin
  Rect := TGPRect.Create(X, Y, Size, Size);
  Pen := TGPPen.Create(Colors[AType].Pen, 2);
  Brush := TGPLinearGradientBrush.Create(Rect, Colors[AType].Gradient1, Colors[AType].Gradient2, -90);
  G.FillRectangle(Brush, Rect);
  G.DrawRectangle(Pen, Rect);
end;

procedure TfrmRecipes.edtPerformanceChange(Sender: TObject);
var
  Value: Single;
begin
  if TryStrToFloat(edtPerformance.Text, Value) then
    FMachineArray.Performance := Value / 100;
end;

procedure TfrmRecipes.Execute(AMachineArray: TMachineArray);
begin
  Left := Mouse.CursorPos.X;
  Top := Mouse.CursorPos.Y;

  FMachineArray := AMachineArray;
  FMachineArray.OnRemove.Add(MachineArrayDestroy);
  FMachineArray.OnChange.Add(MachineArrayChange);
  if FMachineArray.HasRecipe then
    FSelectedGroup := FMachineArray.Recipe.Group;
  MachineArrayChange;

  seCount.Text := IntToStr(FMachineArray.Count);
  edtPerformance.Text := PrettyFloat(FMachineArray.Performance * 100);

  Show;
end;

procedure TfrmRecipes.FormDeactivate(Sender: TObject);
begin
  if FMachineArray <> nil then
  begin
    FMachineArray.OnRemove.Remove(MachineArrayDestroy);
    FMachineArray.OnChange.Remove(MachineArrayChange);
  end;
  Close;
end;

function TfrmRecipes.GetFactorio: TFactorio;
begin
  Result := frmMain.Factorio;
end;

procedure TfrmRecipes.MachineArrayChange;
var
  GroupExists: Boolean;
begin
  Invalidate;
  lbMachineName.Caption := string(FMachineArray.CraftingMachine.DisplayName);

  GroupExists := Groups.Any(
    function(Group: TFactorio.TItemGroup): Boolean
    begin
      Result := Group = FSelectedGroup;
    end);
  if not GroupExists and not Groups.Empty then
    FSelectedGroup := Groups.First;

  UpdateGroupName;
  UpdateRecipeName;
  UpdateSize;
end;

procedure TfrmRecipes.MachineArrayDestroy;
begin
  FMachineArray := nil;
  Close;
end;

procedure TfrmRecipes.MakeOpaque(AControl: TControl);
begin
  AControl.ControlStyle := AControl.ControlStyle + [csOpaque];
end;

procedure TfrmRecipes.pbCraftingMachinePaint(Sender: TObject);
var
  G: IGPGraphics;
  I: Integer;
  Machine: TFactorio.TCraftingMachine;
  BoxType: TBoxDrawType;
begin
  G := pbCraftingMachine.ToGPGraphics;
  I := 0;
  for Machine in Factorio.CraftingMachineOrder do
  begin
    if Machine = FMachineArray.CraftingMachine then
      BoxType := dtSelected
    else
      BoxType := dtDefault;
    DrawBox(G, 1 + I * 38, 1, 34, BoxType);

    G.DrawImage(Machine.Icon, 2 + I * 38, 2);
    Inc(I);
  end;
end;

procedure TfrmRecipes.pbGroupPaint(Sender: TObject);
var
  G: IGPGraphics;
  I: Integer;
  Group: TFactorio.TItemGroup;
  BoxType: TBoxDrawType;
begin
  G := pbGroup.ToGPGraphics;
  I := 0;
  for Group in Groups do
  begin
    if Group = FSelectedGroup then
      BoxType := dtSelected
    else
      BoxType := dtDefault;
    DrawBox(G, 1 + I * 70, 1, 66, BoxType);

    G.DrawImage(Group.Icon, 2 + I * 70, 2);
    Inc(I);
  end;
end;

function TfrmRecipes.Groups: IIterate<TFactorio.TItemGroup>;
begin
  Result := Factorio.ItemGroupOrder.Iterate.Where(
    function(Group: TFactorio.TItemGroup): Boolean
    begin
      Result := Group.Subgroups.Iterate.Any(
        function(Subgroup: TFactorio.TItemSubgroup): Boolean
        begin
          Result := Subgroup.Recipes.Iterate.Any(
            function(Recipe: TFactorio.TRecipe): Boolean
            begin
              Result := FMachineArray.CraftingMachine.CraftingCategories.Contains(Recipe.Category);
            end);
        end);
    end);
end;

function TfrmRecipes.GroupWidth: Integer;
begin
  Result := Groups.Count * 70;
end;

procedure TfrmRecipes.pbCraftingMachineMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
begin
  X := Max(X, 0) div 38;
  if X >= Factorio.CraftingMachineOrder.Count then
    Exit;
  FMachineArray.CraftingMachine := Factorio.CraftingMachineOrder[X];
end;

procedure TfrmRecipes.pbGroupMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
begin
  X := Max(X, 0) div 70;
  if X >= Groups.Count then
    Exit;
  FSelectedGroup := Groups.Items[X];
  Invalidate;
  UpdateSize;
  UpdateGroupName;
end;

procedure TfrmRecipes.pbRecipeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SubgroupRecipes: IIterate<TFactorio.TRecipe>;
begin
  Y := Max(Y, 0) div 38;
  if Y >= Recipes.Count then
    Exit;
  SubgroupRecipes := Recipes.Items[Y];
  X := Max(X, 0) div 38;
  if X >= SubgroupRecipes.Count then
    Exit;
  FMachineArray.Recipe := SubgroupRecipes.Items[X];
end;

procedure TfrmRecipes.pbRecipePaint(Sender: TObject);
var
  G: IGPGraphics;
  Pos: TIntVector2;
  SubgroupRecipes: IIterate<TFactorio.TRecipe>;
  Recipe: TFactorio.TRecipe;
  BoxType: TBoxDrawType;
begin
  G := pbRecipe.ToGPGraphics;
  Pos := 0;
  for SubgroupRecipes in Recipes do
  begin
    for Recipe in SubgroupRecipes do
    begin
      if Recipe = FMachineArray.Recipe then
        BoxType := dtSelected
      else
        BoxType := dtDefault;
      DrawBox(G, 1 + Pos.X * 38, 1 + Pos.Y * 38, 34, BoxType);

      G.DrawImage(Recipe.Icon, 2 + Pos.X * 38, 2 + Pos.Y * 38);
      Inc(Pos.X);
    end;
    Pos.X := 0;
    Inc(Pos.Y);
  end;
end;

function TfrmRecipes.RecipeHeight: Integer;
begin
  Result := Recipes.Count * 38;
end;

function TfrmRecipes.Recipes: IIterate<IIterate<TFactorio.TRecipe>>;
begin
  Result := Subgroups.Generic.Map < IIterate < TFactorio.TRecipe >> (
    function(Subgroup: TFactorio.TItemSubgroup): IIterate<TFactorio.TRecipe>
    begin
      Result := Subgroup.Recipes.Iterate.Where(
        function(Recipe: TFactorio.TRecipe): Boolean
        begin
          Result := FMachineArray.CraftingMachine.CraftingCategories.Contains(Recipe.Category);
        end);
    end);
end;

function TfrmRecipes.RecipeWidth: Integer;
var
  SubgroupRecipes: IIterate<TFactorio.TRecipe>;
begin
  Result := 0;
  for SubgroupRecipes in Recipes do
    Result := Max(Result, SubgroupRecipes.Count);
  Result := Result * 38;
end;

procedure TfrmRecipes.seCountChange(Sender: TObject);
var
  Value: Integer;
begin
  if TryStrToInt(seCount.Text, Value) then
    FMachineArray.Count := Value;
end;

function TfrmRecipes.Subgroups: IIterate<TFactorio.TItemSubgroup>;
begin
  Result := FSelectedGroup.Subgroups.Iterate.Where(
    function(Subgroup: TFactorio.TItemSubgroup): Boolean
    begin
      Result := Subgroup.Recipes.Iterate.Any(
        function(Recipe: TFactorio.TRecipe): Boolean
        begin
          Result := FMachineArray.CraftingMachine.CraftingCategories.Contains(Recipe.Category);
        end);
    end);
end;

procedure TfrmRecipes.tmrResizeTimer(Sender: TObject);
begin
  if (ClientWidth = FTargetWidth) and (ClientHeight = FTargetHeight) then
  begin
    tmrResize.Enabled := False;
    Exit;
  end;
  ClientWidth := Round(ClientWidth * 0.5 + FTargetWidth * 0.5);
  ClientHeight := Round(ClientHeight * 0.5 + FTargetHeight * 0.5);
end;

procedure TfrmRecipes.UpdateGroupName;
begin
  lbGroupName.Caption := string(FSelectedGroup.DisplayName);
end;

procedure TfrmRecipes.UpdateRecipeName;
begin
  if FMachineArray.HasRecipe then
    lbRecipeName.Caption := string(FMachineArray.Recipe.DisplayName)
  else
    lbRecipeName.Caption := '[none]';
end;

procedure TfrmRecipes.UpdateSize;
var
  NewWidth: Integer;
begin
  NewWidth := CraftingMachinesWidth;
  NewWidth := Max(NewWidth, GroupWidth);
  NewWidth := Max(NewWidth, RecipeWidth);
  FTargetWidth := ClientWidth - pbCraftingMachine.Width + NewWidth;
  FTargetHeight := ClientHeight - pbRecipe.Height + RecipeHeight;
  tmrResize.Enabled := True;
end;

end.
