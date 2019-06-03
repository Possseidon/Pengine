unit FactoryDefine;

interface

uses
  System.SysUtils,

  Vcl.ExtCtrls,

  Pengine.Interfaces,
  Pengine.JSON,
  Pengine.ICollections,
  Pengine.Vector,
  Pengine.EventHandling,

  GdiPlus,

  Pengine.Factorio.General;

type

  TFactory = class;

  // TODO: Base class
  TFactoryIO = class
  private

  public

  end;

  TMachineArray = class(TFactoryIO)
  public type

    TEventInfo = TSenderEventInfo<TMachineArray>;

    TEvent = TEvent<TEventInfo>;

  private
    FFactory: TFactory;
    FPos: TVector2;
    FRecipe: TFactorio.TRecipe;
    FCraftingMachine: TFactorio.TCraftingMachine;
    FCount: Integer;
    FPerformance: Single;
    FOnChange: TEvent;
    FOnRemove: TEvent;

    function GetOnChange: TEvent.TAccess;
    function GetOnRemove: TEvent.TAccess;

    procedure Change;
    procedure SetCraftingMachine(const Value: TFactorio.TCraftingMachine);
    procedure SetCount(const Value: Integer);
    procedure SetPos(const Value: TVector2);
    procedure SetRecipe(const Value: TFactorio.TRecipe);
    procedure SetPerformance(const Value: Single);
    function GetBounds: TBounds2;

  public
    constructor Create(AFactory: TFactory; APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine);
    procedure BeforeDestruction; override;
    procedure Remove;

    property Factory: TFactory read FFactory;

    property Pos: TVector2 read FPos write SetPos;
    property Bounds: TBounds2 read GetBounds;

    property CraftingMachine: TFactorio.TCraftingMachine read FCraftingMachine write SetCraftingMachine;
    property Count: Integer read FCount write SetCount;
    property Performance: Single read FPerformance write SetPerformance;
    function HasRecipe: Boolean;
    property Recipe: TFactorio.TRecipe read FRecipe write SetRecipe;

    property OnChange: TEvent.TAccess read GetOnChange;
    property OnRemove: TEvent.TAccess read GetOnRemove;

    procedure Draw(G: IGPGraphics);

  end;

  TFactory = class(TInterfaceBase, IJSerializable)
  public type

    TEventInfo = TSenderEventInfo<TFactory>;

    TEvent = TEvent<TEventInfo>;

    TMachineArrayEventInfo = class(TEventInfo)
    private
      FMachineArray: TMachineArray;

    public
      constructor Create(ASender: TFactory; AMachineArray: TMachineArray);

      property MachineArray: TMachineArray read FMachineArray;

    end;

    TMachineEvent = TEvent<TMachineArrayEventInfo>;

  private
    FMachineArrays: IObjectList<TMachineArray>;
    FOnMachineArrayAdd: TMachineEvent;
    FOnMachineArrayChange: TMachineEvent;
    FOnMachineArrayRemove: TMachineEvent;

    function GetMachineArrays: IReadonlyList<TMachineArray>;

    procedure MachineArrayChange(AInfo: TMachineArray.TEventInfo);
    procedure MachineArrayRemove(AInfo: TMachineArray.TEventInfo);
    function GetOnMachineArrayAdd: TMachineEvent.TAccess;
    function GetOnMachineArrayChange: TMachineEvent.TAccess;
    function GetOnMachineArrayRemove: TMachineEvent.TAccess;

  public
    constructor Create;

    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

    property MachineArrays: IReadonlyList<TMachineArray> read GetMachineArrays;
    function AddMachineArray(APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine): TMachineArray;
    procedure RemoveMachineArray(AMachineArray: TMachineArray);

    property OnMachineArrayAdd: TMachineEvent.TAccess read GetOnMachineArrayAdd;
    property OnMachineArrayRemove: TMachineEvent.TAccess read GetOnMachineArrayRemove;
    property OnMachineArrayChange: TMachineEvent.TAccess read GetOnMachineArrayChange;

    function MachineArrayAt(APos: TVector2): TMachineArray;

    procedure Draw(G: IGPGraphics);

  end;

implementation

{ TFactory }

function TFactory.AddMachineArray(APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine): TMachineArray;
begin
  Result := TMachineArray.Create(Self, APos, ACraftingMachine);
  FMachineArrays.Add(Result);
  Result.OnChange.Add(MachineArrayChange);
  Result.OnRemove.Add(MachineArrayRemove);
  FOnMachineArrayAdd.Execute(TMachineArrayEventInfo.Create(Self, Result));
end;

constructor TFactory.Create;
begin
  FMachineArrays := TObjectList<TMachineArray>.Create;
end;

procedure TFactory.DefineJStorage(ASerializer: TJSerializer);
begin

end;

procedure TFactory.Draw(G: IGPGraphics);
var
  MachineArray: TMachineArray;
begin
  for MachineArray in MachineArrays do
    MachineArray.Draw(G);
end;

function TFactory.GetMachineArrays: IReadonlyList<TMachineArray>;
begin
  Result := FMachineArrays.ReadonlyList;
end;

function TFactory.GetOnMachineArrayAdd: TMachineEvent.TAccess;
begin
  Result := FOnMachineArrayAdd.Access;
end;

function TFactory.GetOnMachineArrayChange: TMachineEvent.TAccess;
begin
  Result := FOnMachineArrayChange.Access;
end;

function TFactory.GetOnMachineArrayRemove: TMachineEvent.TAccess;
begin
  Result := FOnMachineArrayRemove.Access;
end;

function TFactory.MachineArrayAt(APos: TVector2): TMachineArray;
begin
  for Result in MachineArrays.Reverse do
    if APos in Result.Bounds then
      Exit;
  Result := nil;
end;

procedure TFactory.MachineArrayChange(AInfo: TMachineArray.TEventInfo);
begin
  FOnMachineArrayChange.Execute(TMachineArrayEventInfo.Create(Self, AInfo.Sender));
end;

procedure TFactory.MachineArrayRemove(AInfo: TMachineArray.TEventInfo);
begin
  FOnMachineArrayRemove.Execute(TMachineArrayEventInfo.Create(Self, AInfo.Sender));
end;

procedure TFactory.RemoveMachineArray(AMachineArray: TMachineArray);
begin
  FMachineArrays.Remove(AMachineArray);
end;

function TFactory.GetJVersion: Integer;
begin
  Result := 0;
end;

{ TMachineArray }

procedure TMachineArray.BeforeDestruction;
begin
  FOnRemove.Execute(TEventInfo.Create(Self));
end;

procedure TMachineArray.Change;
begin
  FOnChange.Execute(TEventInfo.Create(Self));
end;

constructor TMachineArray.Create(AFactory: TFactory; APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine);
begin
  FFactory := AFactory;
  FPos := APos;
  FCraftingMachine := ACraftingMachine;
  FCount := 1;
  FPerformance := 1;
end;

procedure TMachineArray.Draw(G: IGPGraphics);
var
  B: TBounds2;
  Rect: TGPRectF;
  Pen: IGPPen;
  Font: IGPFont;
  FontBrush: IGPBrush;
begin
  B := Bounds;
  Rect:= TGPRectF.Create(B.C1.X, B.C1.Y, B.Width, B.Height);
  Pen := TGPPen.Create(TGPColor.Black);
  G.DrawRectangle(Pen, Rect);

  Font := TGPFont.Create('Tahoma', 9);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  G.DrawString(Format('%dx', [Count]), Font, TGPPointF.Create(Pos.X + 10, Pos.Y + 32), FontBrush);
  G.DrawString(Format('%.0f%%', [Performance * 100]), Font, TGPPointF.Create(Pos.X + 40, Pos.Y + 32), FontBrush);
  G.DrawImage(CraftingMachine.Icon, Pos.X + 16, Pos.Y);
  if HasRecipe then
    G.DrawImage(Recipe.Icon, Pos.X + 48, Pos.Y);
end;

function TMachineArray.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TMachineArray.HasRecipe: Boolean;
begin
  Result := FRecipe <> nil;
end;

procedure TMachineArray.Remove;
begin
  Factory.RemoveMachineArray(Self);
end;

function TMachineArray.GetBounds: TBounds2;
begin
  // TODO
  Result := Pos.Bounds(100);
end;

function TMachineArray.GetOnChange: TEvent.TAccess;
begin
  Result := FOnChange.Access;
end;

procedure TMachineArray.SetCraftingMachine(const Value: TFactorio.TCraftingMachine);
begin
  if CraftingMachine = Value then
    Exit;
  FCraftingMachine := Value;

  if HasRecipe and not CraftingMachine.CraftingCategories.Contains(Recipe.Category) then
    Recipe := nil;
  Change;
end;

procedure TMachineArray.SetCount(const Value: Integer);
begin
  if Count = Value then
    Exit;
  FCount := Value;
  Change;
end;

procedure TMachineArray.SetPerformance(const Value: Single);
begin
  if Performance = Value then
    Exit;
  FPerformance := Value;
  Change;
end;

procedure TMachineArray.SetPos(const Value: TVector2);
begin
  if Pos = Value then
    Exit;
  FPos := Value;
  Change;
end;

procedure TMachineArray.SetRecipe(const Value: TFactorio.TRecipe);
begin
  if Recipe = Value then
    Exit;
  FRecipe := Value;
  Change;
end;

{ TFactory.TMachineArrayEventInfo }

constructor TFactory.TMachineArrayEventInfo.Create(ASender: TFactory; AMachineArray: TMachineArray);
begin
  inherited Create(ASender);
  FMachineArray := AMachineArray;
end;

end.
