unit FactoryDefine;

interface

uses
  System.SysUtils,
  System.Math,

  Vcl.ExtCtrls,

  Pengine.Interfaces,
  Pengine.JSON,
  Pengine.ICollections,
  Pengine.Vector,
  Pengine.EventHandling,
  Pengine.Utility,

  GdiPlus,

  Pengine.Factorio.General;

type

  TFactory = class;
  TMachineArray = class;
  TMachineConnection = class;

  TMachineIO = class
  public type

    TEventInfo = TSenderEventInfo<TMachineIO>;
    TEvent = TEvent<TEventInfo>;

  private
    FMachineArray: TMachineArray;
    FIndex: Integer;
    FItemStack: TFactorio.TRecipe.TItemStack;
    FConnections: IList<TMachineConnection>;
    FOnRemove: TEvent;

    function GetItemsPerSecond: Single;
    function GetCraftingMachine: TFactorio.TCraftingMachine;
    function GetRecipe: TFactorio.TRecipe;
    function GetOnRemove: TEvent.TAccess;

    function GetPos: TVector2; virtual; abstract;
    function GetBounds: TBounds2;

    procedure DrawItemAmount(G: IGPGraphics; APos: TVector2);
    function GetConnections: IReadonlyList<TMachineConnection>;

  protected
    constructor Create(AMachineArray: TMachineArray; AIndex: Integer; AItemStack: TFactorio.TRecipe.TItemStack);

  public
    destructor Destroy; override;

    property MachineArray: TMachineArray read FMachineArray;
    property Index: Integer read FIndex;
    property CraftingMachine: TFactorio.TCraftingMachine read GetCraftingMachine;
    property Recipe: TFactorio.TRecipe read GetRecipe;

    property ItemStack: TFactorio.TRecipe.TItemStack read FItemStack;
    property ItemsPerSecond: Single read GetItemsPerSecond;

    property Connections: IReadonlyList<TMachineConnection> read GetConnections;

    property Pos: TVector2 read GetPos;
    property Bounds: TBounds2 read GetBounds;

    procedure Draw(G: IGPGraphics); virtual;

    property OnRemove: TEvent.TAccess read GetOnRemove;

  end;

  TMachineOutput = class;

  TMachineInput = class(TMachineIO)
  protected
    function GetPos: TVector2; override;

  public
    constructor Create(AMachineArray: TMachineArray; AIndex: Integer; AItemStack: TFactorio.TRecipe.TItemStack);

    function FindConnection(AOutput: TMachineOutput): TMachineConnection;
    function IsConnected(AOutput: TMachineOutput): Boolean;
    function Connect(AOutput: TMachineOutput): TMachineConnection;
    procedure Disconnect(AOutput: TMachineOutput);
    procedure RemoveConnection(AConnection: TMachineConnection);
    function ToggleConnection(AOutput: TMachineOutput): Boolean;

    procedure Draw(G: IGPGraphics); override;

  end;

  TMachineOutput = class(TMachineIO)
  protected
    function GetPos: TVector2; override;

  public
    constructor Create(AMachineArray: TMachineArray; AIndex: Integer; AItemStack: TFactorio.TRecipe.TItemStack);

    function FindConnection(AInput: TMachineInput): TMachineConnection;
    function IsConnected(AInput: TMachineInput): Boolean;
    function Connect(AInput: TMachineInput): TMachineConnection;
    procedure Disconnect(AInput: TMachineInput);
    procedure RemoveConnection(AConnection: TMachineConnection);
    function ToggleConnection(AOther: TMachineInput): Boolean;

    procedure Draw(G: IGPGraphics); override;

  end;

  TMachineConnection = class
  public type

    TEventInfo = TSenderEventInfo<TMachineConnection>;

    TEvent = TEvent<TEventInfo>;

  private
    FOutput: TMachineOutput;
    FInput: TMachineInput;
    FOnRemove: TEvent;

    function GetOnRemove: TEvent.TAccess;

  public
    constructor Create(AOutput: TMachineOutput; AInput: TMachineInput);
    destructor Destroy; override;

    procedure Remove;

    property Output: TMachineOutput read FOutput;
    property Input: TMachineInput read FInput;

    procedure Draw(G: IGPGraphics);

    property OnRemove: TEvent.TAccess read GetOnRemove;

  end;

  TMachineArray = class
  public type

    TEventInfo = TSenderEventInfo<TMachineArray>;

    TEvent = TEvent<TEventInfo>;

  private
    FFactory: TFactory;
    FPos: TVector2;
    FRecipe: TFactorio.TRecipe;
    FInputs: IObjectList<TMachineInput>;
    FOutputs: IObjectList<TMachineOutput>;
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

    procedure GenerateIO;
    function GetInputs: IReadonlyList<TMachineInput>;
    function GetOutputs: IReadonlyList<TMachineOutput>;
    function GetIOHeight: Integer;

  public
    constructor Create(AFactory: TFactory; APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine);
    destructor Destroy; override;
    procedure Remove;

    property Factory: TFactory read FFactory;

    property Pos: TVector2 read FPos write SetPos;
    property Bounds: TBounds2 read GetBounds;

    property CraftingMachine: TFactorio.TCraftingMachine read FCraftingMachine write SetCraftingMachine;
    property Count: Integer read FCount write SetCount;
    property Performance: Single read FPerformance write SetPerformance;

    function HasRecipe: Boolean;
    property Recipe: TFactorio.TRecipe read FRecipe write SetRecipe;
    property Inputs: IReadonlyList<TMachineInput> read GetInputs;
    property Outputs: IReadonlyList<TMachineOutput> read GetOutputs;
    property IOHeight: Integer read GetIOHeight;

    property OnChange: TEvent.TAccess read GetOnChange;
    property OnRemove: TEvent.TAccess read GetOnRemove;
    procedure Draw(G: IGPGraphics);

    function InputAt(APos: TVector2): TMachineInput;
    function OutputAt(APos: TVector2): TMachineOutput;

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

    TMachineArrayEvent = TEvent<TMachineArrayEventInfo>;

    TConnectionEventInfo = class(TEventInfo)
    private
      FConnection: TMachineConnection;

    public
      constructor Create(ASender: TFactory; AConnection: TMachineConnection);

      property Connection: TMachineConnection read FConnection;

    end;

    TConnectionEvent = TEvent<TConnectionEventInfo>;

  private
    FMachineArrays: IObjectList<TMachineArray>;
    FOnMachineArrayAdd: TMachineArrayEvent;
    FOnMachineArrayChange: TMachineArrayEvent;
    FOnMachineArrayRemove: TMachineArrayEvent;
    FOnConnectionAdd: TConnectionEvent;
    FOnConnectionChange: TConnectionEvent;
    FOnConnectionRemove: TConnectionEvent;

    function GetMachineArrays: IReadonlyList<TMachineArray>;

    procedure MachineArrayChange(AInfo: TMachineArray.TEventInfo);
    procedure MachineArrayRemove(AInfo: TMachineArray.TEventInfo);
    function GetOnMachineArrayAdd: TMachineArrayEvent.TAccess;
    function GetOnMachineArrayChange: TMachineArrayEvent.TAccess;
    function GetOnMachineArrayRemove: TMachineArrayEvent.TAccess;

    procedure NotifyConnectionAdd(AConnection: TMachineConnection);
    procedure NotifyConnectionChange(AConnection: TMachineConnection);
    procedure NotifyConnectionRemove(AConnection: TMachineConnection);
    function GetOnConnectionAdd: TConnectionEvent.TAccess;
    function GetOnConnectionChange: TConnectionEvent.TAccess;
    function GetOnConnectionRemove: TConnectionEvent.TAccess;

  public
    constructor Create;

    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

    property MachineArrays: IReadonlyList<TMachineArray> read GetMachineArrays;
    function AddMachineArray(APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine): TMachineArray;
    procedure RemoveMachineArray(AMachineArray: TMachineArray);

    property OnMachineArrayAdd: TMachineArrayEvent.TAccess read GetOnMachineArrayAdd;
    property OnMachineArrayRemove: TMachineArrayEvent.TAccess read GetOnMachineArrayRemove;
    property OnMachineArrayChange: TMachineArrayEvent.TAccess read GetOnMachineArrayChange;

    property OnConnectionAdd: TConnectionEvent.TAccess read GetOnConnectionAdd;
    property OnConnectionRemove: TConnectionEvent.TAccess read GetOnConnectionRemove;
    property OnConnectionChange: TConnectionEvent.TAccess read GetOnConnectionChange;

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

function TFactory.GetOnConnectionAdd: TConnectionEvent.TAccess;
begin
  Result := FOnConnectionAdd.Access;
end;

function TFactory.GetOnConnectionChange: TConnectionEvent.TAccess;
begin
  Result := FOnConnectionChange.Access;
end;

function TFactory.GetOnConnectionRemove: TConnectionEvent.TAccess;
begin
  Result := FOnConnectionRemove.Access;
end;

function TFactory.GetOnMachineArrayAdd: TMachineArrayEvent.TAccess;
begin
  Result := FOnMachineArrayAdd.Access;
end;

function TFactory.GetOnMachineArrayChange: TMachineArrayEvent.TAccess;
begin
  Result := FOnMachineArrayChange.Access;
end;

function TFactory.GetOnMachineArrayRemove: TMachineArrayEvent.TAccess;
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

procedure TFactory.NotifyConnectionAdd(AConnection: TMachineConnection);
begin
  FOnConnectionAdd.Execute(TConnectionEventInfo.Create(Self, AConnection));
end;

procedure TFactory.NotifyConnectionChange(AConnection: TMachineConnection);
begin
  FOnConnectionChange.Execute(TConnectionEventInfo.Create(Self, AConnection));
end;

procedure TFactory.NotifyConnectionRemove(AConnection: TMachineConnection);
begin
  FOnConnectionRemove.Execute(TConnectionEventInfo.Create(Self, AConnection));
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
  FInputs := TObjectList<TMachineInput>.Create;
  FOutputs := TObjectList<TMachineOutput>.Create;
end;

destructor TMachineArray.Destroy;
begin
  FOnRemove.Execute(TEventInfo.Create(Self));
  inherited;
end;

procedure TMachineArray.Draw(G: IGPGraphics);
var
  B: TBounds2;
  Rect: TGPRectF;
  Pen: IGPPen;
  Font: IGPFont;
  FontBrush: IGPBrush;
  BgBrush: IGPBrush;
  MachineIO: TMachineIO;
begin
  B := Bounds;
  Rect := TGPRectF.Create(B.C1.X, B.C1.Y, B.Width, B.Height);
  Pen := TGPPen.Create(TGPColor.Black);
  BgBrush := TGPLinearGradientBrush.Create(Rect, $FFAFAFDF, $FFDFDFFF, -90);
  G.FillRectangle(BgBrush, Rect);
  G.DrawRectangle(Pen, Rect);

  Font := TGPFont.Create('Tahoma', 8);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  G.DrawString(Format('%dx', [Count]), Font, TGPPointF.Create(Pos.X, Pos.Y + 8), FontBrush);
  // G.DrawString(Format('%.0f%%', [Performance * 100]), Font, TGPPointF.Create(Pos.X + 40, Pos.Y + 32), FontBrush);
  G.DrawImage(CraftingMachine.Icon, Pos.X + 32, Pos.Y);

  for MachineIO in Inputs do
    MachineIO.Draw(G);
  for MachineIO in Outputs do
    MachineIO.Draw(G);

  if HasRecipe and (Recipe.Icon <> Recipe.Results.First.Item.Icon) then
    G.DrawImage(Recipe.Icon, Pos.X + 64, Pos.Y);

  {
    begin
    for I := 0 to Recipe.Ingredients.MaxIndex do
    begin
    G.DrawString(
    Format('%dx', [Recipe.Ingredients[I].Amount]),
    Font,
    TGPPointF.Create(Pos.X + 4, Pos.Y + 40 + I * 48),
    FontBrush
    );
    G.DrawImage(
    Recipe.Ingredients[I].Item.Icon,
    Pos.X, Pos.Y + 56 + I * 48
    );

    G.DrawString(
    PrettyFloat(Single(Recipe.Ingredients[I].Amount * CraftingMachine.CraftingSpeed * Count * Performance /
    Recipe.EnergyRequired)),
    Font,
    TGPPointF.Create(Pos.X - 28, Pos.Y + 64 + I * 48),
    FontBrush
    );
    end;
    for I := 0 to Recipe.Results.MaxIndex do
    begin
    G.DrawString(
    Format('%dx', [Recipe.Results[I].Amount]),
    Font,
    TGPPointF.Create(Pos.X + 68, Pos.Y + 40 + I * 48),
    FontBrush
    );
    G.DrawImage(
    Recipe.Results[I].Item.Icon,
    Pos.X + 64, Pos.Y + 56 + I * 48
    );

    G.DrawString(
    PrettyFloat(Single(Recipe.Results[I].Amount * CraftingMachine.CraftingSpeed * Count * Performance /
    Recipe.EnergyRequired)),
    Font,
    TGPPointF.Create(Pos.X + 100, Pos.Y + 64 + I * 48),
    FontBrush
    );
    end;
    end;
  }
end;

function TMachineArray.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TMachineArray.GetOutputs: IReadonlyList<TMachineOutput>;
begin
  Result := FOutputs.ReadonlyList;
end;

function TMachineArray.HasRecipe: Boolean;
begin
  Result := FRecipe <> nil;
end;

function TMachineArray.InputAt(APos: TVector2): TMachineInput;
begin
  for Result in Inputs do
    if APos in Result.Bounds then
      Exit;
  Result := nil;
end;

function TMachineArray.OutputAt(APos: TVector2): TMachineOutput;
begin
  for Result in Outputs do
    if APos in Result.Bounds then
      Exit;
  Result := nil;
end;

procedure TMachineArray.Remove;
begin
  Factory.RemoveMachineArray(Self);
end;

procedure TMachineArray.GenerateIO;
var
  I: Integer;
begin
  FInputs.Clear;
  FOutputs.Clear;
  if not HasRecipe then
    Exit;

  for I := 0 to Recipe.Ingredients.MaxIndex do
    FInputs.Add(TMachineInput.Create(Self, I, Recipe.Ingredients[I]));
  for I := 0 to Recipe.Results.MaxIndex do
    FOutputs.Add(TMachineOutput.Create(Self, I, Recipe.Results[I]));
end;

function TMachineArray.GetBounds: TBounds2;
begin
  Result := Pos.Bounds(Vec2(96, 40 + IOHeight * 48));
end;

function TMachineArray.GetInputs: IReadonlyList<TMachineInput>;
begin
  Result := FInputs.ReadonlyList;
end;

function TMachineArray.GetIOHeight: Integer;
begin
  Result := Max(Inputs.Count, Outputs.Count);
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
  GenerateIO;
  Change;
end;

{ TFactory.TMachineArrayEventInfo }

constructor TFactory.TMachineArrayEventInfo.Create(ASender: TFactory; AMachineArray: TMachineArray);
begin
  inherited Create(ASender);
  FMachineArray := AMachineArray;
end;

{ TFactoryIO }

constructor TMachineIO.Create(AMachineArray: TMachineArray; AIndex: Integer; AItemStack: TFactorio.TRecipe.TItemStack);
begin
  FMachineArray := AMachineArray;
  FIndex := AIndex;
  FItemStack := AItemStack;
end;

destructor TMachineIO.Destroy;
begin
  FOnRemove.Execute(TEventInfo.Create(Self));
  inherited;
end;

procedure TMachineIO.Draw(G: IGPGraphics);
var
  Font: IGPFont;
  FontBrush: IGPSolidBrush;
  Connection: TMachineConnection;
begin
  Font := TGPFont.Create('Tahoma', 8);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  G.DrawString(Format('%dx', [ItemStack.Amount]), Font, TGPPointF.Create(Pos.X + 4, Pos.Y - 16), FontBrush);
  G.DrawImage(ItemStack.Item.Icon, Pos.X, Pos.Y);
  for Connection in Connections do
    Connection.Draw(G);
end;

procedure TMachineIO.DrawItemAmount(G: IGPGraphics; APos: TVector2);
var
  Font: IGPFont;
  FontBrush: IGPBrush;
begin
  Font := TGPFont.Create('Tahoma', 8);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  G.DrawString(Format('%3.3g/s', [ItemsPerSecond]), Font, TGPPointF.Create(APos.X, APos.Y + 8), FontBrush);
end;

function TMachineIO.GetBounds: TBounds2;
begin
  Result := Pos.Bounds(32);
end;

function TMachineIO.GetConnections: IReadonlyList<TMachineConnection>;
begin
  Result := FConnections.ReadonlyList;
end;

function TMachineIO.GetCraftingMachine: TFactorio.TCraftingMachine;
begin
  Result := MachineArray.CraftingMachine;
end;

function TMachineIO.GetItemsPerSecond: Single;
begin
  Result :=
    ItemStack.Amount *
    CraftingMachine.CraftingSpeed *
    MachineArray.Count *
    MachineArray.Performance /
    Recipe.EnergyRequired;
end;

function TMachineIO.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TMachineIO.GetRecipe: TFactorio.TRecipe;
begin
  Result := MachineArray.Recipe;
end;

{ TMachineConnection }

constructor TMachineConnection.Create(AOutput: TMachineOutput; AInput: TMachineInput);
begin
  FOutput := AOutput;
  FInput := AInput;
  Input.OnRemove.Add(Remove);
  Output.OnRemove.Add(Remove);
  Input.MachineArray.Factory.NotifyConnectionAdd(Self);
end;

destructor TMachineConnection.Destroy;
begin
  Input.MachineArray.Factory.NotifyConnectionRemove(Self);
  Input.OnRemove.Remove(Remove);
  Output.OnRemove.Remove(Remove);
  FOnRemove.Execute(TEventInfo.Create(Self));
  inherited;
end;

procedure TMachineConnection.Draw(G: IGPGraphics);
var
  Pen: IGPPen;
begin
  Pen := TGPPen.Create($7F7F9FAF, 20);
  Pen.EndCap := LineCapArrowAnchor;
  G.DrawLine(Pen, Output.Pos.X + 32, Output.Pos.Y + 16, Input.Pos.X, Input.Pos.Y + 16);
end;

function TMachineConnection.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

procedure TMachineConnection.Remove;
begin
  FOutput.RemoveConnection(Self);
end;

{ TMachineInput }

function TMachineInput.Connect(AOutput: TMachineOutput): TMachineConnection;
begin
  Result := AOutput.Connect(Self);
end;

constructor TMachineInput.Create(AMachineArray: TMachineArray; AIndex: Integer;
  AItemStack: TFactorio.TRecipe.TItemStack);
begin
  inherited;
  FConnections := TList<TMachineConnection>.Create;
end;

procedure TMachineInput.Disconnect(AOutput: TMachineOutput);
begin
  AOutput.Disconnect(Self);
end;

procedure TMachineInput.Draw(G: IGPGraphics);
var
  Connection: TMachineConnection;
begin
  for Connection in Connections do
    Connection.Draw(G);
  inherited;
  DrawItemAmount(G, Pos - Vec2(36, 0));
end;

function TMachineInput.FindConnection(AOutput: TMachineOutput): TMachineConnection;
begin
  Result := AOutput.FindConnection(Self);
end;

function TMachineInput.GetPos: TVector2;
var
  Offset: Integer;
begin
  Offset := MachineArray.IOHeight - MachineArray.Inputs.Count;
  Result := MachineArray.Pos + Vec2(0, 56 + (Index + Offset / 2) * 48);
end;

function TMachineInput.IsConnected(AOutput: TMachineOutput): Boolean;
begin
  Result := AOutput.IsConnected(Self);
end;

procedure TMachineInput.RemoveConnection(AConnection: TMachineConnection);
begin
  AConnection.Output.RemoveConnection(AConnection);
end;

function TMachineInput.ToggleConnection(AOutput: TMachineOutput): Boolean;
var
  Connection: TMachineConnection;
begin
  Connection := FindConnection(AOutput);
  if Connection <> nil then
    Connection.Remove
  else
    Exit(Connect(AOutput) <> nil);
  Result := True;
end;

{ TMachineOutput }

function TMachineOutput.Connect(AInput: TMachineInput): TMachineConnection;
begin
  if ItemStack.Item <> AInput.ItemStack.Item then
    Exit(nil);
  Result := TMachineConnection.Create(Self, AInput);
  FConnections.Add(Result);
  AInput.FConnections.Add(Result);
end;

constructor TMachineOutput.Create(AMachineArray: TMachineArray; AIndex: Integer;
  AItemStack: TFactorio.TRecipe.TItemStack);
begin
  inherited;
  FConnections := TObjectList<TMachineConnection>.Create;
end;

procedure TMachineOutput.Disconnect(AInput: TMachineInput);
var
  Connection: TMachineConnection;
begin
  Connection := FindConnection(AInput);
  if Connection <> nil then
    RemoveConnection(Connection);
end;

procedure TMachineOutput.Draw(G: IGPGraphics);
begin
  inherited;
  DrawItemAmount(G, Pos + Vec2(36, 0));
end;

function TMachineOutput.FindConnection(AInput: TMachineInput): TMachineConnection;
begin
  for Result in FConnections do
    if Result.Input = AInput then
      Exit;
  Result := nil;
end;

function TMachineOutput.GetPos: TVector2;
var
  Offset: Integer;
begin
  Offset := MachineArray.IOHeight - MachineArray.Outputs.Count;
  Result := MachineArray.Pos + Vec2(64, 56 + (Index + Offset / 2) * 48);
end;

function TMachineOutput.IsConnected(AInput: TMachineInput): Boolean;
begin
  Result := FindConnection(AInput) <> nil;
end;

procedure TMachineOutput.RemoveConnection(AConnection: TMachineConnection);
begin
  AConnection.Input.FConnections.Remove(AConnection);
  FConnections.Remove(AConnection);
end;

function TMachineOutput.ToggleConnection(AOther: TMachineInput): Boolean;
begin
  Result := AOther.ToggleConnection(Self);
end;

{ TFactory.TConnectionEventInfo }

constructor TFactory.TConnectionEventInfo.Create(ASender: TFactory; AConnection: TMachineConnection);
begin
  inherited Create(ASender);
  FConnection := AConnection;
end;

end.
