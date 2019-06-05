unit FactoryDefine;

interface

uses
  System.SysUtils,
  System.Math,

  Vcl.ExtCtrls,

  Pengine.Interfaces,
  Pengine.JSON,
  Pengine.JSON.Serialization,
  Pengine.ICollections,
  Pengine.Vector,
  Pengine.EventHandling,
  Pengine.Utility,
  Pengine.Color,

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

    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(const Value: Integer); virtual; abstract;
    function GetRatio: Single; virtual; abstract;

  protected
    constructor Create(AMachineArray: TMachineArray; AItemStack: TFactorio.TRecipe.TItemStack);

  public
    destructor Destroy; override;

    property MachineArray: TMachineArray read FMachineArray;
    property Index: Integer read GetIndex write SetIndex;
    property CraftingMachine: TFactorio.TCraftingMachine read GetCraftingMachine;
    property Recipe: TFactorio.TRecipe read GetRecipe;

    property ItemStack: TFactorio.TRecipe.TItemStack read FItemStack;
    property ItemsPerSecond: Single read GetItemsPerSecond;
    property Ratio: Single read GetRatio;

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

    function GetIndex: Integer; override;
    procedure SetIndex(const Value: Integer); override;
    function GetRatio: Single; override;

  public
    constructor Create(AMachineArray: TMachineArray; AItemStack: TFactorio.TRecipe.TItemStack);

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

    function GetIndex: Integer; override;
    procedure SetIndex(const Value: Integer); override;
    function GetRatio: Single; override;

  public
    constructor Create(AMachineArray: TMachineArray; AItemStack: TFactorio.TRecipe.TItemStack);

    function FindConnection(AInput: TMachineInput): TMachineConnection;
    function IsConnected(AInput: TMachineInput): Boolean;
    function Connect(AInput: TMachineInput): TMachineConnection;
    procedure Disconnect(AInput: TMachineInput);
    procedure RemoveConnection(AConnection: TMachineConnection);
    function ToggleConnection(AInput: TMachineInput): Boolean;

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
    function GetItemsPerSecond: Single;

  public
    constructor Create(AOutput: TMachineOutput; AInput: TMachineInput);
    destructor Destroy; override;

    procedure Remove;

    property Output: TMachineOutput read FOutput;
    property Input: TMachineInput read FInput;

    property ItemsPerSecond: Single read GetItemsPerSecond;

    procedure Draw(G: IGPGraphics);

    property OnRemove: TEvent.TAccess read GetOnRemove;

  end;

  TMachineArray = class(TInterfaceBase, IJSerializable)
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
    constructor Create(AFactory: TFactory);
    destructor Destroy; override;
    procedure Remove;

    property Factory: TFactory read FFactory;

    property Pos: TVector2 read FPos write SetPos;
    property Bounds: TBounds2 read GetBounds;

    function HasCraftingMachine: Boolean;
    property CraftingMachine: TFactorio.TCraftingMachine read FCraftingMachine write SetCraftingMachine;
    property Count: Integer read FCount write SetCount;
    property Performance: Single read FPerformance write SetPerformance;

    function HasRecipe: Boolean;
    property Recipe: TFactorio.TRecipe read FRecipe write SetRecipe;

    procedure SetInputIndex(AInput: TMachineInput; AIndex: Integer);
    property Inputs: IReadonlyList<TMachineInput> read GetInputs;
    function HasCustomInputOrder: Boolean;

    procedure SetOutputIndex(AOutput: TMachineOutput; AIndex: Integer);
    property Outputs: IReadonlyList<TMachineOutput> read GetOutputs;
    function HasCustomOutputOrder: Boolean;

    property IOHeight: Integer read GetIOHeight;

    property OnChange: TEvent.TAccess read GetOnChange;
    property OnRemove: TEvent.TAccess read GetOnRemove;
    procedure Draw(G: IGPGraphics);

    function InputAt(APos: TVector2): TMachineInput;
    function OutputAt(APos: TVector2): TMachineOutput;

    // IJSerializable
    procedure DefineJStorage(ASerializer: TJSerializer);
    function GetJVersion: Integer;

  end;

  IConnectionNet = interface
    function GetOutputs: IReadonlyList<TMachineOutput>;
    function GetInputs: IReadonlyList<TMachineInput>;
    function GetConnections: IReadonlyList<TMachineConnection>;
    function GetEffectivity: Single;
    function GetItemsPerSecond: Single;
    function GetInputPerSecond: Single;
    function GetOutputPerSecond: Single;
    function GetCenter: TVector2;

    property OutputPerSecond: Single read GetOutputPerSecond;
    property InputPerSecond: Single read GetInputPerSecond;
    property ItemsPerSecond: Single read GetItemsPerSecond;
    property Effectivity: Single read GetEffectivity;

    property Center: TVector2 read GetCenter;

    procedure Draw(G: IGPGraphics);

  end;

  TConnectionNet = class(TInterfacedObject, IConnectionNet)
  private
    FOutputs: IList<TMachineOutput>;
    FInputs: IList<TMachineInput>;
    FConnections: IList<TMachineConnection>;

    function GetOutputs: IReadonlyList<TMachineOutput>;
    function GetInputs: IReadonlyList<TMachineInput>;
    function GetConnections: IReadonlyList<TMachineConnection>;
    function GetEffectivity: Single;
    function GetItemsPerSecond: Single;
    function GetInputPerSecond: Single;
    function GetOutputPerSecond: Single;
    function GetCenter: TVector2;

    procedure AddRecursive(AInput: TMachineInput); overload;
    procedure AddRecursive(AOutput: TMachineOutput); overload;

    procedure Init;

  public
    constructor Create(AInput: TMachineInput); overload;
    constructor Create(AOutput: TMachineOutput); overload;

    property Outputs: IReadonlyList<TMachineOutput> read GetOutputs;
    property Inputs: IReadonlyList<TMachineInput> read GetInputs;
    property Connection: IReadonlyList<TMachineConnection> read GetConnections;

    property OutputPerSecond: Single read GetOutputPerSecond;
    property InputPerSecond: Single read GetInputPerSecond;
    property ItemsPerSecond: Single read GetItemsPerSecond;
    property Effectivity: Single read GetEffectivity;

    property Center: TVector2 read GetCenter;

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
    FFactorio: TFactorio;
    FMachineArrays: IObjectList<TMachineArray>;
    FConnectionNets: IList<IConnectionNet>;
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

    function CreateMachineArray: TMachineArray;
    function GetConnectionNets: IReadonlyList<IConnectionNet>;

  public
    constructor Create(AFactorio: TFactorio);

    procedure LoadFromFile(AFilename: string);
    procedure SaveToFile(AFilename: string);

    property Factorio: TFactorio read FFactorio;

    property MachineArrays: IReadonlyList<TMachineArray> read GetMachineArrays;
    function AddMachineArray: TMachineArray;
    procedure RemoveMachineArray(AMachineArray: TMachineArray);

    procedure Clear;

    property OnMachineArrayAdd: TMachineArrayEvent.TAccess read GetOnMachineArrayAdd;
    property OnMachineArrayRemove: TMachineArrayEvent.TAccess read GetOnMachineArrayRemove;
    property OnMachineArrayChange: TMachineArrayEvent.TAccess read GetOnMachineArrayChange;

    property OnConnectionAdd: TConnectionEvent.TAccess read GetOnConnectionAdd;
    property OnConnectionRemove: TConnectionEvent.TAccess read GetOnConnectionRemove;
    property OnConnectionChange: TConnectionEvent.TAccess read GetOnConnectionChange;

    function MachineArrayAt(APos: TVector2): TMachineArray;

    property ConnectionNets: IReadonlyList<IConnectionNet> read GetConnectionNets;

    procedure Draw(G: IGPGraphics);

    // IJSerializable
    procedure DefineJStorage(ASerializer: TJSerializer);
    function GetJVersion: Integer;

  end;

implementation

{ TFactory }

function TFactory.AddMachineArray: TMachineArray;
begin
  Result := CreateMachineArray;
  FMachineArrays.Add(Result);
end;

procedure TFactory.Clear;
begin
  FMachineArrays.Clear;
end;

constructor TFactory.Create(AFactorio: TFactorio);
begin
  FFactorio := AFactorio;
  FMachineArrays := TObjectList<TMachineArray>.Create;
end;

function TFactory.CreateMachineArray: TMachineArray;
begin
  Result := TMachineArray.Create(Self);
  Result.OnChange.Add(MachineArrayChange);
  Result.OnRemove.Add(MachineArrayRemove);
  FOnMachineArrayAdd.Execute(TMachineArrayEventInfo.Create(Self, Result));
end;

procedure TFactory.DefineJStorage(ASerializer: TJSerializer);
begin
  ASerializer.DefineList<TMachineArray>('MachineArrays', FMachineArrays, CreateMachineArray);
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

procedure TFactory.LoadFromFile(AFilename: string);
var
  JObject: TJObject;
begin
  JObject := TJObject.CreateFromFile(AFilename);
  try
    TJSerializer.Unserialize(Self, JObject);
  finally
    JObject.Free;
  end;
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
  FConnectionNets := nil;
  FOnConnectionAdd.Execute(TConnectionEventInfo.Create(Self, AConnection));
end;

procedure TFactory.NotifyConnectionChange(AConnection: TMachineConnection);
begin
  FConnectionNets := nil;
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

procedure TFactory.SaveToFile(AFilename: string);
var
  JObject: TJObject;
begin
  JObject := TJSerializer.Serialize(Self);
  try
    JObject.SaveToFile(AFilename);
  finally
    JObject.Free;
  end;
end;

function TFactory.GetConnectionNets: IReadonlyList<IConnectionNet>;
var
  Used: ISet<TMachineIO>;
  MachineArray: TMachineArray;
  ConnectionNet: TConnectionNet;
  Input: TMachineInput;
  Output: TMachineOutput;

  procedure AddConnectionNet;
  var
    MachineIO: TMachineIO;
  begin
    for MachineIO in ConnectionNet.Outputs do
      Used.Add(MachineIO);
    FConnectionNets.Add(ConnectionNet);
  end;

begin
  if FConnectionNets = nil then
  begin
    Used := TSet<TMachineIO>.Create;
    FConnectionNets := TList<IConnectionNet>.Create;
    for MachineArray in MachineArrays do
    begin
      for Output in MachineArray.Outputs do
      begin
        if Used.Contains(Output) then
          Continue;
        ConnectionNet := TConnectionNet.Create(Output);
        AddConnectionNet;
      end;
      for Input in MachineArray.Inputs do
      begin
        if Used.Contains(Input) then
          Continue;
        ConnectionNet := TConnectionNet.Create(Input);
        AddConnectionNet;
      end;
    end;
  end;
  Result := FConnectionNets.ReadonlyList;
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

constructor TMachineArray.Create(AFactory: TFactory);
begin
  FFactory := AFactory;
  FCount := 1;
  FPerformance := 1;
  FInputs := TObjectList<TMachineInput>.Create;
  FOutputs := TObjectList<TMachineOutput>.Create;
end;

procedure TMachineArray.DefineJStorage(ASerializer: TJSerializer);
var
  JArray: TJArray;
  MachineIO: TMachineIO;
  I: Integer;
begin
  ASerializer.Define('PosX', FPos.X);
  ASerializer.Define('PosY', FPos.Y);
  ASerializer.Define('Performance', FPerformance);
  ASerializer.Define('Count', FCount);

  case ASerializer.Mode of
    smSerialize:
      begin
        ASerializer.Value['CraftingMachine'] := string(CraftingMachine.Name);
        ASerializer.Value['Recipe'] := string(Recipe.Name);

        if HasCustomInputOrder then
        begin
          JArray := ASerializer.Value.AddArray('InputOrder');
          for MachineIO in Inputs do
            JArray.Add(string(MachineIO.ItemStack.Name));
        end;

        if HasCustomOutputOrder then
        begin
          JArray := ASerializer.Value.AddArray('OutputOrder');
          for MachineIO in Outputs do
            JArray.Add(string(MachineIO.ItemStack.Name));
        end;

      end;
    smUnserialize:
      begin
        CraftingMachine := Factory.Factorio.CraftingMachine
          [AnsiString(ASerializer.Value['CraftingMachine'].AsString)];
        Recipe := Factory.Factorio.Recipe[AnsiString(ASerializer.Value['Recipe'].AsString)];

        if ASerializer.Value.Get('InputOrder', JArray) then
        begin
          for MachineIO in Inputs do
          begin
            for I := 0 to JArray.MaxIndex do
            begin
              if string(MachineIO.ItemStack.Name) = JArray[I] then
              begin
                MachineIO.Index := I;
                Break;
              end;
            end;
          end;
        end;

        if ASerializer.Value.Get('OutputOrder', JArray) then
        begin
          for MachineIO in Outputs do
          begin
            for I := 0 to JArray.MaxIndex do
            begin
              if string(MachineIO.ItemStack.Name) = JArray[I] then
              begin
                MachineIO.Index := I;
                Break;
              end;
            end;
          end;
        end;

      end;
  end;

  if ASerializer.IsLoading then
    Change;
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

  if HasCraftingMachine then
    G.DrawImage(CraftingMachine.Icon, Pos.X + 32, Pos.Y);

  for MachineIO in Inputs do
    MachineIO.Draw(G);
  for MachineIO in Outputs do
    MachineIO.Draw(G);

  if HasRecipe and (Recipe.Icon <> Recipe.Results.First.Item.Icon) then
    G.DrawImage(Recipe.Icon, Pos.X + 64, Pos.Y);

end;

function TMachineArray.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TMachineArray.GetOutputs: IReadonlyList<TMachineOutput>;
begin
  Result := FOutputs.ReadonlyList;
end;

function TMachineArray.HasCraftingMachine: Boolean;
begin
  Result := FCraftingMachine <> nil;
end;

function TMachineArray.HasCustomInputOrder: Boolean;
var
  I: Integer;
begin
  for I := 0 to Inputs.MaxIndex do
    if Inputs[I].ItemStack <> Recipe.Ingredients[I] then
      Exit(True);
  Result := False;
end;

function TMachineArray.HasCustomOutputOrder: Boolean;
var
  I: Integer;
begin
  for I := 0 to Outputs.MaxIndex do
    if Outputs[I].ItemStack <> Recipe.Results[I] then
      Exit(True);
  Result := False;
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
  ItemStack: TFactorio.TRecipe.TItemStack;
begin
  FInputs.Clear;
  FOutputs.Clear;
  if not HasRecipe then
    Exit;

  for ItemStack in Recipe.Ingredients do
    FInputs.Add(TMachineInput.Create(Self, ItemStack));
  for ItemStack in Recipe.Results do
    FOutputs.Add(TMachineOutput.Create(Self, ItemStack));
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

function TMachineArray.GetJVersion: Integer;
begin
  Result := 0;
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

procedure TMachineArray.SetInputIndex(AInput: TMachineInput; AIndex: Integer);
begin
  FInputs.Move(AInput, AIndex);
  Change;
end;

procedure TMachineArray.SetOutputIndex(AOutput: TMachineOutput; AIndex: Integer);
begin
  FOutputs.Move(AOutput, AIndex);
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

constructor TMachineIO.Create(AMachineArray: TMachineArray; AItemStack: TFactorio.TRecipe.TItemStack);
begin
  FMachineArray := AMachineArray;
  FItemStack := AItemStack;
end;

destructor TMachineIO.Destroy;
var
  Connection: TMachineConnection;
begin
  for Connection in Connections.Reverse do
    Connection.Remove;
  FOnRemove.Execute(TEventInfo.Create(Self));
  inherited;
end;

procedure TMachineIO.Draw(G: IGPGraphics);
var
  Font: IGPFont;
  FontBrush: IGPSolidBrush;
begin
  Font := TGPFont.Create('Tahoma', 8);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  G.DrawString(Format('%dx', [ItemStack.Amount]), Font, TGPPointF.Create(Pos.X + 4, Pos.Y - 16), FontBrush);
  G.DrawImage(ItemStack.Item.Icon, Pos.X, Pos.Y);
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
  Input.MachineArray.Factory.NotifyConnectionAdd(Self);
end;

destructor TMachineConnection.Destroy;
begin
  Input.MachineArray.Factory.NotifyConnectionRemove(Self);
  FOnRemove.Execute(TEventInfo.Create(Self));
  inherited;
end;

procedure TMachineConnection.Draw(G: IGPGraphics);
var
  Pen: IGPPen;
  Path: TArray<TGPPointF>;
  Color: TColorRGBA;
  Center: TVector2;
  Font: IGPFont;
  FontBrush: IGPBrush;
  RatioText: string;
  TextSize: TGPRectF;
begin
  {
    if Ratio <= 1 then
    Color := TColorRGBA.HSV(Ratio * 2, 0.8, 1, 0.5)
    else
    Color := TColorRGBA.HSV(4 - 0.5 / Ratio, 0.8, 1, 0.5);
  }
  Color := TColorRGBA.Create(0, 1, 0, 0.5);
  Pen := TGPPen.Create(Color, 20);
  Pen.StartCap := LineCapRound;
  Pen.EndCap := LineCapArrowAnchor;
  Path := [
    TGPPointF.Create(Output.Pos.X + 32, Output.Pos.Y + 16),
    TGPPointF.Create((Output.Pos.X + Input.Pos.X) / 2 + 16, Output.Pos.Y + 16),
    TGPPointF.Create((Output.Pos.X + Input.Pos.X) / 2 + 16, Input.Pos.Y + 16),
    TGPPointF.Create(Input.Pos.X, Input.Pos.Y + 16)
    ];
  G.DrawBeziers(Pen, Path);

  Center := (Input.Pos + Output.Pos) / 2 + 16;
  Font := TGPFont.Create('Tahoma', 12);
  RatioText := Format('%3.3g%%', [ItemsPerSecond / Input.ItemsPerSecond * 100]);
  TextSize := G.MeasureString(RatioText, Font, TGPPointF.Create(0, 0));
  FontBrush := TGPSolidBrush.Create($FF000000);
  G.DrawString(RatioText, Font, TGPPointF.Create(Center.X - TextSize.Width / 2, Center.Y - TextSize.Height / 2),
    FontBrush);
end;

function TMachineConnection.GetItemsPerSecond: Single;
begin
  Result := Min(Output.ItemsPerSecond, Input.ItemsPerSecond);
end;

function TMachineConnection.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

procedure TMachineConnection.Remove;
begin
  Output.RemoveConnection(Self);
end;

{ TMachineInput }

function TMachineInput.Connect(AOutput: TMachineOutput): TMachineConnection;
begin
  Result := AOutput.Connect(Self);
end;

constructor TMachineInput.Create(AMachineArray: TMachineArray; AItemStack: TFactorio.TRecipe.TItemStack);
begin
  inherited;
  FConnections := TList<TMachineConnection>.Create;
end;

procedure TMachineInput.Disconnect(AOutput: TMachineOutput);
begin
  AOutput.Disconnect(Self);
end;

procedure TMachineInput.Draw(G: IGPGraphics);
begin
  inherited;
  DrawItemAmount(G, Pos - Vec2(36, 0));
end;

function TMachineInput.FindConnection(AOutput: TMachineOutput): TMachineConnection;
begin
  Result := AOutput.FindConnection(Self);
end;

function TMachineInput.GetIndex: Integer;
begin
  Result := MachineArray.Inputs.IndexOf(Self);
end;

function TMachineInput.GetPos: TVector2;
var
  Offset: Integer;
begin
  Offset := MachineArray.IOHeight - MachineArray.Inputs.Count;
  Result := MachineArray.Pos + Vec2(0, 56 + (Index + Offset / 2) * 48);
end;

function TMachineInput.GetRatio: Single;
var
  Connection: TMachineConnection;
  AllOut: Single;
begin
  AllOut := 0;
  for Connection in Connections do
    AllOut := AllOut + Connection.Output.ItemsPerSecond;
  Result := AllOut / ItemsPerSecond;
end;

function TMachineInput.IsConnected(AOutput: TMachineOutput): Boolean;
begin
  Result := AOutput.IsConnected(Self);
end;

procedure TMachineInput.RemoveConnection(AConnection: TMachineConnection);
begin
  AConnection.Output.RemoveConnection(AConnection);
end;

procedure TMachineInput.SetIndex(const Value: Integer);
begin
  MachineArray.SetInputIndex(Self, Value);
end;

function TMachineInput.ToggleConnection(AOutput: TMachineOutput): Boolean;
begin
  Result := AOutput.ToggleConnection(Self);
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

constructor TMachineOutput.Create(AMachineArray: TMachineArray; AItemStack: TFactorio.TRecipe.TItemStack);
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

function TMachineOutput.GetIndex: Integer;
begin
  Result := MachineArray.Outputs.IndexOf(Self);
end;

function TMachineOutput.GetPos: TVector2;
var
  Offset: Integer;
begin
  Offset := MachineArray.IOHeight - MachineArray.Outputs.Count;
  Result := MachineArray.Pos + Vec2(64, 56 + (Index + Offset / 2) * 48);
end;

function TMachineOutput.GetRatio: Single;
var
  Connection: TMachineConnection;
  AllIn: Single;
begin
  AllIn := 0;
  for Connection in Connections do
    AllIn := AllIn + Connection.Input.ItemsPerSecond;
  Result := ItemsPerSecond / AllIn;
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

procedure TMachineOutput.SetIndex(const Value: Integer);
begin
  MachineArray.SetOutputIndex(Self, Value);
end;

function TMachineOutput.ToggleConnection(AInput: TMachineInput): Boolean;
var
  Connection: TMachineConnection;
begin
  Connection := FindConnection(AInput);
  if Connection <> nil then
    Connection.Remove
  else
    Exit(Connect(AInput) <> nil);
  Result := True;
end;

{ TFactory.TConnectionEventInfo }

constructor TFactory.TConnectionEventInfo.Create(ASender: TFactory; AConnection: TMachineConnection);
begin
  inherited Create(ASender);
  FConnection := AConnection;
end;

{ TConnectionNet }

function TConnectionNet.GetOutputs: IReadonlyList<TMachineOutput>;
begin
  Result := FOutputs.ReadonlyList;
end;

procedure TConnectionNet.Init;
begin
  FOutputs := TList<TMachineOutput>.Create;
  FInputs := TList<TMachineInput>.Create;
  FConnections := TList<TMachineConnection>.Create;
end;

function TConnectionNet.GetInputs: IReadonlyList<TMachineInput>;
begin
  Result := FInputs.ReadonlyList;
end;

function TConnectionNet.GetConnections: IReadonlyList<TMachineConnection>;
begin
  Result := FConnections.ReadonlyList;
end;

constructor TConnectionNet.Create(AInput: TMachineInput);
begin
  Init;
  AddRecursive(AInput);
end;

procedure TConnectionNet.Draw(G: IGPGraphics);
begin

end;

function TConnectionNet.GetCenter: TVector2;
var
  Output: TMachineOutput;
  Input: TMachineInput;
begin
  Result := 0;
  for Output in Outputs do
    Result := Result + Output.Pos;
  for Input in Inputs do
    Result := Result + Input.Pos;
  Result := Result / (Outputs.Count + Inputs.Count) + 16;
end;

function TConnectionNet.GetEffectivity: Single;
var
  IPS: Single;
begin
  IPS := InputPerSecond;
  if IPS = 0 then
    Result := Infinity
  else
    Result := OutputPerSecond / InputPerSecond;
end;

function TConnectionNet.GetItemsPerSecond: Single;
begin
  Result := Min(OutputPerSecond, InputPerSecond)
end;

function TConnectionNet.GetInputPerSecond: Single;
var
  Input: TMachineInput;
begin
  Result := 0;
  for Input in Inputs do
    Result := Result + Input.ItemsPerSecond;
end;

function TConnectionNet.GetOutputPerSecond: Single;
var
  Output: TMachineOutput;
begin
  Result := 0;
  for Output in Outputs do
    Result := Result + Output.ItemsPerSecond;
end;

procedure TConnectionNet.AddRecursive(AOutput: TMachineOutput);
var
  Connection: TMachineConnection;
begin
  FOutputs.Add(AOutput);
  for Connection in AOutput.Connections do
    if not Inputs.Contains(Connection.Input) then
      AddRecursive(Connection.Input);
end;

procedure TConnectionNet.AddRecursive(AInput: TMachineInput);
var
  Connection: TMachineConnection;
begin
  FInputs.Add(AInput);
  for Connection in AInput.Connections do
    if not Outputs.Contains(Connection.Output) then
      AddRecursive(Connection.Output);
end;

constructor TConnectionNet.Create(AOutput: TMachineOutput);
begin
  Init;
  AddRecursive(AOutput);
end;

end.
