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

  EFactory = class(Exception);

  TFactory = class;
  TMachineArray = class;
  IConnectionNet = interface;

  TMachinePort = class
  public type

    TEventInfo = TSenderEventInfo<TMachinePort>;
    TEvent = TEvent<TEventInfo>;

  private
    FMachineArray: TMachineArray;
    FOnRemove: TEvent;

    function GetItemsPerSecond: Single; virtual;
    function GetCraftingMachine: TFactorio.TCraftingMachine;
    function GetRecipe: TFactorio.TRecipe;
    function GetOnRemove: TEvent.TAccess;

    function GetPos: TVector2; virtual; abstract;
    function GetBounds: TBounds2;

    procedure DrawItemAmount(G: IGPGraphics; APos: TVector2);

    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(const Value: Integer); virtual; abstract;
    function GetItemStack: TFactorio.TRecipe.TItemStack; virtual; abstract;
    function GetConnectionNet: IConnectionNet;

  protected
    constructor Create(AMachineArray: TMachineArray);

  public
    destructor Destroy; override;

    property MachineArray: TMachineArray read FMachineArray;
    property Index: Integer read GetIndex write SetIndex;
    property CraftingMachine: TFactorio.TCraftingMachine read GetCraftingMachine;
    property Recipe: TFactorio.TRecipe read GetRecipe;

    property ItemStack: TFactorio.TRecipe.TItemStack read GetItemStack;
    property ItemsPerSecond: Single read GetItemsPerSecond;

    function IsConnected: Boolean;
    property ConnectionNet: IConnectionNet read GetConnectionNet;

    property Pos: TVector2 read GetPos;
    property Bounds: TBounds2 read GetBounds;

    procedure Draw(G: IGPGraphics); virtual;

    property OnRemove: TEvent.TAccess read GetOnRemove;

  end;

  TMachineOutput = class;

  TMachineInput = class(TMachinePort)
  private
    FIngredient: TFactorio.TRecipe.TIngredient;

  protected
    function GetPos: TVector2; override;

    function GetIndex: Integer; override;
    procedure SetIndex(const Value: Integer); override;
    function GetItemStack: TFactorio.TRecipe.TItemStack; override;

  public
    constructor Create(AMachineArray: TMachineArray; AIngredient: TFactorio.TRecipe.TIngredient);

    property Ingredient: TFactorio.TRecipe.TIngredient read FIngredient;

    procedure Draw(G: IGPGraphics); override;

  end;

  TMachineOutput = class(TMachinePort)
  private
    FResult: TFactorio.TRecipe.TResult;

  protected
    function GetPos: TVector2; override;

    function GetIndex: Integer; override;
    procedure SetIndex(const Value: Integer); override;
    function GetItemStack: TFactorio.TRecipe.TItemStack; override;

    function GetItemsPerSecond: Single; override;

  public
    constructor Create(AMachineArray: TMachineArray; AResult: TFactorio.TRecipe.TResult);

    property Result: TFactorio.TRecipe.TResult read FResult;

    procedure Draw(G: IGPGraphics); override;

  end;

  TMachineArray = class(TInterfaceBase, IJSerializable)
  public type

    TEventInfo = TSenderEventInfo<TMachineArray>;

    TEvent = TEvent<TEventInfo>;

  private
    FFactorio: TFactorio;
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

    function GetIndex: Integer;

    procedure SetPos(const Value: TVector2);
    function GetBounds: TBounds2;

    procedure SetCount(const Value: Integer);
    procedure SetPerformance(const Value: Single);
    function GetActualPerformance: Single;

    procedure SetCraftingMachine(const Value: TFactorio.TCraftingMachine);
    procedure SetRecipe(const Value: TFactorio.TRecipe);

    function GetInputs: IReadonlyList<TMachineInput>;
    function GetOutputs: IReadonlyList<TMachineOutput>;
    function GetPortHeight: Integer;

    function GetOnChange: TEvent.TAccess;
    function GetOnRemove: TEvent.TAccess;

    procedure Change;
    procedure GeneratePorts;

    procedure CheckFactory;

    procedure CopyOrder<T: TMachinePort>(AFrom, ATo: IReadonlyList<T>);
    procedure SetIndex(const Value: Integer);

  public
    constructor Create(AFactorio: TFactorio); overload;
    constructor Create(AFactory: TFactory); overload;
    destructor Destroy; override;
    procedure Remove;

    function Copy: TMachineArray;
    procedure Assign(AFrom: TMachineArray);

    property Factorio: TFactorio read FFactorio;
    property Factory: TFactory read FFactory;
    property Index: Integer read GetIndex write SetIndex;

    property Pos: TVector2 read FPos write SetPos;
    property Bounds: TBounds2 read GetBounds;

    property Count: Integer read FCount write SetCount;
    property Performance: Single read FPerformance write SetPerformance;
    property ActualPerformance: Single read GetActualPerformance;

    function HasCraftingMachine: Boolean;
    property CraftingMachine: TFactorio.TCraftingMachine read FCraftingMachine write SetCraftingMachine;
    property Recipe: TFactorio.TRecipe read FRecipe write SetRecipe;
    function HasRecipe: Boolean;

    property Inputs: IReadonlyList<TMachineInput> read GetInputs;
    function HasCustomInputOrder: Boolean;
    procedure SetInputIndex(AInput: TMachineInput; AIndex: Integer);

    property Outputs: IReadonlyList<TMachineOutput> read GetOutputs;
    function HasCustomOutputOrder: Boolean;
    procedure SetOutputIndex(AOutput: TMachineOutput; AIndex: Integer);

    property PortHeight: Integer read GetPortHeight;

    property OnChange: TEvent.TAccess read GetOnChange;
    property OnRemove: TEvent.TAccess read GetOnRemove;

    procedure Draw(G: IGPGraphics);

    function PortAt(APos: TVector2): TMachinePort;
    function InputAt(APos: TVector2): TMachineInput;
    function OutputAt(APos: TVector2): TMachineOutput;

    // IJSerializable
    procedure DefineJStorage(ASerializer: TJSerializer);
    function GetJVersion: Integer;

  end;

  IConnectionNet = interface(IJSerializable)
    function GetMachinePorts: IReadonlyList<TMachinePort>;
    function GetOutputs: IIterate<TMachineOutput>;
    function GetInputs: IIterate<TMachineInput>;
    function GetItemType: TFactorio.TItemOrFluid;
    function GetOutputPerSecond: Single;
    function GetInputPerSecond: Single;
    function GetItemsPerSecond: Single;
    function GetEffectivity: Single;
    function GetPos: TVector2;
    procedure SetPos(const Value: TVector2);
    function GetBounds: TBounds2;
    function GetOnChange: TEvent<TSenderEventInfo<IConnectionNet>>.TAccess;
    function GetOnRemove: TEvent<TSenderEventInfo<IConnectionNet>>.TAccess;

    property MachinePorts: IReadonlyList<TMachinePort> read GetMachinePorts;
    property Outputs: IIterate<TMachineOutput> read GetOutputs;
    property Inputs: IIterate<TMachineInput> read GetInputs;

    function Add(AMachinePort: TMachinePort): Boolean;
    procedure Remove(AMachinePort: TMachinePort);

    property ItemType: TFactorio.TItemOrFluid read GetItemType;
    property OutputPerSecond: Single read GetOutputPerSecond;
    property InputPerSecond: Single read GetInputPerSecond;
    property ItemsPerSecond: Single read GetItemsPerSecond;
    property Effectivity: Single read GetEffectivity;

    function CalculateCenter: TVector2;
    property Pos: TVector2 read GetPos write SetPos;
    property Bounds: TBounds2 read GetBounds;

    procedure Draw(G: IGPGraphics);

    property OnChange: TEvent<TSenderEventInfo<IConnectionNet>>.TAccess read GetOnChange;
    property OnRemove: TEvent<TSenderEventInfo<IConnectionNet>>.TAccess read GetOnRemove;

  end;

  TConnectionNet = class(TInterfacedObject, IConnectionNet, IJSerializable)
  public type

    TEventInfo = TSenderEventInfo<IConnectionNet>;

    TEvent = TEvent<TEventInfo>;

  private
    FFactorio: TFactorio;
    FFactory: TFactory;
    FPos: TVector2;
    FMachinePorts: IList<TMachinePort>;
    FOnChange: TEvent;
    FOnRemove: TEvent;

    function GetMachinePorts: IReadonlyList<TMachinePort>;
    function GetOutputs: IIterate<TMachineOutput>;
    function GetInputs: IIterate<TMachineInput>;

    function GetItemType: TFactorio.TItemOrFluid;
    function GetOutputPerSecond: Single;
    function GetInputPerSecond: Single;
    function GetItemsPerSecond: Single;
    function GetEffectivity: Single;

    function GetPos: TVector2;
    procedure SetPos(const Value: TVector2);
    function GetBounds: TBounds2;

    function GetOnChange: TEvent.TAccess;
    function GetOnRemove: TEvent.TAccess;

    procedure Change;

  public
    constructor Create(AFactory: TFactory);
    destructor Destroy; override;

    property Factorio: TFactorio read FFactorio;
    property Factory: TFactory read FFactory;

    property MachinePorts: IReadonlyList<TMachinePort> read GetMachinePorts;
    property Outputs: IIterate<TMachineOutput> read GetOutputs;
    property Inputs: IIterate<TMachineInput> read GetInputs;

    function Add(AMachinePort: TMachinePort): Boolean;
    procedure Remove(AMachinePort: TMachinePort);

    property ItemType: TFactorio.TItemOrFluid read GetItemType;
    function IsFluid: Boolean;

    property OutputPerSecond: Single read GetOutputPerSecond;
    property InputPerSecond: Single read GetInputPerSecond;
    property ItemsPerSecond: Single read GetItemsPerSecond;
    property Effectivity: Single read GetEffectivity;

    function CalculateCenter: TVector2;
    property Pos: TVector2 read GetPos write SetPos;
    property Bounds: TBounds2 read GetBounds;

    procedure Draw(G: IGPGraphics);

    property OnChange: TEvent.TAccess read GetOnChange;
    property OnRemove: TEvent.TAccess read GetOnRemove;

    // IJSerializable
    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

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

    TConnectionNetEventInfo = class(TEventInfo)
    private
      FConnectionNet: IConnectionNet;

    public
      constructor Create(ASender: TFactory; AConnectionNet: IConnectionNet);

      property ConnectionNet: IConnectionNet read FConnectionNet;

    end;

    TConnectionNetEvent = TEvent<TConnectionNetEventInfo>;

  private
    FFactorio: TFactorio;
    FMachineArrays: IObjectList<TMachineArray>;
    FConnectionNets: IList<IConnectionNet>;
    FOnMachineArrayAdd: TMachineArrayEvent;
    FOnMachineArrayChange: TMachineArrayEvent;
    FOnMachineArrayRemove: TMachineArrayEvent;
    FOnMachineArrayOrderChange: TEvent;
    FOnConnectionNetAdd: TConnectionNetEvent;
    FOnConnectionNetChange: TConnectionNetEvent;
    FOnConnectionNetRemove: TConnectionNetEvent;
    FOnConnectionNetOrderChange: TEvent;

    function GetMachineArrays: IReadonlyList<TMachineArray>;
    function GetConnectionNets: IReadonlyList<IConnectionNet>;

    procedure MachineArrayChange(AInfo: TMachineArray.TEventInfo);
    procedure MachineArrayRemove(AInfo: TMachineArray.TEventInfo);
    procedure ConnectionNetChange(AInfo: TConnectionNet.TEventInfo);
    procedure ConnectionNetRemove(AInfo: TConnectionNet.TEventInfo);

    function GetOnMachineArrayAdd: TMachineArrayEvent.TAccess;
    function GetOnMachineArrayChange: TMachineArrayEvent.TAccess;
    function GetOnMachineArrayRemove: TMachineArrayEvent.TAccess;

    function GetOnConnectionNetAdd: TConnectionNetEvent.TAccess;
    function GetOnConnectionNetChange: TConnectionNetEvent.TAccess;
    function GetOnConnectionNetRemove: TConnectionNetEvent.TAccess;

    function CreateMachineArray: TMachineArray;
    function CreateConnectionNet: IConnectionNet;
    function GetOnConnectionNetIndexChange: TEvent.TAccess;
    function GetOnMachineArayIndexChange: TEvent.TAccess;

  public
    constructor Create(AFactorio: TFactorio);

    procedure LoadFromFile(AFilename: string);
    procedure SaveToFile(AFilename: string);

    property Factorio: TFactorio read FFactorio;

    property MachineArrays: IReadonlyList<TMachineArray> read GetMachineArrays;
    function AddMachineArray: TMachineArray;
    procedure RemoveMachineArray(AMachineArray: TMachineArray);
    procedure SetMachineArrayIndex(AMachineArray: TMachineArray; AIndex: Integer);

    property ConnectionNets: IReadonlyList<IConnectionNet> read GetConnectionNets;
    function AddConnectionNet(A, B: TMachinePort): IConnectionNet;
    procedure RemoveConnectionNet(AConnectionNet: IConnectionNet);
    procedure SetConnectionNetIndex(AConnectionNet: IConnectionNet; AIndex: Integer);

    procedure Connect(A, B: TMachinePort);
    procedure Disconnect(AMachinePort: TMachinePort);
    procedure ToggleConnection(A, B: TMachinePort);
    function FindConnectionNetFor(AMachinePort: TMachinePort): IConnectionNet;

    procedure Clear;

    property OnMachineArrayAdd: TMachineArrayEvent.TAccess read GetOnMachineArrayAdd;
    property OnMachineArrayRemove: TMachineArrayEvent.TAccess read GetOnMachineArrayRemove;
    property OnMachineArrayChange: TMachineArrayEvent.TAccess read GetOnMachineArrayChange;
    property OnMachineArrayIndexChange: TEvent.TAccess read GetOnMachineArayIndexChange;

    property OnConnectionNetAdd: TConnectionNetEvent.TAccess read GetOnConnectionNetAdd;
    property OnConnectionNetRemove: TConnectionNetEvent.TAccess read GetOnConnectionNetRemove;
    property OnConnectionNetChange: TConnectionNetEvent.TAccess read GetOnConnectionNetChange;
    property OnConnectionNetIndexChange: TEvent.TAccess read GetOnConnectionNetIndexChange;

    function MachineArrayAt(APos: TVector2): TMachineArray;
    function MachinePortAt(APos: TVector2): TMachinePort;

    procedure Draw(G: IGPGraphics);

    // IJSerializable
    procedure DefineJStorage(ASerializer: TJSerializer);
    function GetJVersion: Integer;

  end;

implementation

procedure FitString(G: IGPGraphics; AString: string; AFont: IGPFont; ABounds: TBounds2; ABrush: IGPBrush);
var
  Aspect, LongSide: Single;
  BoundsRect, StringRect, ScalingRect: TGPRectF;
  C: TGPGraphicsContainer;
begin
  Aspect := ABounds.Width / ABounds.Height;
  BoundsRect := TGPRectF.Create(ABounds.C1.X, ABounds.C1.Y, ABounds.Width, ABounds.Height);
  StringRect := G.MeasureString(AString, AFont, TGPPointF.Create(0, 0));
  LongSide := Max(StringRect.Width / Aspect, StringRect.Height);
  ScalingRect := TGPRectF.Create(0, 0, LongSide * Aspect, LongSide);
  StringRect.X := (LongSide * Aspect - StringRect.Width) / 2;
  StringRect.Y := (LongSide - StringRect.Height) / 2;
  C := G.BeginContainer(BoundsRect, ScalingRect, UnitPixel);
  G.TextRenderingHint := TextRenderingHintAntiAlias;
  // G.DrawRectangle(TGPPen.Create($FF000000), StringRect);
  G.DrawString(AString, AFont, StringRect, TGPStringFormat.Create, ABrush);
  G.EndContainer(C);
end;

{ TFactory }

function TFactory.AddConnectionNet(A, B: TMachinePort): IConnectionNet;
begin
  Result := CreateConnectionNet;
  if Result.Add(A) and Result.Add(B) then
    FConnectionNets.Add(Result);
end;

function TFactory.AddMachineArray: TMachineArray;
begin
  Result := CreateMachineArray;
  FMachineArrays.Add(Result);
end;

procedure TFactory.Clear;
begin
  FMachineArrays.Clear;
end;

procedure TFactory.Connect(A, B: TMachinePort);
var
  Port: TMachinePort;
begin
  if A = B then
    Exit;
  if A.IsConnected then
  begin
    if B.IsConnected then
    begin
      if A.ConnectionNet = B.ConnectionNet then
        Exit;
      for Port in B.ConnectionNet.MachinePorts do
        A.ConnectionNet.Add(Port);
      RemoveConnectionNet(B.ConnectionNet);
    end
    else
      A.ConnectionNet.Add(B);
  end
  else
  begin
    if B.IsConnected then
      B.ConnectionNet.Add(A)
    else
      AddConnectionNet(A, B);
  end;
end;

procedure TFactory.ConnectionNetChange(AInfo: TConnectionNet.TEventInfo);
begin
  FOnConnectionNetAdd.Execute(TConnectionNetEventInfo.Create(Self, AInfo.Sender));
end;

procedure TFactory.ConnectionNetRemove(AInfo: TConnectionNet.TEventInfo);
begin
  FOnConnectionNetRemove.Execute(TConnectionNetEventInfo.Create(Self, AInfo.Sender));
end;

constructor TFactory.Create(AFactorio: TFactorio);
begin
  FFactorio := AFactorio;
  FMachineArrays := TObjectList<TMachineArray>.Create;
  FConnectionNets := TList<IConnectionNet>.Create;
end;

function TFactory.CreateConnectionNet: IConnectionNet;
begin
  Result := TConnectionNet.Create(Self);
  Result.OnChange.Add(ConnectionNetChange);
  Result.OnRemove.Add(ConnectionNetRemove);
  FOnConnectionNetAdd.Execute(TConnectionNetEventInfo.Create(Self, Result));
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
  ASerializer.DefineList<IConnectionNet>('ConnectionNets', FConnectionNets, CreateConnectionNet);
end;

procedure TFactory.Disconnect(AMachinePort: TMachinePort);
begin
  if AMachinePort.IsConnected then
    AMachinePort.ConnectionNet.Remove(AMachinePort);
end;

procedure TFactory.Draw(G: IGPGraphics);
var
  ConnectionNet: IConnectionNet;
  MachineArray: TMachineArray;
begin
  for ConnectionNet in ConnectionNets do
    ConnectionNet.Draw(G);
  for MachineArray in MachineArrays do
    MachineArray.Draw(G);
end;

function TFactory.FindConnectionNetFor(AMachinePort: TMachinePort): IConnectionNet;
var
  ConnectionNet: IConnectionNet;
begin
  for ConnectionNet in FConnectionNets do
    if ConnectionNet.MachinePorts.Contains(AMachinePort) then
      Exit(ConnectionNet);
  Result := nil;
end;

function TFactory.GetMachineArrays: IReadonlyList<TMachineArray>;
begin
  Result := FMachineArrays.ReadonlyList;
end;

function TFactory.GetOnConnectionNetAdd: TConnectionNetEvent.TAccess;
begin
  Result := FOnConnectionNetAdd.Access;
end;

function TFactory.GetOnConnectionNetChange: TConnectionNetEvent.TAccess;
begin
  Result := FOnConnectionNetChange.Access;
end;

function TFactory.GetOnConnectionNetIndexChange: TEvent.TAccess;
begin
  Result := FOnConnectionNetOrderChange.Access;
end;

function TFactory.GetOnConnectionNetRemove: TConnectionNetEvent.TAccess;
begin
  Result := FOnConnectionNetRemove.Access;
end;

function TFactory.GetOnMachineArayIndexChange: TEvent.TAccess;
begin
  Result := FOnMachineArrayOrderChange.Access;
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

function TFactory.MachinePortAt(APos: TVector2): TMachinePort;
var
  MachineArray: TMachineArray;
begin
  MachineArray := MachineArrayAt(APos);
  if MachineArray = nil then
    Exit(nil);
  Result := MachineArray.InputAt(APos);
end;

procedure TFactory.RemoveConnectionNet(AConnectionNet: IConnectionNet);
begin
  FConnectionNets.Remove(AConnectionNet);
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

procedure TFactory.SetConnectionNetIndex(AConnectionNet: IConnectionNet; AIndex: Integer);
begin
  FConnectionNets.Move(AConnectionNet, AIndex);
  FOnConnectionnetOrderChange.Execute(TEventInfo.Create(Self));
end;

procedure TFactory.SetMachineArrayIndex(AMachineArray: TMachineArray; AIndex: Integer);
begin
  FMachineArrays.Move(AMachineArray, AIndex);
  FOnMachineArrayOrderChange.Execute(TEventInfo.Create(Self));
end;

procedure TFactory.ToggleConnection(A, B: TMachinePort);
begin
  if A.IsConnected and (A.ConnectionNet = B.ConnectionNet) then
    Disconnect(A)
  else
    Connect(A, B);
end;

function TFactory.GetConnectionNets: IReadonlyList<IConnectionNet>;
begin
  Result := FConnectionNets.ReadonlyList;
end;

function TFactory.GetJVersion: Integer;
begin
  Result := 0;
end;

{ TMachineArray }

procedure TMachineArray.Assign(AFrom: TMachineArray);
begin
  Pos := AFrom.Pos;
  Recipe := AFrom.Recipe;
  CraftingMachine := AFrom.CraftingMachine;
  Count := AFrom.Count;
  Performance := AFrom.Count;
  CopyOrder<TMachineInput>(Inputs, AFrom.Inputs);
  CopyOrder<TMachineOutput>(Outputs, AFrom.Outputs);
end;

procedure TMachineArray.Change;
begin
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TMachineArray.CheckFactory;
begin
  if Factory = nil then
    raise EFactory.Create('MachineArray does not have a Factory instance.');
end;

function TMachineArray.Copy: TMachineArray;
begin
  Result := TMachineArray.Create(Factorio);
  Result.Assign(Self);
end;

procedure TMachineArray.CopyOrder<T>(AFrom, ATo: IReadonlyList<T>);
var
  MachinePort, FromMachinePort: TMachinePort;
begin
  for MachinePort in ATo do
  begin
    for FromMachinePort in AFrom do
    begin
      if MachinePort.ItemStack.Item = FromMachinePort.ItemStack.Item then
      begin
        MachinePort.Index := FromMachinePort.Index;
        Break;
      end;
    end;
  end;
end;

constructor TMachineArray.Create(AFactorio: TFactorio);
begin
  FFactorio := AFactorio;
  FCount := 1;
  FPerformance := 1;
  FInputs := TObjectList<TMachineInput>.Create;
  FOutputs := TObjectList<TMachineOutput>.Create;
end;

constructor TMachineArray.Create(AFactory: TFactory);
begin
  Create(AFactory.Factorio);
  FFactory := AFactory;
end;

procedure TMachineArray.DefineJStorage(ASerializer: TJSerializer);
var
  JArray: TJArray;
  MachinePort: TMachinePort;
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
          for MachinePort in Inputs do
            JArray.Add(string(MachinePort.ItemStack.Name));
        end;

        if HasCustomOutputOrder then
        begin
          JArray := ASerializer.Value.AddArray('OutputOrder');
          for MachinePort in Outputs do
            JArray.Add(string(MachinePort.ItemStack.Name));
        end;

      end;
    smUnserialize:
      begin
        CraftingMachine := Factorio.CraftingMachine[AnsiString(ASerializer.Value['CraftingMachine'].AsString)];
        Recipe := Factorio.Recipe[AnsiString(ASerializer.Value['Recipe'].AsString)];

        if ASerializer.Value.Get('InputOrder', JArray) then
        begin
          for MachinePort in Inputs do
          begin
            for I := 0 to JArray.MaxIndex do
            begin
              if string(MachinePort.ItemStack.Name) = JArray[I] then
              begin
                MachinePort.Index := I;
                Break;
              end;
            end;
          end;
        end;

        if ASerializer.Value.Get('OutputOrder', JArray) then
        begin
          for MachinePort in Outputs do
          begin
            for I := 0 to JArray.MaxIndex do
            begin
              if string(MachinePort.ItemStack.Name) = JArray[I] then
              begin
                MachinePort.Index := I;
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
  MachinePort: TMachinePort;
begin
  B := Bounds;
  Rect := TGPRectF.Create(B.C1.X, B.C1.Y, B.Width, B.Height);
  Pen := TGPPen.Create(TGPColor.Black);
  BgBrush := TGPLinearGradientBrush.Create(Rect, $FFAFAFDF, $FFDFDFFF, -90);
  G.FillRectangle(BgBrush, Rect);
  G.DrawRectangle(Pen, Rect);

  Font := TGPFont.Create('Tahoma', 10);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  FitString(G, Format('%dx', [Count]), Font, Pos.Offset(0, 8).Bounds(32, 16), FontBrush);
  // G.DrawString(Format('%dx', [Count]), Font, TGPPointF.Create(Pos.X, Pos.Y + 8), FontBrush);
  // G.DrawString(Format('%.0f%%', [Performance * 100]), Font, TGPPointF.Create(Pos.X + 40, Pos.Y + 32), FontBrush);

  if HasCraftingMachine then
    G.DrawImage(CraftingMachine.Icon, Pos.X + 32, Pos.Y, 32, 32);

  for MachinePort in Inputs do
    MachinePort.Draw(G);
  for MachinePort in Outputs do
    MachinePort.Draw(G);

  if HasRecipe and (Recipe.Icon <> Recipe.Results.First.Item.Icon) then
    G.DrawImage(Recipe.Icon, Pos.X + 64, Pos.Y, 32, 32);

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

function TMachineArray.PortAt(APos: TVector2): TMachinePort;
begin
  Result := InputAt(APos);
  if Result = nil then
    Result := OutputAt(APos);
end;

procedure TMachineArray.Remove;
begin
  CheckFactory;
  Factory.RemoveMachineArray(Self);
end;

procedure TMachineArray.GeneratePorts;
var
  Ingredient: TFactorio.TRecipe.TIngredient;
  Result: TFactorio.TRecipe.TResult;
begin
  FInputs.Clear;
  FOutputs.Clear;
  if not HasRecipe then
    Exit;

  for Ingredient in Recipe.Ingredients do
    FInputs.Add(TMachineInput.Create(Self, Ingredient));
  for Result in Recipe.Results do
    FOutputs.Add(TMachineOutput.Create(Self, Result));
end;

function TMachineArray.GetActualPerformance: Single;
var
  Input: TMachineInput;
begin
  Result := 1;
  for Input in Inputs do
  begin
    if not Input.IsConnected then
      Continue;
    Result := Min(Result, Input.ConnectionNet.Effectivity);
  end;
end;

function TMachineArray.GetBounds: TBounds2;
begin
  Result := Pos.Bounds(Vec2(96, 32 + PortHeight * 48));
end;

function TMachineArray.GetIndex: Integer;
begin
  CheckFactory;
  Result := Factory.MachineArrays.IndexOf(Self);
end;

function TMachineArray.GetInputs: IReadonlyList<TMachineInput>;
begin
  Result := FInputs.ReadonlyList;
end;

function TMachineArray.GetPortHeight: Integer;
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

procedure TMachineArray.SetIndex(const Value: Integer);
begin
  Factory.SetMachineArrayIndex(Self, Value);
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
  if HasRecipe and not(HasCraftingMachine and CraftingMachine.CanCraft(Recipe)) then
    CraftingMachine := Recipe.FindCraftingMachine;
  GeneratePorts;
  Change;
end;

{ TFactory.TMachineArrayEventInfo }

constructor TFactory.TMachineArrayEventInfo.Create(ASender: TFactory; AMachineArray: TMachineArray);
begin
  inherited Create(ASender);
  FMachineArray := AMachineArray;
end;

{ TFactoryPort }

constructor TMachinePort.Create(AMachineArray: TMachineArray);
begin
  FMachineArray := AMachineArray;
end;

destructor TMachinePort.Destroy;
begin
  if IsConnected then
    ConnectionNet.Remove(Self);
  FOnRemove.Execute(TEventInfo.Create(Self));
  inherited;
end;

procedure TMachinePort.Draw(G: IGPGraphics);
var
  Font: IGPFont;
  FontBrush: IGPSolidBrush;
begin
  Font := TGPFont.Create('Tahoma', 8);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  FitString(G, Format('%dx', [ItemStack.Amount]), Font, Pos.Offset(0, -16).Bounds(32, 16), FontBrush);
  // G.DrawString(Format('%dx', [ItemStack.Amount]), Font, TGPPointF.Create(Pos.X + 4, Pos.Y - 16), FontBrush);
  G.DrawImage(ItemStack.Item.Icon, Pos.X, Pos.Y, 32, 32);
end;

procedure TMachinePort.DrawItemAmount(G: IGPGraphics; APos: TVector2);
var
  Font: IGPFont;
  FontBrush: IGPBrush;
begin
  Font := TGPFont.Create('Tahoma', 8);
  FontBrush := TGPSolidBrush.Create(TGPColor.Black);
  FitString(G, Format('%.3g/s', [ItemsPerSecond], TFormatSettings.Invariant), Font, APos.Offset(0, 8).Bounds(32, 16),
    FontBrush);
  // G.DrawString(Format('%3.3g/s', [ItemsPerSecond]), Font, TGPPointF.Create(APos.X, APos.Y + 8), FontBrush);
end;

function TMachinePort.GetBounds: TBounds2;
begin
  Result := Pos.Bounds(32);
end;

function TMachinePort.GetConnectionNet: IConnectionNet;
begin
  MachineArray.CheckFactory;
  Result := MachineArray.Factory.FindConnectionNetFor(Self);
end;

function TMachinePort.GetCraftingMachine: TFactorio.TCraftingMachine;
begin
  Result := MachineArray.CraftingMachine;
end;

function TMachinePort.GetItemsPerSecond: Single;
begin
  Result :=
    ItemStack.Amount
    * CraftingMachine.CraftingSpeed
    * MachineArray.Count
    * MachineArray.Performance
    / Recipe.EnergyRequired;
end;

function TMachinePort.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TMachinePort.GetRecipe: TFactorio.TRecipe;
begin
  Result := MachineArray.Recipe;
end;

function TMachinePort.IsConnected: Boolean;
begin
  Result := ConnectionNet <> nil;
end;

{ TMachineInput }

constructor TMachineInput.Create(AMachineArray: TMachineArray; AIngredient: TFactorio.TRecipe.TIngredient);
begin
  inherited Create(AMachineArray);
  FIngredient := AIngredient;
end;

procedure TMachineInput.Draw(G: IGPGraphics);
begin
  inherited;
  DrawItemAmount(G, Pos - Vec2(32, 0));
end;

function TMachineInput.GetIndex: Integer;
begin
  Result := MachineArray.Inputs.IndexOf(Self);
end;

function TMachineInput.GetItemStack: TFactorio.TRecipe.TItemStack;
begin
  Result := FIngredient;
end;

function TMachineInput.GetPos: TVector2;
var
  Offset: Integer;
begin
  Offset := MachineArray.PortHeight - MachineArray.Inputs.Count;
  Result := MachineArray.Pos + Vec2(0, 48 + (Index + Offset / 2) * 48);
end;

procedure TMachineInput.SetIndex(const Value: Integer);
begin
  MachineArray.SetInputIndex(Self, Value);
end;

{ TMachineOutput }

constructor TMachineOutput.Create(AMachineArray: TMachineArray; AResult: TFactorio.TRecipe.TResult);
begin
  inherited Create(AMachineArray);
  FResult := AResult;
end;

procedure TMachineOutput.Draw(G: IGPGraphics);
begin
  inherited;
  DrawItemAmount(G, Pos + Vec2(32, 0));
end;

function TMachineOutput.GetIndex: Integer;
begin
  Result := MachineArray.Outputs.IndexOf(Self);
end;

function TMachineOutput.GetItemsPerSecond: Single;
begin
  Result := inherited * Self.Result.Probability;
end;

function TMachineOutput.GetItemStack: TFactorio.TRecipe.TItemStack;
begin
  Result := FResult;
end;

function TMachineOutput.GetPos: TVector2;
var
  Offset: Integer;
begin
  Offset := MachineArray.PortHeight - MachineArray.Outputs.Count;
  Result := MachineArray.Pos + Vec2(64, 48 + (Index + Offset / 2) * 48);
end;

procedure TMachineOutput.SetIndex(const Value: Integer);
begin
  MachineArray.SetOutputIndex(Self, Value);
end;

{ TConnectionNet }

function TConnectionNet.GetMachinePorts: IReadonlyList<TMachinePort>;
begin
  Result := FMachinePorts.ReadonlyList;
end;

function TConnectionNet.GetOutputs: IIterate<TMachineOutput>;
begin
  Result := FMachinePorts.Iterate.Generic.OfType<TMachineOutput>;
end;

function TConnectionNet.GetPos: TVector2;
begin
  Result := CalculateCenter + FPos;
end;

function TConnectionNet.IsFluid: Boolean;
begin
  if MachinePorts.Empty then
    Exit(False);
  Result := MachinePorts.First.ItemStack.IsFluid;
end;

function TConnectionNet.GetInputs: IIterate<TMachineInput>;
begin
  Result := FMachinePorts.Iterate.Generic.OfType<TMachineInput>;
end;

function TConnectionNet.GetOnChange: TEvent.TAccess;
begin
  Result := FOnChange.Access;
end;

function TConnectionNet.GetOnRemove: TEvent.TAccess;
begin
  Result := FOnRemove.Access;
end;

function TConnectionNet.GetOutputPerSecond: Single;
var
  Output: TMachineOutput;
begin
  Result := 0;
  for Output in Outputs do
    Result := Result + Output.ItemsPerSecond;
end;

function TConnectionNet.GetInputPerSecond: Single;
var
  Input: TMachineInput;
begin
  Result := 0;
  for Input in Inputs do
    Result := Result + Input.ItemsPerSecond;
end;

function TConnectionNet.GetEffectivity: Single;
var
  IPS: Single;
begin
  IPS := InputPerSecond;
  if IPS = 0 then
    Result := Infinity
  else
    Result := OutputPerSecond / IPS;
end;

function TConnectionNet.GetItemsPerSecond: Single;
begin
  Result := Min(OutputPerSecond, InputPerSecond)
end;

function TConnectionNet.GetItemType: TFactorio.TItemOrFluid;
begin
  if MachinePorts.Empty then
    Exit(nil);
  Result := MachinePorts.First.ItemStack.Item;
end;

function TConnectionNet.GetJVersion: Integer;
begin
  Result := 0;
end;

function TConnectionNet.GetBounds: TBounds2;
begin
  Result := Bounds2(Pos).Outset(Vec2(64, 32));
end;

function TConnectionNet.CalculateCenter: TVector2;
var
  Output: TMachineOutput;
  Input: TMachineInput;
  Bounds: TBounds2;
begin
  if Outputs.Empty and Inputs.Empty then
    Exit(0);

  if not Outputs.Empty then
  begin
    Bounds.C1.X := Outputs.First.Pos.X;
    for Output in Outputs.Iterate.Skip(1) do
      Bounds.C1.X := Max(Bounds.C1.X, Output.Pos.X);
  end;
  if not Inputs.Empty then
  begin
    Bounds.C2.X := Inputs.First.Pos.X;
    for Input in Inputs.Iterate.Skip(1) do
      Bounds.C2.X := Min(Bounds.C2.X, Input.Pos.X);
  end;
  if Outputs.Empty then
    Bounds.C1.X := Bounds.C2.X - 128;
  if Inputs.Empty then
    Bounds.C2.X := Bounds.C1.X + 128;

  if Inputs.Empty then
  begin
    Bounds.C1.Y := Outputs.First.Pos.Y;
    Bounds.C2.Y := Outputs.First.Pos.Y;
  end
  else
  begin
    Bounds.C1.Y := Inputs.First.Pos.Y;
    Bounds.C2.Y := Inputs.First.Pos.Y;
  end;

  for Output in Outputs do
  begin
    Bounds.C1.Y := Min(Bounds.C1.Y, Output.Pos.Y);
    Bounds.C2.Y := Max(Bounds.C2.Y, Output.Pos.Y);
  end;
  for Input in Inputs do
  begin
    Bounds.C1.Y := Min(Bounds.C1.Y, Input.Pos.Y);
    Bounds.C2.Y := Max(Bounds.C2.Y, Input.Pos.Y);
  end;

  Result := Bounds.Center + 16;
end;

procedure TConnectionNet.Change;
begin
  FOnChange.Execute(TEventInfo.Create(Self));
end;

function TConnectionNet.Add(AMachinePort: TMachinePort): Boolean;
begin
  if not FMachinePorts.Empty and (AMachinePort.ItemStack.Item <> ItemType) then
    Exit(False);
  if FMachinePorts.Contains(AMachinePort) then
    Exit(False);
  FMachinePorts.Add(AMachinePort);
  Result := True;
end;

procedure TConnectionNet.Remove(AMachinePort: TMachinePort);
begin
  FMachinePorts.Remove(AMachinePort);
  if MachinePorts.Count <= 1 then
    Factory.RemoveConnectionNet(Self);
end;

procedure TConnectionNet.SetPos(const Value: TVector2);
begin
  if Pos = Value then
    Exit;
  FPos := Value - CalculateCenter;
  Change;
end;

constructor TConnectionNet.Create(AFactory: TFactory);
begin
  FFactorio := AFactory.Factorio;
  FFactory := AFactory;
  FMachinePorts := TList<TMachinePort>.Create;
end;

procedure TConnectionNet.DefineJStorage(ASerializer: TJSerializer);
var
  JConnections: TJArray;
  Port: TMachinePort;
  LoadedItemType: AnsiString;
  JPort: TJValue;
begin
  // Define('PosX', FPos.X);
  // Define('PosY', FPos.Y);

  case ASerializer.Mode of
    smSerialize:
      begin
        ASerializer.Value['Item'] := string(ItemType.Name);
        JConnections := ASerializer.Value.AddArray('Outputs');
        for Port in Outputs do
          JConnections.Add(Port.MachineArray.Index);
        JConnections := ASerializer.Value.AddArray('Inputs');
        for Port in Inputs do
          JConnections.Add(Port.MachineArray.Index);
      end;
    smUnserialize:
      begin
        LoadedItemType := AnsiString(ASerializer.Value['Item'].AsString);
        for JPort in ASerializer.Value['Outputs'].AsArray do
          for Port in Factory.MachineArrays[JPort.AsInt].Outputs do
            if Port.ItemStack.Item.Name = LoadedItemType then
              Add(Port);
        for JPort in ASerializer.Value['Inputs'].AsArray do
          for Port in Factory.MachineArrays[JPort.AsInt].Inputs do
            if Port.ItemStack.Item.Name = LoadedItemType then
              Add(Port);
      end;
  end;
end;

destructor TConnectionNet.Destroy;
begin
  FOnRemove.Execute(TEventInfo.Create(Self));
  inherited;
end;

procedure TConnectionNet.Draw(G: IGPGraphics);
// const
// BezierConst = 0.55191502449;
// BezierConst = 0.9;
var
  Pen: IGPPen;
  Color: TColorRGBA;
  Font: IGPFont;
  FontBrush: IGPBrush;
  RatioText: string;
  TextSize: TGPRectF;
  MachinePort: TMachinePort;
  I: Integer;
  Belts: ISortedList<TFactorio.TTransportBelt>;
  // A, B, C, D: TVector2;
begin
  if Inputs.Empty and Outputs.Empty then
    Exit;

  if Effectivity <= 1 then
    Color := TColorRGBA.HSV(Effectivity * 2, 0.5, 1, 1)
  else
    Color := TColorRGBA.HSV(5 - 3 / Effectivity, 0.5, 1);

  Pen := TGPPen.Create(Color, 16);
  Pen.StartCap := LineCapRound;
  Pen.EndCap := LineCapRound;

  for MachinePort in Outputs do
  begin
    {
      A := MachinePort.Pos + Vec2(32, 16);
      B := Center;
      C := Vec2(A.X + (B.X - A.X) * BezierConst, A.Y);
      D := Vec2(B.X, B.Y + (A.Y - B.Y) * BezierConst);
      G.DrawBezier(Pen, A.X, A.Y, C.X, C.Y, D.X, D.Y, B.X, B.Y);
    }
    G.DrawLines(Pen, [
      TGPPointF.Create(MachinePort.Pos.X + 32, MachinePort.Pos.Y + 16),
      TGPPointF.Create(Pos.X, MachinePort.Pos.Y + 16),
      TGPPointF.Create(Pos.X, Pos.Y)]);
  end;

  Pen.EndCap := LineCapArrowAnchor;

  for MachinePort in Inputs do
  begin
    {
      A := Center;
      B := MachinePort.Pos + Vec2(0, 16);
      C := Vec2(A.X, A.Y + (B.Y - A.Y) * BezierConst);
      D := Vec2(B.X + (A.X - B.X) * BezierConst, B.Y);
      G.DrawBezier(Pen, A.X, A.Y, C.X, C.Y, D.X, D.Y, B.X, B.Y);
    }
    G.DrawLines(Pen, [
      TGPPointF.Create(Pos.X, Pos.Y),
      TGPPointF.Create(Pos.X, MachinePort.Pos.Y + 16),
      TGPPointF.Create(MachinePort.Pos.X, MachinePort.Pos.Y + 16)]);
  end;

  Font := TGPFont.Create('Tahoma', 10, [FontStyleBold]);
  RatioText := Format('%3.3g/s %3.3g%%', [ItemsPerSecond, Effectivity * 100]);
  TextSize := G.MeasureString(RatioText, Font, TGPPointF.Create(0, 0));
  FontBrush := TGPSolidBrush.Create($FF000000);

  if IsFluid then
  begin
    G.DrawString(
      RatioText,
      Font,
      TGPPointF.Create(Pos.X - TextSize.Width / 2, Pos.Y - TextSize.Height / 2),
      FontBrush);
  end
  else
  begin
    G.DrawString(
      RatioText,
      Font,
      TGPPointF.Create(Pos.X - TextSize.Width / 2, Pos.Y - TextSize.Height),
      FontBrush);

    Belts := TSortedList<TFactorio.TTransportBelt>.Create;
    Belts.Compare := function(A, B: TFactorio.TTransportBelt): Boolean
      begin
        Result := A.Speed < B.Speed;
      end;
    Belts.AddRange(Factorio.TransportBeltOrder.Iterate.Where(
      function(Belt: TFactorio.TTransportBelt): Boolean
      begin
        Result := ItemsPerSecond <= Belt.ItemsPerSecond;
      end));

    for I := 0 to Belts.MaxIndex do
    begin
      if ItemsPerSecond <= Belts[I].ItemsPerSecond / 2 then
        G.DrawImage(Belts[I].Icon, Pos.X + 8 + (I - Belts.Count / 2) * 32, Pos.Y, 16, 32, 0, 0, 16, 32, UnitPixel)
      else
        G.DrawImage(Belts[I].Icon, Pos.X + (I - Belts.Count / 2) * 32, Pos.Y, 32, 32);
    end;
  end;
end;

{ TFactory.TConnectionNetEventInfo }

constructor TFactory.TConnectionNetEventInfo.Create(ASender: TFactory; AConnectionNet: IConnectionNet);
begin
  inherited Create(ASender);
  FConnectionNet := AConnectionNet;
end;

end.
