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

  Pengine.Factorio.General;

type

  TFactory = class;

  TMachineArray = class
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
    FOnDestroy: TEvent;

    function GetOnChange: TEvent.TAccess;
    function GetOnDestroy: TEvent.TAccess;

    procedure Change;
    procedure SetCraftingMachine(const Value: TFactorio.TCraftingMachine);
    procedure SetCount(const Value: Integer);
    procedure SetPos(const Value: TVector2);
    procedure SetRecipe(const Value: TFactorio.TRecipe);
    procedure SetPerformance(const Value: Single);

  public
    constructor Create(AFactory: TFactory; APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine);
    procedure BeforeDestruction; override;
    procedure Remove;

    property Factory: TFactory read FFactory;

    property Pos: TVector2 read FPos write SetPos;
    property CraftingMachine: TFactorio.TCraftingMachine read FCraftingMachine write SetCraftingMachine;
    property Count: Integer read FCount write SetCount;
    property Performance: Single read FPerformance write SetPerformance;
    function HasRecipe: Boolean;
    property Recipe: TFactorio.TRecipe read FRecipe write SetRecipe;

    property OnChange: TEvent.TAccess read GetOnChange;
    property OnDestroy: TEvent.TAccess read GetOnDestroy;

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
    FOnMachineArrayDestroy: TMachineEvent;

    function GetMachineArrays: IReadonlyList<TMachineArray>;

    procedure MachineArrayChange(AInfo: TMachineArray.TEventInfo);
    procedure MachineArrayDestroy(AInfo: TMachineArray.TEventInfo);

  public
    constructor Create;

    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

    property MachineArrays: IReadonlyList<TMachineArray> read GetMachineArrays;
    function AddMachineArray(APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine): TMachineArray;
    procedure RemoveMachineArray(AMachineArray: TMachineArray);

  end;

implementation

{ TFactory }

function TFactory.AddMachineArray(APos: TVector2; ACraftingMachine: TFactorio.TCraftingMachine): TMachineArray;
begin
  Result := TMachineArray.Create(Self, APos, ACraftingMachine);
  FMachineArrays.Add(Result);
  Result.OnChange.Add(MachineArrayChange);
  Result.OnDestroy.Add(MachineArrayDestroy);
  FOnMachineArrayAdd.Execute(TMachineArrayEventInfo.Create(Self, Result));
end;

constructor TFactory.Create;
begin
  FMachineArrays := TObjectList<TMachineArray>.Create;
end;

procedure TFactory.DefineJStorage(ASerializer: TJSerializer);
begin

end;

function TFactory.GetMachineArrays: IReadonlyList<TMachineArray>;
begin
  Result := FMachineArrays.ReadonlyList;
end;

procedure TFactory.MachineArrayChange(AInfo: TMachineArray.TEventInfo);
begin
  FOnMachineArrayChange.Execute(TMachineArrayEventInfo.Create(Self, AInfo.Sender));
end;

procedure TFactory.MachineArrayDestroy(AInfo: TMachineArray.TEventInfo);
begin
  FOnMachineArrayDestroy.Execute(TMachineArrayEventInfo.Create(Self, AInfo.Sender));
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
  FOnDestroy.Execute(TEventInfo.Create(Self));
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

function TMachineArray.GetOnDestroy: TEvent.TAccess;
begin
  Result := FOnDestroy.Access;
end;

function TMachineArray.HasRecipe: Boolean;
begin
  Result := FRecipe <> nil;
end;

procedure TMachineArray.Remove;
begin
  Factory.RemoveMachineArray(Self);
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
