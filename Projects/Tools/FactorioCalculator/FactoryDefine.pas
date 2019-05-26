unit FactoryDefine;

interface

uses
  System.SysUtils,

  Pengine.Interfaces,
  Pengine.JSON,
  Pengine.ICollections,
  Pengine.Vector,

  Pengine.Factorio.General;

type

  TFactory = class;

  TMachineArray = class
  private
    FFactory: TFactory;
    FPos: TVector2;
    FRecipe: TFactorio.TRecipe;
    FAssemblingMachine: TFactorio.TAssemblingMachine;
    FCount: Integer;
    FInputs: IList<TMachineArray>;

    function GetInputs: IReadonlyList<TMachineArray>;

  public
    constructor Create(AFactory: TFactory; APos: TVector2; AAssemblingMachine: TFactorio.TAssemblingMachine);

    property Pos: TVector2 read FPos write FPos;
    property Recipe: TFactorio.TRecipe read FRecipe write FRecipe;
    property AssemblingMachine: TFactorio.TAssemblingMachine read FAssemblingMachine write FAssemblingMachine;
    property Count: Integer read FCount write FCount;
    property Inputs: IReadonlyList<TMachineArray> read GetInputs;

    procedure AddInput(AMachineArray: TMachineArray);
    procedure RemoveInput(AMachineArray: TMachineArray);

  end;

  TFactory = class(TInterfaceBase, IJSerializable)
  private
    FMachineArrays: IObjectList<TMachineArray>;

    function GetMachineArrays: IReadonlyList<TMachineArray>;

  public
    constructor Create;

    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

    property MachineArrays: IReadonlyList<TMachineArray> read GetMachineArrays;
    function AddMachineArray(APos: TVector2; AAssemblingMachine: TFactorio.TAssemblingMachine): TMachineArray;

  end;

implementation

{ TFactory }

function TFactory.AddMachineArray(APos: TVector2; AAssemblingMachine: TFactorio.TAssemblingMachine): TMachineArray;
begin
  Result := TMachineArray.Create(Self, APos, AAssemblingMachine);
  FMachineArrays.Add(Result);
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

function TFactory.GetJVersion: Integer;
begin
  Result := 0;
end;

{ TAssemblingMachine }

procedure TMachineArray.AddInput(AMachineArray: TMachineArray);
begin
  if FInputs.Contains(AMachineArray) then
    raise Exception.Create('Assembling machines already connected.');
  FInputs.Add(AMachineArray);
end;

constructor TMachineArray.Create(AFactory: TFactory; APos: TVector2; AAssemblingMachine: TFactorio.TAssemblingMachine);
begin
  FFactory := AFactory;
  FPos := APos;
  FAssemblingMachine := AAssemblingMachine;
  FCount := 1;
  FInputs := TList<TMachineArray>.Create;
end;

function TMachineArray.GetInputs: IReadonlyList<TMachineArray>;
begin
  Result := FInputs.ReadonlyList;
end;

procedure TMachineArray.RemoveInput(AMachineArray: TMachineArray);
begin
  FInputs.Remove(AMachineArray);
end;

end.
