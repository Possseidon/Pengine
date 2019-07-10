unit FactoryActions;

interface

uses
  System.SysUtils,

  Pengine.ActionRecorder,

  FactoryDefine;

type

  TAddMachineArrayAction = class(TUndoableAction)
  private
    FFactory: TFactory;
    FMachineArray: TMachineArray;

  public
    constructor Create(AFatory: TFactory);

    procedure Execute; override;
    procedure Undo; override;

  end;

  TRemoveMachineArrayAction = class(TUndoableAction)
  private
    FFactory: TFactory;
    FIndex: Integer;
    FMachineArray: TMachineArray;

  public
    constructor Create(AMachineArray: TMachineArray);
    destructor Destroy; override;

    procedure Execute; override;
    procedure Undo; override;

  end;

implementation

{ TAddMachineArrayAction }

constructor TAddMachineArrayAction.Create(AFatory: TFactory);
begin
  FFactory := AFatory;
end;

procedure TAddMachineArrayAction.Execute;
begin
  FMachineArray := FFactory.AddMachineArray;
end;

procedure TAddMachineArrayAction.Undo;
begin
  FMachineArray.Remove;
end;

{ TRemoveMachineArrayAction }

constructor TRemoveMachineArrayAction.Create(AMachineArray: TMachineArray);
begin
  FFactory := AMachineArray.Factory;
  FIndex := AMachineArray.Index;
end;

destructor TRemoveMachineArrayAction.Destroy;
begin
  FMachineArray.Free;
  inherited;
end;

procedure TRemoveMachineArrayAction.Execute;
begin
  FMachineArray := FFactory.MachineArrays[FIndex].Copy;
  FFactory.MachineArrays[FIndex].Remove;
end;

procedure TRemoveMachineArrayAction.Undo;
var
  NewMachineArray: TMachineArray;
begin
  NewMachineArray := FFactory.AddMachineArray;
  NewMachineArray.Index := FIndex;
  NewMachineArray.Assign(FMachineArray);
  FreeAndNil(FMachineArray);
end;

end.

