unit ActionRecorder;

interface

uses
  System.SysUtils,

  Pengine.ICollections;

type

  EActionRecorder = class(Exception);

  TUndoableAction = class
  public
    procedure Execute; virtual; abstract;
    procedure Undo; virtual; abstract;

  end;

  TUndoableActionGroup = class(TUndoableAction)
  private
    FActions: IObjectList<TUndoableAction>;
    FLocked: Boolean;

    function GetActions: IReadonlyList<TUndoableAction>;

  public
    constructor Create;

    property Locked: Boolean read FLocked;

    procedure Add(AUndoableAction: TUndoableAction);
    property Actions: IReadonlyList<TUndoableAction> read GetActions;

    procedure Execute; override;
    procedure Undo; override;

  end;

  TActionRecorder = class
  private
    FActions: IObjectList<TUndoableAction>;
    FLastActionIndex: Integer;

    function GetActions: IReadonlyList<TUndoableAction>;
    function GetLastAction: TUndoableAction;

  public
    constructor Create;

    property Actions: IReadonlyList<TUndoableAction> read GetActions;
    property LastAction: TUndoableAction read GetLastAction;

    procedure Execute(AAction: TUndoableAction);

    function CanUndo: Boolean;
    procedure Undo;

    function CanRedo: Boolean;
    procedure Redo;

  end;

implementation

{ TActionRecorder }

function TActionRecorder.CanRedo: Boolean;
begin
  Result := FLastActionIndex < FActions.MaxIndex;
end;

function TActionRecorder.CanUndo: Boolean;
begin
  Result := FLastActionIndex >= 0;
end;

constructor TActionRecorder.Create;
begin
  FActions := TObjectList<TUndoableAction>.Create;
end;

procedure TActionRecorder.Execute(AAction: TUndoableAction);
begin
  FActions.Add(AAction);
  AAction.Execute;
  Inc(FLastActionIndex);
end;

function TActionRecorder.GetActions: IReadonlyList<TUndoableAction>;
begin
  Result := FActions.ReadonlyList;
end;

function TActionRecorder.GetLastAction: TUndoableAction;
begin
  Result := FActions[FLastActionIndex];
end;

procedure TActionRecorder.Redo;
begin
  if not CanRedo then
    Exit;
  Inc(FLastActionIndex);
  LastAction.Execute;
end;

procedure TActionRecorder.Undo;
begin
  if not CanUndo then
    Exit;
  LastAction.Undo;
  Dec(FLastActionIndex);
end;

{ TUndoableActionGroup }

procedure TUndoableActionGroup.Add(AUndoableAction: TUndoableAction);
begin
  if Locked then
    raise EActionRecorder.Create('Action-Group cannot be extended after execution.');

  if AUndoableAction is TUndoableActionGroup then
    FActions.AddRange(TUndoableActionGroup(AUndoableAction).Actions)
  else
    FActions.Add(AUndoableAction);
end;

constructor TUndoableActionGroup.Create;
begin
  FActions := TObjectList<TUndoableAction>.Create;
end;

procedure TUndoableActionGroup.Execute;
var
  Action: TUndoableAction;
begin
  FLocked := True;
  for Action in FActions do
    Action.Execute;
end;

function TUndoableActionGroup.GetActions: IReadonlyList<TUndoableAction>;
begin
  Result := FActions.ReadonlyList;
end;

procedure TUndoableActionGroup.Undo;
var
  Action: TUndoableAction;
begin
  for Action in FActions.Reverse do
    Action.Undo;
end;

end.

