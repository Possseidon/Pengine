unit LuaVisualizer;

interface

uses
  ToolsAPI,

  System.TimeSpan,
  System.SyncObjs,
  System.SysUtils,
  Pengine.LuaHeader;

type

  TPengineLuaVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer, IOTADebuggerVisualizerValueReplacer,
    IOTAThreadNotifier)
  private
    FEvent: TEvent;
    FEvaluateResult: string;

  public
    constructor Create;
    destructor Destroy; override;

    // IOTADebuggerVisualizer
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string; var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;

    // IOTADebuggerVisualizerValueReplacer
    function GetReplacementValue(const Expression, TypeName, EvalResult: string): string;

    // IOTAThreadNotifier
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

  end;

procedure Register;

var
  PengineLuaVisualizer: IOTADebuggerVisualizer;
  DebuggerServices: IOTADebuggerServices;

implementation

{ Register }

procedure Register;
begin
  if PengineLuaVisualizer <> nil then
    Exit;
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    PengineLuaVisualizer := TPengineLuaVisualizer.Create;
    DebuggerServices.RegisterDebugVisualizer(PengineLuaVisualizer);
  end;
end;

procedure RemoveVisualizer;
begin
  DebuggerServices.UnregisterDebugVisualizer(PengineLuaVisualizer);
  PengineLuaVisualizer := nil;
end;

function Process: IOTAProcess; inline;
begin
  Result := DebuggerServices.CurrentProcess;
end;

function Thread: IOTAThread; inline;
begin
  Result := Process.CurrentThread;
end;

{ TPengineLuaVisualizer }

procedure TPengineLuaVisualizer.AfterSave;
begin
  // nothing
end;

procedure TPengineLuaVisualizer.BeforeSave;
begin
  // nothing
end;

constructor TPengineLuaVisualizer.Create;
begin
  FEvent := TEvent.Create(nil, False, False, '');
end;

destructor TPengineLuaVisualizer.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TPengineLuaVisualizer.Destroyed;
begin
  // nothing
end;

procedure TPengineLuaVisualizer.EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean; ResultAddress,
  ResultSize: LongWord; ReturnCode: Integer);
begin
  FEvaluateResult := ResultStr;
  FEvent.SetEvent;
end;

function TPengineLuaVisualizer.GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
var
  L: TLuaState;
  Buffer: TBytes;
  I: Integer;
begin
  L := TLuaState(EvalResult.ToInteger);
  SetLength(Buffer, 824);
  Process.ReadProcessMemory(NativeUInt(L), Length(Buffer), Buffer[0]);

  Result := '';
  for I := 0 to Length(Buffer) - 1 do
  begin
    Result := Result + Buffer[I].ToHexString + ' ';
    if I mod 8 = 7 then
      Result := Result + sLineBreak;
  end;
end;

procedure TPengineLuaVisualizer.GetSupportedType(Index: Integer; var TypeName: string; var AllDescendants: Boolean);
begin
  case Index of
    0:
      TypeName := 'TLuaState';
  end;
end;

function TPengineLuaVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TPengineLuaVisualizer.GetVisualizerDescription: string;
begin
  Result := 'Stack visualization for TLuaState.';
end;

function TPengineLuaVisualizer.GetVisualizerIdentifier: string;
begin
  Result := 'PengineLuaVisualizer';
end;

function TPengineLuaVisualizer.GetVisualizerName: string;
begin
  Result := 'TLuaState Visualizer';
end;

procedure TPengineLuaVisualizer.Modified;
begin
  // nothing
end;

procedure TPengineLuaVisualizer.ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
begin
  // nothing
end;

procedure TPengineLuaVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin
  // nothing
end;

initialization

finalization

RemoveVisualizer;

end.
