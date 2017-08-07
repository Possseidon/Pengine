unit CVAddin;

interface

uses
  Classes, ToolsAPI, SysUtils, Lists, Dialogs;

type

  TPengineListVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer, IOTADebuggerVisualizerValueReplacer)
  public
    // IOTADebuggerVisualizer
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string; var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;

    // IOTADebuggerVisualizerValueReplacer
    function GetReplacementValue(const Expression, TypeName, EvalResult: string): string;

  end;

  TPengineListThreadNotifier = class(TInterfacedObject, IOTAThreadNotifier)
  public
    procedure EvaluteComplete(const ExprStr: string; const ResultStr: string; CanModify: Boolean;
      ResultAddress: Cardinal; ResultSize: Cardinal; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr: string; const ResultStr: string; ReturnCode: Integer);
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;
    procedure Destroyed;
  end;

procedure Register;

implementation

var
  PengineListVisualizer: IOTADebuggerVisualizer;
  DebuggerServices: IOTADebuggerServices;

function Process: IOTAProcess; inline;
begin
  Result := DebuggerServices.CurrentProcess;
end;

function Thread: IOTAThread; inline;
begin
  Result := Process.CurrentThread;
end;

{ TPengineListsVisualizer }

function TPengineListVisualizer.GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
var
  CanModify: Boolean;
  Address: TOTAAddress;
  ResultStr: string;
  ResultSize: Cardinal;
  ResultVal: Cardinal;
  R: TOTAEvaluateResult;
  Notifier: TPengineListThreadNotifier;
  NotifierID: Integer;
begin
  // Result := Format('Expr: "%s" Type: "%s" EvalResult: "%s"', [Expression, TypeName, EvalResult]);
  Setlength(ResultStr, 2);
  R := Thread.Evaluate(Expression + '[''test'']', @ResultStr[1], 1, CanModify, False, nil, Address, ResultSize, ResultVal);
  // Process.ReadProcessMemory(Address, ResultSize, List);
  case R of
    erOK:
      Result := Format('$%.16x %d %d', [Address, ResultSize, ResultVal]);
    erError:
      Result := 'Error';
    erDeferred:
      begin
        Result := 'Calculating...';
        Notifier := TPengineListThreadNotifier.Create;
        NotifierID := Thread.AddNotifier(Notifier);
      end;
    erBusy:
      Result := 'Busy';
  end;
end;

procedure TPengineListVisualizer.GetSupportedType(Index: Integer; var TypeName: string; var AllDescendants: Boolean);
begin
  case Index of
    0:
      begin
        TypeName := 'Lists.TStringMap<System.Integer>';
        AllDescendants := True;
      end;
  end;
end;

function TPengineListVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TPengineListVisualizer.GetVisualizerDescription: string;
begin
  Result := 'Visualizes the content of Classes found in the Lists Unit of Pengine';
end;

function TPengineListVisualizer.GetVisualizerIdentifier: string;
begin
  Result := 'PengineListsVisualizer';
end;

function TPengineListVisualizer.GetVisualizerName: string;
begin
  Result := 'Pengine List Visualization';
end;

{ Register }

procedure Register;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    PengineListVisualizer := TPengineListVisualizer.Create;
    DebuggerServices.RegisterDebugVisualizer(PengineListVisualizer);
  end;
end;

procedure RemoveVisualizer;
var
  LServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, LServices) then
  begin
    LServices.UnregisterDebugVisualizer(PengineListVisualizer);
    PengineListVisualizer := nil;
  end;
end;

{ TPengineListThreadNotifier }

procedure TPengineListThreadNotifier.AfterSave;
begin

end;

procedure TPengineListThreadNotifier.BeforeSave;
begin

end;

procedure TPengineListThreadNotifier.Destroyed;
begin

end;

procedure TPengineListThreadNotifier.EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
  ResultAddress, ResultSize: Cardinal; ReturnCode: Integer);
begin
  ShowMessage('Got Result!');
end;

procedure TPengineListThreadNotifier.Modified;
begin

end;

procedure TPengineListThreadNotifier.ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TPengineListThreadNotifier.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

initialization

finalization

RemoveVisualizer;

end.
