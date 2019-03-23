unit AVVisualizer;

interface

procedure Register;

implementation

uses
  System.SysUtils,

  Vcl.Dialogs,

  ToolsAPI,

  Pengine.Collections;

type

  TArrayVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer250, IOTADebuggerVisualizerValueReplacer,
    IOTAThreadNotifier, IOTAThreadNotifier160)
  private
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
  public
    // IOTADebuggerVisualizer
    procedure GetSupportedType(Index: Integer; var TypeName: string; var AllDescendents: Boolean); overload;
    function GetSupportedTypeCount: Integer;
    function GetVisualizerDescription: string;
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    // IOTADebuggerVisualizer250
    procedure GetSupportedType(Index: Integer; var TypeName: string; var AllDescendants: Boolean;
      var IsGeneric: Boolean); overload;
    // IOTADebuggerVisualizerValueReplacer
    function GetReplacementValue(const Expression: string; const TypeName: string; const EvalResult: string): string;
    // IOTAThreadNotifier
    procedure EvaluteComplete(const ExprStr: string; const ResultStr: string; CanModify: Boolean;
      ResultAddress: Cardinal; ResultSize: Cardinal; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr: string; const ResultStr: string; ReturnCode: Integer);
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAThreadNotifier160
    procedure EvaluateComplete(const ExprStr: string; const ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer);

  end;

  TGenericVisualierType = record
    TypeName: string;
    IsGeneric: Boolean;
  end;

const
  GenericVisualizerTypes: array [0 .. 0] of TGenericVisualierType = (
    (TypeName: 'Pengine.Collections.TArray<T>'; IsGeneric: True)
    );

var
  DebuggerServices: IOTADebuggerServices;
  ArrayVisualizer: IOTADebuggerVisualizer250;

function Process: IOTAProcess; inline;
begin
  Result := DebuggerServices.CurrentProcess;
end;

function Thread: IOTAThread; inline;
begin
  Result := Process.CurrentThread;
end;

{ TArrayVisualizer }

procedure TArrayVisualizer.GetSupportedType(Index: Integer; var TypeName: string; var AllDescendents: Boolean);
begin
  TypeName := GenericVisualizerTypes[Index].TypeName;
  AllDescendents := True;
end;

function TArrayVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := Length(GenericVisualizerTypes);
end;

function TArrayVisualizer.GetVisualizerDescription: string;
begin
  Result := 'Visualizes ' + TArray.QualifiedClassName;
end;

function TArrayVisualizer.GetVisualizerIdentifier: string;
begin
  Result := TArray.QualifiedClassName;
end;

function TArrayVisualizer.GetVisualizerName: string;
begin
  Result := TArray.QualifiedClassName + '-Visualizer';
end;

procedure TArrayVisualizer.GetSupportedType(Index: Integer; var TypeName: string;
  var AllDescendants, IsGeneric: Boolean);
begin
  TypeName := GenericVisualizerTypes[Index].TypeName;
  AllDescendants := True;
  IsGeneric := GenericVisualizerTypes[Index].IsGeneric;
end;

function TArrayVisualizer.GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
var
  ResultStr: string;
  CanModify: Boolean;
  Address: UInt64;
  ResultSize, ResultVal: Cardinal;
  R: TOTAEvaluateResult;
  NotifierIndex: Integer;
  Done: Boolean;
begin
  repeat
    Done := True;
    SetLength(ResultStr, 256);
    R := Thread.Evaluate(Expression + '.FCount', @ResultStr[1], Length(ResultStr), CanModify, False, nil, Address, ResultSize, ResultVal);
    SetLength(ResultStr, ResultSize div 2);
    case R of
      erOK:
        begin
          Result := 'Count: ' + ResultStr;
        end;
      erError:
        begin
          Result := 'error';
        end;
      erDeferred:
        begin
          FCompleted := False;
          NotifierIndex := Thread.AddNotifier(Self);
          while not FCompleted do
            DebuggerServices.ProcessDebugEvents;
          Thread.RemoveNotifier(NotifierIndex);
          if not FDeferredError then
            Result := 'Count: ' + FDeferredResult
          else
            Result := 'deferred error';
        end;
      erBusy:
        begin
          DebuggerServices.ProcessDebugEvents;
          Done := False;
        end;
    end;
  until Done;
end;

procedure TArrayVisualizer.EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean; ResultAddress,
  ResultSize: Cardinal; ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress),
    LongWord(ResultSize), ReturnCode);
end;

procedure TArrayVisualizer.ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TArrayVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

procedure TArrayVisualizer.AfterSave;
begin

end;

procedure TArrayVisualizer.BeforeSave;
begin

end;

procedure TArrayVisualizer.Destroyed;
begin

end;

procedure TArrayVisualizer.Modified;
begin

end;

procedure TArrayVisualizer.EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
  ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredError := ReturnCode = 0;
  if not FDeferredError then
    FDeferredResult := ResultStr
end;

// Registering

procedure Register;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    ArrayVisualizer := TArrayVisualizer.Create;
    DebuggerServices.RegisterDebugVisualizer(ArrayVisualizer);
  end;
end;

procedure RemoveVisualizer;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(ArrayVisualizer);
    ArrayVisualizer := nil;
  end;
end;

initialization

finalization

RemoveVisualizer;

end.
