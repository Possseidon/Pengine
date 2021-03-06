unit Logger;

interface

uses
  Lists, IntfBase, SysUtils;

type

  TErrorSeverity = (
    esVerbose,
    esHint,
    esWarning,
    esError,
    esFatal
  );

  TLogEntry = class
  end;

  TCodeLogEntry = class (TLogEntry)
  private
    FDescription: string;
    FLineNumber: Integer;
    FSeverity: TErrorSeverity;

  public
    constructor Create(ADescription: string; ALineNumber: Integer; ASeverity: TErrorSeverity);

    property Severity: TErrorSeverity read FSeverity;
    property LineNumber: Integer read FLineNumber;
    property Description: string read FDescription;

    function ToString: string; override;    
    
  end;

  { TLog }

  TLog<T: TLogEntry> = class(TInterfaceBase, IIterable<T>)
  private
    FEntries: TObjectArray<T>;
    FEntriesReader: TRefArrayReader<T>;

  protected
    procedure InitLog; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AEntry: T); virtual;
    procedure Clear;

    property Entries: TRefArrayReader<T> read FEntriesReader;

    function GetEnumerator: IIterator<T>;
    function Count: Integer;

    function ToString: string; override;

  end;

  TCodeLog = class(TLog<TCodeLogEntry>)
  private
    FSuccess: Boolean;

  protected
    procedure InitLog; override;

  public
    procedure Add(AEntry: TCodeLogEntry); override;

    property Success: Boolean read FSuccess;
  end;

const
  ErrorSeverityStrings: array [TErrorSeverity] of string = (
    'Verbose',
    'Hint',
    'Warning',
    'Error',
    'Fatal'
  );

implementation

{ TLog<T> }

procedure TLog<T>.Add(AEntry: T);
begin
  FEntries.Add(AEntry);
end;

procedure TLog<T>.Clear;
begin
  FEntries.DelAll;
  InitLog;
end;

function TLog<T>.Count: Integer;
begin
  Result := FEntries.Count;
end;

constructor TLog<T>.Create;
begin
  FEntries := TObjectArray<T>.Create;
  FEntriesReader := TRefArrayReader<T>.Create(FEntries);
end;

destructor TLog<T>.Destroy;
begin
  FEntriesReader.Free;
  FEntries.Free;
  inherited;
end;

function TLog<T>.GetEnumerator: IIterator<T>;
begin
  Result := FEntries.GetEnumerator;
end;

procedure TLog<T>.InitLog;
begin
  // nothing by default
end;

function TLog<T>.ToString: string;
var
  Entry: T;
begin
  Result := '';
  for Entry in FEntries do
    Result := Result + Entry.ToString + sLineBreak;
end;

{ TCodeLogEntry }

constructor TCodeLogEntry.Create(ADescription: string; ALineNumber: Integer; ASeverity: TErrorSeverity);
begin
  FDescription := ADescription;
  FLineNumber := ALineNumber;
  FSeverity := ASeverity;
end;

function TCodeLogEntry.ToString: string;
begin
  Result := Format('[%s] %d: %s', [ErrorSeverityStrings[Severity], LineNumber, Description]);
end;

{ TCodeLog }

procedure TCodeLog.Add(AEntry: TCodeLogEntry);
begin
  inherited;
  if AEntry.FSeverity >= esError then
    FSuccess := False;
end;

procedure TCodeLog.InitLog;
begin
  inherited;
  FSuccess := True;
end;

end.
