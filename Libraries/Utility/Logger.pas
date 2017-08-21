unit Logger;

interface

uses
  Lists, IntfBase;

type

  TErrorSeverity = (
    esFatal,
    esError,
    esWarning,
    esHint,
    esVerbose
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

  end;

  { TLog }

  TLog<T: TLogEntry> = class(TInterfaceBase, IIterable<T>)
  private
    FEntries: TObjectArray<T>;
    FEntriesReader: TRefArrayReader<T>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AEntry: T);
    procedure Clear;

    property Entries: TRefArrayReader<T> read FEntriesReader;

    function GetEnumerator: IIterator<T>;
    function Count: Integer;

  end;

implementation

{ TLog<T> }

procedure TLog<T>.Add(AEntry: T);
begin
  FEntries.Add(AEntry);
end;

procedure TLog<T>.Clear;
begin
  FEntries.DelAll;
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

{ TCodeLogEntry }

constructor TCodeLogEntry.Create(ADescription: string; ALineNumber: Integer; ASeverity: TErrorSeverity);
begin
  FDescription := ADescription;
  FLineNumber := ALineNumber;
  FSeverity := ASeverity;
end;

end.
