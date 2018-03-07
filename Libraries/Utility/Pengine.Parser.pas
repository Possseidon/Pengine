unit Pengine.Parser;

interface

uses
  System.SysUtils,
  System.RTTI,
  System.Character,

  Pengine.CollectionInterfaces;

type

  EParseError = class(Exception);

  TParseInfo = class
  public type

    TIterator = class(TIterator<Char>)
    private
      FInfo: TParseInfo;

    public
      constructor Create(AInfo: TParseInfo);

      function MoveNext: Boolean; override;
      function GetCurrent: Char; override;
    end;

  private
    FText: string;
    FPos: Integer;

    function GetFirst: Char;

  public
    constructor Create(AText: string);

    property Pos: Integer read FPos write FPos;
    property First: Char read GetFirst;
    procedure Advance(AAmount: Integer = 1); inline;
    function StartsWith(AChar: Char; AAdvanceOnMatch: Boolean = True): Boolean; overload;
    function StartsWith(AText: string; AAdvanceOnMatch: Boolean = True): Boolean; overload;
    function ReadWhile(APredicate: TPredicate<Char>; AAdvance: Boolean = True): string;
    procedure SkipWhitespace;
    function ReadUntilWhitespace: string;
    
    function ReachedEnd: Boolean;

    function AllText: string;

    function GetEnumerator: TIterator;

  end;

  TParser = class
  private
    FInfo: TParseInfo;
    FLeftover: string;
    FError: string;
    FSuccess: Boolean;

  protected
    function Parse: Boolean; virtual; abstract;

    property Info: TParseInfo read FInfo;

    procedure Cleanup; virtual;

  public
    constructor Create(AText: string); overload;
    constructor Create(AInfo: TParseInfo; ARequired: Boolean = False); overload;

    class function GetResultName: string; virtual;

    function HasLeftover: Boolean;
    property Leftover: string read FLeftover;
    property Success: Boolean read FSuccess;
    property Error: string read FError;

    procedure RaiseFailure;

  end;

  TParser<T> = class(TParser)
  private
    FParseResult: T;

  protected
    procedure SetParseResult(AResult: T);

  public
    property ParseResult: T read FParseResult;

    class function GetResultName: string; override;

  end;

  TIdentifierParser = class(TParser<string>)
  protected
    function Parse: Boolean; override;
  public 
    class function GetResultName: string; override;
  end;

implementation

{ TParser }

procedure TParser.Cleanup;
begin
  // nothing by default
end;

constructor TParser.Create(AInfo: TParseInfo; ARequired: Boolean);
begin
  FInfo := AInfo;
  try
    if Parse then
    begin
      FSuccess := True;
      FLeftover := Info.AllText;
    end
    else if ARequired then
      RaiseFailure;
  finally
    if not Success then
      Cleanup;
  end;
end;

class function TParser.GetResultName: string;
begin
  Result := Self.ClassName;
end;

function TParser.HasLeftover: Boolean;
begin
  Result := not FLeftover.IsEmpty;
end;

procedure TParser.RaiseFailure;
begin
  raise EParseError.Create('Expected ' + GetResultName + '.');
end;

constructor TParser.Create(AText: string);
begin
  try
    Create(TParseInfo.Create(AText));
  except
    on E: EParseError do
    begin
      FError := E.Message;
    end;
  end;
  if not Success and FError.IsEmpty then
    FError := 'Expected ' + GetResultName + '.';
  Info.Free;
end;

{ TParseInfo }

procedure TParseInfo.Advance(AAmount: Integer);
begin
  Inc(FPos, AAmount);
end;

function TParseInfo.AllText: string;
begin
  Result := FText.Substring(FPos - 1);
end;

constructor TParseInfo.Create(AText: string);
begin
  FText := AText;
  FPos := 1;
end;

function TParseInfo.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

function TParseInfo.GetFirst: Char;
begin
  Result := FText[FPos];
end;

function TParseInfo.ReachedEnd: Boolean;
begin
  Result := FPos > FText.Length;
end;

function TParseInfo.ReadUntilWhitespace: string;
begin
  Result := ReadWhile(
    function (C: Char): Boolean
    begin
      Result := not C.IsWhiteSpace;
    end);
end;

function TParseInfo.ReadWhile(APredicate: TPredicate<Char>; AAdvance: Boolean): string;
var
  Pos, Len: Integer;
begin
  Len := FText.Length;
  Pos := FPos;
  while (Pos <= Len) and APredicate(FText[Pos]) do
    Inc(Pos);
  Result := FText.Substring(FPos - 1, Pos - FPos);
  if AAdvance then
    FPos := Pos;
end;

function TParseInfo.StartsWith(AChar: Char; AAdvanceOnMatch: Boolean): Boolean;
begin
  Result := not ReachedEnd and (FText[FPos] = AChar);
  if Result and AAdvanceOnMatch then
    Advance;
end;

procedure TParseInfo.SkipWhitespace;
begin
  while not ReachedEnd and First.IsWhiteSpace do
    Advance;
end;

function TParseInfo.StartsWith(AText: string; AAdvanceOnMatch: Boolean): Boolean;
var
  Len, I: Integer;
begin
  Len := AText.Length;
  for I := 0 to Len - 1 do
    if FText[FPos + I] <> AText[I] then
      Exit(False);
  if AAdvanceOnMatch then
    Advance(Len);
  Result := True;
end;

{ TParseInfo.TIterator }

constructor TParseInfo.TIterator.Create;
begin
  FInfo := AInfo;
  Dec(FInfo.FPos);
end;

function TParseInfo.TIterator.GetCurrent: Char;
begin
  Result := FInfo.FText[FInfo.FPos];
end;

function TParseInfo.TIterator.MoveNext: Boolean;
begin
  FInfo.Advance(1);
  Result := not FInfo.ReachedEnd;
end;

{ TParser<T> }

class function TParser<T>.GetResultName: string;
var
  RTTI: TRttiContext;
begin
  RTTI := TRttiContext.Create;
  Result := RTTI.GetType(TypeInfo(T)).Name;
  RTTI.Free;
end;

procedure TParser<T>.SetParseResult(AResult: T);
begin
  FParseResult := AResult;
end;

{ TIdentifierParser }

class function TIdentifierParser.GetResultName: string;
begin
  Result := 'Identifier';
end;

function TIdentifierParser.Parse: Boolean;
const
  Alpha = ['a' .. 'z', 'A' .. 'Z', '_'];
  AlphaNum = Alpha + ['0' .. '9'];

var
  C: Char;
begin
  if FInfo.ReachedEnd or not CharInSet(FInfo.First, Alpha) then
    Exit(False);
  FParseResult := FInfo.First;
  FInfo.Advance;
  for C in Info do
  begin
    if not CharInSet(C, AlphaNum) then
      Break;
    FParseResult := FParseResult + C;
  end;
  Result := True;
end;

end.
