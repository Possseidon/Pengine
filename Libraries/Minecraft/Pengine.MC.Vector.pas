unit Pengine.MC.Vector;

interface

uses
  System.SysUtils,
  System.Character,

  Pengine.Utility,
  Pengine.Parser,
  Pengine.IntMaths,
  System.Math;

type

  TMCVecValue = class
  public type

    TMode = (
      vmAbsolute,
      vmRelative,
      vmLocal
      );

    TParser = class(TRefParser<TMCVecValue>)
    private
      FBlockPos: Boolean;

    protected
      function Parse: Boolean; override;

    public
      constructor Create(AParseObject: TMCVecValue; AInfo: TParseInfo; ABlockPos, ARequired: Boolean);

      class function GetResultName: string; override;

    end;

  private
    FValue: Single;
    FMode: TMode;

  public
    constructor Create(AValue: Single = 0; AMode: TMode = vmAbsolute);

    property Value: Single read FValue write FValue;
    property Mode: TMode read FMode write FMode;

    function Format: string;

  end;

  TMCVec2 = class
  public type

    TParser = class(TObjectParser<TMCVec2>)
    private
      FChunkPos: Boolean;

    protected
      function Parse: Boolean; override;

    public
      constructor Create(AInfo: TParseInfo; AChunkPos, ARequired: Boolean);

      class function GetResultName: string; override;

    end;

  private
    FValues: array [TCoordAxis2] of TMCVecValue;

    function GetValue(AAxis: TCoordAxis2): TMCVecValue;

  public
    constructor Create;
    destructor Destroy; override;

    property Values[AAxis: TCoordAxis2]: TMCVecValue read GetValue; default;
    property X: TMCVecValue index caX read GetValue;
    property Y: TMCVecValue index caY read GetValue;

    function Format: string;

  end;

  TMCVec3 = class
  public type

    TParser = class(TObjectParser<TMCVec3>)
    private
      FBlockPos: Boolean;

    protected
      function Parse: Boolean; override;

    public
      constructor Create(AInfo: TParseInfo; ABlockPos, ARequired: Boolean);

      class function GetResultName: string; override;

    end;

  private
    FValues: array [TCoordAxis3] of TMCVecValue;

    function GetValue(AAxis: TCoordAxis3): TMCVecValue;

  public
    constructor Create;
    destructor Destroy; override;

    property Values[AAxis: TCoordAxis3]: TMCVecValue read GetValue; default;
    property X: TMCVecValue index caX read GetValue;
    property Y: TMCVecValue index caY read GetValue;
    property Z: TMCVecValue index caZ read GetValue;

    function Format: string;

  end;

  TMCSwizzle = class
  public type

    TParser = class(TObjectParser<TMCSwizzle>)
    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

    end;

    TSuggestions = class(TParseSuggestionsSimple<TParser>)
    public const

      Suggestions: array [0 .. 6] of string = (
        'x',
        'y',
        'z',
        'xy',
        'xz',
        'yz',
        'xyz'
        );

    public
      class function GetCount: Integer; override;
      class function GetSuggestion(AIndex: Integer): TParseSuggestion; override;

    end;

  private
    FAxes: TCoordAxes3;

  public
    constructor Create(AAxes: TCoordAxes3 = []);

    property Axes: TCoordAxes3 read FAxes write FAxes;

    function Format: string;

  end;

implementation

{ TMCVec2 }

constructor TMCVec2.Create;
var
  Axis: TCoordAxis2;
begin
  for Axis := Low(TCoordAxis2) to High(TCoordAxis2) do
    FValues[Axis] := TMCVecValue.Create;
end;

destructor TMCVec2.Destroy;
var
  Axis: TCoordAxis2;
begin
  for Axis := Low(TCoordAxis2) to High(TCoordAxis2) do
    FValues[Axis].Free;
  inherited;
end;

function TMCVec2.Format: string;
begin
  Result := X.Format + ' ' + Y.Format;
end;

function TMCVec2.GetValue(AAxis: TCoordAxis2): TMCVecValue;
begin
  Result := FValues[AAxis];
end;

{ TMCVec3 }

constructor TMCVec3.Create;
var
  Axis: TCoordAxis3;
begin
  for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    FValues[Axis] := TMCVecValue.Create;
end;

destructor TMCVec3.Destroy;
var
  Axis: TCoordAxis3;
begin
  for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    FValues[Axis].Free;
  inherited;
end;

function TMCVec3.Format: string;
begin
  Result := X.Format + ' ' + Y.Format + ' ' + Z.Format;
end;

function TMCVec3.GetValue(AAxis: TCoordAxis3): TMCVecValue;
begin
  Result := FValues[AAxis];
end;

{ TMCVec3.TParser }

constructor TMCVec3.TParser.Create(AInfo: TParseInfo; ABlockPos, ARequired: Boolean);
begin
  FBlockPos := ABlockPos;
  inherited Create(AInfo, ARequired);
end;

class function TMCVec3.TParser.GetResultName: string;
begin
  Result := '3-Component Vector';
end;

function TMCVec3.TParser.Parse: Boolean;
var
  Axis: TCoordAxis3;
  Parser: TMCVecValue.TParser;
  Local: Boolean;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  SetParseResult(TMCVec3.Create);
  Parser := TMCVecValue.TParser.Create(ParseResult.X, Info, FBlockPos, False);
  Local := ParseResult.X.Mode = vmLocal;
  if not Parser.Success then
  begin
    Parser.Free;
    Exit(False);
  end;
  Parser.Free;
  for Axis := Succ(Low(TCoordAxis3)) to High(TCoordAxis3) do
  begin
    if not ReachedEnd and not First.IsWhitespace then
      raise EParseError.Create('Expected whitespace.');
    Advance;
    SkipWhitespace;
    Parser := TMCVecValue.TParser.Create(ParseResult[Axis], Info, FBlockPos, False);
    try
      if not Parser.Success then
      begin
        Log(1, 'Expected %s-Coordinate.', [CoordAxisNames[Axis]], elFatal);
        Break;
      end;
    finally
      Parser.Free;
    end;
  end;
  for Axis := Succ(Low(TCoordAxis3)) to High(TCoordAxis3) do
  begin
    if (ParseResult[Axis].Mode = vmLocal) <> Local then
    begin
      Log(Marker, 'Local coordinates (^) cannot be mixed with non-local coordinates.');
      Break;
    end;
  end;
  Result := True;
end;

{ TMCVecValue.TParser }

constructor TMCVecValue.TParser.Create(AParseObject: TMCVecValue; AInfo: TParseInfo; ABlockPos, ARequired: Boolean);
begin
  FBlockPos := ABlockPos;
  inherited Create(AParseObject, AInfo, ARequired);
end;

class function TMCVecValue.TParser.GetResultName: string;
begin
  Result := 'Coordinate';
end;

function TMCVecValue.TParser.Parse: Boolean;
const
  NumChars = ['0' .. '9', '-', '.'];
var
  Mode: TMode;
  Value: Single;
  NumString: string;
begin
  if StartsWith('~') then
    Mode := vmRelative
  else if StartsWith('^') then
    Mode := vmLocal
  else if not CharInSet(First, NumChars) then
    Exit(False)
  else
    Mode := vmAbsolute;

  NumString := ReadUntil([' ']);
  if NumString.IsEmpty and (Mode <> vmAbsolute) then
    Value := 0
  else if not TryStrToFloat(NumString, Value, FormatSettings.Invariant) then
    Exit(False);

  if FBlockPos and (Mode = vmAbsolute) and NumString.Contains('.') then
    Log(-NumString.Length, 'Absolute block positions must be integer.');

  ParseObject.Value := Value;
  ParseObject.Mode := Mode;
  Result := True;
end;

{ TMCVec2.TParser }

constructor TMCVec2.TParser.Create(AInfo: TParseInfo; AChunkPos, ARequired: Boolean);
begin
  FChunkPos := AChunkPos;
  inherited Create(AInfo, ARequired);
end;

class function TMCVec2.TParser.GetResultName: string;
begin
  Result := '2-Component Vector';
end;

function TMCVec2.TParser.Parse: Boolean;
var
  Parser: TMCVecValue.TParser;
  Local: Boolean;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  SetParseResult(TMCVec2.Create);
  Parser := TMCVecValue.TParser.Create(ParseResult.X, Info, FChunkPos, False);
  Local := ParseResult.X.Mode = vmLocal;
  Result := Parser.Success;
  Parser.Free;
  if not Result then
    Exit;

  if not ReachedEnd and not First.IsWhitespace then
    raise EParseError.Create('Expected whitespace.');
  Advance;
  SkipWhitespace;
  Parser := TMCVecValue.TParser.Create(ParseResult.Y, Info, FChunkPos, False);
  if not Parser.Success then
    Log(1, 'Expected %s-Coordinate.', [CoordAxisNames[caY]], elFatal);
  Parser.Free;

  if (ParseResult.Y.Mode = vmLocal) <> Local then
    Log(Marker, 'Local coordinates (^) cannot be mixed with non-local coordinates.');

  Result := True;
end;

{ TMCVecValue }

constructor TMCVecValue.Create(AValue: Single; AMode: TMode);
begin
  FValue := AValue;
  FMode := AMode;
end;

function TMCVecValue.Format: string;
begin
  case Mode of
    vmAbsolute:
      Exit(PrettyFloat(Value));
    vmRelative:
      Result := '~';
    vmLocal:
      Result := '^';
  end;
  if Value <> 0 then
    Result := Result + PrettyFloat(Value);
end;

{ TMCSwizzle }

constructor TMCSwizzle.Create(AAxes: TCoordAxes3);
begin
  FAxes := AAxes;
end;

function TMCSwizzle.Format: string;
var
  Axis: TCoordAxis3;
begin
  Result := '';
  for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    if Axis in FAxes then
      Result := Result + CoordAxisNamesLow[Axis];
end;

{ TMCSwizzle.TParser }

class function TMCSwizzle.TParser.GetResultName: string;
begin
  Result := 'Swizzle';
end;

function TMCSwizzle.TParser.Parse: Boolean;
var
  Axis: TCoordAxis3;
  Found: Boolean;
begin
  BeginSuggestions(TSuggestions);
  SetParseResult(TMCSwizzle.Create);
  repeat
    Found := False;
    for Axis := Low(TCoordAxis3) to High(TCoordAxis3) do
    begin
      if StartsWith(CoordAxisNamesLow[Axis]) then
      begin
        if Axis in ParseResult.Axes then
          Log(-1, 'The axis "%s" is already in the swizzle.', [CoordAxisNamesLow[Axis]]);
        ParseResult.Axes := ParseResult.Axes + [Axis];
        Found := True;
        Break;
      end;
    end;
  until not Found;

  if ParseResult.Axes = [] then
    Exit(False);

  Result := True;
end;

{ TMCSwizzle.TSuggestions }

class function TMCSwizzle.TSuggestions.GetCount: Integer;
begin
  Result := Length(Suggestions);
end;

class function TMCSwizzle.TSuggestions.GetSuggestion(AIndex: Integer): TParseSuggestion;
begin
  Result := Suggestions[AIndex];
end;

end.
