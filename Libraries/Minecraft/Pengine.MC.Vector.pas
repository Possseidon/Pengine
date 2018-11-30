unit Pengine.MC.Vector;

interface

uses
  System.SysUtils,
  System.Character,

  Pengine.Utility,
  Pengine.Parsing,
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

    IParser = interface(IDecorateParser<TMCVecValue>)
      function GetBlockPos: Boolean;
      procedure SetBlockPos(const Value: Boolean);

      property BlockPos: Boolean read GetBlockPos write SetBlockPos;

    end;

    TParser = class(TDecorateParser<TMCVecValue>, IParser)
    private
      FBlockPos: Boolean;

      function GetBlockPos: Boolean;
      procedure SetBlockPos(const Value: Boolean);

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

      property BlockPos: Boolean read GetBlockPos write SetBlockPos;

    end;

  private
    FValue: Single;
    FMode: TMode;

  public
    constructor Create(AValue: Single = 0; AMode: TMode = vmAbsolute);

    class function Parser: IParser;

    property Value: Single read FValue write FValue;
    property Mode: TMode read FMode write FMode;

    function Format: string;

  end;

  TMCVec2 = class
  public type

    IParser = interface(IObjectParser<TMCVec2>)
      function GetChunkPos: Boolean;
      procedure SetChunkPos(const Value: Boolean);

      property ChunkPos: Boolean read GetChunkPos write SetChunkPos;

    end;

    TParser = class(TObjectParser<TMCVec2>, IParser)
    private
      FChunkPos: Boolean;

      function GetChunkPos: Boolean;
      procedure SetChunkPos(const Value: Boolean);

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

      property ChunkPos: Boolean read GetChunkPos write SetChunkPos;

    end;

  private
    FValues: array [TCoordAxis2] of TMCVecValue;

    function GetValue(AAxis: TCoordAxis2): TMCVecValue;

  public
    constructor Create;
    destructor Destroy; override;

    class function Parser: IParser;

    property Values[AAxis: TCoordAxis2]: TMCVecValue read GetValue; default;
    property X: TMCVecValue index caX read GetValue;
    property Y: TMCVecValue index caY read GetValue;

    function Format: string;

  end;

  TMCVec3 = class
  public type

    IParser = interface(IObjectParser<TMCVec3>)
      function GetBlockPos: Boolean;
      procedure SetBlockPos(const Value: Boolean);

      property BlockPos: Boolean read GetBlockPos write SetBlockPos;

    end;

    TParser = class(TObjectParser<TMCVec3>, IParser)
    private
      FBlockPos: Boolean;

      function GetBlockPos: Boolean;
      procedure SetBlockPos(const Value: Boolean);

    protected
      function Parse: Boolean; override;

    public
      class function GetResultName: string; override;

      property BlockPos: Boolean read GetBlockPos write SetBlockPos;

    end;

  private
    FValues: array [TCoordAxis3] of TMCVecValue;

    function GetValue(AAxis: TCoordAxis3): TMCVecValue;

  public
    constructor Create;
    destructor Destroy; override;

    class function Parser: IParser;

    property Values[AAxis: TCoordAxis3]: TMCVecValue read GetValue; default;
    property X: TMCVecValue index caX read GetValue;
    property Y: TMCVecValue index caY read GetValue;
    property Z: TMCVecValue index caZ read GetValue;

    function Format: string;

  end;

  TMCSwizzle = class
  public type

    IParser = IObjectParser<TMCSwizzle>;

    TParser = class(TObjectParser<TMCSwizzle>, IParser)
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

    class function Parser: IParser;

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

class function TMCVec2.Parser: IParser;
begin
  Result := TParser.Create;
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

class function TMCVec3.Parser: IParser;
begin
  Result := TParser.Create;
end;

{ TMCVec3.TParser }

function TMCVec3.TParser.GetBlockPos: Boolean;
begin
  Result := FBlockPos;
end;

class function TMCVec3.TParser.GetResultName: string;
begin
  Result := '3-Component Vector';
end;

function TMCVec3.TParser.Parse: Boolean;
var
  Axis: TCoordAxis3;
  Parser: TMCVecValue.IParser;
  Local: Boolean;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  ParseResult := TMCVec3.Create;
  Parser := TMCVecValue.Parser;
  Parser.BlockPos := BlockPos;
  Parser.Parse(ParseResult.X, Info, False);
  Local := ParseResult.X.Mode = vmLocal;
  if not Parser.Success then
    Exit(False);
  for Axis := Succ(Low(TCoordAxis3)) to High(TCoordAxis3) do
  begin
    if not ReachedEnd and not First.IsWhitespace then
      raise EParseError.Create('Expected whitespace.');
    Advance;
    SkipWhitespace;
    Parser.Parse(ParseResult[Axis], Info, False);
    if not Parser.Success then
    begin
      Log(1, 'Expected %s-Coordinate.', [CoordAxisNames[Axis]], elFatal);
      Break;
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

procedure TMCVec3.TParser.SetBlockPos(const Value: Boolean);
begin
  FBlockPos := Value;
end;

{ TMCVecValue.TParser }

function TMCVecValue.TParser.GetBlockPos: Boolean;
begin
  Result := FBlockPos;
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

procedure TMCVecValue.TParser.SetBlockPos(const Value: Boolean);
begin
  FBlockPos := Value;
end;

{ TMCVec2.TParser }

function TMCVec2.TParser.GetChunkPos: Boolean;
begin
  Result := FChunkPos;
end;

class function TMCVec2.TParser.GetResultName: string;
begin
  Result := '2-Component Vector';
end;

function TMCVec2.TParser.Parse: Boolean;
var
  Parser: TMCVecValue.IParser;
  Local: Boolean;
  Marker: TLogMarker;
begin
  Marker := GetMarker;
  ParseResult := TMCVec2.Create;
  Parser := TMCVecValue.Parser;
  Parser.BlockPos := ChunkPos;
  Parser.Parse(ParseResult.X, Info, False);
  Local := ParseResult.X.Mode = vmLocal;
  Result := Parser.Success;
  if not Result then
    Exit;

  if not ReachedEnd and not First.IsWhitespace then
    raise EParseError.Create('Expected whitespace.');
  Advance;
  SkipWhitespace;
  Parser.Parse(ParseResult.Y, Info, False);
  if not Parser.Success then
    Log(1, 'Expected %s-Coordinate.', [CoordAxisNames[caY]], elFatal);
  
  if (ParseResult.Y.Mode = vmLocal) <> Local then
    Log(Marker, 'Local coordinates (^) cannot be mixed with non-local coordinates.');

  Result := True;
end;

procedure TMCVec2.TParser.SetChunkPos(const Value: Boolean);
begin
  FChunkPos := Value;
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

class function TMCVecValue.Parser: IParser;
begin
  Result := TParser.Create;
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

class function TMCSwizzle.Parser: IParser;
begin
  Result := TParser.Create;
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
  ParseResult := TMCSwizzle.Create;
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
