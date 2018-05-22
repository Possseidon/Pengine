unit Pengine.MC.BrigadierParser;

interface

uses
  System.SysUtils,
  System.JSON,
  System.Math,
  System.RegularExpressions,

  Pengine.IntMaths,
  Pengine.Vector,
  Pengine.Parser,

  Pengine.MC.Brigadier;

type

  TBrigadierBool = class(TBrigadierArgumentParameter)
  private
    FValue: Boolean;

  public
    property Value: Boolean read FValue write FValue;

    function Format: string; override;

  end;

  TBrigadierBoolParser = class(TBrigadierParser<TBrigadierBool>)
  public const
    BoolStrings: array [Boolean] of string = ('false', 'true');

  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: string; override;

  end;

  TBrigadierInteger = class(TBrigadierArgumentParameter)
  private
    FValue: Integer;

  public
    property Value: Integer read FValue write FValue;

    function Format: string; override;

  end;

  TBrigadierIntegerProperties = class(TBrigadierParserProperties)
  private
    FBounds: TIntBounds1;

  public
    constructor Create(AProperties: TJSONObject); override;

    property Bounds: TIntBounds1 read FBounds;

  end;

  TBrigadierIntegerParser = class(TBrigadierParser<TBrigadierInteger, TBrigadierIntegerProperties>)
  protected
    function Parse: Boolean; override;

  public
    class function GetParserString: string; override;

  end;

implementation

{ TBrigadierBoolParser }

class function TBrigadierBoolParser.GetParserString: string;
begin
  Result := 'brigadier:bool';
end;

function TBrigadierBoolParser.Parse: Boolean;
var
  B: Boolean;
  Ident: string;
begin
  Ident := Info.ReadUntilWhitespace;
  for B := Low(Boolean) to High(Boolean) do
  begin
    if Ident = BoolStrings[B] then
    begin
      ParseResult.Value := B;
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TBrigadierBool }

function TBrigadierBool.Format: string;
begin
  Result := TBrigadierBoolParser.BoolStrings[Value];
end;

{ TBrigadierInteger }

function TBrigadierInteger.Format: string;
begin
  Result := IntToStr(Value);
end;

{ TBrigadierIntegerProperties }

constructor TBrigadierIntegerProperties.Create(AProperties: TJSONObject);
var
  Node: TJSONNumber;
begin
  inherited;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('min', Node) then
    FBounds.Low := Node.AsInt
  else
    FBounds.Low := Integer.MinValue;
  if (AProperties <> nil) and AProperties.TryGetValue<TJSONNumber>('max', Node) then
    FBounds.High := Node.AsInt
  else
    FBounds.High := Integer.MaxValue;
end;

{ TBrigadierIntegerParser }

class function TBrigadierIntegerParser.GetParserString: string;
begin
  Result := 'brigadier:integer';
end;

function TBrigadierIntegerParser.Parse: Boolean;
var
  Value: Integer;
begin
  if TryStrToInt(Info.ReadUntilWhitespace, Value) then
  begin
    if Value in Properties.Bounds then
      ParseResult.Value := Value
    else
      raise EParseError.CreateFmt('Number must be in range %s.', [Properties.Bounds.ToString]);
  end;
  Result := True;
end;

initialization

TBrigadierBoolParser.RegisterClass;
TBrigadierIntegerParser.RegisterClass;

end.
