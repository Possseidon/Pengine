unit Pengine.MC.General;

interface

uses
  System.SysUtils,

  Pengine.Parser;

type

  TStringParser = class(TParser<string>)
  public const

    TokenQuote = 1;
    TokenContent = 2;
    TokenBackslash = 3;
    TokenEscaped = 4;

    TokenNames: array [TokenQuote .. TokenEscaped] of string = (
      'Quotes',
      'Content',
      'Backslash',
      'Escaped'
      );

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;
    class function GetTokenCount: Integer; override;
    class function GetTokenName(AIndex: Integer): string; override;

  end;

  TStringOrIdentParser = class(TParser<string>)
  private
    FIsIdent: Boolean;

  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;

    property IsIdent: Boolean read FIsIdent;

  end;

const

  IdentChars = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '.', '+', '-'];

  NamespaceChars = IdentChars - ['+']; // wtf

  NamespacePathChars = IdentChars + [':', '/'];

  IdentCharsText = 'letters, numbers and "_ . + -"';

  NamespaceCharsText = 'letters, numbers and "_ . -"';

  UsernameMaxLength = 16;

function DblQuoted(AText: string): string;
function DblUnquoted(AText: string): string;

implementation

function DblQuoted(AText: string): string;
var
  C: Char;
begin
  Result := '"';
  for C in AText do
  begin
    if CharInSet(C, ['\', '"']) then
      Result := Result + '\';
    Result := Result + C;
  end;
  Result := Result + '"';
end;

function DblUnquoted(AText: string): string;
var
  I: Integer;
begin
  if not AText.StartsWith('"') or not AText.EndsWith('"') then
    raise EArgumentException.Create('The string is not quoted.');
  Result := '';
  I := 2;
  while I < AText.Length do
  begin
    if AText[I] = '\' then
    begin
      Inc(I);
      if I = AText.Length then
        raise EArgumentException.Create('The string is not terminated.');
    end;
    Result := Result + AText[I];
    Inc(I);
  end;
end;

{ TIdentifierParser }
{
class function TIdentifierParser.GetResultName: string;
begin
  Result := 'Identifier';
end;

function TIdentifierParser.Parse: Boolean;
const
  Alpha = ['a' .. 'z', 'A' .. 'Z', '_'];
  AlphaNum = Alpha + ['0' .. '9'];
begin
  if not CharInSet(Info.First, Alpha) then
    Exit(False);
  SetParseResult(Info.First);
  Info.Advance;
  FParseResult := FParseResult + Info.ReadWhile(AlphaNum);
  Result := True;
end;
}

{ TStringOrIdentParser }

class function TStringOrIdentParser.GetResultName: string;
begin
  Result := 'String or Identifier';
end;

function TStringOrIdentParser.Parse: Boolean;
var
  Text: string;
begin
  FIsIdent := not Info.StartsWith('"', False);
  if IsIdent then
    Text := ReadWhile(IdentChars)
  else
    Text := TStringParser.Require(Info);
  SetParseResult(Text);
  Result := True;
end;

{ TStringParser }

class function TStringParser.GetResultName: string;
begin
  Result := 'Quoted String';
end;

class function TStringParser.GetTokenCount: Integer;
begin
  Result := Length(TokenNames);
end;

class function TStringParser.GetTokenName(AIndex: Integer): string;
begin
  Result := TokenNames[AIndex];
end;

function TStringParser.Parse: Boolean;
var
  C: Char;
begin
  Token := TokenQuote;
  if not StartsWith('"') then
    Exit(False);
  Token := TokenContent;
  for C in Info do
  begin
    if Token = TokenEscaped then
      Token := TokenContent;
    case C of
      '"':
        begin
          Token := TokenQuote;
          Advance;
          Exit(True);
        end;
      '\':
        begin
          Token := TokenBackslash;
          Advance;
          if not CharInSet(First, ['"', '\']) then
            Log(1, 'Only \" and \\ are valid escape sequences.');
          Token := TokenEscaped;
        end;
    end;
    SetParseResult(ParseResult + First);
  end;
  raise EParseError.Create('Found unterminated string.');
end;

end.
