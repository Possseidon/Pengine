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
  Builder: TStringBuilder;
  Escaped: Char;
begin
  Token := TokenQuote;
  if not StartsWith('"') then
    Exit(False);

  Builder := TStringBuilder.Create;
  try
    while not StartsWith('"') do
    begin
      if ReachedEnd then
        raise EParseError.Create('Found unterminated string.');

      Token := TokenBackslash;
      if StartsWith('\') then
      begin
        Token := TokenEscaped;
        Escaped := First;
        Advance;
        case Escaped of
          '\', '"':
            Builder.Append(Escaped);
        else
          Log(-2, 'Only \" and \\ are valid escape sequences.', elFatal);
        end;
      end
      else
      begin
        Token := TokenContent;
        Builder.Append(First);
        Advance;
      end;
      Token := TokenQuote;
    end;

    SetParseResult(Builder.ToString);
    Result := True;

  finally
    Builder.Free;

  end;
end;

{
begin
  Token := TokenQuote;
  if not StartsWith('"') then
    Exit(False);
  Token := TokenContent;
  while not StartsWith('"') do
  begin
    if ReachedEnd then
      raise EParseError.Create('Found unterminated string.');

    if Token = TokenEscaped then
      Token := TokenContent;

    case First of
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
 }
end.
