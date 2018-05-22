program ParserTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,

  Pengine.MC.NBT,
  Pengine.MC.EntitySelector,

  Pengine.DebugConsole,
  Pengine.Collections,
  Pengine.Parser,
  Pengine.EventHandling,
  Pengine.TimeManager;

type

  TIdentArray = class(TArray<string>)
  public
    class function ItemToString(AItem: string): string; override;

  end;

  TIdentArrayParser = class(TObjectParser<TIdentArray>)
  protected
    function Parse: Boolean; override;

  public
    class function GetResultName: string; override;

  end;

{ TIdentArray }

class function TIdentArray.ItemToString(AItem: string): string;
begin
  Result := AItem;
end;

{ TIdentArrayParser }

class function TIdentArrayParser.GetResultName: string;
begin
  Result := 'Array of Identifiers';
end;

function TIdentArrayParser.Parse: Boolean;
var
  IdentParser: TIdentifierParser;
begin
  if not Info.StartsWith('[') then
    Exit(False);

  Info.SkipWhitespace;

  SetParseResult(TIdentArray.Create);

  if Info.StartsWith(']') then
    Exit(True);

  while True do
  begin
    IdentParser := TIdentifierParser.Create(Info, True);
    try
      ParseResult.Add(IdentParser.ParseResult);
    finally
      IdentParser.Free;
    end;

    Info.SkipWhitespace;

    if Info.StartsWith(']') then
      Exit(True);

    if not Info.StartsWith(',') then
      raise EParseError.Create(Info, 'Expected "," or "]".');

    Info.SkipWhitespace;
  end;
end;

var
  Parser: TEntitySelector.TParser;
  S: string;
  I: Integer;
  ParserClass: TParserClass;
begin
  ReportMemoryLeaksOnShutdown := True;

  try

    while True do
    begin

      Readln(S);

      if S.Equals('exit') then
        Break;

      StartTimer;
      for I := 0 to 1000 do
      begin
        Parser.Free;
        Parser := TEntitySelector.TParser.Create(S);
      end;
      StopTimerAndOutput;

      if Parser.Success then
      begin
        Writeln('Result:   "', Parser.ParseResult.Format, '"');
        if Parser.HasLeftover then
          Writeln('Leftover: "', Parser.Leftover, '"');
      end
      else
      begin
        for I := 1 to Parser.ErrorLinePos - 1 do
          Write(' ');
        Writeln('^');
        Writeln('Error: ', Parser.ErrorMessage);
        Writeln('       at ', Parser.ErrorLine, '|', Parser.ErrorLinePos);
      end;

      for I := 0 to Parser.CharInfo.Count - 1 do
      begin
        Write(S[I + 1], ' ');
        for ParserClass in Parser.CharInfo.Parsers[I] do
          Write(ParserClass.GetResultName, ' | ');
        if Parser.CharInfo.ExtraData[I] <> 0 then
          Writeln(Parser.CharInfo.Parsers[I].Last.GetTokenName(Parser.CharInfo.ExtraData[I]))
        else
          Writeln('-');
      end;

      Parser.Free;
      Parser := nil;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

