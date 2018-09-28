unit LightThemePreset;

interface

uses
  Vcl.Graphics,

  Pengine.Parser,

  Pengine.MC.Brigadier,
  Pengine.MC.NBT,
  Pengine.MC.EntitySelector,
  Pengine.MC.General,

  FunctionTheme;

type

  TLightTheme = class(TFunctionTheme.TPreset)
  public
    class function GetName: string; override;
    class procedure Apply(ATheme: TFunctionTheme); override;

  end;

implementation

{ TLightTheme }

class function TLightTheme.GetName: string;
begin
  Result := 'Light Theme';
end;

class procedure TLightTheme.Apply(ATheme: TFunctionTheme);
begin
  with ATheme do
  begin
    SetDefault(clBlack, clWhite);
    SetComment(clGreen, [fsBold]);
    SetError(clRed, [fsItalic]);

    // Commands
    SetParser(TBrigadierCommandParser, TBrigadierCommandParser.TokenMainCommand, $DF1F7F, [fsBold]);
    SetParser(TBrigadierCommandParser, TBrigadierCommandParser.TokenSubCommand, $DF1F1F);
    SetParser(TBrigadierCommandParser, TBrigadierCommandParser.TokenComment, $007F00, [fsItalic]);
    SetParser(TBrigadierCommandParser, TBrigadierCommandParser.TokenSlash, $DF1F1F, [fsBold]);

    // String
    SetParser(TStringParser, TStringParser.TokenQuote, $FF3F00, [fsBold]);
    SetParser(TStringParser, TStringParser.TokenContent, $FF7F00);
    SetParser(TStringParser, TStringParser.TokenBackslash, $AF3F3F, [fsBold]);
    SetParser(TStringParser, TStringParser.TokenEscaped, $AF4F4F, [fsBold]);

    // Entity-Selector
    SetParser(TEntitySelector.TParser, TEntitySelector.TParser.TokenPrefix, $2FCF2F, [fsBold]);
    SetParser(TEntitySelector.TParser, TEntitySelector.TParser.TokenVariable, $2F2FCF);
    SetParser(TEntitySelector.TParser, TEntitySelector.TParser.TokenBrackets, $2FCF2F, [fsBold]);
    SetParser(TEntitySelector.TParser, TEntitySelector.TParser.TokenComma, $2FCF2F, [fsBold]);

    SetParser(TEntitySelector.TIntRangeParser, TEntitySelector.TIntRangeParser.TokenValue, $);
    SetParser(TEntitySelector.TIntRangeParser, TEntitySelector.TIntRangeParser.TokenSplitter, $);

    SetParser(TEntitySelector.TRangeParser, TEntitySelector.TRangeParser.TokenValue, $);
    SetParser(TEntitySelector.TRangeParser, TEntitySelector.TRangeParser.TokenSplitter, $);

    SetParser(TEntitySelector.TOption.TParser, TEntitySelector.TOption.TParser.TokenOption, $);
    SetParser(TEntitySelector.TOption.TParser, TEntitySelector.TOption.TParser.TokenEquals, $);
    SetParser(TEntitySelector.TOption.TParser, TEntitySelector.TOption.TParser.TokenInvert, $);

    SetParser(TEntitySelector.TOptionInteger.TParser, TParser.TokenNone, $);

    SetParser(TEntitySelector.TOptionFloat.TParser, TParser.TokenNone, $);

    SetParser(TEntitySelector.TOptionIdentifier.TParser, TParser.TokenNone, $);

    SetParser(TEntitySelector.TOptionSort.TParser, TParser.TokenNone, $);

    SetParser(TEntitySelector.TOptionScores.TParser, TEntitySelector.TOptionScores.TParser.TokenBrackets, $);
    SetParser(TEntitySelector.TOptionScores.TParser, TEntitySelector.TOptionScores.TParser.TokenName, $);
    SetParser(TEntitySelector.TOptionScores.TParser, TEntitySelector.TOptionScores.TParser.TokenEquals, $);
    SetParser(TEntitySelector.TOptionScores.TParser, TEntitySelector.TOptionScores.TParser.TokenComma, $);

    SetParser(TEntitySelector.TOptionType.TParser, TParser.TokenNone, $);

    SetParser(TEntitySelector.TOptionAdvancements.TParser, TEntitySelector.TOptionAdvancements.TParser.TokenBrackets, $);
    SetParser(TEntitySelector.TOptionAdvancements.TParser, TEntitySelector.TOptionAdvancements.TParser.TokenName, $);
    SetParser(TEntitySelector.TOptionAdvancements.TParser, TEntitySelector.TOptionAdvancements.TParser.TokenEquals, $);
    SetParser(TEntitySelector.TOptionAdvancements.TParser, TEntitySelector.TOptionAdvancements.TParser.TokenComma, $);
    SetParser(TEntitySelector.TOptionAdvancements.TParser, TEntitySelector.TOptionAdvancements.TParser.TokenBoolean, $);

    SetParser(TEntitySelector.TOptionGamemode.TParser, TParser.TokenNone, $);

    // NBT
    SetParser(TNBTParserCompound, TNBTParserCompound.TokenBracket, $DF3F3F, [fsBold]);
    SetParser(TNBTParserCompound, TNBTParserCompound.TokenTag, $DF3F3F);
    SetParser(TNBTParserCompound, TNBTParserCompound.TokenColon, $DF3F3F, [fsBold]);
    SetParser(TNBTParserCompound, TNBTParserCompound.TokenComma, $DF3F3F, [fsBold]);

    SetParser(TNBTParserListOrArray, TNBTParserListOrArray.TokenBracket, $3FDF3F);
    SetParser(TNBTParserListOrArray, TNBTParserListOrArray.TokenComma, $3FDF3F);

    SetParser(TNBTParserNumber, TNBTParserNumber.TokenNumber, $004FDF);
    SetParser(TNBTParserNumber, TNBTParserNumber.TokenSuffix, $0000DF, [fsBold]);

    // ...
  end;
end;

end.
