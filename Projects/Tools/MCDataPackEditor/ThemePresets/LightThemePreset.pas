unit LightThemePreset;

interface

uses
  Vcl.Graphics,

  Pengine.Parser,

  Pengine.MC.Brigadier,
  Pengine.MC.BrigadierParser,
  Pengine.MC.NBT,
  Pengine.MC.EntitySelector,
  Pengine.MC.General,

  FunctionTheme,
  Pengine.JSON;

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
    CurrentLineColor := $00E6FFFA;
    //SetDefault(clWhite, $2F1F0F);
    //CurrentLineColor := $4A3036;
    SetError(clRed, clWhite, [fsItalic]);

    // Command
    Text[TBrigadierCommandParser, TBrigadierCommandParser.TokenMainCommand].SetFg($DF1F7F, [fsBold]);
    Text[TBrigadierCommandParser, TBrigadierCommandParser.TokenSubCommand].SetFg($DF1F1F);
    Text[TBrigadierCommandParser, TBrigadierCommandParser.TokenComment].SetFg($007F00, [fsItalic]);
    Text[TBrigadierCommandParser, TBrigadierCommandParser.TokenSlash].SetFg($DF1F1F, [fsBold]);

    // Basic Types
    Text[TBrigadierBoolParser, TBrigadierBoolParser.TokenNone].SetFg($BF0000, [fsBold]);
    Text[TBrigadierIntegerParser, TBrigadierIntegerParser.TokenNone].SetFg($BF0000);
    Text[TBrigadierFloatParser, TBrigadierFloatParser.TokenNone].SetFg($BF0000);
    Text[TBrigadierDoubleParser, TBrigadierDoubleParser.TokenNone].SetFg($BF0000);
    Text[TBrigadierStringParser, TBrigadierStringParser.TokenNone].SetFg($BF0000);

    // NBT
    Text[TNBTCompound.TParser, TNBTCompound.TParser.TokenBracket].SetFg($DF3F3F);
    Text[TNBTCompound.TParser, TNBTCompound.TParser.TokenColon].SetFg($DF3F3F);
    Text[TNBTCompound.TParser, TNBTCompound.TParser.TokenComma].SetFg($DF3F3F);

    Text[TNBTListOrArrayParser, TNBTListOrArrayParser.TokenBracket].SetFg($3FDF3F);
    Text[TNBTListOrArrayParser, TNBTListOrArrayParser.TokenComma].SetFg($3FDF3F);
    Text[TNBTListOrArrayParser, TNBTListOrArrayParser.TokenArrayType].SetFg($AF3F00, [fsBold]);
    Text[TNBTListOrArrayParser, TNBTListOrArrayParser.TokenArraySeperator].SetFg($3FDF3F);

    Text[TNBTString.TStringOrIdentParser, TNBTString.TStringOrIdentParser.TokenNone].SetFg($006FEF);
    Text[TNBTString.TStringParser, TNBTString.TStringParser.TokenQuotes].SetFg($AF0000);
    Text[TNBTString.TStringParser, TNBTString.TStringParser.TokenContent].SetFg($FF3F3F);
    Text[TNBTString.TStringParser, TNBTString.TStringParser.TokenBackslash].SetFg($FF8F3F);
    Text[TNBTString.TStringParser, TNBTString.TStringParser.TokenEscaped].SetFg($FF8F8F);

    Text[TNBTNumberParser, TNBTNumberParser.TokenNumber].SetFg($004FDF);
    Text[TNBTNumberParser, TNBTNumberParser.TokenSuffix].SetFg($0000DF);

    Text[TNBTPath.TParser, TNBTPath.TParser.TokenKey].SetFg($006FEF);
    Text[TNBTPath.TParser, TNBTPath.TParser.TokenBrackets].SetFg($3FDF3F);
    Text[TNBTPath.TParser, TNBTPath.TParser.TokenIndex].SetFg($003FEF);
    Text[TNBTPath.TParser, TNBTPath.TParser.TokenDot].SetFg($3FDF3F);

    // Text
    Text[TBrigadierMessageParser, TBrigadierMessageParser.TokenNone].SetFg($DF1F1F);
         {
    Text[TBrigadierComponentParser, TBrigadierComponentParser.TokenNone].SetFg();
    // Text[TJValue.TParser, TJValue.TParser.TokenNone].SetFg();
    Text[TJObject.TParser, TJObject.TParser.TokenNone].SetFg();
    Text[TJArray.TParser, TJArray.TParser.TokenNone].SetFg();
    Text[TJString.TParser, TJString.TParser.TokenNone].SetFg();
    Text[TJNumber.TParser, TJNumber.TParser.TokenNone].SetFg();
    Text[TJBool.TParser, TJBool.TParser.TokenNone].SetFg();
    Text[TJNull.TParser, TJNull.TParser.TokenNone].SetFg();
         }
  end;
end;

end.
