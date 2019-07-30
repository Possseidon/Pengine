unit Pengine.Random;

interface

uses
  System.SysUtils,

  System.Diagnostics;

type

  ERandomError = class(Exception);

  TRandom = record
  private const

    Two2Neg32Single: Single = ((1.0 / $10000) / $10000);
    Two2Neg32Double: Double = ((1.0 / $10000) / $10000);

  private
    FState: Cardinal;

    class function SplitMix(AValue: UInt64): UInt64; static; inline;
    procedure Step; inline;

  public
    class function Create: TRandom; static; inline;
    class function FromSeed(ASeed: Int64): TRandom; static; inline;

    function NextChar: Char; overload; inline;
    function NextChar(ARange: Char): Char; overload; inline;
    function NextChar(ALow, AHigh: Char): Char; overload; inline;

    function NextShortInt: ShortInt; overload; inline;
    function NextShortInt(ARange: ShortInt): ShortInt; overload; inline;
    function NextShortInt(ALow, AHigh: ShortInt): ShortInt; overload; inline;

    function NextByte: Byte; overload; inline;
    function NextByte(ARange: Byte): Byte; overload; inline;
    function NextByte(ALow, AHigh: Byte): Byte; overload; inline;

    function NextSmallInt: SmallInt; overload; inline;
    function NextSmallInt(ARange: SmallInt): SmallInt; overload; inline;
    function NextSmallInt(ALow, AHigh: SmallInt): SmallInt; overload; inline;

    function NextWord: Word; overload; inline;
    function NextWord(ARange: Word): Word; overload; inline;
    function NextWord(ALow, AHigh: Word): Word; overload; inline;

    function NextInteger: Integer; overload; inline;
    function NextInteger(ARange: Integer): Integer; overload; inline;
    function NextInteger(ALow, AHigh: Integer): Integer; overload; inline;

    function NextCardinal: Cardinal; overload; inline;
    function NextCardinal(ARange: Cardinal): Cardinal; overload; inline;
    function NextCardinal(ALow, AHigh: Cardinal): Cardinal; overload; inline;

    function NextSingle: Single; overload; inline;
    function NextDouble: Double; overload; inline;

  end;

implementation

{ TRandom }

class function TRandom.SplitMix(AValue: UInt64): UInt64;
begin
  Result := AValue + $9E3779B97F4A7C15;
  Result := (Result xor (Result shr 30)) * $BF58476D1CE4E5B9;
  Result := (Result xor (Result shr 27)) * $94D049BB133111EB;
  Result := Result xor (Result shr 31);
end;

procedure TRandom.Step;
begin
  FState := FState * $08088405 + 1;
end;

class function TRandom.Create: TRandom;
begin
  Result := FromSeed(TStopwatch.GetTimeStamp);
end;

class function TRandom.FromSeed(ASeed: Int64): TRandom;
var
  Split: UInt64;
begin
  Split := SplitMix(ASeed);
  Result.FState := Cardinal(Split) xor Cardinal(Split shr 32);
end;

function TRandom.NextChar: Char;
begin
  Step;
  Result := Char(FState);
end;

function TRandom.NextChar(ARange: Char): Char;
begin
  Result := Char(NextCardinal(Ord(ARange)));
end;

function TRandom.NextChar(ALow, AHigh: Char): Char;
begin
  Result := Char(NextCardinal(Ord(ALow), Ord(AHigh)));
end;

function TRandom.NextShortInt: ShortInt;
begin
  Step;
  Result := ShortInt(FState);
end;

function TRandom.NextShortInt(ARange: ShortInt): ShortInt;
begin
  Result := ShortInt(NextCardinal(ARange));
end;

function TRandom.NextShortInt(ALow, AHigh: ShortInt): ShortInt;
begin
  Result := ShortInt(NextInteger(ALow, AHigh));
end;

function TRandom.NextByte: Byte;
begin
  Step;
  Result := Byte(FState);
end;

function TRandom.NextByte(ARange: Byte): Byte;
begin
  Result := Byte(NextCardinal(ARange));
end;

function TRandom.NextByte(ALow, AHigh: Byte): Byte;
begin
  Result := Byte(NextCardinal(ALow, AHigh));
end;

function TRandom.NextSmallInt: SmallInt;
begin
  Step;
  Result := SmallInt(FState);
end;

function TRandom.NextSmallInt(ARange: SmallInt): SmallInt;
begin
  Result := SmallInt(NextCardinal(ARange));
end;

function TRandom.NextSmallInt(ALow, AHigh: SmallInt): SmallInt;
begin
  Result := SmallInt(NextInteger(ALow, AHigh));
end;

function TRandom.NextWord: Word;
begin
  Step;
  Result := Word(FState);
end;

function TRandom.NextWord(ARange: Word): Word;
begin
  Result := Word(NextCardinal(ARange));
end;

function TRandom.NextWord(ALow, AHigh: Word): Word;
begin
  Result := Word(NextCardinal(ALow, AHigh));
end;

function TRandom.NextInteger: Integer;
begin
  Step;
  Result := Integer(FState);
end;

function TRandom.NextInteger(ARange: Integer): Integer;
begin
  Result := NextCardinal(ARange);
end;

function TRandom.NextInteger(ALow, AHigh: Integer): Integer;
begin
  Result := ALow + NextInteger(AHigh - ALow + 1);
end;

function TRandom.NextCardinal: Cardinal;
begin
  Step;
  Result := FState;
end;

function TRandom.NextCardinal(ARange: Cardinal): Cardinal;
begin
  Step;
  Result := (UInt64(FState) * ARange) shr 32;
end;

function TRandom.NextCardinal(ALow, AHigh: Cardinal): Cardinal;
begin
  Result := ALow + NextCardinal(AHigh - ALow + 1);
end;

function TRandom.NextSingle: Single;
begin
  Step;
  Result := FState * Two2Neg32Single;
end;

function TRandom.NextDouble: Double;
begin
  Step;
  Result := FState * Two2Neg32Double;
end;

end.
