program RandomTesting;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Diagnostics,

  Pengine.Random,
  System.Math;

const
  Count = 100000000;

var
  Rand: TRandom;
  I: Integer;
  Stopwatch: TStopwatch;
begin
  try

    Stopwatch := TStopwatch.StartNew;
    Rand := TRandom.Create;
    for I := 1 to Count do
      Rand.NextChar('A', 'Z');
    Writeln(Stopwatch.Elapsed.ToString);

    Stopwatch := TStopwatch.StartNew;
    for I := 1 to Count do
      Random(26);
    Writeln(Stopwatch.Elapsed.ToString);

    Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

