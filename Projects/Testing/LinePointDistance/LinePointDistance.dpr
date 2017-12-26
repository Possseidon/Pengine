program LinePointDistance;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Pengine.Vector,
  Pengine.Collections,
  Pengine.TimeManager;

const
  Count = 10000000;

var
  L: TArray<TLine3>;
  A: TArray<TVector3>;
  I: Integer;

begin
  try

    Writeln('- Line to Point distance performance test -');
    Writeln('Calling it ', Count, ' times.');
    Writeln;
  
    RandSeed := 42;

    L := TArray<TLine3>.Create;
    A := TArray<TVector3>.Create;

    L.Capacity := Count;
    A.Capacity := Count;

    Writeln('Storing values in arrays...');
    
    for I := 0 to Count - 1 do
    begin
      L.Add(Line3(TVector3.Random, TVector3.Random));
      A.Add(TVector3.Random);
    end;
    Writeln;

    StartTimer;
    for I := 0 to Count - 1 do
      L[I].Height(A[I]);
    StopTimerAndOutput;

    Readln;

    L.Free;
    A.Free;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

