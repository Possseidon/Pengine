program HashMap;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Pengine.Hasher,
  Pengine.DebugConsole,
  Pengine.Vector,
  Pengine.TimeManager,
  Pengine.IntMaths,
  Pengine.Collections in '..\..\..\Libraries\Utility\Pengine.Collections.pas';

function ComparePair(L, R: TPair<Integer, Integer>): Boolean;
begin
  Result := L.Key < R.Key;
end;

function NotZero(Value: Integer): Boolean;
begin
  Result := Value <> 0;
end;

var
  Map: TIntVector3Map<TIntVector3>;
  Text: string;
  Pair: TIntVector3Map<TIntVector3>.TPair;
  IPair: TPair<Integer, Integer>;
  I, Count: Integer;
  Buckets: TIntArray;
  BucketsNoZero: TIntArray;
  BucketCounts: TIntMap<Integer>;
  IPairs: TArray<TPair<Integer, Integer>>;

begin

  Map := nil;
  BucketCounts := nil;
  IPairs := nil;
  Buckets := nil;
  BucketsNoZero := nil;

  ReportMemoryLeaksOnShutdown := True;
  try
    try

      FormatSettings := TFormatSettings.Invariant;

      StartTimer;
      
      Map := TIntVector3Map<TIntVector3>.Create(False);

      Map.Buckets := 10000000;

      for I := 0 to 9999999 do
        Map[(TVector3.Random * 10000).Floor] := (TVector3.Random * 100).Floor;

      Buckets := Map.BucketCounts;

      BucketCounts := TIntMap<Integer>.Create;

      for I in Buckets do
      begin
        if BucketCounts.Get(I, Count) then
          BucketCounts[I] := Count + 1
        else
          BucketCounts[I] := 1;
      end;

      BucketsNoZero := Buckets.FindAsIntArray(NotZero);
      BucketsNoZero := BucketsNoZero.Copy;

      Writeln('Average: ' + BucketsNoZero.Average.ToString);
      Writeln('Bounds: ' + Buckets.Bounds);
      Writeln('Buckets: ', Map.Buckets);
      Writeln('Count:   ', Map.Count);

      IPairs := TArray<TPair<Integer, Integer>>.Create;
      for IPair in BucketCounts do
        IPairs.Add(IPair);

      IPairs.Sort(ComparePair);

      for IPair in IPairs do
        Writeln(IPair.Key, ' = ', IPair.Value);

      StopTimerAndOutput;

      while True do
      begin
        try
          Readln(Text);
          if Text = 'exit' then
            Break;

          if Text = 'pairs' then
          begin
            for Pair in Map do
              Writeln(Pair.Key.ToString, ' = ', Pair.Value.ToString, ' Hash: ', Map.GetCappedHash(Pair.Key));
            Continue;
          end;

        except
          on E: Exception do
            Writeln(E.ClassName, ': ', E.Message);
        end;

      end;

    finally
      Map.Free;
      Buckets.Free;
      BucketCounts.Free;
      IPairs.Free;
      BucketsNoZero.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
