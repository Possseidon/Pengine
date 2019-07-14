program CoroutineTesting;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,

  Pengine.ICollections,
  Pengine.Coroutines,
  Pengine.DebugConsole;

var
  Coroutine: ICoroutine;
  Generator: IGenerator<Integer>;
  Consumer: IConsumer<Integer>;
  ConsumingGenerator: IConsumingGenerator<Integer, Integer>;

begin

  try
    ReportMemoryLeaksOnShutdown := True;

    Coroutine := TSimpleCoroutine.Create(
      procedure(Yield: TYield)
      begin
        repeat
          Writeln('Coroutine running!');
        until not Yield;
      end
      );

    for var I := 0 to 4 do
      Coroutine.Resume;

    Generator := TSimpleGenerator<Integer>.Create(
      procedure(Generate: TGenerate<Integer>)
      begin
        for var I in IntRange(10) do
        begin
          Writeln('Yielding ', I);
          if not Generate(Sqr(I)) then
            Break;
        end;
        Writeln('Generator says Goodbye!');
      end
      );

    Consumer := TSimpleConsumer<Integer>.Create(
      procedure(Consume: TConsume<Integer>)
      var
        Value: Integer;
        I: Integer;
      begin
        for I := 0 to 11 do
        begin
          if not Consume(Value) then
            Break;
          Writeln('Consumed: ', Value);
        end;
        Writeln('Consumer says Goodbye!');
      end
      );

    ConsumingGenerator := TSimpleConsumingGenerator<Integer, Integer>.Create(
      procedure(Consume: TConsume<Integer>; Generate: TGenerate<Integer>)
      var
        First, Second: Integer;
      begin
        while True do
        begin
          if not Consume(First) then
            Break;
          Writeln('Consumed first: ', First);
          if not Consume(Second) then
            Break;
          Writeln('Consumed second: ', Second);
          Writeln('Generating result...');
          if not Generate(First + Second) then
            Break;
        end;
      end
      );

    for var I in ConsumingGenerator.Send(IntRange(10)) do
      Writeln('Generated: ', I);
    {
      if Consumer.Send(IntRange(5)) then
      Writeln('Consumed all')
      else
      Writeln('Could not consume all');
    }
    // for var S in Gen.Iterate.Take(5) do
    // Writeln(S);

    {
      Writeln('Rest:');
      for var S in Gen do
      Writeln(S);
    }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
