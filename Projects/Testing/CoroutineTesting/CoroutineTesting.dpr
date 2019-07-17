program CoroutineTesting;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,

  Winapi.Windows,

  Pengine.TimeManager,
  Pengine.ICollections,
  Pengine.Coroutines,
  Pengine.DebugConsole;
                       {
type

  TMyCoroutine = class(TCoroutine)
  private
    FOther: TCoroutine;
    FValue: Integer;

  protected
    procedure Execute; override;

  end;

procedure MyConsumingGenerator(Consume: TConsume<Integer>; Generate: TGenerate<string>);
var
  First, Second: Integer;
begin
  while True do
  begin
    if not Consume(First) then
      Break;
    if not Generate(First.ToString) then
      Break;
    if not Consume(Second) then
      Break;
    if not Generate(' * ' + Second.ToString) then
      Break;
    if not Generate(' = ' + (First * Second).ToString + sLineBreak) then
      Break;
  end;
end;

const
  TestCount = 100000;
                                                  }
var
  Coroutine: ICoroutine;{
  Generator: IGenerator<Integer>;
  Consumer: IConsumer<Integer>;
  ConsumingGenerator: IConsumingGenerator<Integer, string>;
                }
{ TMyCoroutine }
                 {
procedure TMyCoroutine.Execute;
begin
  // TODO: Think about this, does symmetric switching actually make sense? Should this raise exceptions?
  while FOther.Resume do
  begin
    Writeln(Format('%p: %d', [Pointer(Self), FValue]));
    Inc(FValue);
  end;
end;
           }
procedure Test;
begin
  try
    raise Exception.Create('Fehlermeldung');
  finally
    Writeln('iro iro');
  end;
end;

begin

  try
    ReportMemoryLeaksOnShutdown := True;

    // Test;
    Writeln('abc');

    Coroutine := TSimpleCoroutine.Create(
      procedure(Yield: TYield)
      begin
        Writeln('hello');
        Yield;
        Writeln('world');
      end
    );

    Coroutine.Resume;
    Coroutine.Resume;

    {
    Generator := TSimpleGenerator<Integer>.Create(
      procedure(Generate: TGenerate<Integer>)
      begin
        // raise Exception.Create('Fehlermeldung');
        Writeln('this?');
        for var I in IntRange(10) do
        begin
          Writeln('Yielding ', I);
          if not Generate(Sqr(I)) then
            Break;
        end;
        Writeln('Generator says Goodbye!');
      end
      );

    for var S in Generator do
      Writeln(S);

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

    ConsumingGenerator := TSimpleConsumingGenerator<Integer, string>.Create(MyConsumingGenerator);

    for var I in ConsumingGenerator.Send(IntRange(9)) do
      Write(I);

    for var I in ConsumingGenerator.Send(IntRange(10)) do
      Write(I);
              }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
