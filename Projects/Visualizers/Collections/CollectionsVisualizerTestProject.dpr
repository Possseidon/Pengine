program CollectionsVisualizerTestProject;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Vcl.Graphics,

  Pengine.Collections;

type

  TIntArray = TArray<Integer>;

var
  A: TIntArray;
begin
  try
    A := TIntArray.Create;
    A.Add(42);
    Writeln(A[0]);
    A.Free;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

