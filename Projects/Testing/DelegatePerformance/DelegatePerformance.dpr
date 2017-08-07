program DelegatePerformance;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Delegates,
  TimeManager;

type

  TBigArray = array [1 .. 512] of Integer;

  TMyClass = class
  private
    procedure A(Sender: TMyClass; X, Y: Integer);
    procedure B(Sender: TMyClass; X, Y: Integer);

  public
    /// <summary>Call(Sender: TMyClass; Data: array [1 .. 512] of Integer)</summary>
    MyDelegate: TDelegate<TMyClass, TBigArray>;

  end;

procedure TMyClass.A(Sender: TMyClass; X, Y: Integer);
begin
  Writeln(Format('A got called from %s at [%d, %d]', [Sender.ToString, X, Y]));
end;

procedure TMyClass.B(Sender: TMyClass; X, Y: Integer);
begin
  Writeln(Format('B got called from %s at [%d, %d]', [Sender.ToString, X, Y]));
end;

procedure StaticProc(Sender: TMyClass; X, Y: Integer);
begin
  Writeln(Format('Static called from %s at [%d, %d]', [Sender.ToString, X, Y]));
end;

procedure StaticProc2(Sender: TMyClass; X, Y: Integer);
begin
  Writeln(Format('Static2 called from %s at [%d, %d]', [Sender.ToString, X, Y]));
end;

var
  MyObject: TMyClass;
  I: Integer;
  Data: TBigArray;
begin
  try
    MyObject := TMyClass.Create;

    // MyObject.MyDelegate.Add(StaticProc);
    // MyObject.MyDelegate.Add(StaticProc2);
    // MyObject.MyDelegate.Add(MyObject.A);
    // MyObject.MyDelegate.Add(MyObject.B);

    StartTimer;

    for I := 0 to 99999999 do
      MyObject.MyDelegate.Call(MyObject, Data);

    StopTimerAndOutput;

    MyObject.Free;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Readln;
end.

