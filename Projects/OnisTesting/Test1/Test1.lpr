program Test1;

{$R *.res}

uses
  SysUtils;

type

  TMyEnum = (meBacon, meOtherBacon, meCount);
  TMyEnums = set of TMyEnum;

  { TMyClass }

  TMyClass = class(TObject)
  private
    FX, FY, FZ: Integer;
    procedure SetX(const Value: Integer);

  protected
    function calcSthStupid(A,B: Integer): string;
  public
    constructor Create;
    procedure DoSthStupid;
    property X: Integer read FX write SetX ;
  end;

  { TMyOtherClass }

  TMyOtherClass = class(TMyClass)
  public
    constructor Create;
  end;

var
  B , C: Integer;
  A: Double;
  O, P: TMyClass;

constructor TMyOtherClass.Create;
begin
  FX := 5;
  FY := 512;
  FZ := 0;
end;

{ TMyClass }

constructor TMyClass.Create;
begin
  FX := 42;
  FY := 255;
  FZ := -1;
end;

procedure TMyClass.DoSthStupid;
begin
  WriteLn(calcSthStupid(FX,FY-FZ));
end;

procedure TMyClass.SetX(const Value: Integer);
begin
  if FX = Value then
    Exit;
  FX := Value;
end;

function TMyClass.calcSthStupid(A, B: Integer): string;
begin
  Result := Format('%d Bacon %d',[A,B]);
end;

begin
  GlobalSkipIfNoLeaks := True;
  Write('Neger''!'#10);
  A := 42;
  ReadLn(B);
  Write(Format('%f Baum %d'#10,[A,B]));
  if B = 1 then
  begin
    WriteLn('I like ones! Hmmmmmmmmm');
  end;
  for C := 0 to 42 do
  begin
    WriteLn('42x42!');
  end;
  while B <> 42 do
  begin
    ReadLn(B);
  end;
  WriteLn('You did it!');

  O := TMyClass.Create;
  O.DoSthStupid;
  P := TMyOtherClass.Create;
  P.DoSthStupid;

  O.Free;
  P.Free;

  ReadLn;
end.

