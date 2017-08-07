program VectorGeometryTesting;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, IntegerMaths, VectorGeometry, TimeManager, Math;

var
  Plane1, Plane2: TPlane3;
  Line: TLine3;
  Coord: TVector2;

begin
  ReportMemoryLeaksOnShutdown := True;

  try
    Plane1.Create(Vec3(0, 0, 0), Vec3(0, 0, 1000), Vec3(1000, 0, 0));
    Plane2.Create(Vec3(0, 0, 0), Vec3(0, 1000, 0), Vec3(0, 0, 1000));

    if Plane1.Intsec(Plane2, Line) then
      Writeln('Intersection: ' + Line.Tail + ' -> ' + Line.Head.Normalize)
    else
      Writeln('None or infinite intersections!');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Readln;
end.
