program LineRiderGen;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.IOUtils,

  Pengine.JSON,
  Pengine.Vector,

  LineRider in 'LineRider.pas';

var
  JTrack: TJObject;
  LineRider: TLineRider;
  I: Integer;
  R1, R2, S1, S2: Single;
  Line: TLineRider.TLine;

begin
  try
    LineRider := TLineRider.Create;
    LineRider.Creator := 'Possseidon';
    LineRider.TrackLabel := 'Circle';
    LineRider.Description := 'A perfect circle.';
    LineRider.Version := '6.2';
    for I := 0 to 35999 do
    begin
      Line := LineRider.Lines.Add(TLineRider.TLine.Create);
      Line.ID := I + 1;
      // if I mod 2 = 0 then
        Line.LineType := ltBoost;
      R1 := I / 18000 * Pi;
      R2 := (I - 1) / 18000 * Pi;
      S1 := Sin(-R1 * 100) * 0.005 + 1;
      S2 := Sin(-R2 * 100) * 0.005 + 1;
      Line.Pos1 := TVector2.FromAngleRad(R1) * 50000 * S1 - Vec2(0, 49980);
      Line.Pos2 := TVector2.FromAngleRad(R2) * 50000 * S2 - Vec2(0, 49980);
    end;

    JTrack := TJSerializer.Serialize(LineRider);
    TFile.WriteAllText('circle.json', JTrack.Format);

    LineRider.Free;
    JTrack.Free;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
