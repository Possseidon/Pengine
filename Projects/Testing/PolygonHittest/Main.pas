unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VectorGeometry, Lists, IntegerMaths, BitField, System.Threading;

type
  TForm2 = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPoints: TGenericArray<TVector2>;

    function Convert(APoint: TVector2): TIntVector2;

    function GetLine(I: Integer): TLine2;
    property Lines[I: Integer]: TLine2 read GetLine;

    function CheckPoint(APoint: TVector2): Boolean;

  end;

var
  Form2: TForm2;

implementation

uses
  Math;

{$R *.dfm}

function TForm2.CheckPoint(APoint: TVector2): Boolean;
var
  L: TLine2;
  HitCount: Integer;           
  Data: TLine2.TIntsecData;
  I: Integer;
begin
  L := TLine2.Create(APoint, Vec2(0, 1));
  HitCount := 0;
  for I := 0 to FPoints.Count - 1 do
  begin
    if L.Intsec(Lines[I], Data) and (Data.DistanceOther >= 0) and (Data.DistanceOther < 1) and (Data.Distance > 0) then
      InterlockedIncrement(HitCount);
  end;
  Result := Odd(HitCount);
end;

function TForm2.Convert(APoint: TVector2): TIntVector2;
begin
  Result := ((APoint + 1) / 2 * Vec2(ClientWidth, ClientHeight) + 0.5).Floor;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FPoints := TGenericArray<TVector2>.Create;
  for I := 0 to 60 do
  begin
    FPoints.Add(TVector2.RandomNormal)
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FPoints.Free;
end;

procedure TForm2.FormPaint(Sender: TObject);
var
  Points: array of TPoint;
  I: Integer;
  P: TIntVector2;
  PointF: TVector2;
begin       
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Style := bsSolid;
    
  // Polygon
  Canvas.Brush.Color := $A04010;
  SetLength(Points, FPoints.Count);
  for I := 0 to FPoints.Count - 1 do
    Points[I] := Convert(FPoints[I]);
  Canvas.Polygon(Points);

  // Points
  for P in Range2(-100, 101) do
  begin              
    PointF := TVector2(P) / 100;                                             
    if CheckPoint(PointF) then
      Canvas.Brush.Color := $2020F0
    else
      Canvas.Brush.Color := $40E030;

    Canvas.Pixels[Convert(PointF).X, Convert(PointF).Y] := Canvas.Brush.Color;  
  end;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  Invalidate;
end;

function TForm2.GetLine(I: Integer): TLine2;
begin
  Result := FPoints[I].LineTo(FPoints[(I + 1) mod FPoints.Count]);  
end;

end.

