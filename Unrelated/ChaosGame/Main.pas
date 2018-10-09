unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Pengine.Vector,
  Vcl.ExtCtrls,
  Pengine.Collections,
  Pengine.TimeManager,
  Pengine.Color, System.Math;

type
  TForm1 = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    Jumper: TVector2;
    Points: TArray<TVector2>;
    Pic: TBitmap;
    PixelCounter: Cardinal;

    Timer: TDeltaTimer;
    RepaintTimestamp: Single;
    PixelsPerSecond: Single;

    procedure IdleHandler(Sender: TObject; var Done: Boolean);

  private const
    Factor = 0.6;
    UpdateInterval = 0.01;

  public
    { Public-Deklarationen }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  R: Single;
begin
  Points := TArray<TVector2>.Create;

    {
    // Classic Sierpinski
    Points.Add(TVector2.Create(0.5, 0.1));
    Points.Add(TVector2.Create(0.9, 0.9));
    Points.Add(TVector2.Create(0.1, 0.9));
    // }


    // Rectangle
    Points.Add(TVector2.Create(0.1, 0.1));
    Points.Add(TVector2.Create(0.9, 0.9));
    Points.Add(TVector2.Create(0.1, 0.9));
    Points.Add(TVector2.Create(0.9, 0.1));
    Points.Add(TVector2.Create(0.5, 0.5));
    // }

  {
    // Circle stuff
    for I := 0 to 6 - 1 do
    begin
    R := I / 6 * 2 * Pi;
    Points.Add(TGVector2.Create(Sin(R), Cos(R)) * 0.5 + 0.5);
    end;
    Points.Add(TGVector2.Create(0.5, 0.5));
    // }
  // {
  // Random
  //for I := 0 to 99 do
  //  Points.Add(TVector2.Create(Random, Random));
  // }
  Jumper := TVector2.Create(Random, Random);
  Application.OnIdle := IdleHandler;

  Pic := TBitmap.Create;
  Timer := TDeltaTimer.Create;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, Pic.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Pic.SetSize(ClientWidth, ClientHeight);
  Pic.Canvas.Brush.Style := bsSolid;
  Pic.Canvas.Brush.Color := clBlack;
  Pic.Canvas.Pen.Style := psClear;
  Pic.Canvas.Rectangle(0, 0, Pic.Width, Pic.Height);
  Invalidate;
end;

procedure TForm1.IdleHandler(Sender: TObject; var Done: Boolean);
var
  X, Y: Integer;
  SelectedPoint: Integer;
  ColorFull, ColorNew: TColorRGB;
begin
  SelectedPoint := Random(Points.Count);
  Jumper := Jumper + Jumper.VectorTo(Points[SelectedPoint]) * Factor;
  X := EnsureRange(Round(Jumper.X * ClientWidth), 0, ClientWidth - 1);
  Y := EnsureRange(Round(Jumper.Y * ClientHeight), 0, ClientHeight - 1);

  ColorFull := TColorRGB.Rainbow(SelectedPoint / Points.Count * 6);
  ColorNew := (TColorRGB.Create(Pic.Canvas.Pixels[X, Y]) + ColorFull * 0.1).EnsureColor;
  Pic.Canvas.Pixels[X, Y] := ColorNew.ToWinColor;
  {
  Pic.Canvas.Pen.Style := psSolid;
  Pic.Canvas.Pen.Color := ColorNew.ToWinColor;
  Pic.Canvas.Brush.Style := bsSolid;
  Pic.Canvas.Brush.Color := ColorNew.ToWinColor;
  Pic.Canvas.Ellipse(X - Random(8), Y - Random(8), X + Random(8), Y + Random(8));
  {
    Pic.Canvas.Brush.Style := bsClear;
    Pic.Canvas.Pen.Style := psSolid;
    Pic.Canvas.Pen.Color := $FF7F7F;
    Pic.Canvas.Rectangle(X - 1, Y - 1, X + 2, Y + 2);
  }

  Inc(PixelCounter);

  Timer.Update;
  // if Timer. then
  //   Caption := Format('Pixels/Second: %f', [PixelsPerSecond]);

  if Timer.Time - RepaintTimestamp >= UpdateInterval then
  begin
    PixelsPerSecond := PixelCounter / (Timer.Time - RepaintTimestamp);
    RepaintTimestamp := Timer.Time;
    PixelCounter := 0;
    Invalidate;
  end;

  Done := False;
end;

end.
