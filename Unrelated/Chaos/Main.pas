unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VectorGeometry, Vcl.ExtCtrls, Lists, Math, TimeManager;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    Jumper: TGVector2;
    Points: TArray<TGVector2>;
    Pic: TBitmap;
    PixelCounter: Cardinal;

    Timer: TDeltaTimer;
    RepaintTimestamp: Single;

    procedure IdleHandler(Sender: TObject; var Done: Boolean);

    const
      Factor = 0.51;
      UpdateInterval = 0.01;

  public
    { Public-Deklarationen }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Points := TArray<TGVector2>.Create;

  {
  // Classic Sierpinski
  Points.Add(TGVector2.Create(0.5, 0.1));
  Points.Add(TGVector2.Create(0.9, 0.9));
  Points.Add(TGVector2.Create(0.1, 0.9));
  }

  // Rectangle
  Points.Add(TGVector2.Create(0.1, 0.1));
  Points.Add(TGVector2.Create(0.9, 0.9));
  Points.Add(TGVector2.Create(0.1, 0.9));
  Points.Add(TGVector2.Create(0.9, 0.1));

  Jumper := TGVector2.Create(Random, Random);
  Application.OnIdle := IdleHandler;

  Pic := TBitmap.Create;
  Timer.Init;
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
begin
  Jumper := Jumper + Jumper.VectorTo(Points[Random(Points.Count)]) * Factor;
  X := EnsureRange(Round(Jumper.X * ClientWidth), 0, ClientWidth - 1);
  Y := EnsureRange(Round(Jumper.Y * ClientHeight), 0, ClientHeight - 1);
  Pic.Canvas.Pixels[X, Y] := $FF7F7F;
  Inc(PixelCounter);

  if Timer.Update then
      Caption := Format('Pixels/Second: %f', [PixelsPerSecond]);

  if Timer.Seconds - RepaintTimestamp >= UpdateInterval then
  begin
    PixelsPerSecond := (Timer.Seconds - RepaintTimestamp) / UpdateInterval;
    RepaintTimestamp := Timer.Seconds;
    PixelCounter := 0;
    Invalidate;
  end;

  Done := False;
end;

end.
