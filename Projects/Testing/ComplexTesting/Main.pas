unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.Texture,
  Pengine.Color,
  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.Complex;

type

  TForm17 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBounds: TBounds2;

  end;

var
  Form17: TForm17;

implementation

{$R *.dfm}

procedure TForm17.FormCreate(Sender: TObject);
begin
  FBounds := Bounds2(-1, +1) / 10;
end;

procedure TForm17.FormPaint(Sender: TObject);
var
  PixelCoord: TIntVector2;
  Coord: TVector2;
  Color: TColorRGB;
  Start, Current: TComplex;
  I: Integer;
  A: Double;
  Bitmap: TBitmap;
  WinColor: TColor;
  Scan: PByte;
  Bytes: TColorRGB.TBytes;
  Tmp: Byte;
begin
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf24bit;
  Bitmap.SetSize(ClientWidth, ClientHeight);

  for PixelCoord in IVec2(ClientWidth, ClientHeight) do
  begin
    Coord := Bounds2(0, IVec2(ClientWidth, ClientHeight)).Convert(PixelCoord, FBounds);
    Coord.X := Coord.X * Width / Height;
    Start.Create(Coord.X, Coord.Y);
    Current := Start;
    I := 1;
    while I < 1000 do
    begin
      A := Current.Abs;
      if A > 2 then
        Break;
      Current := Current.Sqr + Start;
      Inc(I);
    end;

    if A > 2 then
      Color := TColorRGB.HSV(I * 0.2, 1, 1)
    else
      Color := ColorBlack;

    if PixelCoord.X = 0 then
      Scan := Bitmap.ScanLine[PixelCoord.Y];
    Bytes := Color.ToBytes;
    Tmp := Bytes.R;
    Bytes.R := Bytes.B;
    Bytes.B := Tmp;
    Move(Bytes, Scan^, 3);
    Inc(Scan, 3);
  end;

  BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  Bitmap.Free;
end;

procedure TForm17.FormResize(Sender: TObject);
begin
  Invalidate;
end;

end.
