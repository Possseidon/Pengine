unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, TextureManager, Vcl.StdCtrls, PngImage;

type
  TForm2 = class(TForm)
    btnDecode: TButton;
    procedure btnDecodeClick(Sender: TObject);
  private
    procedure Decode(AFileName: TFileName);

  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Decode(AFileName: TFileName);
var
  Image: TPngImage;
  ResultFile: TFileStream;
  X, Y, B: Integer;
  Data: Byte;
begin
  Image := TPngImage.Create;
  Image.LoadFromFile(AFileName);

  ResultFile := TFileStream.Create('decoded.txt', fmCreate or fmOpenWrite);
  B := 0;
  Data := 0;
  for Y := 0 to Image.Height - 1 do
  begin
    for X := 0 to Image.Width - 1 do
    begin
      Data := Data shl 1;
      if Image.Pixels[X, Y] = clWhite then
        Data := Data or 1;
      Inc(B);
      if B = 8 then
      begin
        ResultFile.Write(Data, 1);
        B := 0;
        Data := 0;
      end;
    end;
  end;
  ResultFile.Free;
  Image.Free;
end;

procedure TForm2.btnDecodeClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  begin
    if Execute then
    begin
      Decode(FileName);
    end;
    Free;
  end;
end;

end.

