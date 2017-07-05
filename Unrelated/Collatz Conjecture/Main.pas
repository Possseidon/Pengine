unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Lists;

type

  TAutoSizeArray<T> = class (TArray<T>)
  protected
    procedure SetItem(AIndex: Integer; AValue: T); override;
  end;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    Numbers: TAutoSizeArray<Integer>;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TAutoSizeArray<T> }

procedure TAutoSizeArray<T>.SetItem(AIndex: Integer; AValue: T);
begin
  if AIndex >= Count then
    Resize(AIndex + 1);
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Numbers := TAutoSizeArray<Integer>.Create;
  for I := 1 to 10000 do
  begin
    if True then
    
    Numbers[I] := ;
  end;
end;

end.
