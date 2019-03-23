unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Lists;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  List: TStringMap<Integer>;
  S: string;
  I: Integer;
begin
  I := 42;
  List := TStringMap<Integer>.Create;
  S := Format('Address: $%.16x', [42]);
  List['test'] := 42;
  List['hallo'] := 256;
  List['welt'] := 1024;
  List.Free;
end;

end.
