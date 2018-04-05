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

  Pengine.GLForm,
  Pengine.GLState,
  Pengine.Equaller,
  Pengine.Color,
  Pengine.GLEnums;

type

  TForm1 = class(TGLForm)
  public
    procedure Init; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Init;
begin

end;

end.
