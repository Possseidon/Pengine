program Physics2DTesting;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Physics2D in '..\..\..\Librarys\Game Utils\Physics2D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
