program RopeRenderer;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form3},
  RopeDefine in 'RopeDefine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

