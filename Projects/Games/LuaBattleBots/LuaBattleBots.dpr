program LuaBattleBots;

{$APPTYPE CONSOLE}

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Game in 'Game.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
