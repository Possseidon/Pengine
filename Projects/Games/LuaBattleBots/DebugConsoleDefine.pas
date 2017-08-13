unit DebugConsoleDefine;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDebugConsole = class(TForm)
    memConsole: TMemo;
  public
    procedure WriteLine(AMessage: string = '');
    procedure Write(AMessage: string);
  end;

var
  DebugConsole: TDebugConsole;

implementation

{$R *.dfm}

{ TDebugConsole }

procedure TDebugConsole.Write(AMessage: string);
begin
  memConsole.Text := memConsole.Text + AMessage;
end;

procedure TDebugConsole.WriteLine(AMessage: string);
begin
  memConsole.Text := memConsole.Text + AMessage + sLineBreak;
end;

end.

