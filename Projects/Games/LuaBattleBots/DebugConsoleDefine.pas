unit DebugConsoleDefine;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TDebugConsole = class(TForm)
    memConsole: TMemo;
    Panel1: TPanel;
    cbPaused: TCheckBox;
  private
    FBuffer: string;

    procedure ScrollToEnd;

  public
    procedure WriteLine(AMessage: string = '');
    procedure Write(AMessage: string);

    procedure Update;
  end;

var
  DebugConsole: TDebugConsole;

implementation

{$R *.dfm}

{ TDebugConsole }

procedure TDebugConsole.ScrollToEnd;
begin
  SendMessage(memConsole.Handle, EM_LINESCROLL, 0, memConsole.Lines.Count);
end;

procedure TDebugConsole.Update;
begin
  if not cbPaused.Checked and not FBuffer.IsEmpty then
  begin
    memConsole.Text := memConsole.Text + FBuffer;
    FBuffer := '';
    ScrollToEnd;
  end;
end;

procedure TDebugConsole.Write(AMessage: string);
begin
  FBuffer := FBuffer + AMessage;
end;

procedure TDebugConsole.WriteLine(AMessage: string);
begin
  FBuffer := FBuffer + AMessage + sLineBreak;
end;

end.

