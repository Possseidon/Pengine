unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, SynEdit, Lua, LuaHeader, Vcl.Samples.Spin,
  TimeManager;

type

  TfrmMain = class(TForm)
    pnlBottom: TPanel;
    seCode: TSynEdit;
    lbError: TLabel;
    btnRun: TButton;
    seTimeout: TSpinEdit;
    Label1: TLabel;
    procedure seCodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    FLua: TLua;

  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

procedure TfrmMain.btnRunClick(Sender: TObject);
var
  Err: TLuaPCallError;
  I: Integer;
begin
  StartTimer;
  for I := 0 to 9 do
  begin
    FLua.L.GetGlobal('code');
    if FLua.LCall(0, 0, seTimeout.Value / 1000, Err) then
    begin
      if Err <> lceOK then
      begin
        ShowMessage(string(FLua.L.ToString));
        FLua.L.Pop;
      end
    end
    else
    begin
      // ShowMessage('Timeout!');
      seCodeChange(nil);
    end;
  end;
  ShowMessage(StopTimerGetString);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLua := TLua.Create;
  seCodeChange(nil);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLua.Free;
end;

procedure TfrmMain.seCodeChange(Sender: TObject);
begin
  case FLua.L.LoadString(AnsiString(seCode.Text)) of
    lleOK:
      begin
        FLua.L.SetGlobal('code');
        lbError.Caption := 'Compiled without Error';
        lbError.Font.Color := clDefault;
      end;
    lleErrorSyntax:
      begin
        lbError.Caption := string(FLua.L.ToString_X(1));
        FLua.L.Pop;
        lbError.Font.Color := clRed;
      end;
    lleErrorMemory:
      begin
        lbError.Caption := 'Memory Error';
        lbError.Font.Color := clRed;
      end;
    lleErrorGCMM:
      begin
        lbError.Caption := 'Memory Error';
        lbError.Font.Color := clRed;
      end;
  end;
end;

end.
