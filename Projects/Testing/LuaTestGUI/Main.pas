unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, SynEdit, LuaDefine, LuaHeader, Vcl.Samples.Spin,
  TimeManager, LuaDefaultLibs;

type

  TLuaLibHelp = class(TLuaLib)
  private
    class function LuaPrint(L: TLuaState): Integer; static; cdecl;
  protected
    class function CreateEntry: TLuaLib.TEntry; override;
  end;

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
  NoTimeout: Boolean;
begin
  FLua.L.GetGlobal('code');
  NoTimeout := FLua.LCall(0, 0, seTimeout.Value / 1000, Err);
  if NoTimeout then
  begin
    if Err <> lceOK then
    begin
      ShowMessage(string(FLua.L.ToString));
      FLua.L.Pop;
    end
  end
  else
  begin
    ShowMessage('Timeout!');
    seCodeChange(nil);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLua := TLua.Create;
  FLua.AddLib(TLuaLibBasic);
  FLua.AddLib(TLuaLibHelp);
  seCodeChange(nil);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLua.Free;
end;

procedure TfrmMain.seCodeChange(Sender: TObject);
begin
  if FLua.L.LoadString(AnsiString(seCode.Text), 'code') = lleOK then
  begin
    FLua.L.SetGlobal('code');
    lbError.Caption := 'Compiled without Error';
    lbError.Font.Color := clDefault;
  end
  else
  begin
    lbError.Caption := string(FLua.L.ToString_X(1));
    FLua.L.Pop;
    lbError.Font.Color := clRed;
  end;
end;

{ THelpLib }

class function TLuaLibHelp.CreateEntry: TLuaLib.TEntry;
var
  Env: TTableEntry absolute Result;
begin
  Env := TTableEntry.Create;
  with Env do
  begin
    Add('print', LuaPrint);
  end;
end;

class function TLuaLibHelp.LuaPrint(L: TLuaState): Integer;
begin
  L.CheckAny(1);
  L.CheckEnd(2);
  MessageBoxA(0, L.ToString, 'print', 0);
  Result := 0;
end;

end.
