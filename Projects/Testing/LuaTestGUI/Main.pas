unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.TypInfo,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.ActnList,

  System.Actions,

  SynEdit,

  Pengine.Utility,
  Pengine.Lua,
  Pengine.LuaHeader,
  Pengine.LuaWrapper,
  Pengine.TimeManager,
  Pengine.LuaDefaultLibs,
  Pengine.Collections;

type

  TLuaLibHelp = class(TLuaLib)
  private
    class function LuaPrint(L: TLuaState): Integer; static; cdecl;

  protected
    class procedure CreateEntry(AEntry: TLuaLib.TTableEntry); override;
  end;

  TTest = Word;

  TfrmMain = class(TForm)
    pnlBottom: TPanel;
    seCode: TSynEdit;
    lbError: TLabel;
    btnRun: TButton;
    seTimeout: TSpinEdit;
    cbTimeout: TCheckBox;
    ActionList1: TActionList;
    actRun: TAction;
    procedure seCodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbTimeoutClick(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
  private
    FLua: TLua;
    FTest: TTest;

  published

    property Test: TTest read FTest write FTest;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.actRunExecute(Sender: TObject);
var
  Err: TLuaPCallError;
  NoTimeout: Boolean;
begin
  TLuaWrapper.PushObject(FLua.L, Self, mvPublic);
  FLua.L.SetGlobal('form');

  StartTimer;

  FLua.L.GetGlobal('code');
  if cbTimeout.Checked then
    NoTimeout := FLua.CallTimeout(0, 0, seTimeout.Value / 1000, Err)
  else
  begin
    NoTimeout := True;
    Err := FLua.L.PCall(0, 0, 0);
  end;
  if NoTimeout then
  begin
    if Err <> lceOK then
    begin
      ShowMessage(string(FLua.L.ToString));
      FLua.L.Pop;
    end
    else
    begin
      ShowMessage(Format('Success! %s', [StopTimerGetString(tfMilliseconds)]));
    end;
  end
  else
  begin
    ShowMessage(Format('Timeout! %s', [StopTimerGetString(tfMilliseconds)]));
    seCodeChange(nil);
  end;
end;

procedure TfrmMain.cbTimeoutClick(Sender: TObject);
begin
  seTimeout.Enabled := cbTimeout.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLua := TLua.Create;
  FLua.AddLib(TLuaLibBasic);
  FLua.AddLib(TLuaLibHelp);
  FLua.AddLib(TLuaLibTable);
  FLua.AddLib(TLuaLibMath);
  FLua.AddLib(TLuaLibCoroutine);

  TLuaWrapper.PushEnum<TAlign>(FLua.L);
  FLua.L.SetGlobal('TAlign');

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
    actRun.Enabled := True;
  end
  else
  begin
    lbError.Caption := string(FLua.L.ToString_X(1));
    FLua.L.Pop;
    lbError.Font.Color := clRed;
    actRun.Enabled := False;
  end;
end;

{ THelpLib }

class procedure TLuaLibHelp.CreateEntry(AEntry: TLuaLib.TTableEntry);
begin
  with AEntry do
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
