unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, SynEdit, LuaHeader;

type

  TLuaThread = class(TThread)
  private
    FLua: TLuaState;
  protected
    procedure Execute; override;
  public
    constructor Create(ALua: TLuaState);
  end;

  TfrmMain = class(TForm)
    pnlBottom: TPanel;
    seCode: TSynEdit;
    lbError: TLabel;
    btnRun: TButton;
    procedure seCodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    L: TLuaState;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TLuaThread.Execute;
begin
  FLua.GetGlobal('code');
  if FLua.PCall(0, 0, 0) = lceErrorRun then
  begin
    ShowMessage(FLua.ToString);
  end;
end;

constructor TLuaThread.Create(ALua: TLuaState);
begin
  inherited Create;
  FLua := ALua;
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
var
  Thread: TLuaThread;
begin
  Thread := TLuaThread.Create(L);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  L := NewLuaState;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  L.Close;
end;

procedure TfrmMain.seCodeChange(Sender: TObject);
begin
  case L.LoadString(AnsiString(seCode.Text)) of
    lleOK:
      begin
        L.SetGlobal('code');
        lbError.Caption := 'Compiled without Error';
        lbError.Font.Color := clDefault;
      end;
    lleErrorSyntax:
      begin
        lbError.Caption := string(L.ToString_X(1));
        L.Pop;
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
