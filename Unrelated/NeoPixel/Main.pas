unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Net.HttpClientComponent,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  Pengine.Color, Vcl.ExtCtrls;

type

  TRunningThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TfrmMain = class(TForm)
    btnStart: TButton;
    pnlColor: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    FThread: TRunningThread;

  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.Net.HttpClient;

{$R *.dfm}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FThread.Free;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  if FThread = nil then
  begin
    FThread := TRunningThread.Create;
    btnStart.Caption := 'Running...';
  end
  else
  begin
    FThread.Free;
    FThread := nil;
    btnStart.Caption := 'Start';
  end;
end;

{ TRunningThread }

procedure TRunningThread.Execute;
var
  Value: Single;
  Url: string;
  Client: TNetHTTPClient;
  Color: TColorRGB;
  Response: IHTTPResponse;
begin
  Client := TNetHTTPClient.Create(nil);
  try
    Value := 0;
    while not Terminated do
    begin
      Color := TColorRGB.Rainbow(Value);
      Synchronize(
        procedure
        begin
          frmMain.pnlColor.Color := Color.ToWinColor;
        end);
      Url := 'http://ledclock/?cp=' + IntToHex(Color.ToWinColor) + '&b=80';
      Client.Get(Url);
      Value := Value + 0.1;
      Sleep(100);
    end;
  finally
    Client.Free;
  end;
end;

end.
