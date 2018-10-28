unit StartupView;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,

  Vcl.Forms,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  Pengine.Settings,
  Pengine.Collections,

  Pengine.MC.ItemIcons,
  Pengine.MC.Item,
  Pengine.MC.BlockState,
  Pengine.MC.Brigadier,
  Pengine.JSON;

type

  TLoadJob = class
  private
    FUpdateGUI: Boolean;

  protected
    function GetProgressLength: Integer; virtual;
    procedure StepProgress;
    procedure Log(AText: string);

    procedure DoPerform; virtual; abstract;

  public
    procedure Perform(AUpdateGUI: Boolean);

  end;

  TLoadJobs = TObjectArray<TLoadJob>;

  TLoadSettingsJob = class(TLoadJob)
  private
    FSettingsClass: TSettingsClass;

  protected
    procedure DoPerform; override;

  public
    constructor Create(ASettingsClass: TSettingsClass);

  end;

  TStartupThread = class(TThread)
  private
    FJobs: TLoadJobs.TReader;
    FUpdateGUI: Boolean;

  protected
    procedure Execute; override;

  public
    constructor Create(AJobs: TLoadJobs.TReader; AUpdateGUI: Boolean);

    property UpdateGUI: Boolean read FUpdateGUI;
    property Jobs: TLoadJobs.TReader read FJobs;

  end;

  TfrmStartup = class(TForm)
    pbProgress: TProgressBar;
    Label1: TLabel;
    pbProgressSecondary: TProgressBar;
    memProgress: TMemo;
    procedure FormShow(Sender: TObject);
  private
    FJobs: TLoadJobs.TReader;

    procedure StartupDone(Sender: TObject);

    procedure StartJobs(AUpdateGUI: Boolean);

  public
    procedure Execute(AJobs: TLoadJobs.TReader);

  end;

var
  frmStartup: TfrmStartup;

implementation

{$R *.dfm}

{ TfrmStartup }

procedure TfrmStartup.StartJobs(AUpdateGUI: Boolean);
var
  Thread: TStartupThread;
begin
  Thread := TStartupThread.Create(FJobs, AUpdateGUI);
  Thread.OnTerminate := StartupDone;
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

procedure TfrmStartup.Execute(AJobs: TLoadJobs.TReader);
begin
  FJobs := AJobs;
  ShowModal;
end;

procedure TfrmStartup.FormShow(Sender: TObject);
begin
  StartJobs(True);
end;

procedure TfrmStartup.StartupDone(Sender: TObject);
begin
  FreeAndNil(FJobs);
  Close;
end;

{ TStartupThread }

constructor TStartupThread.Create(AJobs: TLoadJobs.TReader; AUpdateGUI: Boolean);
begin
  inherited Create(True);
  FJobs := AJobs;
  FUpdateGUI := AUpdateGUI;
  frmStartup.pbProgress.Max := Jobs.Count * frmStartup.pbProgress.Step;
end;

procedure TStartupThread.Execute;
var
  Job: TLoadJob;
begin
  for Job in FJobs do
  begin
    Job.Perform(UpdateGUI);
    if UpdateGUI then
      Synchronize(
        procedure
        begin
          frmStartup.pbProgress.StepIt;
        end);
  end;
end;

{ TLoadSettingsJob }

constructor TLoadSettingsJob.Create(ASettingsClass: TSettingsClass);
begin
  FSettingsClass := ASettingsClass;
end;

procedure TLoadSettingsJob.DoPerform;
begin
  Log('Loading ' + FSettingsClass.GetTitle + '...');
  RootSettingsG.Reload(FSettingsClass);
end;

{ TLoadJob }

function TLoadJob.GetProgressLength: Integer;
begin
  Result := 0;
end;

procedure TLoadJob.Log(AText: string);
begin
  if FUpdateGUI then
    TThread.Synchronize(TThread.Current,
      procedure
      begin
        frmStartup.memProgress.Lines.Add(AText);
      end);
end;

procedure TLoadJob.Perform(AUpdateGUI: Boolean);
begin
  FUpdateGUI := AUpdateGUI;
  if FUpdateGUI then
    TThread.Synchronize(TThread.Current,
      procedure
      begin
        frmStartup.pbProgressSecondary.Max := GetProgressLength;
      end);
  DoPerform;
end;

procedure TLoadJob.StepProgress;
begin
  if FUpdateGUI then
    TThread.Synchronize(TThread.Current,
      procedure
      begin
        frmStartup.pbProgressSecondary.StepIt;
      end);
end;

end.
