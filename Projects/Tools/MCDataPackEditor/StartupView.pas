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
  Pengine.MC.Brigadier;

type

  TStartupSettings = class(TSettings)
  public const

    Path = 'settings.json';

  private
    FBackgroundLoading: Boolean;
    FLoadEvertyhingOnStartup: Boolean;

  public
    procedure SetDefaults; override;

    property BackgroundLoading: Boolean read FBackgroundLoading write FBackgroundLoading;
    property LoadEverythingOnStartup: Boolean read FLoadEvertyhingOnStartup write FLoadEvertyhingOnStartup;

  end;

  TLoadJob = class
  protected
    function GetProgressLength: Integer; virtual;
    procedure StepProgress;
    procedure Log(AText: string);

    procedure DoPerform; virtual; abstract;

  public
    procedure Perform;

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

  protected
    procedure Execute; override;

  public
    constructor Create(AJobs: TLoadJobs.TReader);

    property Jobs: TLoadJobs.TReader read FJobs;

  end;

  TfrmStartup = class(TForm)
    pbProgress: TProgressBar;
    Label1: TLabel;
    pbProgressSecondary: TProgressBar;
    memProgress: TMemo;
    procedure FormShow(Sender: TObject);
  public const

    SettingsPath = '%appdata%/.MCDPE';

  private
    FJobs: TLoadJobs.TReader;

    procedure StartupDone(Sender: TObject);

  public
    procedure Execute(AJobs: TLoadJobs.TReader);

  end;

var
  frmStartup: TfrmStartup;

implementation

{$R *.dfm}

{ TfrmStartup }

procedure TfrmStartup.Execute(AJobs: TLoadJobs.TReader);
var
  StartupSettings: TStartupSettings;
begin
  FJobs := AJobs;
  StartupSettings := RootSettingsG.Get<TStartupSettings>;
  if StartupSettings.BackgroundLoading then
    raise ENotImplemented.Create('Loading in background.')
  else
    ShowModal;
end;

procedure TfrmStartup.FormShow(Sender: TObject);
var
  Thread: TStartupThread;
begin
  Thread := TStartupThread.Create(FJobs);
  Thread.OnTerminate := StartupDone;
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

procedure TfrmStartup.StartupDone(Sender: TObject);
begin
  Close;
end;

{ TStartupThread }

constructor TStartupThread.Create(AJobs: TLoadJobs.TReader);
begin
  inherited Create(True);
  FJobs := AJobs;
  frmStartup.pbProgress.Max := Jobs.Count * frmStartup.pbProgress.Step;
end;

procedure TStartupThread.Execute;
var
  Job: TLoadJob;
begin
  for Job in FJobs do
  begin
    Job.Perform;
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
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      frmStartup.memProgress.Lines.Add(AText);
    end);
end;

procedure TLoadJob.Perform;
begin
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      frmStartup.pbProgressSecondary.Max := GetProgressLength;
    end);
  DoPerform;
end;

procedure TLoadJob.StepProgress;
begin
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      frmStartup.pbProgressSecondary.StepIt;
    end);
end;

{ TStartupSettings }

procedure TStartupSettings.SetDefaults;
begin
  FBackgroundLoading := False;
  FLoadEvertyhingOnStartup := False;
end;

end.
