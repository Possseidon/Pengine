unit Main;

interface

uses
  Winapi.Windows,

  System.Classes,
  System.SysUtils,

  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.ExtCtrls,

  StatisticsForm;

type

  TfrmMain = class(TForm)
    edtInput: TEdit;
    lbQuestion: TLabel;
    Panel1: TPanel;
    cbMode: TComboBox;
    lbStatistics: TLabel;
    btnStatistics: TButton;
    procedure btnStatisticsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbModeChange(Sender: TObject);
    procedure edtInputKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    FTotalQuestions: Integer;
    FCorrectQuestions: Integer;
    FMistake: Boolean;
    FWords: TStringList;
    FStatistics: TStatistics;

    procedure GenerateQuestion;
    procedure UpdateStatistics;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


procedure TfrmMain.btnStatisticsClick(Sender: TObject);
begin
  frmStatistics.Show(FStatistics);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FWords.Free;
end;

procedure TfrmMain.cbModeChange(Sender: TObject);
begin
  GenerateQuestion;
end;

procedure TfrmMain.edtInputKeyPress(Sender: TObject; var Key: Char);
var
  C: Char;
begin
  if CharInSet(Key, [#10, #13]) then
  begin
    Key := #0;
    if SameText(edtInput.Text, lbQuestion.Caption) then
    begin
      edtInput.Text := '';
      Inc(FTotalQuestions);
      if not FMistake then
      begin
        Inc(FCorrectQuestions);
        for C in lbQuestion.Caption do
          if CharInSet(UpCase(C), ['A' .. 'Z']) then
            Inc(FStatistics[UpCase(C)]);
      end;
      GenerateQuestion;
      UpdateStatistics;
    end
    else
    begin
      FMistake := True;
      edtInput.Text := lbQuestion.Caption;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ResourceStream: TResourceStream;
  FontCount: Cardinal;
begin
  ResourceStream := TResourceStream.Create(HInstance, 'SGA', RT_RCDATA);
  try
    AddFontMemResourceEx(ResourceStream.Memory, ResourceStream.Size, nil, @FontCount);
  finally
    ResourceStream.Free;
  end;

  ResourceStream := TResourceStream.Create(HInstance, 'WORDS', RT_RCDATA);
  try
    FWords := TStringList.Create;
    FWords.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;

  GenerateQuestion;
  UpdateStatistics;
end;

procedure TfrmMain.GenerateQuestion;
begin
  edtInput.Text := '';
  FMistake := False;
  case cbMode.ItemIndex of
    0:
      lbQuestion.Caption := Chr(Ord('a') + Random(26));
    1:
      lbQuestion.Caption := FWords[Random(FWords.Count)];
  end;
end;

procedure TfrmMain.UpdateStatistics;
begin
  lbStatistics.Caption := Format('Correct Answers: %d / %d', [FCorrectQuestions, FTotalQuestions]);
end;

end.
