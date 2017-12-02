unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.RegularExpressions,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  Pengine.TimeManager,
  Pengine.Vector;

type

  TPatternType = (
    ptInterface,
    ptImplementation,
    ptClass,
    ptRecord,
    ptRecordHelper,
    ptFunction,
    ptProcedure
  );

  TForm2 = class(TForm)
    btnOpen: TButton;
    pnlMain: TPanel;
    spltMain: TSplitter;
    gbInterface: TGroupBox;
    gbImplementation: TGroupBox;
    odOpenFile: TOpenDialog;
    lbInterface: TListBox;
    lbImplementation: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure pnlMainResize(Sender: TObject);
    procedure spltMainMoved(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);

  private
    FSplitAspect: Single;

    FPatterns: array [TPatternType] of TRegex;

    function MainWidth: Integer;

    procedure LoadFile(AFileName: string);

  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

const

  TypePattern = '\w+(?:\.\w+)*(?:<(?R)>)';

  Patterns: array [TPatternType] of string = (
    'interface',
    'implementation',
    'A(?R)?B',
    //'\w+(?:<\w+ *(?:, *\w)*>)? *= *class(?:\((\w+(?:\.\w+)*(?:<(?1)>)?)\))?',
    //'\w+(?:<\w+ *(?:, *\w)*>)? *= *class(?:\(' + TypePattern + '\))?',
    '\w+(?:<\w+ *(?:, *\w)*>)? *= *record',
    '\w+(?:<\w+ *(?:, *\w)*>)? *= *record +helper +for +' + TypePattern,
    'function +(\w+(?:\w\.+)*)(?:\((.*)\))? *: *(\w+(?:\.\w+)*) *;',
    'procedure +(\w+(?:\w\.+)*)(?:\((.*)\))? *;'
  );

implementation

{$R *.dfm}

function TForm2.MainWidth: Integer;
begin
  Result :=
    pnlMain.Width -
    gbInterface.Margins.Left -
    gbInterface.Margins.Right -
    gbImplementation.Margins.Left -
    gbImplementation.Margins.Right -
    spltMain.Width;
end;

procedure TForm2.LoadFile(AFileName: string);
var
  Line: string;
  Match: TMatch;
  ImplementationReached: Boolean;
begin
  // StartTimer;

  FSourceFile.LoadFromFile(AFileName);
  lbInterface.Items.BeginUpdate; 
  
  ImplementationReached := False;
  for Line in FSourceFile do
  begin
    if Line.Trim.ToLower.Equals('implementation') then
    begin
      ImplementationReached := True;
      Continue;
    end;

    // Match := FMethodRegex.Match(Line.Trim);
    if Match.Success then
    begin
      if ImplementationReached then
        lbImplementation.Items.Add(Match.Value + ' ' + Match.Groups.Count.ToString)
      else
        lbInterface.Items.Add(Match.Value + ' ' + Match.Groups.Count.ToString);
    end
    //else if Line.Trim.StartsWith('function') or Line.Trim.StartsWith('procedure') then
    //  raise Exception.Create(Line);
  end;
  lbInterface.Items.EndUpdate;

  // ShowMessage(StopTimerGetString);
end;

procedure TForm2.btnOpenClick(Sender: TObject);
begin
  if not odOpenFile.Execute then
    Exit;
  LoadFile(odOpenFile.FileName);
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  PatternType: TPatternType;
begin
  FSourceFile := TStringList.Create;
  FSplitAspect := 0.5;

  for PatternType := Low(TPatternType) to High(TPatternType) do
    FPatterns[PatternType] := TRegex.Create('^' + Patterns[PatternType] + '$');

  ShowMessage(FPatterns[ptClass].Match('AAABBB').Value);
    
  LoadFile('Data\TestFile.pas');
end;

procedure TForm2.pnlMainResize(Sender: TObject);
var
  TotalWidth: Integer;
begin
  TotalWidth := MainWidth;
  gbInterface.Width := Round(FSplitAspect * TotalWidth);
  gbImplementation.Width := Round((1 - FSplitAspect) * TotalWidth);
end;

procedure TForm2.spltMainMoved(Sender: TObject);
begin
  FSplitAspect := gbInterface.Width / MainWidth;
end;

end.
