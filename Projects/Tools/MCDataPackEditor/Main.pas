unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShLwApi,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Zip,
  System.Actions,
  System.ImageList,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.ToolWin,

  SynEdit,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.TimeManager,
  Pengine.Parser,

  Minecraft.BrigadierParser,
  Minecraft.Brigadier,
  Minecraft.NBT,

  FrameSynEdit;

type

  TfrmMain = class(TForm)
    alActions: TActionList;
    mmMain: TMainMenu;
    Splitter1: TSplitter;
    File1: TMenuItem;
    NewDatapack1: TMenuItem;
    OpenDatapack1: TMenuItem;
    Datapack1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Namespace1: TMenuItem;
    advancement1: TMenuItem;
    LootTable1: TMenuItem;
    Recipe1: TMenuItem;
    Structure1: TMenuItem;
    ag1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    tvFiles: TTreeView;
    ToolBar1: TToolBar;
    tcFiles: TTabControl;
    frmMCFunction1: TfrmSynEdit;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBrigadierRoot: TBrigadierRoot;

    procedure InitBrigadierSystem;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  CommandParser: TBrigadierCommandParser;
  Suggestion: TBrigadierChild;
begin
  // SHAutoComplete(edtTestInput.Handle, SHACF_AUTOAPPEND_FORCE_OFF or SHACF_AUTOSUGGEST_FORCE_OFF);
  InitBrigadierSystem;

  CommandParser := TBrigadierCommandParser.Create(FBrigadierRoot, 'publish -1');
  frmMCFunction1.synEdit.Lines.Add(CommandParser.ParseResult.Format);
  if CommandParser.ParseResult.Success then
    frmMCFunction1.synEdit.Lines.Add('success')
  else
    frmMCFunction1.synEdit.Lines.Add('no success');
  if CommandParser.ParseResult.Executable then
    frmMCFunction1.synEdit.Lines.Add('executable')
  else
    frmMCFunction1.synEdit.Lines.Add('not executable');

  for Suggestion in CommandParser.ParseResult.Suggestions do
  begin
    frmMCFunction1.synEdit.Lines.Add(Suggestion.ToString);
  end;

  CommandParser.ParseResult.Free;
  CommandParser.Free;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FBrigadierRoot.Free;
end;

procedure TfrmMain.InitBrigadierSystem;
var
  CommandTreeText: string;
  CommandTree: TJSONObject;
  Parser: TPair<string, TSet<string, TStringHasher>>;
  Prop: string;
begin
  CommandTreeText := TFile.ReadAllText('Data/reports/commands.json');

  CommandTree := TJSONObject.ParseJSONValue(CommandTreeText) as TJSONObject;

  try
    FBrigadierRoot := TBrigadierRoot.Create(CommandTree);

  finally
    CommandTree.Free;
  end;

end;

end.
