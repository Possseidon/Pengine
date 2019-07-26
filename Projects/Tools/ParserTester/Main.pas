unit Main;

interface

uses
  System.SysUtils,
  System.Classes,

  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  Pengine.ICollections,
  Pengine.Parsing,

  // Parser
  Pengine.Lua.Parsing,

  SynEdit,
  SynEditHighlighter;

type



  TSynHighlighter = class(TSynCustomHighlighter)
  private
    FDummyAttributes: TSynHighlighterAttributes;
    FCurrentAttribtues: TSynHighlighterAttributes;

  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetEol: Boolean; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;

    procedure Next; override;

  end;

  TfrmMain = class(TForm)
    synEditor: TSynEdit;
    pnlSettings: TPanel;
    cbParsers: TComboBox;
    memLog: TMemo;
    procedure cbParsersChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure synEditorChange(Sender: TObject);
  private
    FAvailableParsers: IList<TParserClass>;
    FContext: TParseInfo.TContext;

    procedure SearchAvailableParsers;
    procedure FillParserCombobox;
    procedure UpdateLog;

    procedure Parse;

    function CurrentParser: TParserClass;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


procedure TfrmMain.cbParsersChange(Sender: TObject);
begin
  Parse;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FContext.Free;
end;

procedure TfrmMain.Parse;
var
  ParserClass: TParserClass;
  Parser: IParser;
begin
  FreeAndNil(FContext);
  ParserClass := CurrentParser;
  if ParserClass <> nil then
  begin
    Parser := ParserClass.Create as IParser;
    Parser.Parse(synEditor.Text, True);
    FContext := Parser.Context.Own;
  end;
  UpdateLog;
end;

function TfrmMain.CurrentParser: TParserClass;
begin
  if cbParsers.ItemIndex = -1 then
    Exit(nil);
  Result := TParserClass(cbParsers.Items.Objects[cbParsers.ItemIndex]);
end;

procedure TfrmMain.FillParserCombobox;
var
  ParserClass: TParserClass;
begin
  cbParsers.Items.BeginUpdate;
  try
    cbParsers.Items.Clear;
    for ParserClass in FAvailableParsers do
      if ParserClass.GetResultName = ParserClass.ClassName then
        cbParsers.AddItem(ParserClass.GetResultName, TObject(ParserClass))
      else
        cbParsers.AddItem(ParserClass.GetResultName + ': ' + ParserClass.ClassName, TObject(ParserClass));
  finally
    cbParsers.Items.EndUpdate;
  end;
  cbParsers.ItemIndex := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  synEditor.Highlighter := TSynHighlighter.Create(Self);
  SearchAvailableParsers;
  FillParserCombobox;
  Parse;
end;

procedure TfrmMain.SearchAvailableParsers;
begin
  FAvailableParsers := TList<TParserClass>.Create([
    TLuaFunctionBody.TParser,
    TLuaParameterList.TParser,
    TLuaNameList.TParser,
    TLuaFunctionName.TParser,
    TLuaNameParser
  ]);
end;

procedure TfrmMain.synEditorChange(Sender: TObject);
begin
  Parse;
end;

procedure TfrmMain.UpdateLog;
var
  LogEntry: TParseError;
begin
  if FContext = nil then
  begin
    memLog.Text := 'Select a parser';
    Exit;
  end;

  memLog.Lines.BeginUpdate;
  try
    memLog.Lines.Clear;
    if FContext.Success then
      memLog.Lines.Add('Parse success!');
    if FContext.Length < synEditor.Text.Length then
      memLog.Lines.Add('Trailing data: "' + synEditor.Text.Substring(FContext.Length) + '"');
    for LogEntry in FContext.Log do
      memLog.Lines.Add(LogEntry.LevelName + ': ' + LogEntry.Message);
  finally
    memLog.Lines.EndUpdate;
  end;
end;

{ TSynHighlighter }

constructor TSynHighlighter.Create(AOwner: TComponent);
begin
  inherited;

  FCaseSensitive := True;

  FDummyAttributes := TSynHighlighterAttributes.Create('Dummy');
  AddAttribute(FDummyAttributes);

  FCurrentAttribtues := TSynHighlighterAttributes.Create('Current');
end;

destructor TSynHighlighter.Destroy;
begin
  FCurrentAttribtues.Free;
  inherited;
end;

function TSynHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

function TSynHighlighter.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FDummyAttributes;
end;

function TSynHighlighter.GetTokenKind: Integer;
begin
  Result := 0;
end;

procedure TSynHighlighter.Next;
begin
  Inc(Run);
  inherited;
end;

end.
