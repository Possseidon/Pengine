unit EditorFrameFunction;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  SynEdit,
  SynEditHighlighter,

  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,

  Minecraft.Brigadier,

  DatapackView;

type

  TFunctionHighlighter = class(TSynCustomHighlighter)
  public type

    TToken = (
      tkComment,
      tkCommand,
      tkSubCommand,
      tkUnknown
    );

  private
    FSynEdit: TSynEdit;
    FToken: TToken;
    FAttributes: array [TToken] of TSynHighlighterAttributes;

  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;

  public
    constructor Create(AOwner: TComponent); override;

    property SynEdit: TSynEdit read FSynEdit write FSynEdit;

    function GetEol: Boolean; override;
    procedure Next; override;
    function GetTokenKind: Integer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;

  end;

  TEditorFunctions = class(TEditor)
  public
    function GetFrameClass: TFrameClass; override;

  end;

  TLineData = class
  private
    FBrigadier: TBrigadierRoot;
    FText: string;
    FParseResult: TBrigadierParseResult;

    procedure SetText(const Value: string);
    procedure SetBrigadier(const Value: TBrigadierRoot);

  public
    constructor Create(ABrigadier: TBrigadierRoot; AText: string);
    destructor Destroy; override;

    property Brigadier: TBrigadierRoot read FBrigadier write SetBrigadier;
    property Text: string read FText write SetText;
    property ParseResult: TBrigadierParseResult read FParseResult;

  end;

  TfrmEditorFunctions = class(TFrame)
    SynEdit: TSynEdit;
    procedure synEditChange(Sender: TObject);
    procedure synEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FHighlighter: TFunctionHighlighter;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

uses
  Main;

{$R *.dfm}

{ TfrmEditorFunctions }

constructor TfrmEditorFunctions.Create(AOwner: TComponent);
begin
  inherited;
  FHighlighter := TFunctionHighlighter.Create(Self);
  FHighlighter.SynEdit := synEdit;
  synEdit.Highlighter := FHighlighter;
  synEdit.Text := TFile.ReadAllText(Editor.NodeData.FullPath);
  synEditChange(nil);
end;

procedure TfrmEditorFunctions.synEditChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to synEdit.Lines.Count - 1 do
  begin
    if synEdit.Lines.Objects[I] = nil then
    begin
      synEdit.Lines.Objects[I] := TLineData.Create(frmMain.Brigadier, synEdit.Lines[I]);
    end
    else
    begin
      TLineData(synEdit.Lines.Objects[I]).Text := synEdit.Lines[I];
    end;
  end;
end;

procedure TfrmEditorFunctions.synEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scModified in Changes then  
    Editor.Modified := synEdit.Modified;
end;

{ TEditorFunctions }

function TEditorFunctions.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorFunctions;
end;

{ THighlighter }

constructor TFunctionHighlighter.Create(AOwner: TComponent);
begin
  inherited;

  fCaseSensitive := True;

  FAttributes[tkComment] := TSynHighlighterAttributes.Create('Comment', 'Comment');
  FAttributes[tkComment].Foreground := clGreen;
  FAttributes[tkComment].Style := [fsBold];
  AddAttribute(FAttributes[tkComment]);

  FAttributes[tkUnknown] := TSynHighlighterAttributes.Create('Default', 'Default');
  AddAttribute(FAttributes[tkUnknown]);
end;

function TFunctionHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

function TFunctionHighlighter.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TFunctionHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FAttributes[FToken];
end;

function TFunctionHighlighter.GetTokenKind: Integer;
begin
  Result := Ord(FToken);
end;

procedure TFunctionHighlighter.Next;
begin
  fTokenPos := Run;
  FToken := tkUnknown;
  Inc(Run);
  inherited;
end;

{ TLineData }

constructor TLineData.Create(ABrigadier: TBrigadierRoot; AText: string);
begin
  FBrigadier := ABrigadier;
  Text := AText;
end;

destructor TLineData.Destroy;
begin
  FParseResult.Free;
  inherited;
end;

procedure TLineData.SetBrigadier(const Value: TBrigadierRoot);
begin
  FBrigadier := Value;
end;

procedure TLineData.SetText(const Value: string);
var
  Parser: TBrigadierCommandParser;
begin
  if Text = Value then
    Exit;
  FText := Value;
  FParseResult.Free;
  if Text.StartsWith('#') then
    FParseResult := nil
  else
  begin
    Parser := TBrigadierCommandParser.Create(Brigadier, Value);
    FParseResult := Parser.ParseResult;
    Parser.Free;
  end;
end;

end.
