unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  SynEdit,
  SynEditHighlighter,
  SynEditKeyCmds,
  SynCompletionProposal,

  Pengine.MC.EntitySelector,
  Pengine.IntMaths,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Parser,
  Pengine.TimeManager;

type

  TLine = class
  private
    FText: string;
    FErrorMessage: string;
    FSuccess: Boolean;
    FSelector: TEntitySelector;
    FContext: TParseInfo.TContext;

    procedure SetText(const Value: string);

  public
    constructor Create(AText: string);
    destructor Destroy; override;

    property Text: string read FText write SetText;
    property Context: TParseInfo.TContext read FContext;
    property Selector: TEntitySelector read FSelector;
    property Success: Boolean read FSuccess;
    property ErrorMessage: string read FErrorMessage;

  end;

  TLines = TObjectArray<TLine>;
  TLinesRef = TRefArray<TLine>;

  THighlighter = class(TSynCustomHighlighter)
  public type

    TTokenAttributes = TObjectArray<TSynHighlighterAttributes>;
    TAttributes = TClassObjectMap<TTokenAttributes>;

  private
    FAttributes: TAttributes;
    FDefaultAttributes: TSynHighlighterAttributes;
    FErrorAttributes: TSynHighlighterAttributes;
    FLines: TLinesRef;

  protected
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;

  public
    constructor Create(AOwner: TComponent; ALines: TLinesRef); reintroduce;
    destructor Destroy; override;

    property Attributes: TAttributes read FAttributes;

    function GetEol: Boolean; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;

  end;

  TfrmMain = class(TForm)
    synEdit: TSynEdit;
    btnAutoFormat: TButton;
    lbError: TLabel;
    synCompletion: TSynCompletionProposal;
    procedure btnAutoFormatClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure synCompletionExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: Integer;
        var CanExecute: Boolean);
    procedure synEditChange(Sender: TObject);
  private
    FLines: TLines;

  protected
    procedure UpdateActions; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.btnAutoFormatClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FLines.MaxIndex do
    if FLines[I].Selector <> nil then
      SynEdit.Lines[I] := FLines[I].Selector.Format;

  synEditChange(nil);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLines.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Highlighter: THighlighter;
  Attributes: THighlighter.TTokenAttributes;
  I: Integer;
begin
  FLines := TLines.Create;

  Highlighter := THighlighter.Create(Self, FLines);

  SynEdit.Highlighter := Highlighter;
  SynEdit.OnChange(nil);

  // TEntitySelector
  Attributes := THighlighter.TTokenAttributes.Create;
  Highlighter.Attributes[TEntitySelector.TParser] := Attributes;
  Attributes.Capacity := TEntitySelector.TParser.GetTokenCount + 1;
  Attributes.ForceCount(Attributes.Capacity);
  for I := 0 to Attributes.MaxIndex do
    Attributes[I] := TSynHighlighterAttributes.Create('', '');
  with TEntitySelector.TParser do
  begin
    Attributes[TokenPrefix].Foreground := $007FFF;
    Attributes[TokenPrefix].Style := [fsBold];
    Attributes[TokenVariable].Foreground := $0000FF;
    Attributes[TokenVariable].Style := [fsBold];
    Attributes[TokenBrackets].Foreground := $FF3F3F;
    Attributes[TokenBrackets].Style := [fsBold];
    Attributes[TokenComma].Foreground := $FF3F3F;
    Attributes[TokenComma].Style := [fsBold];
  end;

  // TEntitySelector.TOption
  Attributes := THighlighter.TTokenAttributes.Create;
  Highlighter.Attributes[TEntitySelector.TOption.TParser] := Attributes;
  Attributes.Capacity := TEntitySelector.TOption.TParser.GetTokenCount + 1;
  Attributes.ForceCount(Attributes.Capacity);
  for I := 0 to Attributes.MaxIndex do
    Attributes[I] := TSynHighlighterAttributes.Create('', '');
  with TEntitySelector.TOption.TParser do
  begin
    Attributes[TokenOption].Foreground := $3FBF3F;
    Attributes[TokenEquals].Foreground := $007F00;
    Attributes[TokenInvert].Foreground := $0000FF;
  end;

  // TOption
  Attributes := THighlighter.TTokenAttributes.Create;
  Highlighter.Attributes[TStringOrIdentParser] := Attributes;
  Attributes.Capacity := TStringOrIdentParser.GetTokenCount + 1;
  Attributes.ForceCount(Attributes.Capacity);
  for I := 0 to Attributes.MaxIndex do
    Attributes[I] := TSynHighlighterAttributes.Create('', '');
  with TStringOrIdentParser do
  begin
    Attributes[TokenQuote].Foreground := $FF0000;
    Attributes[TokenContent].Foreground := $FF0000;
    Attributes[TokenBackslash].Foreground := $FF7F7F;
    Attributes[TokenEscaped].Foreground := $FF7F7F;
    Attributes[TokenUnquoted].Foreground := $007FFF;
  end;
end;

procedure TfrmMain.synCompletionExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string; var x, y:
    Integer; var CanExecute: Boolean);
var
  Context: TParseInfo.TContext;
  Suggestions: TParseSuggestionsClass;
  I: Integer;
begin
  synCompletion.ClearList;

  if not FLines.RangeCheck(synEdit.CaretY - 1) then
    Exit;
  Context := FLines[synEdit.CaretY - 1].Context;
  if Context = nil then
    Exit;

  Suggestions := Context.Suggestions[synEdit.CaretX - 1];
  if Suggestions = nil then
    Suggestions := Context.Suggestions[synEdit.CaretX - 2];
  if Suggestions = nil then
    Exit;

  for I := 0 to Suggestions.GetCount - 1 do
  begin
    synCompletion.AddItem(Suggestions.GetDisplayText(I), Suggestions.GetInsertText(I));
  end;

  synCompletion.NbLinesInWindow := 8;
end;

procedure TfrmMain.synEditChange(Sender: TObject);
var
  I: Integer;
begin
  while FLines.Count > SynEdit.Lines.Count do
    FLines.RemoveLast;
  for I := 0 to FLines.MaxIndex do
    FLines[I].Text := SynEdit.Lines[I];
  while FLines.Count < SynEdit.Lines.Count do
    FLines.Add(TLine.Create(SynEdit.Lines[FLines.Count]));
end;

procedure TfrmMain.UpdateActions;
begin
  inherited;
  if FLines.RangeCheck(synEdit.CaretY - 1) and FLines[synEdit.CaretY - 1].ErrorMessage.IsEmpty then
  begin
    lbError.Caption := '';
    lbError.Height := 0;
  end
  else
  begin
    lbError.Caption := 'Error: ' + FLines[synEdit.CaretY - 1].ErrorMessage;
  end;
end;

{ TLine }

constructor TLine.Create(AText: string);
begin
  Text := AText;
end;

destructor TLine.Destroy;
begin
  FSelector.Free;
  FContext.Free;
  inherited;
end;

procedure TLine.SetText(const Value: string);
var
  Parser: TEntitySelector.TParser;
begin
  if Text = Value then
    Exit;
  FText := Value;

  FreeAndNil(FSelector);
  FreeAndNil(FContext);

  Parser := TEntitySelector.TParser.Create(Text);
  FSuccess := Parser.Success;
  if Success then
  begin
    FSelector := Parser.OwnParseResult;
    FErrorMessage := '';
  end
  else
  begin
    FErrorMessage := Parser.ErrorMessage;
  end;
  FContext := Parser.OwnContext;
  Parser.Free;
end;

{ THighlighter }

constructor THighlighter.Create(AOwner: TComponent; ALines: TLinesRef);
begin
  inherited Create(AOwner);
  FLines := ALines;

  FAttributes := TAttributes.Create;

  FDefaultAttributes := TSynHighlighterAttributes.Create('Default', 'Default');
  AddAttribute(FDefaultAttributes);

  FErrorAttributes := TSynHighlighterAttributes.Create('Error', 'Error');
  FErrorAttributes.Style := [fsItalic];
  FErrorAttributes.Foreground := $0000FF;
  AddAttribute(FErrorAttributes);

end;

destructor THighlighter.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

function THighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := FDefaultAttributes;
end;

function THighlighter.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function THighlighter.GetTokenAttribute: TSynHighlighterAttributes;
var
  Pos: Integer;
  Line: TLine;
  Parser: TParserClass;
  Token: Integer;
  TokenAttributes: TTokenAttributes;
begin
  if not FLines.RangeCheck(fLineNumber) then
    Exit(FDefaultAttributes);

  Line := FLines[fLineNumber];

  if Line = nil then
    Exit(FDefaultAttributes);

  if Line.Context = nil then
    Exit(FDefaultAttributes);

  Pos := Run - 1;

  if Pos >= Line.Context.Count then
    Exit(FErrorAttributes);

  Token := Line.Context.Tokens[Pos];
  Parser := Line.Context.Parsers[Pos].Last;

  if not Attributes.Get(Parser, TokenAttributes) then
    Exit(FDefaultAttributes);

  if not TokenAttributes.RangeCheck(Token) then
    Exit(FDefaultAttributes);

  Result := TokenAttributes[Token];
end;

function THighlighter.GetTokenKind: Integer;
begin
  Result := 0;
end;

procedure THighlighter.Next;
begin
  Inc(Run);
  inherited;
end;

end.
