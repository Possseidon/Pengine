unit EditorFrameFunction;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  System.ImageList,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ImgList,

  SynEdit,
  SynEditHighlighter,
  SynEditKeyCmds,
  SynEditorFrame,
  SynEditTextBuffer,
  SynEditTypes,
  SynCompletionProposal,

  Pengine.Collections,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.Parser,
  Pengine.IntMaths,

  Pengine.MC.Brigadier,
  Pengine.MC.EntitySelector,

  DatapackView,
  FunctionTheme,
  Pengine.MC.BrigadierParser;

type

  TLinesMap = class
  public type

    TMap = TToObjectMap<string, TLine, TStringHasher>;

  private
    FMap: TMap;
    FSettings: TBrigadierCommandParser.TSettings;

    function GetLine(AText: string): TLine;

  public
    constructor Create;
    destructor Destroy; override;

    property Settings: TBrigadierCommandParser.TSettings read FSettings;
    property Lines[AText: string]: TLine read GetLine; default;

    procedure RemoveUnreferenced;

  end;

  TfrmEditorFunctions = class(TFrame)
    lvErrors: TListView;
    splLog: TSplitter;
    ilFunctions: TImageList;
    frmSynEditor: TfrmSynEditor;
    synCompletion: TSynCompletionProposal;
    procedure frmSynEditorsynEditorPaint(Sender: TObject; ACanvas: TCanvas);
    procedure lvErrorsDblClick(Sender: TObject);
    procedure synCompletionExecute(Kind: SynCompletionType; Sender: TObject; var CurrentInput: string;
      var X, Y: Integer; var CanExecute: Boolean);
    procedure synEditorChange(Sender: TObject);
    procedure synEditorCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: Char;
      Data: Pointer);
    procedure synEditorGutterPaint(Sender: TObject; ALine, X, Y: Integer);
    procedure synEditorSpecialLineColors(Sender: TObject; ALine: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure synEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FLines: TLines;
    FLinesMap: TLinesMap;
    FHighlighter: TFunctionHighlighter;
    FErrorImages: TObjectArray<TBitmap>;

    procedure ReplaceTabsWithSpaces;
    function GetSynEditor: TSynEdit;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Format;

    property synEditor: TSynEdit read GetSynEditor;

  end;

  TEditorFunctions = class(TEditor)
  protected
    procedure SaveProc; override;
    procedure LoadProc; override;
    procedure ModifiedChanged; override;

  public
    class function GetFrameClass: TFrameClass; override;

    function Frame: TfrmEditorFunctions;

    procedure Format;

  end;

implementation

uses
  Main;

{$R *.dfm}

{ TLinesMap }

constructor TLinesMap.Create;
var
  Brigadier: TBrigadierRoot;
begin
  Brigadier := frmMain.Settings.Sub<TBrigadierSettings>.Brigadier;
  FSettings := TBrigadierCommandParser.TSettings.Create(Brigadier);
  FSettings.SlashMode := smNone;
  FSettings.AllowComment := True;
  FSettings.AllowPreceedingSpace := False;
  FSettings.AllowEmpty := True;
  FMap := TMap.Create;
end;

destructor TLinesMap.Destroy;
begin
  FMap.Free;
  FSettings.Free;
  inherited;
end;

function TLinesMap.GetLine(AText: string): TLine;
begin
  if not FMap.Get(AText, Result) then
  begin
    Result := TLine.Create(FSettings, AText);
    FMap[AText] := Result;
  end;
  Result.AddRef;
end;

procedure TLinesMap.RemoveUnreferenced;
var
  Unreferenced: TArray<string>;
  Pair: TMap.TPair;
  Text: string;
begin
  Unreferenced := TArray<string>.Create;
  try
    for Pair in FMap do
      if not Pair.Value.HasReference then
        Unreferenced.Add(Pair.Key);

    for Text in Unreferenced do
      FMap.Remove(Text);

  finally
    Unreferenced.Free;
  end;
end;

{ TfrmEditorFunctions }

constructor TfrmEditorFunctions.Create(AOwner: TComponent);
begin
  inherited;

  synEditor.Lines.WriteBOM := False;

  FErrorImages := TObjectArray<TBitmap>.Create;
  ilFunctions.GetBitmap(0, FErrorImages.Add(TBitmap.Create));
  ilFunctions.GetBitmap(1, FErrorImages.Add(TBitmap.Create));
  ilFunctions.GetBitmap(2, FErrorImages.Add(TBitmap.Create));
  ilFunctions.GetBitmap(2, FErrorImages.Add(TBitmap.Create));

  FLinesMap := TLinesMap.Create;
  FLines := TLines.Create;

  FHighlighter := TFunctionHighlighter.Create(Self);
  FHighlighter.Lines := FLines.Reader;
  FHighlighter.Theme := frmMain.FunctionTheme;

  synEditor.Highlighter := FHighlighter;

end;

destructor TfrmEditorFunctions.Destroy;
begin
  FErrorImages.Free;
  FLines.Free;
  FLinesMap.Free;
  inherited;
end;

procedure TfrmEditorFunctions.Format;
var
  I: Integer;
  AnyChanged: Boolean;
  Formatted: string;
  StartCoord, EndCoord: TBufferCoord;
  OldCaret: TBufferCoord;
begin
  OldCaret := synEditor.CaretXY;
  AnyChanged := False;

  synEditor.BeginUndoBlock;
  synEditor.UndoList.AddChange(crCaret, OldCaret, OldCaret, '', smNormal);
  synEditor.UndoList.AddChange(crSelection, OldCaret, OldCaret, '', smNormal);
  try
    for I := synEditor.Lines.Count - 1 downto 0 do
    begin
      if FLines.RangeCheck(I) and (FLines[I].Command <> nil) and (FLines[I].Context.HighestErrorLevel <= elError) then
      begin
        Formatted := FLines[I].Command.Format;
        if Formatted <> synEditor.Lines[I] then
        begin
          StartCoord := BufferCoord(1, I + 1);
          EndCoord := BufferCoord(synEditor.Lines[I].Length + 1, I + 1);
          synEditor.UndoList.AddChange(crDelete, StartCoord, EndCoord, '', smNormal);
          EndCoord := BufferCoord(Formatted.Length + 1, I + 1);
          synEditor.UndoList.AddChange(crInsert, StartCoord, EndCoord, synEditor.Lines[I], smNormal);
          AnyChanged := True;
          synEditor.Lines[I] := Formatted;
        end;
      end;
    end;

    synEditor.UndoList.AddChange(crSelection, OldCaret, OldCaret, '', smNormal);
    synEditor.UndoList.AddChange(crCaret, OldCaret, OldCaret, '', smNormal);

    if not AnyChanged then
    begin
      synEditor.Undo;
      synEditor.RedoList.Clear;
    end;

  finally
    synEditor.EndUndoBlock;
  end;

  synEditorChange(nil);
end;

procedure TfrmEditorFunctions.frmSynEditorsynEditorPaint(Sender: TObject; ACanvas: TCanvas);
var
  I: Integer;
  Err: TParseError;
  DisplayPos: TDisplayCoord;
  P: TPoint;
  OldPenStyle: TPenStyle;
  Line: TLine;
  LastParam: TBrigadierChild;
  LineHint: string;
  OldBrushStyle: TBrushStyle;
begin
  // TODO: Fix for windows XP, as cursor movement isn't invalidating the new line and therefore doesn't update command help

  OldPenStyle := ACanvas.Pen.Style;
  OldBrushStyle := ACanvas.Brush.Style;
  for I := 0 to FLines.MaxIndex do
  begin
    for Err in FLines[I].Context.Log.InReverse do
    begin
      ACanvas.Pen.Width := 1;
      case Err.Level of
        elNone:
          begin
            ACanvas.Pen.Style := psDot;
            ACanvas.Pen.Color := clGray;
          end;
        elHint:
          begin
            ACanvas.Pen.Style := psDot;
            ACanvas.Pen.Color := $FF7F7F;
          end;
        elWarning:
          begin
            ACanvas.Pen.Style := psDot;
            ACanvas.Pen.Color := $007FFF;
          end;
        elError, elFatal:
          begin
            ACanvas.Pen.Style := psDot;
            ACanvas.Pen.Color := $0000FF;
          end;
      end;

      DisplayPos := synEditor.BufferToDisplayPos(BufferCoord(Err.Position.LinePos, I + 1));
      P := synEditor.RowColumnToPixels(DisplayPos);
      P.Y := P.Y - synEditor.Font.Height;
      ACanvas.MoveTo(P.X, P.Y);

      DisplayPos := synEditor.BufferToDisplayPos(BufferCoord(Err.Position.LinePos + Err.Length, I + 1));
      P := synEditor.RowColumnToPixels(DisplayPos);
      P.Y := P.Y - synEditor.Font.Height;
      ACanvas.LineTo(P.X, P.Y);
    end;
  end;

  if FLines.RangeCheck(synEditor.CaretY - 1) then
  begin
    Line := FLines[synEditor.CaretY - 1];
    if (Line.Command <> nil) and (Line.Context.HighestErrorLevel <= elError) then
    begin
      if not Line.Command.Parameters.Empty then
      begin
        LastParam := Line.Command.Parameters.Last.Child;
        if not LastParam.Arguments.Empty or not LastParam.Literals.Empty then
        begin
          if LastParam.Arguments.Empty then
            LineHint := '...'
          else
          begin
            LineHint := LastParam.Arguments.First.Name;
            for I := 1 to LastParam.Arguments.MaxIndex do
              LineHint := LineHint + '|' + LastParam.Arguments[I].Name;
            if not LastParam.Literals.Empty then
              LineHint := LineHint + '|...';
          end;
          if LastParam.Executable then
            LineHint := '[' + LineHint + ']'
          else
            LineHint := '<' + LineHint + '>';

          DisplayPos := synEditor.BufferToDisplayPos(
            BufferCoord(synEditor.Lines[synEditor.CaretY - 1].Length + 2, synEditor.CaretY));
          P := synEditor.RowColumnToPixels(DisplayPos);
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := synEditor.ActiveLineColor;
          ACanvas.Font.Color := clGrayText;
          ACanvas.TextOut(P.X, P.Y, LineHint);
        end;

      end;
    end;
  end;
  ACanvas.Brush.Style := OldBrushStyle;
  ACanvas.Pen.Style := OldPenStyle;
end;

function TfrmEditorFunctions.GetSynEditor: TSynEdit;
begin
  Result := frmSynEditor.synEditor;
end;

procedure TfrmEditorFunctions.lvErrorsDblClick(Sender: TObject);
begin
  if lvErrors.Selected <> nil then
  begin
    synEditor.CaretY := Integer(lvErrors.Selected.Data) + 1;
    synEditor.CaretX := lvErrors.Selected.Caption.Split([':'])[1].ToInteger;
    synEditor.SetFocus;
  end;
end;

procedure TfrmEditorFunctions.ReplaceTabsWithSpaces;
var
  I, X, R: Integer;
  Rep: string;
begin
  for I := 0 to synEditor.Lines.Count - 1 do
  begin
    X := 1;
    while X <= synEditor.Lines[I].Length do
    begin
      if synEditor.Lines[I][X] = #9 then
      begin
        Rep := ' ';
        for R := 2 to synEditor.TabWidth - (X - 1) mod synEditor.TabWidth do
          Rep := Rep + ' ';
        synEditor.Lines[I] := StuffString(synEditor.Lines[I], X, 1, Rep);
        Inc(X, Length(Rep));
      end;
      Inc(X);
    end;
  end;
  synEditorChange(nil);
end;

procedure TfrmEditorFunctions.synCompletionExecute(Kind: SynCompletionType; Sender: TObject;
  var CurrentInput: string;
  var X, Y: Integer; var CanExecute: Boolean);
var
  Context: TParseInfo.TContext;
  Line, Pos, I: Integer;
  Suggestion: TParseSuggestion;
begin
  synCompletion.ClearList;

  Line := synEditor.CaretY - 1;
  if not FLines.RangeCheck(Line) or (FLines[Line] = nil) then
    Exit;

  Context := FLines[Line].Context;

  Pos := synEditor.CaretX - 1;
  if not Context.HasSuggestions(Pos) then
    Exit;

  synCompletion.Title := Context.SuggestionTitle[Pos];
  for I := 0 to Context.SuggestionCount[Pos] - 1 do
  begin
    Suggestion := Context.Suggestion[Pos, I];
    synCompletion.AddItem(Suggestion.Display, Suggestion.Insert);
  end;

  CanExecute := synCompletion.ItemList.Count > 0;
end;

procedure TfrmEditorFunctions.synEditorChange(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
  Err: TParseError;
begin
  while FLines.Count > synEditor.Lines.Count do
  begin
    if FLines.Last <> nil then
      FLines.Last.RemoveRef;
    FLines.RemoveLast;
  end;

  for I := 0 to FLines.MaxIndex do
  begin
    if FLines[I] <> nil then
    begin
      if FLines[I].Text = synEditor.Lines[I] then
        Continue;
      FLines[I].RemoveRef;
    end;
    FLines[I] := FLinesMap[synEditor.Lines[I]];
  end;

  while FLines.Count < synEditor.Lines.Count do
    FLines.Add(FLinesMap[synEditor.Lines[FLines.Count]]);

  FLinesMap.RemoveUnreferenced;

  lvErrors.Items.BeginUpdate;
  try
    lvErrors.Clear;
    for I := 0 to FLines.MaxIndex do
    begin
      for Err in FLines[I].Context.Log do
      begin
        Item := lvErrors.Items.Add;
        Item.Caption := (I + 1).ToString + ':' + Err.Position.LinePos.ToString;
        Item.SubItems.Add(Err.LevelName);
        Item.SubItems.Add(Err.Message);
        Item.Data := TCustomData(I);
      end;
    end;
  finally
    lvErrors.Items.EndUpdate;
  end;
end;

procedure TfrmEditorFunctions.synEditorCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar:
  Char; Data: Pointer);
begin
  case Command of
    ecPaste:
      ReplaceTabsWithSpaces;
  end;
end;

procedure TfrmEditorFunctions.synEditorGutterPaint(Sender: TObject; ALine, X, Y: Integer);
var
  Level: TParseError.TLevel;
begin
  ALine := ALine - 1;
  if FLines.RangeCheck(ALine) and (FLines[ALine] <> nil) then
  begin
    Level := FLines[ALine].Context.HighestErrorLevel;
    if Level > elNone then
      synEditor.Canvas.Draw(X, Y, FErrorImages[Ord(Level) - 1]);
  end;
end;

procedure TfrmEditorFunctions.synEditorSpecialLineColors(Sender: TObject; ALine: Integer; var Special: Boolean;
  var FG, BG: TColor);
begin
  Exit;
  if synEditor.CaretY = ALine then
    Exit;
  ALine := ALine - 1;
  Special := FLines.RangeCheck(ALine) and (FLines[ALine] <> nil) and not FLines[ALine].Context.Success;
  if Special then
    BG := $CFCFFF;
end;

procedure TfrmEditorFunctions.synEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scModified in Changes then
    Editor.Modified := synEditor.Modified;
end;

{ TEditorFunctions }

procedure TEditorFunctions.Format;
begin
  Frame.Format;
end;

function TEditorFunctions.Frame: TfrmEditorFunctions;
begin
  Result := TfrmEditorFunctions(inherited Frame);
end;

class
  function TEditorFunctions.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorFunctions;
end;

procedure TEditorFunctions.LoadProc;
begin
  Frame.synEditor.Lines.LoadFromFile(NodeData.FullPath, TEncoding.UTF8);
  Frame.synEditorChange(nil);
end;

procedure TEditorFunctions.ModifiedChanged;
begin
  Frame.synEditor.Modified := Modified;
end;

procedure TEditorFunctions.SaveProc;
begin
  Frame.synEditor.Lines.SaveToFile(NodeData.FullPath, TEncoding.UTF8);
end;

end.
