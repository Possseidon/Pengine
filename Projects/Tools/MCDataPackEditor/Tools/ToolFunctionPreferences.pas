unit ToolFunctionPreferences;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  TextAttributeFrame,
  ParserHighlightingFrame,
  Pengine.MC.Brigadier,
  Pengine.MC.EntitySelector,
  SynEditorFrame,
  Vcl.Menus,
  FunctionTheme, Pengine.Parser;

type

  TfrmFunctionPreferences = class(TForm)
    pnlBottom: TPanel;
    btnCancel: TButton;
    btnOk: TButton;
    cbBackground: TColorBox;
    cbCurrentLine: TColorBox;
    lbBackground: TLabel;
    lbCurrentLineBackground: TLabel;
    btnSelectFont: TButton;
    FontDialog: TFontDialog;
    gbBasic: TGroupBox;
    pnlPreset: TPanel;
    btnLoadTheme: TButton;
    btnSaveTheme: TButton;
    gbSyntaxHighlighting: TGroupBox;
    pnlElementSettings: TPanel;
    pnlElement: TPanel;
    cbElement: TComboBox;
    phElementHighlighting: TfrmParserHighlighting;
    frmSynPreview: TfrmSynEditor;
    splPreview: TSplitter;
    lbTextDefault: TLabel;
    cbTextDefault: TColorBox;
    cbTextDefaultBold: TCheckBox;
    cbTextItalic: TCheckBox;
    cbTextUndeline: TCheckBox;
    cbCommentBold: TCheckBox;
    cbCommentItalic: TCheckBox;
    cbCommentUnderline: TCheckBox;
    cbComment: TColorBox;
    lbComment: TLabel;
    cbErrorBold: TCheckBox;
    cbErrorItalic: TCheckBox;
    cbErrorUnderline: TCheckBox;
    cbError: TColorBox;
    lbError: TLabel;
    cbPreset: TComboBox;
    lbLoadPreset: TLabel;
    lbElemenr: TLabel;
    procedure btnSelectFontClick(Sender: TObject);
    procedure cbElementChange(Sender: TObject);
  private
    FTheme: TFunctionTheme;

    procedure LoadTheme(ATheme: TFunctionTheme);

    procedure InitPresets;
    procedure InitHighlightElements;

  public
    constructor Create(AOwner: TComponent); override;

    function Execute(ATheme: TFunctionTheme): Boolean;

  end;

var
  frmFunctionPreferences: TfrmFunctionPreferences;

implementation

{$R *.dfm}

procedure TfrmFunctionPreferences.btnSelectFontClick(Sender: TObject);
begin
  raise ENotImplemented.Create('Font Dialog');
end;

{ TfrmFunctionPreferences }

constructor TfrmFunctionPreferences.Create(AOwner: TComponent);
begin
  inherited;
  InitPresets;
  InitHighlightElements;
end;

procedure TfrmFunctionPreferences.cbElementChange(Sender: TObject);
begin
  phElementHighlighting.ParserClass := TFunctionTheme.ParserClasses[cbElement.ItemIndex];
end;

function TfrmFunctionPreferences.Execute(ATheme: TFunctionTheme): Boolean;
begin
  LoadTheme(ATheme);
  FTheme := ATheme.Copy;
  try
    Result := ShowModal = mrOk;
    if Result then
      ATheme.Assign(FTheme);
  finally
    FTheme.Free;
  end;
end;

procedure TfrmFunctionPreferences.InitHighlightElements;
var
  ParserClass: TParserClass;
begin
  cbElement.Clear;
  for ParserClass in TFunctionTheme.ParserClasses do
    cbElement.Items.Add(ParserClass.GetResultName);
end;

procedure TfrmFunctionPreferences.InitPresets;
var
  PresetClass: TFunctionTheme.TPresetClass;
begin
  cbPreset.Clear;
  for PresetClass in TFunctionTheme.Presets do
    cbPreset.Items.Add(PresetClass.GetName);
end;

procedure TfrmFunctionPreferences.LoadTheme(ATheme: TFunctionTheme);
begin

end;

end.
