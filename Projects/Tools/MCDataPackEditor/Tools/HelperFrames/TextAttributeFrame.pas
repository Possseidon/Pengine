unit TextAttributeFrame;

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
  Vcl.StdCtrls,
  Vcl.CheckLst,
  Vcl.ExtCtrls,
  SynEditHighlighter;

type

  TfrmHighlighterAttributes = class(TFrame)
    cbColor: TColorBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderline: TCheckBox;
    lbTitle: TLabel;
    procedure cbBoldClick(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure cbItalicClick(Sender: TObject);
    procedure cbUnderlineClick(Sender: TObject);
  private
    FAttributes: TSynHighlighterAttributes;

    procedure SetAttributes(const Value: TSynHighlighterAttributes);
    procedure SetStyle(AStyle: TFontStyle; AInclude: Boolean);

    procedure UpdateAll;
    procedure AttributesChange(Sender: TObject);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Attributes: TSynHighlighterAttributes read FAttributes write SetAttributes;

    property Title: string read GetTitle write SetTitle;

  end;

implementation

{$R *.dfm}

{ TFrame1 }

constructor TfrmHighlighterAttributes.Create(AOwner: TComponent);
begin
  inherited;
  FAttributes := TSynHighlighterAttributes.Create('', '');
  FAttributes.OnChange := AttributesChange;
end;

destructor TfrmHighlighterAttributes.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

function TfrmHighlighterAttributes.GetTitle: string;
begin
  Result := lbTitle.Caption;
end;

procedure TfrmHighlighterAttributes.AttributesChange(Sender: TObject);
begin
  UpdateAll;  
end;

procedure TfrmHighlighterAttributes.cbBoldClick(Sender: TObject);
begin
  SetStyle(fsBold, cbBold.Checked);
end;

procedure TfrmHighlighterAttributes.cbColorChange(Sender: TObject);
begin
  Attributes.Foreground := cbColor.Selected;
end;

procedure TfrmHighlighterAttributes.cbItalicClick(Sender: TObject);
begin
  SetStyle(fsItalic, cbItalic.Checked);
end;

procedure TfrmHighlighterAttributes.cbUnderlineClick(Sender: TObject);
begin
  SetStyle(fsUnderline, cbUnderline.Checked);
end;

procedure TfrmHighlighterAttributes.SetAttributes(const Value: TSynHighlighterAttributes);
begin
  Attributes.Assign(Value);
end;

procedure TfrmHighlighterAttributes.SetStyle(AStyle: TFontStyle; AInclude: Boolean);
begin
  if AInclude then
    Attributes.Style := Attributes.Style + [AStyle]
  else
    Attributes.Style := Attributes.Style - [AStyle];
end;

procedure TfrmHighlighterAttributes.SetTitle(const Value: string);
begin
  lbTitle.Caption := Value;
end;

procedure TfrmHighlighterAttributes.UpdateAll;
begin                                                    
  cbColor.Selected := Attributes.Foreground;
  cbBold.Checked := fsBold in Attributes.Style;
  cbItalic.Checked := fsItalic in Attributes.Style;
  cbUnderline.Checked := fsUnderline in Attributes.Style;
end;

end.
