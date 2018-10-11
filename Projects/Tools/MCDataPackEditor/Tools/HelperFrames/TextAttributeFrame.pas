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
    lbTitle: TLabel;
    lbExample: TLabel;
    Button1: TButton;
  private
    FAttributes: TSynHighlighterAttributes;

    procedure SetAttributes(const Value: TSynHighlighterAttributes);

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

procedure TfrmHighlighterAttributes.SetAttributes(const Value: TSynHighlighterAttributes);
begin
  Attributes.Assign(Value);
end;

procedure TfrmHighlighterAttributes.SetTitle(const Value: string);
begin
  lbTitle.Caption := Value;
end;

procedure TfrmHighlighterAttributes.UpdateAll;
begin                                                    
  lbExample.Font.Color := Attributes.Foreground;
  lbExample.Font.Style := Attributes.Style;
end;

end.
