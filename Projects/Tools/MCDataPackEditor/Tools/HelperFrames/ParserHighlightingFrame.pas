unit ParserHighlightingFrame;

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

  Pengine.Parser,
  Pengine.Collections,

  TextAttributeFrame;

type
  TfrmParserHighlighting = class(TFrame)
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled:
        Boolean);
  public type

    TTokenAttributes = TObjectArray<TfrmHighlighterAttributes>;
    
  private
    FParserClass: TParserClass;
    FTokenAttributes: TTokenAttributes;

    procedure SetParserClass(const Value: TParserClass);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    property ParserClass: TParserClass read FParserClass write SetParserClass;

  end;

implementation

{$R *.dfm}

{ TFrame1 }

constructor TfrmParserHighlighting.Create(AOwner: TComponent);
begin
  inherited;
  FTokenAttributes := TTokenAttributes.Create;
end;

destructor TfrmParserHighlighting.Destroy;
begin
  FTokenAttributes.Free;
  inherited;
end;

procedure TfrmParserHighlighting.FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos:
    TPoint; var Handled: Boolean);
var
  Factor: Integer;
begin
  if Mouse.WheelScrollLines < 0 then
    Factor := -Height
  else
    Factor := 10;

  VertScrollBar.Position := VertScrollBar.Position - WheelDelta * Factor * Mouse.WheelScrollLines div WHEEL_DELTA;
  Handled := True;
end;

procedure TfrmParserHighlighting.SetParserClass(const Value: TParserClass);
var
  I: Integer;
  AttributeFrame: TfrmHighlighterAttributes;
begin
  if ParserClass = Value then
    Exit;

  FTokenAttributes.Clear;

  FParserClass := Value;

  DisableAlign;

  for I := 0 to ParserClass.GetTokenCount do
  begin
    AttributeFrame := FTokenAttributes.Add(TfrmHighlighterAttributes.Create(nil));
    if I = 0 then
      AttributeFrame.Title := 'Default'
    else
      AttributeFrame.Title := ParserClass.GetTokenName(I);
    AttributeFrame.Align := alTop;
    AttributeFrame.AlignWithMargins := True;
    AttributeFrame.Top := I * AttributeFrame.Height;
    AttributeFrame.Parent := Self;
  end;

  EnableAlign;
end;

end.
