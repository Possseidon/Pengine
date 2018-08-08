unit CodeSuggestionBox;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSuggestionBox = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled:
        Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

  end;

var
  frmSuggestionBox: TfrmSuggestionBox;

implementation

{$R *.dfm}

procedure TfrmSuggestionBox.FormCreate(Sender: TObject);
var
  WindowStyle: NativeInt;
begin
  WindowStyle := GetWindowLong(Handle, GWL_STYLE);
  WindowStyle := WindowStyle or WS_THICKFRAME;
  SetWindowLong(Handle, GWL_STYLE, WindowStyle);
  Show;
end;

procedure TfrmSuggestionBox.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
    var Handled: Boolean);
var
  ScrollInfo: tagSCROLLINFO;
begin
  VertScrollBar.Position := VertScrollBar.Position - WheelDelta * Mouse.WheelScrollLines * 20 div WHEEL_DELTA;
  with THintWindow.Create(Self) do
  begin
    Parent := Self;
    Text := 'Hello World!';
  end;
  Handled := True;
end;

procedure TfrmSuggestionBox.FormPaint(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 200 do
    Canvas.TextOut(10, I * 20 - VertScrollBar.ScrollPos, 'Hello World! ' + I.ToString);
end;

procedure TfrmSuggestionBox.FormResize(Sender: TObject);
begin
  VertScrollBar.Range := 4000;
end;

end.

