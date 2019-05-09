unit SynEditorFrame;

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

  SynEdit;

type

  TfrmSynEditor = class(TFrame)
    synEditor: TSynEdit;
    procedure synEditorKeyPress(Sender: TObject; var Key: Char);
  private

  public

  end;

implementation

{$R *.dfm}

procedure TfrmSynEditor.synEditorKeyPress(Sender: TObject; var Key: Char);
var
  KeyboardState: TKeyboardState;
  ShiftState: TShiftState;
begin
  // Don't add spaces if control is held down even if no suggestions exist
  if Key = #32 then
  begin
    GetKeyboardState(KeyboardState);
    ShiftState := KeyboardStateToShiftState(KeyboardState);
    if ssCtrl in ShiftState then
      Key := #0;
  end;
end;

end.
