unit EditorFrameAdvancement;

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

  DatapackView;

type

  TfrmEditorAdvancements = class(TFrame)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TEditorAdvancements = class(TEditor)
  protected
    procedure SaveProc; override;
    procedure LoadProc; override;
    procedure ModifiedChanged; override;

  public
    class function GetFrameClass: TFrameClass; override;

    function Frame: TfrmEditorAdvancements;

  end;

implementation

{$R *.dfm}

{ TEditorAdvancements }

function TEditorAdvancements.Frame: TfrmEditorAdvancements;
begin
  Result := TfrmEditorAdvancements(inherited Frame);
end;

class function TEditorAdvancements.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorAdvancements;
end;

procedure TEditorAdvancements.LoadProc;
begin
  // TODO: Load
end;

procedure TEditorAdvancements.ModifiedChanged;
begin
  // TODO: What?
end;

procedure TEditorAdvancements.SaveProc;
begin
  // TODO: Save
end;

end.
