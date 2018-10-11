unit EditorFrameTag;

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

  TfrmEditorTags = class(TFrame)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TEditorTags = class(TEditor)
  protected
    procedure SaveProc; override;
    procedure LoadProc; override;
    procedure ModifiedChanged; override;

  public
    class function GetFrameClass: TFrameClass; override;

    function Frame: TfrmEditorTags;

  end;

implementation

{$R *.dfm}

{ TEditorTags}

function TEditorTags.Frame: TfrmEditorTags;
begin
  Result := TfrmEditorTags(inherited Frame);
end;

class function TEditorTags.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorTags;
end;

procedure TEditorTags.LoadProc;
begin
  // TODO: Load
end;

procedure TEditorTags.ModifiedChanged;
begin
  // TODO: What?
end;

procedure TEditorTags.SaveProc;
begin
  // TODO: Save
end;

end.
