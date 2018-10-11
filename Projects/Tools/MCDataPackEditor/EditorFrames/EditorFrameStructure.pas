unit EditorFrameStructure;

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
  TfrmEditorStructures = class(TFrame)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TEditorStructures = class(TEditor)
  protected
    procedure SaveProc; override;
    procedure LoadProc; override;
    procedure ModifiedChanged; override;

  public
    class function GetFrameClass: TFrameClass; override;

    function Frame: TfrmEditorStructures;

  end;

implementation

{$R *.dfm}

{ TEditorStructures }

function TEditorStructures.Frame: TfrmEditorStructures;
begin
  Result := TfrmEditorStructures(inherited Frame);
end;

class function TEditorStructures.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorStructures;
end;

procedure TEditorStructures.LoadProc;
begin
  // TODO: Load
end;

procedure TEditorStructures.ModifiedChanged;
begin
  // TODO: What?
end;

procedure TEditorStructures.SaveProc;
begin
  // TODO: Save
end;

end.
