unit EditorFrameRecipe;

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
  TfrmEditorRecipes = class(TFrame)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  TEditorRecipes = class(TEditor)
  protected
    procedure SaveProc; override;
    procedure LoadProc; override;
    procedure ModifiedChanged; override;

  public
    class function GetFrameClass: TFrameClass; override;

    function Frame: TfrmEditorRecipes;

  end;

implementation

{$R *.dfm}

{ TEditorRecipes }

function TEditorRecipes.Frame: TfrmEditorRecipes;
begin
  Result := TfrmEditorRecipes(inherited Frame);
end;

class function TEditorRecipes.GetFrameClass: TFrameClass;
begin
  Result := TfrmEditorRecipes;
end;

procedure TEditorRecipes.LoadProc;
begin
  // TODO: Load
end;

procedure TEditorRecipes.ModifiedChanged;
begin
  // TODO: What
end;

procedure TEditorRecipes.SaveProc;
begin
  // TODO: Save
end;

end.
