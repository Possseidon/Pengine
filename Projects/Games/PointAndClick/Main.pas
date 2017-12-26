unit Main;

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

  Pengine.GLContext,
  Pengine.SpriteSystem,
  Pengine.GLProgram;

type

  TSpriteGLProgram = class(TSpriteGLProgamBase)
  protected
    class function GetFileName: string; override;
  end;

  TfrmMain = class(TGLForm)
  private
    FSpriteGLProgram: TGLProgram;
    FSpriteSystem: TSpriteSystem;
    FAspectUniform: TGLProgram.TUniform<Single>;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure ResizeGL; override;
    procedure UpdateGL; override;
    procedure RenderGL; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
var
  TestSprite: TSprite;
begin
  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResourceParam);
  FAspectUniform := FSpriteGLProgram.Uniform<Single>('aspect');

  FSpriteSystem := TSpriteSystem.Create(FSpriteGLProgram);

  TestSprite := TSprite.Create(FSpriteSystem);
  TestSprite.
end;

procedure TfrmMain.Finalize;
begin
  FSpriteSystem.Free;
  TSpriteGLProgram.Release(GLState.ResourceParam);
end;

procedure TfrmMain.ResizeGL;
begin
  FAspectUniform.Value := ClientWidth / ClientHeight;
end;

procedure TfrmMain.UpdateGL;
begin
  if Context.MustUpdateFPS then
    Caption := Format('Point and Click - FPS: %d', [Context.FPSInt]);

  FSpriteSystem.Update;
end;

procedure TfrmMain.RenderGL;
begin
  FSpriteSystem.Render;
end;

{ TSpriteGLProgram }

class function TSpriteGLProgram.GetFileName: string;
begin
  Result := 'data/sprite';
end;

end.
