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

  Pengine.GLProgram,
  Pengine.GLForm,
  Pengine.GUI,
  Pengine.GUIControls,
  Pengine.SpriteSystem,
  Pengine.GLEnums,
  Pengine.Color,
  Pengine.IntMaths,
  
  SpriteGLProgram,
  TetrisBoard;

type

  TfrmMain = class(TGLForm)
  private
    FSpriteGLProgram: TGLProgram;
    FGUI: TGUI;
    FScore: TLabel;
    FTetrisBoard: TTetrisBoard;

  public
    procedure Init; override;
    procedure Finalize; override;

    procedure KeyDown; reintroduce;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.Init;
begin
  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResParam);

  FGUI := TGUI.Create(Game, FSpriteGLProgram);
  FGUI.TextureAtlas.Texture.MagFilter := magNearest;
  FGUI.TextureAtlas.AddFromResource('font', 'font');
  FGUI.Font := 'font';
  FGUI.TextureAtlas.AddFromResource('block', 'block');

  FScore := FGUI.Add<TLabel>;
  FScore.Origin := TOrigin.Create(oxCenter, oyTop);
  FScore.Caption := '0';
  FScore.Location.Scale := 0.2;

  FTetrisBoard := FGUI.Add<TTetrisBoard>;
  FTetrisBoard.OriginY := oyBottom;
  FTetrisBoard.Location.Scale := 1.8;
  FTetrisBoard.AddDefaultTemplates;

  Game.Input.OnKeyDown.Add(KeyDown);

end;

procedure TfrmMain.KeyDown;
begin

end;

procedure TfrmMain.Finalize;
begin
  FGUI.Free;

  if FSpriteGLProgram <> nil then
    TSpriteGLProgram.Release(GLState.ResParam);
end;

end.
