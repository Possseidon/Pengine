unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Types,
  System.Variants,
  System.Classes,
  System.IOUtils,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.GLForm,
  Pengine.SpriteSystem,
  Pengine.GLProgram,
  Pengine.TextureAtlas,
  Pengine.GLEnums,
  Pengine.Collections,
  Pengine.IntMaths,
  Pengine.Vector,

  VD.Entity;

type

  TSpriteGLProgram = class(TSpriteGLProgamBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

  TfrmMain = class(TGLForm)
  private
    FSpriteGLProgram: TGLProgram;
    FSpriteSystem: TSpriteSystem;
    FTextureAtlas: TTextureAtlas;

    FTemplate: TEntity.TTemplate;
    FEntities: TObjectArray<TEntity>;

    procedure GameUpdate;
    procedure FPSUpdate;

  public
    procedure Init; override;
    procedure Finalize; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TForm9 }

procedure TfrmMain.FPSUpdate;
begin
  Caption := Format('VoidDefender: %.0f FPS', [Game.FPS]);
end;

procedure TfrmMain.GameUpdate;
var
  Entity: TEntity;
begin
  for Entity in FEntities do
    Entity.Location.Rotation := Entity.Location.Rotation + DeltaTime * 90 * Entity.Location.PosX * Entity.Location.PosY;
end;

procedure TfrmMain.Init;
var
  Dir, Path: string;
  Entity: TEntity;
  Pos: TIntVector2;
begin
  Context.VSync := False;
  Context.Samples := Context.MaxSamples;

  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResParam);

  FSpriteSystem := TSpriteSystem.Create(Game, FSpriteGLProgram);

  FTextureAtlas := FSpriteSystem.SpriteAtlas;
  for Dir in TDirectory.GetDirectories('Data\Textures') do
    for Path in TDirectory.GetFiles(Dir) do
      FTextureAtlas.AddFromFile(ExtractFileName(Path.Substring(0, Path.Length - 4)), Path);
  FTextureAtlas.Texture.MagFilter := magNearest;

  FTemplate := TEntity.TTemplate.Create;
  FTemplate.MainPart := FTemplate.AddPart;
  FTemplate.MainPart.Texture := FTextureAtlas['spaceship_body_1'];

  with FTemplate.AddPart do
  begin
    Texture := FTextureAtlas['spaceship_body_2'];
    Location.Scale := 0.4;
    Location.PosX := -0.9;
    Location.Rotation := -10;
  end;

  with FTemplate.AddPart do
  begin
    Texture := FTextureAtlas['spaceship_body_2'];
    Location.Scale := 0.4;
    Location.ScaleX := -0.4;
    Location.PosX := 0.9;
    Location.Rotation := 10;
  end;

  with FTemplate.AddPart do
  begin
    Texture := FTextureAtlas['spaceship_body_3'];
    Location.Scale := 1.5;
    Location.PosX := -1.5;
    Location.Rotation := -10;
  end;

  with FTemplate.AddPart do
  begin
    Texture := FTextureAtlas['spaceship_body_3'];
    Location.Scale := 1.5;
    Location.ScaleX := -1.5;
    Location.PosX := 1.5;
    Location.Rotation := 10;
  end;

  FEntities := TObjectArray<TEntity>.Create;
  Entity := TEntity.Create(FSpriteSystem, FTemplate);
  FEntities.Add(Entity);

  Game.OnUpdate.Add(GameUpdate);
  Game.Timer.OnFPSUpdate.Add(FPSUpdate);
end;

procedure TfrmMain.Finalize;
begin
  FEntities.Free;
  FTemplate.Free;
  FSpriteSystem.Free;
  if FSpriteGLProgram <> nil then
    TSpriteGLProgram.Release(GLState.ResParam);
end;

{ TSpriteGLProgram }

class procedure TSpriteGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := False;
  if AResource then
    AName := 'SHADER_SPRITE'
  else
    AName := 'Data/Shader/sprite';
end;

end.
