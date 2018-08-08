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

    FStructure: TStructure;
    FEntitySystem: TEntitySystem;

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
  for Entity in FEntitySystem.Entities do
    Entity.Location.Rotation := Entity.Location.Rotation + DeltaTime * 360 * Sqr(Entity.Location.PosX) * Entity.Location.PosY;

  if Input.KeyTyped(VK_SPACE) and not FEntitySystem.Entities.Empty then
    FEntitySystem.Entities.First.Remove;
end;

procedure TfrmMain.Init;
var
  Dir, Path: string;
  Part: TEntityBase.TPart;
  P: TVector2;
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

  FStructure := TStructure.Create;
  Part := FStructure.AddPart;
  Assert(Part.Entity = FStructure);
  FStructure.MainPart := Part;
  FStructure.MainPart.DefaultTexture := FTextureAtlas['spaceship_body_1'];
  FStructure.BaseTransformation.Scale := 0.2;

  with FStructure.AddPart do
  begin
    DefaultTexture := FTextureAtlas['spaceship_body_3'];
    Location.Scale := 1.5;
    Location.Pos := Vec2(-1.5, 0.2);
    Location.Rotation := -10;
  end;

  with FStructure.AddPart do
  begin
    DefaultTexture := FTextureAtlas['spaceship_body_3'];
    Location.Scale := 1.5;
    Location.ScaleX := -1.5;
    Location.Pos := Vec2(1.5, 0.2);
    Location.Rotation := 10;
  end;

  with FStructure.AddPart do
  begin
    DefaultTexture := FTextureAtlas['spaceship_body_2'];
    Location.Scale := 0.4;
    Location.PosX := -0.9;
    Location.Rotation := -10;
  end;

  with FStructure.AddPart do
  begin
    DefaultTexture := FTextureAtlas['spaceship_body_2'];
    Location.Scale := 0.4;
    Location.ScaleX := -0.4;
    Location.PosX := 0.9;
    Location.Rotation := 10;
  end;

  FEntitySystem := TEntitySystem.Create(Game, FSpriteGLProgram);
  for P in IBounds2I(-5, 5) do
  begin
    with FEntitySystem.Add(FStructure) do
    begin
      Location.Pos := P * 0.15;
      Location.Scale := 1;
    end;
  end;

  Game.OnUpdate.Add(GameUpdate);
  Game.Timer.OnFPSUpdate.Add(FPSUpdate);
end;

procedure TfrmMain.Finalize;
begin
  FEntitySystem.Free;
  FStructure.Free;
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
