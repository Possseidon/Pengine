unit SpriteGLProgram;

interface

uses
  Pengine.SpriteSystem;

type

  TSpriteGLProgram = class(TSpriteGLProgamBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

implementation

{ TSpriteGLProgram }

class procedure TSpriteGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  AName := 'SPRITE';
end;

end.

