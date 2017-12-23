unit ModelShader;

interface

uses
  Pengine.ResourceManager,
  Pengine.GLProgram,
  Pengine.Vector,
  Pengine.Color;

type

  TModelGLProgram = class(TGLProgramResource)
  public type

    TData = record
      Pos: TVector3;
      Color: TColorRGB;
      Normal: TVector3;
    end;

  protected
    class function GetFileName: string; override;
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
  end;

implementation

{ TModelShader }

class function TModelGLProgram.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := ['vpos', 'vcolor', 'vnormal'];
end;

class function TModelGLProgram.GetFileName: string;
begin
  Result := 'Data\model';
end;

end.

