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
    class procedure GetData(out AName: string; out AResource: Boolean); override;
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
  end;

implementation

{ TModelShader }

class function TModelGLProgram.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := ['vpos', 'vcolor', 'vnormal'];
end;

class procedure TModelGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  if AResource then
    AName := 'MODEL'
  else
    AName := 'Data/model';
end;

end.

