unit ModelShader;

interface

uses
  Pengine.ResourceManager,
  Pengine.Shader,
  Pengine.Vector,
  Pengine.Color;

type

  TModelShader = class(TShaderResource)
  public type

    TData = record
      Pos: TVector3;
      Color: TColorRGB;
      Normal: TVector3;
    end;

  protected
    class function GetShaderSource: string; override;
    class function GetAttributeOrder: TShader.TAttributeOrder; override;
  end;

implementation

{ TModelShader }

class function TModelShader.GetAttributeOrder: TShader.TAttributeOrder;
begin
  Result := ['vpos', 'vcolor', 'vnormal'];
end;

class function TModelShader.GetShaderSource: string;
begin
  Result := 'Data\model';
end;

end.

