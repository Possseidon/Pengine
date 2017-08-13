unit ResourceManager;

interface

uses
  OpenGLContext, Shaders, TextureManager, VAOManager, GLEnums, VectorGeometry;

type
  TBaseVAOType = (vtCube, vtPlane);

  TData = record
    Pos: TVector3;
    TexCoord: TTexCoord2;
    Normal: TVector3;
    Tangent: TVector3;
    Bitangent: TVector3;
    Border: TBounds2;
  end;

  TResourceManager = class
  private
    FShader: TShader;
    FTexturePage: TTexturePage;


  public
    procedure Init;

    procedure LoadVAO(AName: string; AFilePath: string); overload;
    procedure LoadVAO(AName: string; AType: TBaseVAOType); overload;
  end;

implementation

procedure TResourceManager.Init;
begin

end;

procedure TResourceManager.LoadVAO(AName: string; AFilePath: string);
begin

end;

procedure TResourceManager.LoadVAO(AName: string; AType: TBaseVAOType);
var
  VAO: TVAO;
  P: TPlane3;
  Data: TData;
  T: TTexCoord2;
begin
  VAO := TVAO.Create(FShader);

  if AType = vtCube then
  begin
    VAO.Generate(6 * 6, buStaticDraw);
    VAO.Map(baWriteOnly);

    for P in CubePlanes do
    begin
      Data.Border := FTexturePage.GetTexBounds('stone_bricks', FRange2(0, 1));
      Data.Normal := P.Normal;
      Data.Tangent := P.DVS;
      Data.Bitangent := P.DVT;
      for T in QuadTexCoords do
      begin
        Data.Pos := P[T];
        Data.TexCoord := Data.Border[T];
        VAO.AddVertex(Data);
      end;
      Data.Border := FTexturePage.HalfPixelInset(Data.Border);
    end;
  end;
  if AType = vtPlane then
  begin

  end;

  VAO.Unmap;
end;

end.
