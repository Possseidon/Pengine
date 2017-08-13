unit ResourceManager;

interface

uses
  OpenGLContext, Shaders, TextureManager, VAOManager, GLEnums, VectorGeometry, Lists, SysUtils;

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
    class procedure LoadVAO(AName: string; AType: TBaseVAOType); overload;

    class var
      FVAOs: TStringObjectMap<TVAO>;

      FShader: TShader;
      FTexturePage: TTexturePage;
  public
    class procedure Init(AShader: TShader; ATexturePage: TTexturePage);

    class procedure LoadVAO(AName: string; AFilePath: string); overload;
    class function GetVAO(AName: string): TVAO;
  end;

implementation

class procedure TResourceManager.Init(AShader: TShader; ATexturePage: TTexturePage);
begin
  FShader := AShader;
  FTexturePage := ATexturePage;

  LoadVAO('cube', vtCube);
  //LoadVAO('plane', vtPlane);
end;

class procedure TResourceManager.LoadVAO(AName: string; AFilePath: string);
begin
end;

class procedure TResourceManager.LoadVAO(AName: string; AType: TBaseVAOType);
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

    VAO.Unmap;
  end;
  if AType = vtPlane then
  begin

  end;

  FVAOs[AName] := VAO;
end;

class function TResourceManager.GetVAO(AName: string): TVAO;
begin
  if Not(FVAOs.HasKey(AName)) then
    raise Exception.Create('Model ' + AName + ' is not found!');
  Result := FVAOs.Data[AName];
end;

end.
