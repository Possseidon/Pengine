unit CustomVAOs;

interface

uses
  VAOManager, Shaders, Lists, VectorGeometry, Color, GLEnums, SysUtils, TextureManager, Camera;

type

  { TTextureLookup }

  TTextureLookup = TArray<String>;

  {
  TTextureLookup = class
  private
    FNames: TObjectArray;

    function GetCount: Integer;
    function GetName(I: Integer): String;
    procedure SetName(I: Integer; AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    property Names[I: Integer]: String read GetName write SetName; default;
    procedure AddName(AName: String);
    property Count: Integer read GetCount;

    procedure DelAll;
  end;
  }

  { TRenderObject }

  TRenderObject = class abstract
  public
    procedure AddToVAO(AVAO: TVAO); virtual; abstract;
    class function GetVAOSize: Integer; virtual; abstract;
  end;

  { TRenderPoint }

  TRenderPoint = class (TRenderObject)
  private
    type

      { TData }

      TData = record
        Pos: TGVector3;
        Tex: TTexCoord2;
        Color: TColorRGB;
        Size: Single;
      end;

  private
    FPos: TGVector3;
    FColor: TColorRGB;
    FSize: Single;

    function GetData(ASide: TQuadSide): TData;
  public
    constructor Create(AColor: TColorRGB; ASize: Single); overload;
    constructor Create(APos: TGVector3; AColor: TColorRGB; ASize: Single); overload;

    property Pos: TGVector3 read FPos write FPos;
    property Color: TColorRGB read FColor write FColor;
    property Size: Single read FSize write FSize;

    class function GetVAOSize: Integer; override;
    procedure AddToVAO(AVAO: TVAO); override;

  end;

  { TRenderLine }

  TRenderLine = class (TRenderObject)
  private
    type

      { TData }

      TData = record
        Pos1, Pos2: TGVector3;
        Tex: TTexCoord2;
        Color: TColorRGB;
        Width: Single;
      end;

  private
    FPos1, FPos2: TGVector3;
    FColor: TColorRGB;
    FWidth: Single;

    function GetData(ASide: TQuadSide): TData;
  public
    constructor Create(AColor: TColorRGB; AWidth: Single); overload;
    constructor Create(APos1, APos2: TGVector3; AColor: TColorRGB; AWidth: Single); overload;

    property Pos1: TGVector3 read FPos1 write FPos1;
    property Pos2: TGVector3 read FPos2 write FPos2;
    property Color: TColorRGB read FColor write FColor;
    property Width: Single read FWidth write FWidth;

    class function GetVAOSize: Integer; override;
    procedure AddToVAO(AVAO: TVAO); override;

  end;

  TFacePositions = array [TTriangleSide] of TGVector3;
  TFaceTexCoords = array [TTriangleSide] of TGVector2;
  TFaceNormals = array [TTriangleSide] of TGVector3;

  { TRenderFace }

  TRenderFace = class (TRenderObject)
  private
    type

      { TData }

      TData = record
        Pos, Normal: TGVector3;
        Tex: TTexCoord2;
        Color: TColorRGB;
        Offset: Single;
      end;

  private
    FPositions: TFacePositions;
    FNormals: TFaceNormals;
    FColor: TColorRGB;
    FOffset: Single;

    function GetNormal(I: TTriangleSide): TGVector3;
    function GetPosition(I: TTriangleSide): TGVector3;
    procedure SetColor(AValue: TColorRGB);
    procedure SetNormal(I: TTriangleSide; AValue: TGVector3);
    procedure SetOffset(AValue: Single);
    procedure SetPosition(I: TTriangleSide; AValue: TGVector3);
    function GetData(ASide: TTriangleSide): TData;
  public
    constructor Create(AColor: TColorRGB; AOffset: Single; APositions: TFacePositions); overload;
    constructor Create(AColor: TColorRGB; AOffset: Single); overload;

    property Positions: TFacePositions read FPositions;
    property Pos[I: TTriangleSide]: TGVector3 read GetPosition write SetPosition;
    property Normals: TFaceNormals read FNormals;
    property Normal[I: TTriangleSide]: TGVector3 read GetNormal write SetNormal;

    class function GetVAOSize: Integer; override;
    procedure AddToVAO(AVAO: TVAO); override;

  end;

  { TRenderQuad }

  TRenderQuad = class (TRenderObject)
  private
    type

      { TData }

      TData = record
        Pos: TGVector3;
        Tex: TTexCoord2;
        Color: TColorRGBA;
        Normal: TGVector3;
      end;

  private
    FPlane: TGPlane;
    FColor: TColorRGBA;
    FTexture: Integer;
    FOffset: Single;

    FTexLookup: TTextureLookup;
    FTexturePage: TTexturePage;

    FNormal: TGVector3;

    function GetData(ASide: TQuadSide): TData;
  public
    constructor Create(ATexturePage: TTexturePage; ATexLookup: TTextureLookup);

    property Plane: TGPlane read FPlane write FPlane;
    property Color: TColorRGBA read FColor write FColor;
    property Texture: Integer read FTexture write FTexture;
    property Offset: Single read FOffset write FOffset;

    class function GetVAOSize: Integer; override;
    procedure AddToVAO(AVAO: TVAO); override;
  end;


  { TPointVAO }
  // multiple points
  TPointVAO = class (TAutoUpdateVAO)
  private
    FPoints: TObjectArray<TRenderPoint>;
  protected
    procedure BuildVAO; override;
  public
    constructor Create(AShader: TShader);
    destructor Destroy; override;

    procedure AddPoint(APos: TGVector3; AColor: TColorRGB; ASize: Single);
    procedure DelAll;
  end;

  { TLineVAO }
  // multiple lines
  TLineVAO = class (TAutoUpdateVAO)
  private
    FLines: TObjectArray<TRenderLine>;
    FCamera: TCamera;
  protected
    procedure BuildVAO; override;
    procedure BeforeRender; override;
  public
    constructor Create(AShader: TShader; ACamera: TCamera);
    destructor Destroy; override;

    procedure AddLine(APos1, APos2: TGVector3; AColor: TColorRGB; AWidth: Single);
    procedure DelAll;
  end;

  { TFaceVAO }
  // multiple faces
  TFaceVAO = class (TAutoUpdateVAO)
  private
    FFaces: TObjectArray<TRenderFace>;
    FCamera: TCamera;
    FLocation: TLocation;
  protected
    procedure BuildVAO; override;
    procedure BeforeRender; override;
  public
    constructor Create(AShader: TShader; ACamera: TCamera);
    destructor Destroy; override;

    procedure AddFace(APositions: TFacePositions; AColor: TColorRGB; AOffset: Single = 0); overload;
    procedure AddFace(APlane: TGPlane; AColor: TColorRGB; AOffset: Single = 0); overload;
    procedure DelAll;

    property Location: TLocation read FLocation;
  end;

implementation

{ TTextureLookup }
{
function TTextureLookup.GetName(I: Integer): String;
begin
  Result := TString(FNames[I]).Text;
end;

function TTextureLookup.GetCount: Integer;
begin
  Result := FNames.Count;
end;

procedure TTextureLookup.SetName(I: Integer; AValue: String);
begin
  FNames[I] := TString.Create(AValue);
end;

constructor TTextureLookup.Create;
begin
  FNames := TObjectArray.Create;
end;

destructor TTextureLookup.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

procedure TTextureLookup.AddName(AName: String);
begin
  FNames.Add(TString.Create(AName));
end;

procedure TTextureLookup.DelAll;
begin
  FNames.DelAll;
end;
}
{ TRenderQuad }

function TRenderQuad.GetData(ASide: TQuadSide): TData;
begin
  Result.Pos := FPlane.GetPoint(QuadTexCoords[ASide]) + FNormal * FOffset;
  Result.Tex := FTexturePage.GetTexCoord(FTexLookup[FTexture], QuadTexCoords[ASide]);
  Result.Color := FColor;
  Result.Normal := FNormal;
end;

constructor TRenderQuad.Create(ATexturePage: TTexturePage; ATexLookup: TTextureLookup);
begin
  FColor := ColorWhite;
  FTexturePage := ATexturePage;
  FTexLookup := ATexLookup;
end;

class function TRenderQuad.GetVAOSize: Integer;
begin
  Result := 6;
end;

procedure TRenderQuad.AddToVAO(AVAO: TVAO);
var
  S: TQuadSide;
begin
  FNormal := Plane.Normal;
  for S := Low(TQuadSide) to High(TQuadSide) do
    AVAO.AddVertex(GetData(S));
end;

{ TRenderFace }

function TRenderFace.GetData(ASide: TTriangleSide): TData;
begin
  Result.Pos := FPositions[ASide];
  Result.Normal := FNormals[ASide];
  Result.Color := FColor;
  Result.Offset := FOffset;
  Result.Tex := TriangleTexCoords[ASide];
end;

function TRenderFace.GetNormal(I: TTriangleSide): TGVector3;
begin
  Result := FNormals[I];
end;

function TRenderFace.GetPosition(I: TTriangleSide): TGVector3;
begin
  Result := FPositions[I];
end;

procedure TRenderFace.SetColor(AValue: TColorRGB);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
end;

procedure TRenderFace.SetNormal(I: TTriangleSide; AValue: TGVector3);
begin
  FNormals[I] := AValue;
end;

procedure TRenderFace.SetOffset(AValue: Single);
begin
  if FOffset = AValue then
    Exit;
  FOffset := AValue;
end;

procedure TRenderFace.SetPosition(I: TTriangleSide; AValue: TGVector3);
begin
  FPositions[I] := AValue;
end;

constructor TRenderFace.Create(AColor: TColorRGB; AOffset: Single; APositions: TFacePositions);
var
  I: TTriangleSide;
begin
  for I := Low(TTriangleSide) to High(TTriangleSide) do
    FPositions[I] := APositions[I];
  FColor := AColor;
  FOffset := AOffset;
end;

constructor TRenderFace.Create(AColor: TColorRGB; AOffset: Single);
begin
  FColor := AColor;
  FOffset := AOffset;
end;

class function TRenderFace.GetVAOSize: Integer;
begin
  Result := 3;
end;

procedure TRenderFace.AddToVAO(AVAO: TVAO);
var
  S: TTriangleSide;
begin
  for S := Low(TTriangleSide) to High(TTriangleSide) do
    AVAO.AddVertex(GetData(S));
end;

{ TFaceVAO }

procedure TFaceVAO.BuildVAO;
var
  I: Integer;
begin
  Generate(FFaces.Count * 3, buStaticDraw);
  Map(baWriteOnly);
  for I := 0 to FFaces.Count - 1 do with TRenderFace(FFaces[I]) do
    AddToVAO(Self);
  Unmap;
end;

procedure TFaceVAO.BeforeRender;
begin
  inherited BeforeRender;
  //FCamera.SetModelLocation(FLocation);
  //FCamera.Render;
end;

constructor TFaceVAO.Create(AShader: TShader; ACamera: TCamera);
begin
  inherited Create(AShader);
  FCamera := ACamera;
  FFaces := TObjectArray<TRenderFace>.Create;
  FLocation := TLocation.Create;
end;

destructor TFaceVAO.Destroy;
begin
  FFaces.Free;
  FLocation.Free;
  inherited Destroy;
end;

procedure TFaceVAO.AddFace(APositions: TFacePositions; AColor: TColorRGB; AOffset: Single);
begin
  FFaces.Add(TRenderFace.Create(AColor, AOffset, APositions));
  NotifyChanges;
end;

procedure TFaceVAO.AddFace(APlane: TGPlane; AColor: TColorRGB; AOffset: Single);
var
  Points: TFacePositions;
begin
  Points[0] := APlane[0, 0];
  Points[1] := APlane[1, 0];
  Points[2] := APlane[0, 1];
  AddFace(Points, AColor, AOffset);
end;

procedure TFaceVAO.DelAll;
begin
  if FFaces.Count = 0 then
    Exit;
  FFaces.DelAll;
  NotifyChanges;
end;

{ TLineVAO }

procedure TLineVAO.BuildVAO;
var
  L: TRenderLine;
begin
  Generate(6 * FLines.Count, buStaticDraw);
  Map(baWriteOnly);
  for TObject(L) in FLines do
    L.AddToVAO(Self);
  Unmap;
end;

procedure TLineVAO.BeforeRender;
begin
  inherited BeforeRender;
  //FCamera.ResetModelLocation;
  //FCamera.Render;
end;

constructor TLineVAO.Create(AShader: TShader; ACamera: TCamera);
begin
  inherited Create(AShader);
  FLines := TObjectArray<TRenderLine>.Create;
  FCamera := ACamera;
end;

destructor TLineVAO.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TLineVAO.AddLine(APos1, APos2: TGVector3; AColor: TColorRGB; AWidth: Single);
begin
  FLines.Add(TRenderLine.Create(APos1, APos2, AColor, AWidth));
  NotifyChanges;
end;

procedure TLineVAO.DelAll;
begin
  if FLines.Count = 0 then
    Exit;
  FLines.DelAll;
  NotifyChanges;
end;

{ TRenderLine }

function TRenderLine.GetData(ASide: TQuadSide): TData;
begin
  Result.Pos1 := FPos1;
  Result.Pos2 := FPos2;
  Result.Color := FColor;

  Result.Width := FWidth;
  Result.Tex := QuadMiddleCoords[ASide];
end;

constructor TRenderLine.Create(AColor: TColorRGB; AWidth: Single);
begin
  FColor := AColor;
  FWidth := AWidth;
end;

constructor TRenderLine.Create(APos1, APos2: TGVector3; AColor: TColorRGB; AWidth: Single);
begin
  FPos1 := APos1;
  FPos2 := APos2;
  FColor := AColor;
  FWidth := AWidth;
end;

class function TRenderLine.GetVAOSize: Integer;
begin
  Result := 6;
end;

procedure TRenderLine.AddToVAO(AVAO: TVAO);
var
  S: TQuadSide;
begin
  for S := Low(TQuadSide) to High(TQuadSide) do
    AVAO.AddVertex(GetData(S));
end;

{ TRenderPoint }

constructor TRenderPoint.Create(APos: TGVector3; AColor: TColorRGB; ASize: Single);
begin
  FPos := APos;
  FColor := AColor;
  FSize := ASize;
end;

class function TRenderPoint.GetVAOSize: Integer;
begin
  Result := 6;
end;

procedure TRenderPoint.AddToVAO(AVAO: TVAO);
var
  S: TQuadSide;
begin
  for S := Low(TQuadSide) to High(TQuadSide) do
    AVAO.AddVertex(GetData(S));
end;

function TRenderPoint.GetData(ASide: TQuadSide): TData;
begin
  Result.Pos := FPos;
  Result.Color := FColor;
  Result.Size := FSize;
  Result.Tex := QuadMiddleCoords[ASide];
end;

constructor TRenderPoint.Create(AColor: TColorRGB; ASize: Single);
begin
  FColor := AColor;
  FSize := ASize;
end;

{ TPointVAO }

procedure TPointVAO.BuildVAO;
var
  Current: TRenderPoint;
begin
  Generate(6 * FPoints.Count, buStaticDraw);
  Map(baWriteOnly);
  for TObject(Current) in FPoints do
    Current.AddToVAO(Self);
  Unmap;
end;

constructor TPointVAO.Create(AShader: TShader);
begin
  inherited Create(AShader);
  FPoints := TObjectArray<TRenderPoint>.Create;
end;

destructor TPointVAO.Destroy;
begin
  FPoints.Free;
  inherited Destroy;
end;

procedure TPointVAO.AddPoint(APos: TGVector3; AColor: TColorRGB; ASize: Single);
begin
  FPoints.Add(TRenderPoint.Create(APos, AColor, ASize));
  NotifyChanges;
end;

procedure TPointVAO.DelAll;
begin
  if FPoints.Count = 0 then
    Exit;
  FPoints.DelAll;
  NotifyChanges;
end;

end.

