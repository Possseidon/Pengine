unit ModelDefine;

interface

uses
  IntfBase, Camera, VectorGeometry, VAOManager, Lists, SysUtils, Classes, Logger, ResourceManager, Shaders, GLEnums;

type

  TModelShaderBase = class(TShaderResource)
  public type

    TData = record
      Pos: TVector3;
      TexCoord: TTexCoord2;
      Normal: TVector3;
      Tangent: TVector3;
      Bitangent: TVector3;
      Border: TBounds2;
    end;

  protected
    class function GetAttributeOrder: TShaderAttributeOrder; override;

  end;

  { TModel }

  TModel = class
  public
    procedure GenerateVAO(AVAO: TVAO); virtual; abstract;
  end;

  { TModelOBJ }

  TModelOBJ = class(TModel)
  public type

    TVertices = TGenericArray<TVector4>;

    TTexCoords = TGenericArray<TTexCoord3>;

    TNormals = TGenericArray<TVector3>;

    TFacePoint = record
      VertexIndex: Integer;
      TexCoordIndex: Integer;
      NormalIndex: Integer;
    end;

    TFace = array of TFacePoint;

    TFaces = TGenericArray<TFace>;

    TGroup = class
    private
      FFaces: TFaces;

    public
      constructor Create;
      destructor Destroy; override;

    end;

    TGroups = TStringObjectMap<TGroup>;

    TLoader = class
    {$M+}
    private type
      TCommandType = (         
        {$REGION 'Vertex data'}
        ctVertex, 
        ctTexCoord, 
        ctNormal,
        ctVertexParam,
        {$ENDREGION}
        {$REGION 'Free-form curve/surface attributes'}
        ctDegree,
        ctBasisMatrix,
        ctStepSize,
        ctCurveSurfaceType,
        {$ENDREGION}
        {$REGION 'Elements'}
        ctPoint, 
        ctLine,
        ctFace,
        ctCurve,
        ctCurve2D,
        ctSurface,
        {$ENDREGION}
        {$REGION 'Free-form curve/surface body statements'}
        ctParam,
        ctTrim,
        ctHole,
        ctSpecialCurve,
        ctSpecialPoint,
        ctEnd,
        {$ENDREGION}
        {$REGION 'Connectivity between free-form surfaces'}
        ctConnect,
        {$ENDREGION}
        {$REGION 'Grouping'}
        ctGroup,
        ctSmoothingGroup,
        ctMergingGroup,
        ctObjectName,
        {$ENDREGION}
        {$REGION 'Display/render attributes'}
        ctBevel,
        ctColorInterp,
        ctDissolveInterp,
        ctLevelOfDetail,
        ctUseMtl,
        ctMtlLib,
        ctShadowObject,
        ctTraceObject,
        ctCurveApproxTechnique,
        ctSurfaceApproxTechnique
        {$ENDREGION}
        );   
        
      TProcessingFunc = procedure(AArguments: TArray<string>) of object;

      ELoadError = class(Exception);

    private const
      CommandNames: array [TCommandType] of string = (
        {$REGION 'Vertex data'}
        'v', 
        'vt',
        'vn',
        'vp',
        {$ENDREGION}
        {$REGION 'Free-form curve/surface attributes'}
        'deg',
        'bmat',
        'step',
        'cstype',
        {$ENDREGION}
        {$REGION 'Elements'}
        'p', 
        'l',
        'f',
        'curv',
        'curv2',
        'surf',
        {$ENDREGION}
        {$REGION 'Free-form curve/surface body statements'}
        'parm',
        'trim',
        'hole',
        'scrv',
        'sp',
        'end',
        {$ENDREGION}
        {$REGION 'Connectivity between free-form surfaces'}
        'con',
        {$ENDREGION}
        {$REGION 'Grouping'}
        'g',
        's',
        'mg',
        'o',
        {$ENDREGION}
        {$REGION 'Display/render attributes'}
        'bevel',
        'c_interp',
        'd_interp',
        'lod',
        'usemtl',
        'mtllib',
        'shadow_obj',
        'trace_obj',
        'ctech',
        'stech'
        {$ENDREGION}
      );
      
    private class var
      FCommands: TStringMap<Pointer>;

    private
      FModel: TModelOBJ;
      FMethodHelper: TMethod;
      FLineNumber: Integer;

      FCurrentGroup: TGroup;

      function GetCurrentGroup: TGroup;

      property CurrentGroup: TGroup read GetCurrentGroup write FCurrentGroup;

      procedure ProcessLine(ALine: string);

    public
      class constructor Create;
      class destructor Destroy;

      constructor Create(AModel: TModelOBJ; ALines: TStrings);

      function ParseSingle(const AString: string): Single;
      function ParseInteger(const AString :string): Integer;
      function ParseIndex(const AString: string; ACount: Integer): Integer;

    published      
      {$REGION 'Vertex data'}
      procedure Process_v(AArguments: TArray<string>);
      procedure Process_vt(AArguments: TArray<string>);
      procedure Process_vn(AArguments: TArray<string>);
      // procedure Process_vp(AArguments: TArray<string>);
      {$ENDREGION}
      {$REGION 'Free-form curve/surface attributes'}
      // procedure Process_deg(AArguments: TArray<string>);
      // procedure Process_bmat(AArguments: TArray<string>);
      // procedure Process_step(AArguments: TArray<string>);
      // procedure Process_cstype(AArguments: TArray<string>);
      {$ENDREGION}
      {$REGION 'Elements'}
      // procedure Process_p(AArguments: TArray<string>);
      // procedure Process_l(AArguments: TArray<string>);
      procedure Process_f(AArguments: TArray<string>);
      // procedure Process_curv(AArguments: TArray<string>);
      // procedure Process_curv2(AArguments: TArray<string>);
      // procedure Process_surf(AArguments: TArray<string>);
      {$ENDREGION}
      {$REGION 'Free-form curve/surface body statements'}
      // procedure Process_parm(AArguments: TArray<string>);
      // procedure Process_trim(AArguments: TArray<string>);
      // procedure Process_hole(AArguments: TArray<string>);
      // procedure Process_scrv(AArguments: TArray<string>);
      // procedure Process_sp(AArguments: TArray<string>);
      // procedure Process_end(AArguments: TArray<string>);
      {$ENDREGION}
      {$REGION 'Connectivity between free-form surfaces'}
      // procedure Process_con(AArguments: TArray<string>);
      {$ENDREGION}
      {$REGION 'Grouping'}
      // procedure Process_g(AArguments: TArray<string>);
      // procedure Process_s(AArguments: TArray<string>);
      // procedure Process_mg(AArguments: TArray<string>);
      // procedure Process_o(AArguments: TArray<string>);
      {$ENDREGION}
      {$REGION 'Display/render attributes'}
      // procedure Process_bevel(AArguments: TArray<string>);
      // procedure Process_c_interp(AArguments: TArray<string>);
      // procedure Process_d_interp(AArguments: TArray<string>);
      // procedure Process_lod(AArguments: TArray<string>);
      // procedure Process_usemtl(AArguments: TArray<string>);
      // procedure Process_mtllib(AArguments: TArray<string>);
      // procedure Process_shadow_obj(AArguments: TArray<string>);
      // procedure Process_trace_obj(AArguments: TArray<string>);
      // procedure Process_ctech(AArguments: TArray<string>);
      // procedure Process_stech(AArguments: TArray<string>);
      {$ENDREGION}
    end;
   
  private
    FGroups: TGroups;

    FVertices: TVertices;
    FTexCoords: TTexCoords;
    FNormals: TNormals;

    FLog: TCodeLog;
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(AFileName: string);

    function LoadSuccess: Boolean;
    property Log: TCodeLog read FLog;

    procedure GenerateVAO(AVAO: TVAO); override;

  end;

implementation

{ TModelOBJ }

constructor TModelOBJ.Create;
begin
  FGroups := TGroups.Create;
  FVertices := TVertices.Create;
  FTexCoords := TTexCoords.Create;
  FNormals := TNormals.Create;
  FLog := TCodeLog.Create;
end;

destructor TModelOBJ.Destroy;
begin
  FGroups.Free;
  FNormals.Free;
  FTexCoords.Free;
  FVertices.Free;
  FLog.Free;
  inherited;
end;

procedure TModelOBJ.GenerateVAO(AVAO: TVAO);
var
  Data: TModelShaderBase.TData;
  GroupPair: TPair<string, TGroup>;
  Group: TGroup;
  Face: TFace;
  FacePoint: TFacePoint;
  Size: Integer;
begin
  Size := 0;
  for GroupPair in FGroups do
    Size := Size + GroupPair.Data.FFaces.Count * 3; // TODO: Performance, possibly save this while loading
  AVAO.Generate(Size, buStaticDraw);
  AVAO.Map(baWriteOnly);

  Data.Tangent := Vec3(1, 0, 0); // TODO: remove bzw chnage
  Data.Bitangent := Vec3(1, 0, 0);
  
  for GroupPair in FGroups do
  begin
    Group := GroupPair.Data;
    for Face in Group.FFaces do
    begin
      if Length(Face) <> 3 then
        raise Exception.Create('Only triangles are supported by the Model VAO-Generator!');

      Data.Border := FRange2(0, 1); // TODO: Texture and stuff

      if FacePoint.TexCoordIndex = 0 then
        Data.TexCoord := 0;
      if FacePoint.NormalIndex = 0 then
        Data.Normal := 
          TVector3(FVertices[Face[0].VertexIndex - 1]).VectorTo(FVertices[Face[1].VertexIndex - 1]).Cross(
          TVector3(FVertices[Face[0].VertexIndex - 1]).VectorTo(FVertices[Face[2].VertexIndex - 1]));
              
      for FacePoint in Face do
      begin
        Data.Pos := FVertices[FacePoint.VertexIndex - 1];
        if FacePoint.TexCoordIndex <> 0 then
          Data.TexCoord := FTexCoords[FacePoint.TexCoordIndex - 1].XY / 2;
        if FacePoint.NormalIndex <> 0 then
          Data.Normal := FNormals[FacePoint.NormalIndex - 1];
        AVAO.AddVertex(Data); 
      end;
    end;
  end;
  AVAO.Unmap;
  Assert(AVAO.Size = AVAO.MaxSize); // TODO: remove
end;

procedure TModelOBJ.LoadFromFile(AFileName: string);
var
  Lines: TStrings;
  Loader: TLoader;
begin
  Loader := nil;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    Loader := TLoader.Create(Self, Lines);
  finally
    Loader.Free;
    Lines.Free;
  end;
end;

function TModelOBJ.LoadSuccess: Boolean;
begin
  Result := FLog.Success;
end;

{ TModelOBJ.TLoader }

constructor TModelOBJ.TLoader.Create(AModel: TModelOBJ; ALines: TStrings);
var
  I: Integer;
begin
  FModel := AModel;

  FModel.Log.Clear;

  FMethodHelper.Data := Self;
  for I := 0 to ALines.Count - 1 do
  begin
    FLineNumber := I + 1;
    try
      ProcessLine(ALines[I]);
    except
      on E: ELoadError do
        FModel.Log.Add(TCodeLogEntry.Create(E.Message, FLineNumber, esError));
      on E: Exception do
        FModel.Log.Add(TCodeLogEntry.Create(E.Message, FLineNumber, esFatal));
    end;
  end;
end;

class constructor TModelOBJ.TLoader.Create;
var
  Command: TCommandType;
begin
  FCommands := TStringMap<Pointer>.Create;
  for Command := Low(TCommandType) to High(TCommandType) do
    FCommands[CommandNames[Command]] := MethodAddress('Process_' + CommandNames[Command]);
end;

class destructor TModelOBJ.TLoader.Destroy;
begin
  FCommands.Free;
end;

function TModelOBJ.TLoader.GetCurrentGroup: TGroup;
begin
  if FCurrentGroup = nil then
  begin
    FCurrentGroup := TGroup.Create;
    FModel.FGroups['default'] := FCurrentGroup;
  end;
  Result := FCurrentGroup;
end;

function TModelOBJ.TLoader.ParseIndex(const AString: string; ACount: Integer): Integer;
begin
  Result := ParseInteger(AString);
  if Result = 0 then
    raise ELoadError.Create('0 is not a valid index.');
  if Abs(Result) > ACount then
    raise ELoadError.CreateFmt('Index %d can be at most (-)%d at this point.', [Result, ACount]);
  if Result < 0 then
    Result := ACount + Result + 1;
end;

function TModelOBJ.TLoader.ParseInteger(const AString: string): Integer;
begin
  if not Integer.TryParse(AString, Result) then
    raise ELoadError.CreateFmt('"%s" is not a valid index value.', [AString]);
end;

function TModelOBJ.TLoader.ParseSingle(const AString: string): Single;
begin
  if not Single.TryParse(AString, Result, TFormatSettings.Invariant) then
    raise ELoadError.CreateFmt('"%s" is not a valid floating point value.', [AString]);
end;

procedure TModelOBJ.TLoader.ProcessLine(ALine: string);
var
  Arguments: TArray<string>;
begin
  ALine := ALine.Trim;
  if ALine.StartsWith('#') then
    Exit;
  Arguments := ALine.Split([' '], ExcludeEmpty);
  if Length(Arguments) = 0 then
    Exit;
  if not FCommands.Get(Arguments[0], FMethodHelper.Code) then
    raise ELoadError.CreateFmt('No such Command "%s"', [Arguments[0]]);
  if FMethodHelper.Code <> nil then
  begin
    Delete(Arguments, 0, 1);
    TProcessingFunc(FMethodHelper)(Arguments);
  end
  else
  begin
    FModel.Log.Add(TCodeLogEntry.Create(
      Format('Ignored unsupported Command "%s"', [Arguments[0]]), FLineNumber, esWarning));
  end;
end;

procedure TModelOBJ.TLoader.Process_v(AArguments: TArray<string>);
var
  V: TVector4;
begin
  if not (Length(AArguments) in [3 .. 4]) then
    raise ELoadError.CreateFmt('"v" expectes 3 or 4 arguments, got %d', [Length(AArguments)]);
  V.X := ParseSingle(AArguments[0]);
  V.Y := ParseSingle(AArguments[1]);
  V.Z := ParseSingle(AArguments[2]);
  if Length(AArguments) = 4 then
    V.W := ParseSingle(AArguments[3])
  else
    V.W := 1;
  FModel.FVertices.Add(V);
end;

procedure TModelOBJ.TLoader.Process_vt(AArguments: TArray<string>);
var
  T: TTexCoord3;
begin
  if not Length(AArguments) in [1 .. 3] then
    raise ELoadError.CreateFmt('"vt" expectes between 1 and 3 arguments, got %d', [Length(AArguments)]);
  T.S := ParseSingle(AArguments[0]);
  if Length(AArguments) >= 2 then
    T.T := ParseSingle(AArguments[1])
  else
    T.T := 0;
  if Length(AArguments) = 3 then
    T.U := ParseSingle(AArguments[2])
  else
    T.U := 0;
  FModel.FTexCoords.Add(T);
end;

procedure TModelOBJ.TLoader.Process_vn(AArguments: TArray<string>);
var
  N: TVector3;
begin
  if Length(AArguments) <> 3 then
    raise ELoadError.CreateFmt('"vn" expects exactly 3 arguments, got %d', [Length(AArguments)]);
  N.X := ParseSingle(AArguments[0]);
  N.Y := ParseSingle(AArguments[1]);
  N.Z := ParseSingle(AArguments[2]);
  FModel.FNormals.Add(N);
end;

procedure TModelOBJ.TLoader.Process_f(AArguments: TArray<string>);
const
  InconsitentIndicesString =
    'The %d indice-sets for "f" must be consistent with the previous ones!';

type
  TFaceMode = (
    fmNone,
    fmV,
    fmVT,
    fmVN,
    fmVTN
  );
var
  I: Integer;
  Face: TFace;
  Mode: TFaceMode;
  Indices: TArray<string>;
begin
  if Length(AArguments) < 3 then
    raise ELoadError.CreateFmt('"f" expectes at least three parameters, got %d', [Length(AArguments)]);

  Mode := fmNone;

  SetLength(Face, Length(AArguments));

  for I := 0 to Length(AArguments) - 1 do
  begin
    Indices := AArguments[I].Split(['/']);

    case Length(Indices) of
      1:
        begin
          if Mode = fmNone then
            Mode := fmV
          else if Mode <> fmV then
            raise ELoadError.CreateFmt(InconsitentIndicesString, [Length(Indices)]);

          Face[I].VertexIndex := ParseIndex(Indices[0], FModel.FVertices.Count);
          Face[I].TexCoordIndex := 0;
          Face[I].NormalIndex := 0;
        end;

      2:
        begin
          if Mode = fmNone then
            Mode := fmVT
          else if Mode <> fmVT then
            raise ELoadError.CreateFmt(InconsitentIndicesString, [Length(Indices)]);

          Face[I].VertexIndex := ParseIndex(Indices[0], FModel.FVertices.Count);
          Face[I].TexCoordIndex := ParseIndex(Indices[1], FModel.FTexCoords.Count);
          Face[I].NormalIndex := 0;
        end;

      3:
        begin
          if Indices[1] = '' then
          begin
            if Mode = fmNone then
              Mode := fmVN
            else if Mode <> fmVN then
              raise ELoadError.CreateFmt(InconsitentIndicesString, [Length(Indices)]);

            Face[I].VertexIndex := ParseIndex(Indices[0], FModel.FVertices.Count);
            Face[I].TexCoordIndex := 0;
            Face[I].NormalIndex := ParseIndex(Indices[2], FModel.FNormals.Count);
          end
          else
          begin
            if Mode = fmNone then
              Mode := fmVTN
            else if Mode <> fmVTN then
              raise ELoadError.CreateFmt(InconsitentIndicesString, [Length(Indices)]);

            Face[I].VertexIndex := ParseIndex(Indices[0], FModel.FVertices.Count);
            Face[I].TexCoordIndex := ParseIndex(Indices[1], FModel.FTexCoords.Count);
            Face[I].NormalIndex := ParseIndex(Indices[2], FModel.FNormals.Count);
          end;
        end

    else
      raise ELoadError.CreateFmt('"f-indice-set" expects between 1 and 3 indices, got %d', [Length(Indices)]);
    end;
  end;
  CurrentGroup.FFaces.Add(Face);
end;

{ TModelOBJ.TGroup }

constructor TModelOBJ.TGroup.Create;
begin
  FFaces := TFaces.Create;
end;

destructor TModelOBJ.TGroup.Destroy;
begin
  FFaces.Free;
  inherited;
end;

{ TModelShaderBase }

class function TModelShaderBase.GetAttributeOrder: TShaderAttributeOrder;
begin
  Result := TShaderAttributeOrder.Create(
    'vpos',
    'vtexcoord',
    'vnormal',
    'vtangent',
    'vbitangent',
    'vborderlow',
    'vborderhigh'
    );
end;

end.
