unit ModelDefine;

interface

uses
  IntfBase, Camera, VectorGeometry, VAOManager, Lists, SysUtils, Classes, Logger;

type

  { TModel }

  TModel = class
  public
    procedure GenerateVAO(AVAO: TVAO); virtual; abstract;
  end;

  { TModelOBJ }

  TModelOBJ = class(TModel)
  public type
    TVertices = TGenericArray<TVector4>;
    TNormals = TGenericArray<TVector3>;
    TTexCoord = TGenericArray<TTexCoord3>;
    TFace = array [TTriangleSide] of Integer;

    TGroup = class
    private
      FVertices: TGenericArray<TVector3>;
      FFaces: TGenericArray<TFace>;

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

      procedure ProcessLine(ALine: string);

    public
      class constructor Create;
      class destructor Destroy;

      constructor Create(AModel: TModelOBJ; ALines: TStrings);

    published      
      {$REGION 'Vertex data'}
      procedure Process_v(AArguments: TArray<string>);
      // procedure Process_vt(AArguments: TArray<string>);
      // procedure Process_vn(AArguments: TArray<string>);
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
    FDefaultGroup: TGroup;
    FGroups: TGroups;
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
  FDefaultGroup := TGroup.Create;
  FGroups := TGroups.Create;
  FLog := TCodeLog.Create;
end;

destructor TModelOBJ.Destroy;
begin
  FLog.Free;
  FGroups.Free;
  FDefaultGroup.Free;
  inherited;
end;

procedure TModelOBJ.GenerateVAO(AVAO: TVAO);
begin
  // TODO: GenerateVAO
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

procedure TModelOBJ.TLoader.ProcessLine(ALine: string);
var
  Arguments: TArray<string>;
begin
  ALine := ALine.Trim;
  if ALine.StartsWith('#') then
    Exit;
  Arguments := ALine.Split([' ']);
  if Length(Arguments) = 0 then
    Exit;
  if not FCommands.Get(Arguments[0], FMethodHelper.Code) then
    raise ELoadError.Create('No such Command "%s"');
  if FMethodHelper.Code <> nil then
  begin
    Delete(Arguments, 0, 1);
    TProcessingFunc(FMethodHelper)(Arguments);
  end
  else
  begin
    FModel.Log.Add(TCodeLogEntry.Create(
      Format('Ignored unsupported Command "%s"', []), FLineNumber, esWarning));
  end;
end;

procedure TModelOBJ.TLoader.Process_v(AArguments: TArray<string>);
begin
  
end;

procedure TModelOBJ.TLoader.Process_f(AArguments: TArray<string>);
begin

end;

{ TModelOBJ.TGroup }

constructor TModelOBJ.TGroup.Create;
begin

end;

destructor TModelOBJ.TGroup.Destroy;
begin

  inherited;
end;

end.
