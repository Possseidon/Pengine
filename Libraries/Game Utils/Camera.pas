unit Camera;

interface

uses
  dglOpenGL, VectorGeometry, Matrix, Shaders, SysUtils, Lists, GLEnums;

const
  RotMin = -180;
  RotMax = RotMin + 360;

type

  IRenderable = interface
    ['{ADF8AA5E-F78C-482C-8CD5-DCDB03B71D20}']
    function GetVisible: Boolean;

    property Visible: Boolean read GetVisible;

    function HasBounds: Boolean;
    function Bounds: TBounds3;

    function ModelMatrix: TMatrix4;
    procedure Render;
  end;

  { TCamera }

  TCamera = class
  public type

    TMatrixType = (
      mtModel,
      mtView,
      mtModelView,
      mtProjection,
      mtMVP
      );

    { TUniform }

    TUniform = class abstract
    public type
      TUniformList = TRefArray<TShaderUniform<TMatrix4>>;
      TRotationUniformList = TRefArray<TShaderUniform<TMatrix3>>;

    private
      FCalculatesTo: TUniform;
      FValid: Boolean;
      FData: TMatrix4;
      FUniforms: TUniformList;
      FRotationUniforms: TRotationUniformList;

      function GetData: TMatrix4;

    protected
      procedure SendToUniforms;
      function Calculate: TMatrix4; virtual; abstract;

      property CalculatesTo: TUniform read FCalculatesTo write FCalculatesTo;

    public
      constructor Create;
      destructor Destroy; override;

      procedure SendAllMatrices;
      procedure Invalidate;

      procedure AddUniform(AUniform: TShaderUniform<TMatrix4>); overload;
      procedure DelUniform(AUniform: TShaderUniform<TMatrix4>); overload;

      procedure AddUniform(AUniform: TShaderUniform<TMatrix3>); overload;
      procedure DelUniform(AUniform: TShaderUniform<TMatrix3>); overload;

      property Data: TMatrix4 read GetData;

    end;

    TUniformBasic = class(TUniform)
    public type
      TCalculationMethod = function: TMatrix4 of object;
    private
      FCalculationMethod: TCalculationMethod;
    protected
      function Calculate: TMatrix4; override;
    public
      constructor Create(ACalculationMethod: TCalculationMethod);
    end;

    TUniformCombined = class(TUniform)
    private
      FComponent1: TUniform;
      FComponent2: TUniform;
    protected
      constructor Create(AComponent1, AComponent2: TUniform);
      function Calculate: TMatrix4; override;
    end;

  private
    FRenderObjects: TInterfaceArray<IRenderable>;

    // Translation and Rotation
    FLocation: TLocation;

    // Projection
    FFOV: Single;
    FAspect: Single;
    FFarClip: Single;
    FNearClip: Single;

    FOrtho: Boolean;
    FOrthoChanged: Boolean;
    FOrthoFactor: Single; // Zooms the whole scene

    FMat: array [TMatrixType] of TUniform;
    // FRotMat: array [TRotationMatrixType] of TUniformMatrix3;

    FModelMatrix: TMatrix4;

    function GetHorizontalFOV: Single;

    procedure SetAspect(const AValue: Single);
    procedure SetFarClip(const AValue: Single);
    procedure SetFOV(const AValue: Single);
    procedure SetNearClip(const AValue: Single);
    procedure SetOrtho(const AValue: Boolean);

    function GetModelMatrix: TMatrix4;
    function GetViewMatrix: TMatrix4;
    function GetProjectionMatrix: TMatrix4;

    procedure SetOrthoFactor(const Value: Single);
    function GetMatrix(AMatrixType: TMatrixType): TMatrix4;
    function GetRotMatrix(AMatrixType: TMatrixType): TMatrix3;

  protected
    function GetLocation: TLocation; virtual;

    function RenderObjectVisible(const AFrustum: TGHexahedron; ARenderObject: IRenderable): Boolean; overload;

  public
    constructor Create(FOV, Aspect, NearClip, FarClip: Single);
    destructor Destroy; override;

    property Matrix[AMatrixType: TMatrixType]: TMatrix4 read GetMatrix;
    property RotMatrix[AMatrixType: TMatrixType]: TMatrix3 read GetRotMatrix;

    // Projection Matrix
    property FOV: Single read FFOV write SetFOV;
    property HorizontalFOV: Single read GetHorizontalFOV;
    property Aspect: Single read FAspect write SetAspect;
    property NearClip: Single read FNearClip write SetNearClip;
    property FarClip: Single read FFarClip write SetFarClip;

    property Ortho: Boolean read FOrtho write SetOrtho;
    property OrthoFactor: Single read FOrthoFactor write SetOrthoFactor;

    // Location Data
    property Location: TLocation read GetLocation;

    // Location Movement
    procedure SlideLift(AVector: TVector2; AVertical: Boolean = False);
    procedure SlideMove(AVector: TVector2; AVertical: Boolean = False);

    procedure TurnPitch(AAngles: TVector2);

    function GetCursorLine(APos: TVector2): TLine3;
    function GetUntransformedCursorLine(APos: TVector2): TLine3;

    function GetViewHexahedron: TGHexahedron;

    // Rendering
    procedure Render;

    // Shader Uniform Connection

    /// <summary>
    /// Adds all Uniforms with their Default-Names.
    /// <para>See:</para>
    /// <para><see cref="Camera|TCamera.MatrixNames"/></para>
    /// <para><see cref="Camera|TCamera.MatrixSuffix"/></para>
    /// <para><see cref="Camera|TCamera.RotMatrixSuffix"/></para>
    /// </summary>
    procedure AddUniforms(AShader: TShader);
    /// <summary>
    /// Removes all Uniforms with their Default-Names.
    /// <para>See:</para>
    /// <para><see cref="Camera|TCamera.MatrixNames"/></para>
    /// <para><see cref="Camera|TCamera.MatrixSuffix"/></para>
    /// <para><see cref="Camera|TCamera.RotMatrixSuffix"/></para>
    /// </summary>
    procedure DelUniforms(AShader: TShader);

    /// <summary>Registers a specific Matrix-Uniform-Connection</summary>
    /// <param name="AType">Which Matrix Type to bind the Uniform to</param>
    /// <param name="AUniform">The Uniform, getting bound</param>
    /// <remarks>Inactive Uniforms get ignored automatically</remarks>
    /// <remarks>Inactive Uniforms get ignored automatically</remarks>
    /// <remarks>Inactive Uniforms get ignored automatically</remarks>
    procedure AddUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix4>); overload;
    /// <summary>Registers a specific RotationMatrix-Uniform-Connection</summary>
    /// <param name="AType">Which Matrix Type to bind the Uniform to</param>
    /// <param name="AUniform">The Uniform, getting bound</param>
    procedure AddUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix3>); overload;

    /// <summary>Unregisters a specific Matrix-Uniform-Connection</summary>
    /// <param name="AType">Which Matrix Type to unbind the Uniform from</param>
    /// <param name="AUniform">The Uniform, getting unbound</param>
    procedure DelUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix4>); overload;
    /// <summary>Unregisters a specific RotationMatrix-Uniform-Connection</summary>
    /// <param name="AType">Which Matrix Type to unbind the Uniform from</param>
    /// <param name="AUniform">The Uniform, getting unbound</param>
    procedure DelUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix3>); overload;

    // VAOs to render with given Model Matrix
    procedure AddRenderObject(ARenderObject: IRenderable);
    procedure DelRenderObject(ARenderObject: IRenderable);

    function RenderObjectVisible(AVAO: IRenderable): Boolean; overload;

  const
    /// <summary>
    /// Default Matrix Names:
    /// <code>
    /// <para>model_matrix</para>
    /// <para>view_matrix</para>
    /// <para>modelview_matrix</para>
    /// <para>projection_matrix</para>
    /// <para>mvp_matrix</para>
    /// </code>
    /// <para>For mat3 Rotation-Matrices replace the _matrix with _rmatrix</para>
    /// </summary>
    MatrixNames: array [TMatrixType] of AnsiString = (
      'model', // mtModel
      'view', // mtView
      'modelwiew', // mtModelView
      'projection', // mtProjection
      'mvp' // mtMVP
      );

    /// <summary>For Matrix-Default-Names see: <see cref="Camera|TCamera.MatrixNames"/></summary>
    MatrixSuffix: AnsiString = '_matrix';
    /// <summary>For Matrix-Default-Names see: <see cref="Camera|TCamera.MatrixNames"/></summary>
    RotMatrixSuffix: AnsiString = '_rmatrix';

  end;

implementation

uses
  Math;

{ TCamera }

constructor TCamera.Create(FOV, Aspect, NearClip, FarClip: Single);
begin
  if FOV = 0 then
    FOrtho := True;
  FOrthoFactor := 1;

  FFOV := FOV;
  FAspect := Aspect;
  FNearClip := NearClip;
  FFarClip := FarClip;

  FLocation := TLocation.Create(True);

  FMat[mtModel] := TUniformBasic.Create(GetModelMatrix);
  FMat[mtView] := TUniformBasic.Create(GetViewMatrix);
  FMat[mtModelView] := TUniformCombined.Create(FMat[mtView], FMat[mtModel]);
  FMat[mtProjection] := TUniformBasic.Create(GetProjectionMatrix);
  FMat[mtMVP] := TUniformCombined.Create(FMat[mtProjection], FMat[mtModelView]);

  FRenderObjects := TInterfaceArray<IRenderable>.Create;
end;

destructor TCamera.Destroy;
var
  Mat: TUniform;
begin
  for Mat in FMat do
    Mat.Free;
  FLocation.Free;
  FRenderObjects.Free;
  inherited;
end;

function TCamera.GetCursorLine(APos: TVector2): TLine3;
var
  Look, Right, Up: TVector3;
begin
  Look := Location.Look;
  Right := Location.Right;
  Up := Location.Up;
  if Ortho then
  begin
    Result.SV := (APos.X * Right + APos.Y * Up) / OrthoFactor - Location.RealPosition;
    Result.DV := Look;
  end
  else
  begin
    Result.SV := Location.RealPosition;
    APos.X := APos.X * Tan(FOV / 360 * Pi);
    APos.Y := APos.Y * Tan(FOV / 360 * Pi);
    Result.DV := Look + APos.X * Right + APos.Y * Up;
  end;
end;

function TCamera.GetUntransformedCursorLine(APos: TVector2): TLine3;
begin
  if Ortho then
  begin
    raise Exception.Create('Might not work!');
    Result.SV := (Vec3(APos.X * Aspect, 0, 0) + Vec3(0, APos.Y, 0)) / OrthoFactor - FLocation.Offset;
    Result.DV := Vec3(0, 0, -1);
  end
  else
  begin
    Result.SV := FLocation.Offset;
    Result.DV.X := APos.X * Tan(FOV / 360 * Pi);
    Result.DV.Y := APos.Y * Tan(FOV / 360 * Pi);
    Result.DV.Z := -1;
  end;
end;

function TCamera.GetViewHexahedron: TGHexahedron;
var
  P, L, R, U: TVector3;
  F: Single;
begin
  P := Location.RealPosition;
  R := Location.Right;
  U := Location.Up;
  L := Location.Look;
  if Ortho then
  begin
    Result.FaceNormals[sdRight].Create(P + R * OrthoFactor * Aspect, -R);
    Result.FaceNormals[sdLeft].Create(P - R * OrthoFactor * Aspect, +R);
    Result.FaceNormals[sdUp].Create(P + U * OrthoFactor, -U);
    Result.FaceNormals[sdDown].Create(P - U * OrthoFactor, +U);
    Result.FaceNormals[sdFront].Create(P + L * NearClip, L);
    Result.FaceNormals[sdBack].Create(P + L * FarClip, -L);
  end
  else
  begin
    F := cot(FOV / 360 * Pi);
    Result.FaceNormals[sdRight].Create(P, L - R * F / Aspect);
    Result.FaceNormals[sdLeft].Create(P, L + R * F / Aspect);
    Result.FaceNormals[sdUp].Create(P, L - U * F);
    Result.FaceNormals[sdDown].Create(P, L + U * F);
    Result.FaceNormals[sdFront].Create(P + L * NearClip, L);
    Result.FaceNormals[sdBack].Create(P + L * FarClip, -L);
  end;
end;

function TCamera.GetViewMatrix: TMatrix4;
begin
  Result := FLocation.Matrix;
end;

procedure TCamera.AddUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix4>);
begin
  if AUniform.Active then
    FMat[AType].AddUniform(AUniform);
end;

procedure TCamera.AddUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix3>);
begin
  if AUniform.Active then
    FMat[AType].AddUniform(AUniform);
end;

procedure TCamera.AddUniforms(AShader: TShader);
var
  MatrixType: TMatrixType;
begin
  for MatrixType := Low(TMatrixType) to High(TMatrixType) do
  begin
    FMat[MatrixType].AddUniform(AShader.Uniform<TMatrix4>(MatrixNames[MatrixType] + MatrixSuffix));
    FMat[MatrixType].AddUniform(AShader.Uniform<TMatrix3>(MatrixNames[MatrixType] + RotMatrixSuffix));
  end;
end;

procedure TCamera.AddRenderObject(ARenderObject: IRenderable);
begin
  FRenderObjects.Add(ARenderObject);
end;

procedure TCamera.DelUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix4>);
begin
  if AUniform.Active then
    FMat[AType].DelUniform(AUniform);
end;

procedure TCamera.DelUniform(AType: TMatrixType; AUniform: TShaderUniform<TMatrix3>);
begin
  if AUniform.Active then
    FMat[AType].DelUniform(AUniform);
end;

procedure TCamera.DelUniforms(AShader: TShader);
var
  MatrixType: TMatrixType;
begin
  for MatrixType := Low(TMatrixType) to High(TMatrixType) do
  begin
    FMat[MatrixType].DelUniform(AShader.Uniform<TMatrix4>(MatrixNames[MatrixType] + MatrixSuffix));
    FMat[MatrixType].DelUniform(AShader.Uniform<TMatrix3>(MatrixNames[MatrixType] + RotMatrixSuffix));
  end;
end;

procedure TCamera.DelRenderObject(ARenderObject: IRenderable);
begin
  FRenderObjects.Del(ARenderObject);
end;

function TCamera.RenderObjectVisible(AVAO: IRenderable): Boolean;
begin
  Result := RenderObjectVisible(GetViewHexahedron, AVAO);
end;

procedure TCamera.SlideLift(AVector: TVector2; AVertical: Boolean);
begin
  FLocation.Slide(AVector.X, AVertical);
  FLocation.Lift(AVector.Y, AVertical);
end;

procedure TCamera.SlideMove(AVector: TVector2; AVertical: Boolean);
begin
  FLocation.Slide(AVector.X, AVertical);
  FLocation.Move(AVector.Y, AVertical);
end;

procedure TCamera.Render;
var
  RenderObject: IRenderable;
  Frustum: TGHexahedron;
begin
  Frustum := GetViewHexahedron;
  if Location.Changed then
  begin
    FMat[mtView].Invalidate;
    Location.NotifyChanges;
  end;

  for RenderObject in FRenderObjects do
    if RenderObject.Visible and RenderObjectVisible(Frustum, RenderObject) then
    begin
      FModelMatrix := RenderObject.ModelMatrix;
      FMat[mtModel].Invalidate;
      FMat[mtMVP].SendAllMatrices;
      RenderObject.Render;
    end;
end;

procedure TCamera.TurnPitch(AAngles: TVector2);
begin
  FLocation.Turn(AAngles.X);
  FLocation.Pitch(AAngles.Y);
end;

procedure TCamera.SetAspect(const AValue: Single);
begin
  FAspect := AValue;
  FMat[mtProjection].Invalidate;
end;

procedure TCamera.SetFarClip(const AValue: Single);
begin
  FFarClip := AValue;
  FMat[mtProjection].Invalidate;
end;

procedure TCamera.SetFOV(const AValue: Single);
begin
  FFOV := AValue;
  FMat[mtProjection].Invalidate;
end;

procedure TCamera.SetNearClip(const AValue: Single);
begin
  FNearClip := AValue;
  FMat[mtProjection].Invalidate;
end;

procedure TCamera.SetOrtho(const AValue: Boolean);
begin
  FOrtho := AValue;
  FOrthoChanged := True;
  FMat[mtProjection].Invalidate;
end;

procedure TCamera.SetOrthoFactor(const Value: Single);
begin
  FOrthoFactor := Value;
  FMat[mtProjection].Invalidate;
end;

function TCamera.GetHorizontalFOV: Single;
begin
  Result := arctan(Tan(FOV / 360 * Pi) * Aspect) * 360 / Pi;
end;

function TCamera.GetLocation: TLocation;
begin
  Result := FLocation;
end;

function TCamera.RenderObjectVisible(const AFrustum: TGHexahedron; ARenderObject: IRenderable): Boolean;
var
  I: Integer;
  Points: TBounds3.TCorners;
begin
  if not ARenderObject.HasBounds then
    Exit(True);

  Points := ARenderObject.Bounds.GetCorners;
  for I := 0 to 7 do
    Points[I] := ARenderObject.ModelMatrix * Points[I];

  Result := not AFrustum.AllOutside(Points);
end;

function TCamera.GetMatrix(AMatrixType: TMatrixType): TMatrix4;
begin
  Result := FMat[AMatrixType].Data;
end;

function TCamera.GetModelMatrix: TMatrix4;
begin
  Result := FModelMatrix;
end;

function TCamera.GetProjectionMatrix: TMatrix4;
var
  F: Single;
begin
  Result.Clear;
  if Ortho then
  begin
    Result[0, 0] := OrthoFactor / Aspect;
    Result[1, 1] := OrthoFactor;
    Result[2, 2] := -2 / (FarClip - NearClip);
    Result[3, 2] := -(FarClip + NearClip) / (FarClip - NearClip);
    Result[3, 3] := 1;
  end
  else
  begin
    F := cot(DegToRad(FOV) / 2);
    Result[0, 0] := F / Aspect;
    Result[1, 1] := F;
    Result[2, 2] := (NearClip + FarClip) / (NearClip - FarClip);
    Result[2, 3] := -1;
    Result[3, 2] := (2 * NearClip * FarClip) / (NearClip - FarClip);
  end;
end;

function TCamera.GetRotMatrix(AMatrixType: TMatrixType): TMatrix3;
begin
  Result := FMat[AMatrixType].Data.Minor[3, 3];
end;

{ TCamera.TUniform }

procedure TCamera.TUniform.AddUniform(AUniform: TShaderUniform<TMatrix3>);
begin
  FRotationUniforms.Add(AUniform);
end;

procedure TCamera.TUniform.AddUniform(AUniform: TShaderUniform<TMatrix4>);
begin
  FUniforms.Add(AUniform);
end;

constructor TCamera.TUniform.Create;
begin
  FUniforms := TUniformList.Create;
  FRotationUniforms := TRotationUniformList.Create;
end;

procedure TCamera.TUniform.DelUniform(AUniform: TShaderUniform<TMatrix3>);
begin
  FRotationUniforms.Del(AUniform);
end;

procedure TCamera.TUniform.DelUniform(AUniform: TShaderUniform<TMatrix4>);
begin
  FUniforms.Del(AUniform);
end;

destructor TCamera.TUniform.Destroy;
begin
  FUniforms.Free;
  FRotationUniforms.Free;
  inherited;
end;

function TCamera.TUniform.GetData: TMatrix4;
begin
  if not FValid then
  begin
    FData := Calculate;
    FValid := True;
  end;
  Result := FData;
end;

procedure TCamera.TUniform.SendAllMatrices;
begin
  SendToUniforms;
end;

procedure TCamera.TUniform.SendToUniforms;
var
  Uniform: TShaderUniform<TMatrix4>;
  RotationUniform: TShaderUniform<TMatrix3>;
begin
  for Uniform in FUniforms do
    Uniform.Value := Data;
  for RotationUniform in FRotationUniforms do
    RotationUniform.Value := Data.Minor[3, 3];
end;

procedure TCamera.TUniform.Invalidate;
begin
  FValid := False;
  if FCalculatesTo <> nil then
    FCalculatesTo.Invalidate;
end;

{ TCamera.TUniformCombined }

function TCamera.TUniformCombined.Calculate: TMatrix4;
begin
  FComponent1.SendAllMatrices;
  FComponent2.SendAllMatrices;
  Result := FComponent1.Data * FComponent2.Data;
end;

constructor TCamera.TUniformCombined.Create(AComponent1, AComponent2: TUniform);
begin
  inherited Create;
  FComponent1 := AComponent1;
  FComponent2 := AComponent2;
  FComponent1.CalculatesTo := Self;
  FComponent2.CalculatesTo := Self;
end;

{ TCamera.TUniformBasic }

function TCamera.TUniformBasic.Calculate: TMatrix4;
begin
  Result := FCalculationMethod;
end;

constructor TCamera.TUniformBasic.Create(ACalculationMethod: TCalculationMethod);
begin
  inherited Create;
  FCalculationMethod := ACalculationMethod;
end;

end.
