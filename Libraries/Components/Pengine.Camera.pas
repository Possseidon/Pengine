unit Pengine.Camera;

interface

uses
  dglOpenGL,

  System.SysUtils,

  Pengine.GLEnums,
  Pengine.Matrix,
  Pengine.GLProgram,
  Pengine.CollectionInterfaces,
  Pengine.Collections,
  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.Interfaces,
  Pengine.TimeManager;

  // TODO: camera aspect via resize event

const
  RotMin = -180;
  RotMax = RotMin + 360;

type

  /// <summary>An interface for objects, which can be rendered by a <see cref="Pengine.Camera|TCamera"/>.</summary>
  IRenderable = interface
    ['{ADF8AA5E-F78C-482C-8CD5-DCDB03B71D20}']

    function GetVisible: Boolean;
    function GetLocation: TLocation3;
    procedure SetLocation(const Value: TLocation3);

    /// <summary>Completly supresses the rendering of the object, if set to false.</summary>
    property Visible: Boolean read GetVisible;

    /// <summary>A location, to transform the rendered object.</summary>
    /// <remarks>Returning nil causes no transformation to happen.<p/>
    /// Setting this property is not used by the <see cref="Pengine.Camera|TCamera"/>, but should call
    /// <see cref="Pengine.Vector|TLocation3.Assign"/>.</remarks>
    property Location: TLocation3 read GetLocation write SetLocation;

    /// <returns>An iterable of points of a convex polyhedron, which is used to check, whether the object is visible
    /// in the <see cref="Pengine.Camera|TCamera"/>'s view-frustum.</returns>
    /// <remarks>Return nil, to ignore this occlusion-check.<p/>
    /// For simple cube-bounds you can use <see cref="Pengine.Vector|TBounds3.GetCorners"/>.</remarks>
    function CullPoints: IIterable<TVector3>;

    /// <returns>A radius for a sphere, surrounding the whole object, which is used to check, wether the object is
    /// visible in the <see cref="Pengine.Camera|TCamera"/>'s view-frustum.</returns>
    /// <remarks>Return <see cref="System.Math|Infinity"/>, to ignore this occlusion-check.<p/>
    /// This is remarkably quicker than a lot of <see cref="Pengine.Camera|IRenderable.CullPoints"/>, but by far
    /// not as flexible. In other words, there are more cases, where it will render the object, even though it is not
    /// visible, but the calculation is faster.<p/>
    /// You can combine occlusion-checks, to get vague but quick pre-checks and more exact main-checks, as they are
    /// shortcutted and the quick checks are called first.</remarks>
    function CullRadius: Single;

    /// <returns>An iterable of <see cref="Pengine.Camera|IRenderable"/>, which also get rendered, whenever the parent
    /// gets rendered.</returns>
    /// <remarks>Returning nil is equal to returning an empty iterable.<p/>
    /// The location of the parent will get multiplied onto the locations of the children.</remarks>
    function RenderableChildren: IIterable<IRenderable>;

    /// <summary>Called by the <see cref="Pengine.Camera|TCamera"/>, whenever the object should get rendered.</summary>
    procedure Render;

  end;

  /// <summary>A basic implementation of IRenderable, returning default values for all functions.</summary>
  /// <remarks>
  /// <see cref="Pengine.Camera|IRenderable.GetVisible"/> returns <c>True</c>.<p/>
  /// <see cref="Pengine.Camera|IRenderable.Location"/> getter returns <c>nil</c>.<p/>
  /// <see cref="Pengine.Camera|IRenderable.Location"/> setter calls assign.<p/>
  /// <see cref="Pengine.Camera|IRenderable.CullPoints"/> returns <c>nil</c>.<p/>
  /// <see cref="Pengine.Camera|IRenderable.CullRadius"/> returns <see cref="System.Math|Infinity"/> or...<p/>
  /// <see cref="Pengine.Camera|IRenderable.CullRadius"/> returns furthest cull point.<p/>
  /// <see cref="Pengine.Camera|IRenderable.RenderableChildren"/> returns <c>nil</c>.<p/>
  /// <see cref="Pengine.Camera|IRenderable.Render"/> is <c>abstract</c>.
  /// </remarks>
  TRenderable = class(TInterfaceBase, IRenderable)
  private
    procedure SetLocation(const Value: TLocation3);

  protected
    function GetVisible: Boolean; virtual;

    function GetLocation: TLocation3; virtual;

  public
    property Visible: Boolean read GetVisible;

    property Location: TLocation3 read GetLocation write SetLocation;
    function CullPoints: IIterable<TVector3>; virtual;
    function CullRadius: Single; virtual;
    function RenderableChildren: IIterable<IRenderable>; virtual;

    procedure Render; virtual; abstract;

  end;

  // TODO: XmlDoc
  // Important: free renderables AFTER camera
  // Using the same location for objects rendered in succession speeds up the rendering significantly
  // as the model matrix doesn'T change and no matrix calculations/uniform sending have to be done at all
  TCamera = class
  public type

    /// <summary>An enum, containing all matrices, used in calculations of the <see cref="Pengine.Camera|TCamera"/>.
    /// </summary>
    /// <remarks>See <see cref="Pengine.Camera|TCamera.DefaultMatrixNames"/> to see their respective names to use in shaders.
    /// </remarks>
    TMatrixType = (
      mtModel,
      mtView,
      mtModelView,
      mtProjection,
      mtMVP
      );

    TUniform = class abstract
    public type
      TUniforms = TRefArray<TGLProgram.TUniform<TMatrix4>>;
      TRotationUniforms = TRefArray<TGLProgram.TUniform<TMatrix3>>;

    private
      FCalculatesTo: TUniform;
      FValid: Boolean;
      FData: TMatrix4;
      FUniforms: TUniforms;
      FRotationUniforms: TRotationUniforms;

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

      procedure AddUniform(AUniform: TGLProgram.TUniform<TMatrix4>); overload;
      procedure DelUniform(AUniform: TGLProgram.TUniform<TMatrix4>); overload;

      procedure AddUniform(AUniform: TGLProgram.TUniform<TMatrix3>); overload;
      procedure DelUniform(AUniform: TGLProgram.TUniform<TMatrix3>); overload;

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

    FLocation: TLocation3;

    FFOV: Single;
    FAspect: Single;
    FFarClip: Single;
    FNearClip: Single;

    FOrtho: Boolean;
    FOrthoChanged: Boolean;
    FOrthoFactor: Single;

    FMat: array [TMatrixType] of TUniform;

    FModelLocation: TLocation3;

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

    procedure LocationChanged(AInfo: TLocation3.TChangeEventInfo);

    function OcclusionRadiusVisible(const AFrustum: THexahedron; ARenderable: IRenderable): Boolean;
    function OcclusionPointsVisible(const AFrustum: THexahedron; ARenderable: IRenderable): Boolean;

  protected
    function GetLocation: TLocation3; virtual;
    procedure SetLocation(const Value: TLocation3); virtual;

    function RenderableVisible(const AFrustum: THexahedron; ARenderable: IRenderable): Boolean; overload;

  public
    constructor Create(AFOV, AAspect, ANearClip, AFarClip: Single);
    destructor Destroy; override;

    property Matrix[AMatrixType: TMatrixType]: TMatrix4 read GetMatrix;
    property RotMatrix[AMatrixType: TMatrixType]: TMatrix3 read GetRotMatrix;

    property FOV: Single read FFOV write SetFOV;
    property HorizontalFOV: Single read GetHorizontalFOV;
    property Aspect: Single read FAspect write SetAspect;
    property NearClip: Single read FNearClip write SetNearClip;
    property FarClip: Single read FFarClip write SetFarClip;

    property Ortho: Boolean read FOrtho write SetOrtho;
    property OrthoFactor: Single read FOrthoFactor write SetOrthoFactor;

    property Location: TLocation3 read GetLocation write SetLocation;

    procedure SlideLift(AVector: TVector2; AVertical: Boolean = False);
    procedure SlideMove(AVector: TVector2; AVertical: Boolean = False);

    procedure TurnPitch(AAngles: TVector2);

    function GetCursorLine(APos: TVector2): TLine3;
    function GetUntransformedCursorLine(APos: TVector2): TLine3;

    function GetViewFrustum: THexahedron;

    procedure Render;

    /// <summary>Adds all uniforms with the <see cref="Pengine.Camera|TCamera.DefaultMatrixNames"/>.</summary>
    /// <remarks>Use <see cref="Pengine.Camera|TCamera.AddUniform"/> for more control.</remarks>
    procedure AddUniforms(AGLProgram: TGLProgram); overload;
    /// <summary>Removes all uniforms with the <see cref="Pengine.Camera|TCamera.DefaultMatrixNames"/>.</summary>
    /// <remarks>Use <see cref="Pengine.Camera|TCamera.DelUniform"/> for more control.</remarks>
    procedure DelUniforms(AGLProgram: TGLProgram); overload;

    /// <summary>Registers a specific matrix-uniform.</summary>
    /// <param name="AType">Which matrix-type to bind the uniform to.</param>
    /// <param name="AUniform">The uniform to bind to the specified type.</param>
    /// <remarks>Inactive uniforms get ignored automatically.<p/>
    /// Use <see cref="Pengine.Camera|TCamera.AddUniforms"/> to use the
    /// <see cref="Pengine.Camera|TCamera.DefaultMatrixNames"/>.</remarks>
    procedure AddUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix4>); overload;
    /// <summary>Registers a specific rotation-matrix-uniform.</summary>
    /// <param name="AType">Which matrix-type to bind the uniform to.</param>
    /// <param name="AUniform">The uniform to bind to the specified type.</param>
    /// <remarks>Inactive uniforms get ignored automatically.<p/>
    /// Use <see cref="Pengine.Camera|TCamera.AddUniforms"/> to use the
    /// <see cref="Pengine.Camera|TCamera.DefaultMatrixNames"/>.</remarks>
    procedure AddUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix3>); overload;

    /// <summary>Unregisters a specific matrix-uniform.</summary>
    /// <param name="AType">Which matrix-type to unbind the uniform from.</param>
    /// <param name="AUniform">The uniform to unbind from the specified type.</param>
    procedure DelUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix4>); overload;
    /// <summary>Unregisters a specific rotation-matrix-uniform.</summary>
    /// <param name="AType">Which matrix-type to unbind the uniform from.</param>
    /// <param name="AUniform">The uniform to unbind from the specified type.</param>
    procedure DelUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix3>); overload;

    procedure AddRenderable(ARenderable: IRenderable); overload;
    procedure AddRenderable(ARenderables: IIterable<IRenderable>); overload;

    procedure DelRenderable(ARenderable: IRenderable); overload;
    procedure DelRenderable(ARenderables: IIterable<IRenderable>); overload;

    function RenderableVisible(ARenderable: IRenderable): Boolean; overload;

  const
    /// <summary>
    /// The default matrix-names to use in shaders:
    /// <code>
    /// <p>model_matrix</p>
    /// <p>view_matrix</p>
    /// <p>modelview_matrix</p>
    /// <p>projection_matrix</p>
    /// <p>mvp_matrix</p>
    /// </code>
    /// </summary>
    /// <remarks>Replace <c>matrix</c> with <c>rmatrix</c> for rotation-matrices.</remarks>
    DefaultMatrixNames: array [TMatrixType] of AnsiString = (
      'model',
      'view',
      'modelview',
      'projection',
      'mvp'
      );

    MatrixSuffix: AnsiString = '_matrix';
    RotMatrixSuffix: AnsiString = '_rmatrix';

  end;

implementation

uses
  Math;

{ TCamera }

constructor TCamera.Create(AFOV, AAspect, ANearClip, AFarClip: Single);
begin
  if AFOV = 0 then
    FOrtho := True;
  FOrthoFactor := 1;

  FFOV := AFOV;
  FAspect := AAspect;
  FNearClip := ANearClip;
  FFarClip := AFarClip;

  FLocation := TLocation3.Create(True);
  FLocation.OnChanged.Add(LocationChanged);

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
    Result.S := (APos.X * Right + APos.Y * Up) / OrthoFactor - Location.RealPosition;
    Result.D := Look;
  end
  else
  begin
    Result.S := Location.RealPosition;
    APos.X := APos.X * Tan(FOV / 360 * Pi);
    APos.Y := APos.Y * Tan(FOV / 360 * Pi);
    Result.D := Look + APos.X * Right + APos.Y * Up;
  end;
end;

function TCamera.GetUntransformedCursorLine(APos: TVector2): TLine3;
begin
  if Ortho then
  begin
    raise Exception.Create('Might not work!');
    Result.S := (Vec3(APos.X * Aspect, 0, 0) + Vec3(0, APos.Y, 0)) / OrthoFactor - FLocation.Offset;
    Result.D := Vec3(0, 0, -1);
  end
  else
  begin
    Result.S := FLocation.Offset;
    Result.D.X := APos.X * Tan(FOV / 360 * Pi);
    Result.D.Y := APos.Y * Tan(FOV / 360 * Pi);
    Result.D.Z := -1;
  end;
end;

function TCamera.GetViewFrustum: THexahedron;
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
    Result[bdLeft] := Line3(P - R / OrthoFactor * Aspect, -R);
    Result[bdRight] := Line3(P + R / OrthoFactor * Aspect, +R);
    Result[bdDown] := Line3(P - U / OrthoFactor, -U);
    Result[bdUp] := Line3(P + U / OrthoFactor, +U);
    Result[bdBack] := Line3(P + L * FarClip, +L);
    Result[bdFront] := Line3(P + L * NearClip, -L);
  end
  else
  begin
    F := cot(FOV / 360 * Pi);
    Result[bdRight] := Line3(P, R * F / Aspect - L);
    Result[bdLeft] := Line3(P, -R * F / Aspect - L);
    Result[bdUp] := Line3(P, U * F - L);
    Result[bdDown] := Line3(P, -U * F - L);
    Result[bdBack] := Line3(P + L * FarClip, +L);
    Result[bdFront] := Line3(P + L * NearClip, -L);
  end;
end;

function TCamera.GetViewMatrix: TMatrix4;
begin
  Result := FLocation.Matrix;
end;

procedure TCamera.AddUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix4>);
begin
  if AUniform.Active then
    FMat[AType].AddUniform(AUniform);
end;

procedure TCamera.AddRenderable(ARenderables: IIterable<IRenderable>);
var
  Renderable: IRenderable;
begin
  for Renderable in ARenderables do
    AddRenderable(Renderable);
end;

procedure TCamera.AddUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix3>);
begin
  if AUniform.Active then
    FMat[AType].AddUniform(AUniform);
end;

procedure TCamera.AddUniforms(AGLProgram: TGLProgram);
var
  MatrixType: TMatrixType;
begin
  for MatrixType := Low(TMatrixType) to High(TMatrixType) do
  begin
    FMat[MatrixType].AddUniform(AGLProgram.Uniform<TMatrix4>(DefaultMatrixNames[MatrixType] + MatrixSuffix));
    FMat[MatrixType].AddUniform(AGLProgram.Uniform<TMatrix3>(DefaultMatrixNames[MatrixType] + RotMatrixSuffix));
  end;
end;

procedure TCamera.AddRenderable(ARenderable: IRenderable);
begin
  FRenderObjects.Add(ARenderable);
end;

procedure TCamera.DelUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix4>);
begin
  if AUniform.Active then
    FMat[AType].DelUniform(AUniform);
end;

procedure TCamera.DelRenderable(ARenderables: IIterable<IRenderable>);
var
  Renderable: IRenderable;
begin
  for Renderable in ARenderables do
    DelRenderable(Renderable);
end;

procedure TCamera.DelUniform(AType: TMatrixType; AUniform: TGLProgram.TUniform<TMatrix3>);
begin
  if AUniform.Active then
    FMat[AType].DelUniform(AUniform);
end;

procedure TCamera.DelUniforms(AGLProgram: TGLProgram);
var
  MatrixType: TMatrixType;
begin
  for MatrixType := Low(TMatrixType) to High(TMatrixType) do
  begin
    FMat[MatrixType].DelUniform(AGLProgram.Uniform<TMatrix4>(DefaultMatrixNames[MatrixType] + MatrixSuffix));
    FMat[MatrixType].DelUniform(AGLProgram.Uniform<TMatrix3>(DefaultMatrixNames[MatrixType] + RotMatrixSuffix));
  end;
end;

procedure TCamera.DelRenderable(ARenderable: IRenderable);
begin
  FRenderObjects.Del(ARenderable);
end;

function TCamera.RenderableVisible(ARenderable: IRenderable): Boolean;
begin
  Result := RenderableVisible(GetViewFrustum, ARenderable);
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
  Frustum: THexahedron;

  procedure RenderList(AList: IIterable<IRenderable>);
  var
    RenderObject: IRenderable;
    NewModelLocation: TLocation3;
  begin
    for RenderObject in AList do
    begin
      if RenderObject.Visible then
      begin
        if RenderableVisible(Frustum, RenderObject) then
        begin
          NewModelLocation := RenderObject.Location;
          if FModelLocation <> NewModelLocation then
          begin
            FModelLocation := NewModelLocation;
            FMat[mtModel].Invalidate;
            FMat[mtMVP].SendAllMatrices;
          end;
          RenderObject.Render;
        end;
        if RenderObject.RenderableChildren <> nil then
          RenderList(RenderObject.RenderableChildren);
      end;
    end;
  end;

begin
  Frustum := GetViewFrustum;
  FMat[mtMVP].SendAllMatrices;
  RenderList(FRenderObjects);
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

procedure TCamera.SetLocation(const Value: TLocation3);
begin
  FLocation.Assign(Value);
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
  Result := ArcTan(Tan(FOV / 360 * Pi) * Aspect) * 360 / Pi;
end;

function TCamera.GetLocation: TLocation3;
begin
  Result := FLocation;
end;

function TCamera.RenderableVisible(const AFrustum: THexahedron; ARenderable: IRenderable): Boolean;
begin
  Result :=
    OcclusionRadiusVisible(AFrustum, ARenderable) and
    OcclusionPointsVisible(AFrustum, ARenderable);
end;

function TCamera.GetMatrix(AMatrixType: TMatrixType): TMatrix4;
begin
  Result := FMat[AMatrixType].Data;
end;

function TCamera.GetModelMatrix: TMatrix4;
begin
  if FModelLocation <> nil then
    Result := FModelLocation.Matrix
  else
    Result.LoadIdentity;
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
  Result := FMat[AMatrixType].Data.Minor[3];
end;

procedure TCamera.LocationChanged(AInfo: TLocation3.TChangeEventInfo);
begin
  FMat[mtView].Invalidate;
end;

function TCamera.OcclusionRadiusVisible(const AFrustum: THexahedron; ARenderable: IRenderable): Boolean;
var
  R: Single;
  Point: TVector3;
begin
  R := ARenderable.CullRadius;
  if IsInfinite(R) then
    Exit(True);
  if ARenderable.Location <> nil then
  begin
    Point.X := ARenderable.Location.Matrix[3, 0];
    Point.Y := ARenderable.Location.Matrix[3, 1];
    Point.Z := ARenderable.Location.Matrix[3, 2];
  end
  else
    Point := Vec3(0);
  Result := AFrustum.SphereVisible(Point, R);
end;

function TCamera.OcclusionPointsVisible(const AFrustum: THexahedron; ARenderable: IRenderable): Boolean;
var
  CullPoints: TArray<TVector3>;
  Point: TVector3;
begin
  if ARenderable.CullPoints = nil then
    Exit(True);

  CullPoints := TArray<TVector3>.Create;

  if ARenderable.CullPoints.CountOptimized then
    CullPoints.Capacity := ARenderable.CullPoints.Count;

  for Point in ARenderable.CullPoints do
    CullPoints.Add(ARenderable.Location.Matrix * Point);

  Result := AFrustum.AnyVisible(CullPoints);

  CullPoints.Free;
end;

{ TCamera.TUniform }

procedure TCamera.TUniform.AddUniform(AUniform: TGLProgram.TUniform<TMatrix3>);
begin
  if AUniform.Active then
    FRotationUniforms.Add(AUniform);
end;

procedure TCamera.TUniform.AddUniform(AUniform: TGLProgram.TUniform<TMatrix4>);
begin
  if AUniform.Active then
    FUniforms.Add(AUniform);
end;

constructor TCamera.TUniform.Create;
begin
  FUniforms := TUniforms.Create;
  FRotationUniforms := TRotationUniforms.Create;
end;

procedure TCamera.TUniform.DelUniform(AUniform: TGLProgram.TUniform<TMatrix3>);
begin
  if AUniform.Active then
    FRotationUniforms.Del(AUniform);
end;

procedure TCamera.TUniform.DelUniform(AUniform: TGLProgram.TUniform<TMatrix4>);
begin
  if AUniform.Active then
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
  I: Integer;
begin
  for I := 0 to FUniforms.MaxIndex do
    FUniforms[I].Value := Data;
  for I := 0 to FRotationUniforms.MaxIndex do
    FRotationUniforms[I].Value := Data.Minor[3];
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

{ TRenderable }

function TRenderable.GetLocation: TLocation3;
begin
  Result := nil;
end;

function TRenderable.GetVisible: Boolean;
begin
  Result := True;
end;

function TRenderable.CullPoints: IIterable<TVector3>;
begin
  Result := nil;
end;

function TRenderable.CullRadius: Single;
var
  Point: TVector3;
begin
  if CullPoints = nil then
    Exit(Infinity);
  Result := 0;
  for Point in CullPoints do
    Result := Max(Result, Point.Length);
end;

function TRenderable.RenderableChildren: IIterable<IRenderable>;
begin
  Result := nil;
end;

procedure TRenderable.SetLocation(const Value: TLocation3);
begin
  Location.Assign(Value);
end;

end.

