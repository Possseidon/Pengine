unit Camera;

interface

uses
  dglOpenGL, VectorGeometry, Matrix, Shaders, SysUtils, Lists, GLEnums, VAOManager;

const
  RotMin = -180;
  RotMax = RotMin + 360;

type
  TMatrixType = (
    // 4x4
    mtModel,
    mtView,
    mtModelView,
    mtProjection,
    mtMVP,
    // 3x3 rotation
    mtModelRotation,
    mtViewRotation,
    mtModelViewRotation
  );

  // MVP = ModelView * Projection
  // ModelView = View * Model

  TMatrixData = record
    Data: TMatrix4;
    Changed: Boolean;
  end;

  TMatrixUniformNames = array [TMatrixType] of PChar;

  { TCamera }
  TCamera = class
  private
    FShaders: TObjectArray<TShader>;
    FMatrixUniformNames: TMatrixUniformNames;
    FVAOs: TObjectArray<TVAO>;

    function GetHorizontalFOV: Single;
    function GetMatrix(I: TMatrixType): TMatrix4;
    function GetMatrixUniformName(I: TMatrixType): PChar;
    procedure SetMatrixUniformName(I: TMatrixType; AValue: PChar);

    procedure SetPos(AValue: TGVector3);
    procedure SetOffset(const AValue: TGVector3);
    procedure SetAspect(const AValue: Single);
    procedure SetFarClip(const AValue: Single);
    procedure SetFOV(const AValue: Single);
    procedure SetNearClip(const AValue: Single);
    procedure SetOrtho(const AValue: Boolean);

    procedure BuildModelViewMatrix;
    procedure BuildProjectionMatrix;
    procedure BuildMVPMatrix;

    procedure SetOrthoFactor(const Value: Single);

    procedure SetMatrixChanged(M: TMatrixType);

    procedure SendLocation(AShader: TObject);

  protected
    // Translation and Rotation
    FLocation: TLocation;
    FModelMatrix: TMatrix4;

      // Projection
    FFOV: Single;
    FAspect: Single;
    FFarClip: Single;
    FNearClip: Single;

    FOrtho: Boolean;
    FOrthoChanged: Boolean;
    FOrthoFactor: Single; // Zooms the whole scene

    FForceResendLocation: Boolean;

    FMat: array [mtModelView .. mtMVP] of TMatrixData;

    function GetLocation: TLocation; virtual;

    function VAOVisible(const AFrustum: TGHexahedron; AVAO: TVAO): Boolean; overload;

  public
    constructor Create(FOV, Aspect, NearClip, FarClip: Single);
    destructor Destroy; override;

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
    procedure SlideLift(AVector: TGVector2; AVertical: Boolean = False);
    procedure SlideMove(AVector: TGVector2; AVertical: Boolean = False);

    procedure TurnPitch(AAngles: TGVector2);

    function GetCursorLine(APos: TGVector2): TGLine;
    function GetUntransformedCursorLine(APos: TGVector2): TGLine;

    function GetViewHexahedron: TGHexahedron;

    // Rendering
    procedure SendMatrices;
    procedure Render;

    property Matrix[I: TMatrixType]: TMatrix4 read GetMatrix;

    // Shader Connection (shaders to which, the matrix-uniforms will get sent to)
    procedure AddShader(AShader: TShader);
    procedure DelShader(AShader: TShader);

    property MatrixUniformNames[I: TMatrixType]: PChar read GetMatrixUniformName write SetMatrixUniformName;

    // VAOs
    procedure AddVAO(AVAO: TVAO);
    procedure DelVAO(AVAO: TVAO);

    function VAOVisible(AVAO: TVAO): Boolean; overload;

  end;

implementation

uses
  Math;

{ TCamera }

procedure TCamera.BuildModelViewMatrix;
begin
  FMat[mtModelView].Data := Location.Matrix * FModelMatrix;
  FMat[mtModelView].Changed := False;
end;

procedure TCamera.BuildMVPMatrix;
begin
  if FMat[mtModelView].Changed then
    BuildModelViewMatrix;
  if FMat[mtProjection].Changed then
    BuildProjectionMatrix;

  FMat[mtMVP].Data := FMat[mtProjection].Data * FMat[mtModelView].Data;
  FMat[mtMVP].Changed := False;
  Location.NotifyChanges;
end;

procedure TCamera.BuildProjectionMatrix;
var
  F: Single;
begin
  if FOrthoChanged then
  begin
    FMat[mtProjection].Data.Clear;
    FOrthoChanged := False;
  end;

  if Ortho then
  begin
    FMat[mtProjection].Data[0, 0] := OrthoFactor / Aspect;
    FMat[mtProjection].Data[1, 1] := OrthoFactor;
    FMat[mtProjection].Data[2, 2] := -2 / (FarClip - NearClip);
    FMat[mtProjection].Data[3, 2] := -(FarClip + NearClip) / (FarClip - NearClip);
    FMat[mtProjection].Data[3, 3] := 1;
  end
  else
  begin
    F := cot(FOV * Pi / 360);
    FMat[mtProjection].Data[0, 0] := F / Aspect;
    FMat[mtProjection].Data[1, 1] := F;
    FMat[mtProjection].Data[2, 2] := -(FarClip + NearClip) / (FarClip - NearClip);
    FMat[mtProjection].Data[2, 3] := -1;
    FMat[mtProjection].Data[3, 2] := -(2 * FarClip * NearClip) / (FarClip - NearClip);
  end;
  FMat[mtProjection].Changed := False;    
end;

constructor TCamera.Create(FOV, Aspect, NearClip, FarClip: Single);
var
  M: TMatrixType;
begin
  if FOV = 0 then
    FOrtho := True;
  FOrthoFactor := 1;

  FFOV := FOV;
  FAspect := Aspect;
  FNearClip := NearClip;
  FFarClip := FarClip;

  FLocation := TLocation.Create(True);
  FModelMatrix.LoadIdentity;

  // Default Names
  // 3x3 (Rotation)
  MatrixUniformNames[mtViewRotation] := 'vr';
  MatrixUniformNames[mtModelRotation] := 'mr';
  MatrixUniformNames[mtModelViewRotation] := 'r';
  // 4x4
  MatrixUniformNames[mtProjection] := 'p';
  MatrixUniformNames[mtModel] := 'm';
  MatrixUniformNames[mtView] := 'v';
  MatrixUniformNames[mtModelView] := 'mv';
  MatrixUniformNames[mtMVP] := 'mvp';

  for M := mtModelView to mtMVP do
    FMat[M].Changed := True;

  FShaders := TObjectArray<TShader>.Create(True);

  FVAOs := TObjectArray<TVAO>.Create(True);
end;

destructor TCamera.Destroy;
begin
  FShaders.Free;
  FLocation.Free;
  FVAOs.Free;
  inherited;
end;

function TCamera.GetCursorLine(APos: TGVector2): TGLine;
var
  Look, Right, Up: TGVector;
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

function TCamera.GetUntransformedCursorLine(APos: TGVector2): TGLine;
begin
  if Ortho then
  begin
    raise Exception.Create('Might not work!');
    Result.SV := (APos.X * UVecX * Aspect + APos.Y * UVecY) / OrthoFactor - FLocation.Offset;
    Result.DV := -UVecZ;
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
  P, L, R, U: TGVector3;
  F: Single;
begin
  P := Location.RealPosition;
  R := Location.Right;
  U := Location.Up;
  L := Location.Look;
  if Ortho then
  begin
    Result.Faces[sdRight] := TGLine.Create(P + R * OrthoFactor * Aspect, -R);
    Result.Faces[sdLeft] :=  TGLine.Create(P - R * OrthoFactor * Aspect, +R);
    Result.Faces[sdUp] :=    TGLine.Create(P + U * OrthoFactor, -U);
    Result.Faces[sdDown] :=  TGLine.Create(P - U * OrthoFactor, +U);
    Result.Faces[sdFront] := TGLine.Create(P + L * NearClip, L);
    Result.Faces[sdBack] :=  TGLine.Create(P + L * FarClip, -L);
  end
  else
  begin
    F := cot(FOV / 360 * Pi);
    Result.Faces[sdRight] := TGLine.Create(P, L - R * F / Aspect);
    Result.Faces[sdLeft] :=  TGLine.Create(P, L + R * F / Aspect);
    Result.Faces[sdUp] :=    TGLine.Create(P, L - U * F);
    Result.Faces[sdDown] :=  TGLine.Create(P, L + U * F);
    Result.Faces[sdFront] := TGLine.Create(P + L * NearClip, L);
    Result.Faces[sdBack] :=  TGLine.Create(P + L * FarClip, -L);
  end;
end;

procedure TCamera.AddShader(AShader: TShader);
begin
  FShaders.Add(AShader);
  SendLocation(AShader);
end;

procedure TCamera.DelShader(AShader: TShader);
begin
  FShaders.DelObject(AShader);
end;

procedure TCamera.AddVAO(AVAO: TVAO);
begin
  FVAOs.Add(AVAO);
end;

procedure TCamera.DelVAO(AVAO: TVAO);
begin
  FVAOs.DelObject(AVAO);
end;

function TCamera.VAOVisible(AVAO: TVAO): Boolean;
begin
  Result := VAOVisible(GetViewHexahedron, AVAO);
end;

procedure TCamera.SlideLift(AVector: TGVector2; AVertical: Boolean);
begin
  FLocation.Slide(AVector.X, AVertical);
  FLocation.Lift(AVector.Y, AVertical);
end;

procedure TCamera.SlideMove(AVector: TGVector2; AVertical: Boolean);
begin
  FLocation.Slide(AVector.X, AVertical);
  FLocation.Move(AVector.Y, AVertical);
end;

procedure TCamera.SendLocation(AShader: TObject);
var
  M: TMatrixType;
  L: Integer;
  Mat3: TMatrix3;
  OldShader: TShader;
begin
  OldShader := TShader.ActiveShader;

  with AShader as TShader do
  begin
    Enable;
    for M := Low(TMatrixType) to High(TMatrixType) do
    begin
      if FMatrixUniformNames[M] = nil then
        Continue;
      L := UniformLocation[FMatrixUniformNames[M]];
      if L <> -1 then
      begin
        if M in [mtModelRotation, mtViewRotation, mtModelViewRotation] then
        begin
          case M of
            mtModelRotation:
              Mat3 := FModelMatrix.Minor[3, 3];
            mtViewRotation:
              Mat3 := Location.RotMatrix;
            mtModelViewRotation:
              Mat3 := Matrix[mtModelView].Minor[3, 3];
          end;
          glUniformMatrix3fv(L, 1, ToByteBool(blFalse), @Mat3);
        end
        else if M = mtModel then
          glUniformMatrix4fv(L, 1, ToByteBool(blFalse), @FModelMatrix)
        else if M = mtView then
          glUniformMatrix4fv(L, 1, ToByteBool(blFalse), FLocation.Matrix.DataPointer)
        else
          glUniformMatrix4fv(L, 1, ToByteBool(blFalse), @FMat[M].Data);
      end;
    end;
  end;

  OldShader.Enable;
end;

procedure TCamera.SendMatrices;
var
  Shader: TShader;
begin
  if Location.Changed then
  begin
    SetMatrixChanged(mtModelView);
  end;
  if FMat[mtMVP].Changed then
  begin
    BuildMVPMatrix;
    for Shader in FShaders do
      SendLocation(Shader);
  end
  else if FForceResendLocation then
  begin
    for Shader in FShaders do
      SendLocation(Shader);
    FForceResendLocation := False;
  end;
end;

procedure TCamera.Render;
var
  VAO: TVAO;
  Frustum: TGHexahedron;
begin
  Frustum := GetViewHexahedron;
  for VAO in FVAOs do
    if VAO.Visible and VAOVisible(Frustum, VAO) then
    begin
      FModelMatrix := VAO.ModelMatrix;
      SetMatrixChanged(mtModelView);
      SendMatrices;
      VAO.Render;
    end;
end;

procedure TCamera.TurnPitch(AAngles: TGVector2);
begin
  FLocation.Turn(AAngles.X);
  FLocation.Pitch(AAngles.Y);
end;

procedure TCamera.SetAspect(const AValue: Single);
begin
  FAspect := AValue;
  SetMatrixChanged(mtProjection);
end;

procedure TCamera.SetFarClip(const AValue: Single);
begin
  FFarClip := AValue;
  SetMatrixChanged(mtProjection);
end;

procedure TCamera.SetFOV(const AValue: Single);
begin
  FFOV := AValue;
  SetMatrixChanged(mtProjection);
end;

procedure TCamera.SetMatrixChanged(M: TMatrixType);
begin
  // given type changed
  FMat[M].Changed := True;
  // anything changed -> MVP changes
  FMat[mtMVP].Changed := True;
end;

procedure TCamera.SetNearClip(const AValue: Single);
begin
  FNearClip := AValue;
  SetMatrixChanged(mtProjection);
end;

procedure TCamera.SetOffset(const AValue: TGVector3);
begin
  FLocation.Offset := AValue;
  SetMatrixChanged(mtModelView);
end;

procedure TCamera.SetOrtho(const AValue: Boolean);
begin
  FOrtho := AValue;
  FOrthoChanged := True;
  SetMatrixChanged(mtProjection);
end;

procedure TCamera.SetOrthoFactor(const Value: Single);
begin
  FOrthoFactor := Value;
  SetMatrixChanged(mtProjection);
end;

procedure TCamera.SetPos(AValue: TGVector3);
begin
  FLocation.Pos := AValue;
  SetMatrixChanged(mtModelView);
end;

function TCamera.GetMatrix(I: TMatrixType): TMatrix4;
begin
  Result := FMat[I].Data;
end;

function TCamera.GetHorizontalFOV: Single;
begin
  Result := arctan(tan(FOV / 360 * Pi) * Aspect) * 360 / Pi;
end;

function TCamera.GetLocation: TLocation;
begin
  Result := FLocation;
end;

function TCamera.VAOVisible(const AFrustum: TGHexahedron; AVAO: TVAO): Boolean;
var
  I: Integer;
  Points: TGBounds3.TCorners;
begin
  if not AVAO.HasBounds then
    Exit(True);

  Points := AVAO.Bounds.GetCorners;
  for I := 0 to 7 do
    Points[I] := AVAO.ModelMatrix * Points[I].ToVec4;

  Result := not AFrustum.AllOutside(Points);
end;

function TCamera.GetMatrixUniformName(I: TMatrixType): PChar;
begin
  Result := FMatrixUniformNames[I];
end;

procedure TCamera.SetMatrixUniformName(I: TMatrixType; AValue: PChar);
begin
  FMatrixUniformNames[I] := AValue;
  FForceResendLocation := True;
end;

end.
