unit ControlledCamera;

interface

uses
  Classes, SysUtils, Camera, InputHandler, Controls, VectorGeometry, Windows, OpenGLContext
  {$IFNDEF FPC}
  , UITypes
  {$ENDIF};

type

  TRequestLockPointFunction = function(out APoint: TVector3): Boolean of object;

  { TControlledCamera }

  TControlledCamera = class (TCamera)
  private
    FInput: TInputHandler;
    FMoving: Boolean;

    FOldMousePos: TVector2;

    FResetLocation: TLocation;

    FLowerLimit, FUpperLimit: TVector3;
    FZoomLowerLimit: Single;
    FZoomSpeed: Single;
    FZoomUpperLimit: Single;
    FPitchLowerLimit: Single;
    FPitchUpperLimit: Single;

    FGetLockPoint: TRequestLockPointFunction;
    FLockPointOnFront: Boolean;
    FLockPointEnabled: Boolean;
    FLockPointRadius: Single;
    FLockPointStart: TVectorDir;
    FSaveCamTurn, FSaveCamPitch, FSaveCamRoll: Single;

    function GetLockPointDirection: TVectorDir;

    procedure SetLowerLimit(AValue: TVector3);
    procedure SetLowerLimitX(AValue: Single);
    procedure SetLowerLimitY(AValue: Single);
    procedure SetLowerLimitZ(AValue: Single);
    procedure SetPitchLowerLimit(AValue: Single);
    procedure SetPitchUpperLimit(AValue: Single);
    procedure SetUpperLimit(AValue: TVector3);
    procedure SetUpperLimitX(AValue: Single);
    procedure SetUpperLimitY(AValue: Single);
    procedure SetUpperLimitZ(AValue: Single);
    procedure SetZoomLowerLimit(AValue: Single);
    procedure SetZoomUpperLimit(AValue: Single);

    procedure DoMovement;

  public
    constructor Create(FOV, Aspect, NearClip, FarClip: Single; AInput: TInputHandler);
    destructor Destroy; override;

    property Moving: Boolean read FMoving;

    procedure SetReset; overload;
    procedure SetReset(FPos, FOffset, FScale: TVector3; FTurn, FPitch: Single; FRoll: Single = 0); overload;
    procedure Reset;

    property PosLowerLimit: TVector3 read FLowerLimit write SetLowerLimit;
    property PosLowerLimitX: Single read FLowerLimit.X write SetLowerLimitX;
    property PosLowerLimitY: Single read FLowerLimit.Y write SetLowerLimitY;
    property PosLowerLimitZ: Single read FLowerLimit.Z write SetLowerLimitZ;
    property PosUpperLimit: TVector3 read FUpperLimit write SetUpperLimit;
    property PosUpperLimitX: Single read FUpperLimit.X write SetUpperLimitX;
    property PosUpperLimitY: Single read FUpperLimit.Y write SetUpperLimitY;
    property PosUpperLimitZ: Single read FUpperLimit.Z write SetUpperLimitZ;
    property PitchUpperLimit: Single read FPitchUpperLimit write SetPitchUpperLimit;
    property PitchLowerLimit: Single read FPitchLowerLimit write SetPitchLowerLimit;

    property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed;

    property ZoomLowerLimit: Single read FZoomLowerLimit write SetZoomLowerLimit;
    property ZoomUpperLimit: Single read FZoomUpperLimit write SetZoomUpperLimit;

    property GetLockPoint: TRequestLockPointFunction write FGetLockPoint;

    procedure Update; virtual;
  end;

  { TSmoothControlledCamera }
  {
  TSmoothControlledCamera = class (TControlledCamera)
  private
    FVisibleLocation: TLocation;
    FGLForm: TGLForm;
    FSmoothSpeed: Single;
    function GetEndLocation: TLocation;

  protected
    function GetLocation: TLocation; override;
  public
    constructor Create(FOV, Aspect, NearClip, FarClip: Single; AGLForm: TGLForm);
    destructor Destroy; override;

    procedure HardReset;

    property SmoothSpeed: Single read FSmoothSpeed write FSmoothSpeed;
    property EndLocation: TLocation read GetEndLocation;

    procedure Update; override;
  end;
  }
implementation

uses
  Math;

const
  MouseSensitivity = 200;

{ TSmoothControlledCamera }
{
procedure TSmoothControlledCamera.Update;
begin
  inherited Update;
  FVisibleLocation.Approach(EndLocation, 1 - exp(-FSmoothSpeed * FGLForm.DeltaTime));
end;

function TSmoothControlledCamera.GetEndLocation: TLocation;
begin
  Result := Location;
end;

function TSmoothControlledCamera.GetLocation: TLocation;
begin
  Result := FVisibleLocation;
end;

constructor TSmoothControlledCamera.Create(FOV, Aspect, NearClip, FarClip: Single; AGLForm: TGLForm);
begin
  FGLForm := AGLForm;
  FVisibleLocation := TLocation.Create(True);
  FSmoothSpeed := 20;
  inherited Create(FOV, Aspect, NearClip, FarClip, AGLForm.Input);
end;

destructor TSmoothControlledCamera.Destroy;
begin
  FVisibleLocation.Free;
  inherited Destroy;
end;

procedure TSmoothControlledCamera.HardReset;
begin
  Reset;
  FVisibleLocation.Assign(EndLocation);
end;
}
{ TControlledCamera }

procedure TControlledCamera.DoMovement;
var
  DeltaMouse: TVector2;
  LockPoint: TVector3;
  LockPointCurrent: TVectorDir;
begin
  with FInput do
  begin
    if ButtonPressed(mbRight) or ButtonPressed(mbLeft) then
    begin
      FOldMousePos := FInput.MousePos;
      if KeyDown(VK_SHIFT) and not KeyDown(VK_CONTROL) then
      begin
        if Assigned(FGetLockPoint) and FGetLockPoint(LockPoint) then
        begin
          FSaveCamTurn := Location.TurnAngle;
          FSaveCamPitch := Location.PitchAngle;
          FSaveCamRoll := Location.RollAngle;
          FLockPointRadius := Location.Pos.DistanceTo(LockPoint);
          FLockPointOnFront := Location.Look.Dot(Location.Pos.VectorTo(LockPoint)) <= 0;
          FLockPointStart := GetLockPointDirection;
          FLockPointEnabled := True;
        end
        else
          FLockPointEnabled := False;
      end;
    end;

    DeltaMouse := FInput.MousePos - FOldMousePos;

    if KeyDown(VK_SHIFT) then
    begin
      if KeyDown(VK_CONTROL) then
      begin
        // Alt Mode
        if ButtonDown(mbRight) then
        begin
          Location.Turn(MouseSensitivity * DeltaMouse.X);
          Location.Lift(-Location.OffsetZ * DeltaMouse.Y, True);
        end
        else if ButtoNDown(mbLeft) then
          SlideMove(-Location.OffsetZ * DeltaMouse);
      end
      else
      begin
        // Locked Mode
        if ButtonDown(mbRight) and FLockPointEnabled then
        begin
          LockPointCurrent := GetLockPointDirection;
          Location.TurnAngle := FSaveCamTurn +
            (LockPointCurrent.TurnAngle - FLockPointStart.TurnAngle);
          Location.PitchAngle := FSaveCamPitch +
            (LockPointCurrent.PitchAngle - FLockPointStart.PitchAngle);
        end;
      end;
    end
    else
    begin
      // Normal Mode
      if ButtonDown(mbRight) then
        TurnPitch(MouseSensitivity * DeltaMouse)
      else if ButtonDown(mbLeft) then
        SlideLift(-Location.OffsetZ * DeltaMouse);
    end;

    if ButtonDown(mbLeft) or buttonDown(mbRight) then
      FOldMousePos := FInput.MousePos;

    Location.Pos := Vec3(
      EnsureRange(Location.Pos.X, FLowerLimit.X, FUpperLimit.X),
      EnsureRange(Location.Pos.Y, FLowerLimit.Y, FUpperLimit.Y),
      EnsureRange(Location.Pos.Z, FLowerLimit.Z, FUpperLimit.Z)
    );

    Location.PitchAngle := EnsureRange(Location.PitchAngle, FPitchLowerLimit, FPitchUpperLimit);

    if ScrolledUp then
      Location.OffsetZ := Max(Location.OffsetZ / (1 + FZoomSpeed), FZoomLowerLimit);
    if ScrolledDown then
      Location.OffsetZ := Location.OffsetZ * (1 + FZoomSpeed);

    Location.OffsetZ := Min(Location.OffsetZ, FZoomUpperLimit);

  end;
end;

function TControlledCamera.GetLockPointDirection: TVectorDir;
var
  L: TLine3;
  D, P: Single;
  OldT, OldP, OldR: Single;
  Distance: Single;
begin
  OldT := Location.TurnAngle;
  OldP := Location.PitchAngle;
  OldR := Location.RollAngle;

  Location.TurnAngle := FSaveCamTurn;
  Location.PitchAngle := FSaveCamPitch;
  Location.RollAngle := FSaveCamRoll;

  L := GetCursorLine(FInput.MousePos);

  Location.TurnAngle := OldT;
  Location.PitchAngle := OldP;
  Location.RollAngle := OldR;

  L.SV := L.SV - Location.Pos;
  L.DV := L.DV.Normalize * FLockPointRadius;
  Distance := L.OrthoProj(0);
  P := L[Distance].Length / FLockPointRadius; // percentage (0 = on sphere-middle, 1 = on sphere-side)

  if P >= 1 then
  begin
    // Try changing the line, so it fits perfectly on the circle
    P := 1;
  end;

  D := Sqrt(1 - Sqr(P));
  if FLockPointOnFront then
    D := Distance - D
  else
    D := Distance + D;

  Result := L[D];
  if not FLockPointOnFront then
  begin
    Result.TurnAngleRad := Result.TurnAngleRad + Pi;
    Result.PitchAngleRad := -Result.PitchAngle;
  end;
end;

procedure TControlledCamera.SetLowerLimit(AValue: TVector3);
begin
  if FLowerLimit = AValue then
    Exit;
  FLowerLimit := AValue;
  Location.Pos := Vec3(
    Max(Location.Pos.X, AValue.X),
    Max(Location.Pos.Y, AValue.Y),
    Max(Location.Pos.Z, AValue.Z)
  );
end;

procedure TControlledCamera.SetLowerLimitX(AValue: Single);
begin
  if FLowerLimit.X = AValue then
    Exit;
  FLowerLimit.X := AValue;
  Location.PosX := Max(Location.Pos.X, AValue);
end;

procedure TControlledCamera.SetLowerLimitY(AValue: Single);
begin
  if FLowerLimit.Y = AValue then
    Exit;
  FLowerLimit.Y := AValue;
  Location.PosY := Max(Location.Pos.Y, AValue);
end;

procedure TControlledCamera.SetLowerLimitZ(AValue: Single);
begin
  if FLowerLimit.Z = AValue then
    Exit;
  FLowerLimit.Z := AValue;
  Location.PosZ := Max(Location.Pos.Z, AValue);
end;

procedure TControlledCamera.SetPitchLowerLimit(AValue: Single);
begin
  if FPitchLowerLimit = AValue then
    Exit;
  FPitchLowerLimit := AValue;
  Location.PitchAngle := Max(Location.PitchAngle, AValue);
end;

procedure TControlledCamera.SetPitchUpperLimit(AValue: Single);
begin
  if FPitchUpperLimit = AValue then
    Exit;
  FPitchUpperLimit := AValue;
  Location.PitchAngle := Min(Location.PitchAngle, AValue);
end;

procedure TControlledCamera.SetUpperLimit(AValue: TVector3);
begin
  if FUpperLimit = AValue then
    Exit;
  FUpperLimit := AValue;
  Location.Pos := Vec3(
    Min(Location.Pos.X, AValue.X),
    Min(Location.Pos.Y, AValue.Y),
    Min(Location.Pos.Z, AValue.Z)
  );
end;

procedure TControlledCamera.SetUpperLimitX(AValue: Single);
begin
  if FUpperLimit.X = AValue then
    Exit;
  FUpperLimit.X := AValue;
  Location.PosX := Min(Location.Pos.X, AValue);
end;

procedure TControlledCamera.SetUpperLimitY(AValue: Single);
begin
  if FUpperLimit.Y = AValue then
    Exit;
  FUpperLimit.Y := AValue;
  Location.PosY := Min(Location.Pos.Y, AValue);
end;

procedure TControlledCamera.SetUpperLimitZ(AValue: Single);
begin
  if FUpperLimit.Z = AValue then
    Exit;
  FUpperLimit.Z := AValue;
  Location.PosZ := Min(Location.Pos.Z, AValue);
end;

procedure TControlledCamera.SetZoomLowerLimit(AValue: Single);
begin
  if AValue <= 0 then
    raise Exception.Create('Zoom-Limit must be greater than zero!');
  if FZoomLowerLimit = AValue then
    Exit;
  FZoomLowerLimit := AValue;
  Location.OffsetZ := Max(Location.OffsetZ, AValue);
end;

procedure TControlledCamera.SetZoomUpperLimit(AValue: Single);
begin
  if FZoomUpperLimit = AValue then
    Exit;
  FZoomUpperLimit := AValue;
  Location.OffsetZ := Min(Location.OffsetZ, AValue);
end;

constructor TControlledCamera.Create(FOV, Aspect, NearClip, FarClip: Single; AInput: TInputHandler);
begin
  inherited Create(FOV, Aspect, NearClip, FarClip);

  FInput := AInput;
  FResetLocation := TLocation.Create;

  PosLowerLimit := -InfVec3;
  PosUpperLimit := +InfVec3;

  PitchLowerLimit := -90;
  PitchUpperLimit := +90;

  ZoomLowerLimit := 0.1;
  ZoomUpperLimit := Infinity;
  ZoomSpeed := 0.1;
end;

destructor TControlledCamera.Destroy;
begin
  FResetLocation.Free;
  inherited;
end;

procedure TControlledCamera.SetReset;
begin
  FResetLocation.Pos := Location.Pos;
  FResetLocation.Offset := Location.Offset;
  FResetLocation.Scale := Location.Scale;
  FResetLocation.TurnAngle := Location.TurnAngle;
  FResetLocation.PitchAngle := Location.PitchAngle;
  FResetLocation.RollAngle := Location.RollAngle;
end;

procedure TControlledCamera.SetReset(FPos, FOffset, FScale: TVector3; FTurn, FPitch: Single; FRoll: Single);
begin
  FResetLocation.Pos := FPos;
  FResetLocation.Offset := FOffset;
  FResetLocation.Scale := FScale;
  FResetLocation.TurnAngle := FTurn;
  FResetLocation.PitchAngle := FPitch;
  FResetLocation.RollAngle := FRoll;
end;

procedure TControlledCamera.Reset;
begin
  Location.Pos := FResetLocation.Pos;
  Location.Offset := FResetLocation.Offset;
  Location.Scale := FResetLocation.Scale;
  Location.TurnAngle := FResetLocation.TurnAngle;
  Location.PitchAngle := FResetLocation.PitchAngle;
  Location.RollAngle := FResetLocation.RollAngle;
end;

procedure TControlledCamera.Update;
begin
  if (FInput.AsyncKeyDown(VK_CONTROL) or FInput.AsyncKeyDown(VK_SHIFT)) and
      FInput.ButtonUp(mbLeft) and FInput.ButtonUp(mbRight) then
    FMoving := True
  else if (FInput.AsyncKeyUp(VK_CONTROL) and FInput.AsyncKeyUp(VK_SHIFT)) then
    FMoving := False;

  if FMoving then
    DoMovement;
end;

end.

