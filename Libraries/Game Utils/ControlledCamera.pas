unit ControlledCamera;

interface

uses
  Classes, SysUtils, Camera, InputHandler, Controls, VectorGeometry, Windows, OpenGLContext
  {$IFNDEF FPC}
  , UITypes
  {$ENDIF};

type

  { TControlledCamera }

  TControlledCamera = class (TCamera)
  private
    FInput: TInputHandler;
    FMoving: Boolean;

    FOldMousePos: TVector2;

    FLowerLimit, FUpperLimit: TVector3;
    FZoomLowerLimit: Single;
    FZoomSpeed: Single;
    FZoomUpperLimit: Single;
    FPitchLowerLimit: Single;
    FPitchUpperLimit: Single;

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

    procedure RecordMovement;
    procedure EnsureLimits;

  protected
    FDeltaRotation: TVector3;
    FDeltaPos: TVector3;
    FDeltaOffset: TVector3;

    procedure ProcessMovement; virtual;


  public
    constructor Create(AFOV, AAspect, ANearClip, AFarClip: Single; AInput: TInputHandler);
    destructor Destroy; override;

    property Moving: Boolean read FMoving;

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

    procedure Update;
  end;

  { TSmoothControlledCamera }

  TSmoothControlledCamera = class(TControlledCamera)
  private
    FGLForm: TGLForm;
    FSmoothSpeed: Single;

  protected
    procedure ProcessMovement; override;

  public
    constructor Create(AFOV, AAspect, ANearClip, AFarClip: Single; AGLForm: TGLForm);

    property SmoothSpeed: Single read FSmoothSpeed write FSmoothSpeed;

  end;

implementation

uses
  Math;

const
  MouseSensitivity = 200;

{ TControlledCamera }

procedure TControlledCamera.RecordMovement;
var
  DeltaMouse: TVector2;
begin
  with FInput do
  begin
    if ButtonPressed(mbRight) or ButtonPressed(mbLeft) then
      FOldMousePos := FInput.MousePos;

    DeltaMouse := FInput.MousePos - FOldMousePos;

    if KeyDown(VK_SHIFT) then
    begin
      // Alt Mode
      if ButtonDown(mbRight) then
      begin
        FDeltaRotation.Y := FDeltaRotation.Y + MouseSensitivity * DeltaMouse.X;
        FDeltaPos.Y := FDeltaPos.Y - Location.OffsetZ * DeltaMouse.Y;
      end
      else if ButtonDown(mbLeft) then
      begin
        FDeltaPos := FDeltaPos - Location.Right * Location.OffsetZ * DeltaMouse.X;
        FDeltaPos := FDeltaPos - Location.Look * Location.OffsetZ * DeltaMouse.Y;
      end;
    end
    else
    begin
      // Normal Mode
      if ButtonDown(mbRight) then
        FDeltaRotation.YX := FDeltaRotation.YX + MouseSensitivity * DeltaMouse
      else if ButtonDown(mbLeft) then
      begin
        FDeltaPos := FDeltaPos - Location.Right * Location.OffsetZ * DeltaMouse.X;
        FDeltaPos := FDeltaPos - Location.Up * Location.OffsetZ * DeltaMouse.Y;
      end;
    end;

    if ButtonDown(mbLeft) or buttonDown(mbRight) then
      FOldMousePos := FInput.MousePos;

    if ScrolledUp then
      FDeltaOffset.Z := Location.OffsetZ - Location.OffsetZ * (1 + FZoomSpeed);
    if ScrolledDown then
      FDeltaOffset.Z := Location.OffsetZ - Location.OffsetZ / (1 + FZoomSpeed);

  end;
end;

procedure TControlledCamera.EnsureLimits;
begin
  Location.Pos := Vec3(
    EnsureRange(Location.Pos.X, FLowerLimit.X, FUpperLimit.X),
    EnsureRange(Location.Pos.Y, FLowerLimit.Y, FUpperLimit.Y),
    EnsureRange(Location.Pos.Z, FLowerLimit.Z, FUpperLimit.Z)
  );
  Location.PitchAngle := EnsureRange(Location.PitchAngle, FPitchLowerLimit, FPitchUpperLimit);
  Location.OffsetZ := EnsureRange(Location.OffsetZ, FZoomLowerLimit, FZoomUpperLimit);
end;

procedure TControlledCamera.ProcessMovement;
begin
  Location.Translate(FDeltaPos);
  Location.Rotate(FDeltaRotation);
  Location.MoveOffset(FDeltaOffset);

  FDeltaRotation := 0;
  FDeltaPos := 0;
  FDeltaOffset := 0;
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

constructor TControlledCamera.Create(AFOV, AAspect, ANearClip, AFarClip: Single; AInput: TInputHandler);
begin
  inherited Create(AFOV, AAspect, ANearClip, AFarClip);

  FInput := AInput;
  
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
  inherited;
end;

procedure TControlledCamera.Update;
begin
  if (FInput.AsyncKeyDown(VK_CONTROL) or FInput.AsyncKeyDown(VK_SHIFT)) and
      FInput.ButtonUp(mbLeft) and FInput.ButtonUp(mbRight) then
    FMoving := True
  else if (FInput.AsyncKeyUp(VK_CONTROL) and FInput.AsyncKeyUp(VK_SHIFT)) then
    FMoving := False;

  if FMoving then
    RecordMovement;

  ProcessMovement;

  EnsureLimits;
end;

{ TSmoothControlledCamera }

procedure TSmoothControlledCamera.ProcessMovement;
var
  Factor: Single;
  DRotation, DPos, DOffset: TVector3;
begin
  Factor := 1 - Exp(-FSmoothSpeed * FGLForm.DeltaTime);

  DRotation := FDeltaRotation * Factor;
  DPos := FDeltaPos * Factor;
  DOffset := FDeltaOffset * Factor;

  Location.Rotate(DRotation);
  Location.Translate(DPos);
  Location.MoveOffset(DOffset);

  FDeltaRotation := FDeltaRotation - DRotation;
  FDeltaPos := FDeltaPos - DPos;
  FDeltaOffset := FDeltaOffset - DOffset;
end;

constructor TSmoothControlledCamera.Create(AFOV, AAspect, ANearClip, AFarClip: Single; AGLForm: TGLForm);
begin
  inherited Create(AFOV, AAspect, ANearClip, AFarClip, AGLForm.Input);
  FGLForm := AGLForm;
  FSmoothSpeed := 42;
end;

end.

