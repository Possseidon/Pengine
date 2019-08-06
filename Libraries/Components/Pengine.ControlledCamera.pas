unit Pengine.ControlledCamera;

interface

uses
  Winapi.Windows,

  System.Classes,
  System.SysUtils,
  System.Math,
  System.UITypes,

  Vcl.Controls,

  Pengine.Camera,
  Pengine.InputHandler,
  Pengine.Vector,
  Pengine.GLContext,
  Pengine.GLGame;

type

  ECameraInvalidZoomLimit = class(Exception)
  public
    constructor Create;
  end;

  TControlledCamera = class(TCamera)
  public const
    DefaultMouseSensitivity = 90;
    DefaultZoomSpeed = 0.1;

    DefaultPosBounds: TBounds3 = (
      C1: (X: -Infinity; Y: -Infinity; Z: -Infinity);
      C2: (X: Infinity; Y: Infinity; Z: Infinity)
      );
    DefaultPitchLimit: TBounds1 = (C1: -90; C2: +90);
    DefaultZoomLimit: TBounds1 = (C1: 0.1; C2: Infinity);

  private
    FGame: TGLGame;
    FMoving: Boolean;

    FOldMousePos: TVector2;

    FMouseSensitivity: Single;
    FZoomSpeed: Single;

    FPosBounds: TBounds3;
    FPitchLimit: TBounds1;
    FZoomLimit: TBounds1;

    function GetInputHandler: TInputHandler;

    procedure RecordMovement;
    procedure EnsureLimits;

    procedure SetPosBounds(const Value: TBounds3);

    procedure SetPosLowerBound(const Value: TVector3);
    procedure SetPosLowerBoundX(const Value: Single);
    procedure SetPosLowerBoundY(const Value: Single);
    procedure SetPosLowerBoundZ(const Value: Single);

    procedure SetPosUpperBound(const Value: TVector3);
    procedure SetPosUpperBoundX(const Value: Single);
    procedure SetPosUpperBoundY(const Value: Single);
    procedure SetPosUpperBoundZ(const Value: Single);

    procedure SetPitchLimit(const Value: TBounds1);
    procedure SetPitchLowerLimit(const Value: Single);
    procedure SetPitchUpperLimit(const Value: Single);

    procedure SetZoomLimit(const Value: TBounds1);
    procedure SetZoomLowerLimit(const Value: Single);
    procedure SetZoomUpperLimit(const Value: Single);

    procedure Update;
    procedure Resize;

  protected
    FDeltaRotation: TVector3;
    FDeltaPos: TVector3;
    FDeltaOffset: TVector3;

    procedure ProcessMovement; virtual;

  public
    constructor Create(AGame: TGLGame; AFOV, ANearClip, AFarClip: Single);

    property Game: TGLGame read FGame;
    property Input: TInputHandler read GetInputHandler;

    property Moving: Boolean read FMoving;

    property MouseSensitivity: Single read FMouseSensitivity write FMouseSensitivity;

    property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed;

    property PosBounds: TBounds3 read FPosBounds write SetPosBounds;

    property PosLowerBound: TVector3 read FPosBounds.C1 write SetPosLowerBound;
    property PosLowerBoundX: Single read FPosBounds.C1.X write SetPosLowerBoundX;
    property PosLowerBoundY: Single read FPosBounds.C1.Y write SetPosLowerBoundY;
    property PosLowerBoundZ: Single read FPosBounds.C1.Z write SetPosLowerBoundZ;

    property PosUpperBound: TVector3 read FPosBounds.C2 write SetPosUpperBound;
    property PosUpperBoundX: Single read FPosBounds.C2.X write SetPosUpperBoundX;
    property PosUpperBoundY: Single read FPosBounds.C2.Y write SetPosUpperBoundY;
    property PosUpperBoundZ: Single read FPosBounds.C2.Z write SetPosUpperBoundZ;

    property PitchLimit: TBounds1 read FPitchLimit write SetPitchLimit;
    property PitchLowerLimit: Single read FPitchLimit.C1 write SetPitchLowerLimit;
    property PitchUpperLimit: Single read FPitchLimit.C2 write SetPitchUpperLimit;

    property ZoomLimit: TBounds1 read FZoomLimit write SetZoomLimit;
    property ZoomLowerLimit: Single read FZoomLimit.C1 write SetZoomLowerLimit;
    property ZoomUpperLimit: Single read FZoomLimit.C2 write SetZoomUpperLimit;

  end;

  TSmoothControlledCamera = class(TControlledCamera)
  public const
    DefaultSmoothSpeed = 42;

  private
    FSmoothSpeed: Single;

  protected
    procedure ProcessMovement; override;

  public
    constructor Create(AGLGame: TGLGame; AFOV, ANearClip, AFarClip: Single);

    property SmoothSpeed: Single read FSmoothSpeed write FSmoothSpeed;

  end;

implementation

{ ECameraInvalidZoomLimit }

constructor ECameraInvalidZoomLimit.Create;
begin
  inherited Create('The lower camera zoom-limit must be greater than zero!');
end;

{ TControlledCamera }

procedure TControlledCamera.RecordMovement;
var
  DeltaMouse: TVector2;
begin
  if Input.ButtonPressed(mbRight) or Input.ButtonPressed(mbLeft) then
    FOldMousePos := Input.MousePos;

  DeltaMouse := Input.MousePos - FOldMousePos;

  if Input.KeyDown(VK_SHIFT) then
  begin
    // Alt Mode
    if Input.ButtonDown(mbRight) then
    begin
      FDeltaRotation.Y := FDeltaRotation.Y + MouseSensitivity * DeltaMouse.X;
      FDeltaPos.Y := FDeltaPos.Y - Location.OffsetZ * DeltaMouse.Y;
    end
    else if Input.ButtonDown(mbLeft) then
    begin
      FDeltaPos := FDeltaPos - Location.Right * Location.OffsetZ * DeltaMouse.X;
      FDeltaPos := FDeltaPos - Location.Look * Location.OffsetZ * DeltaMouse.Y;
    end;
  end
  else
  begin
    // Normal Mode
    if Input.ButtonDown(mbRight) then
      FDeltaRotation.YX := FDeltaRotation.YX + MouseSensitivity * DeltaMouse
    else if Input.ButtonDown(mbLeft) then
    begin
      FDeltaPos := FDeltaPos - Location.Right * Location.OffsetZ * DeltaMouse.X;
      FDeltaPos := FDeltaPos - Location.Up * Location.OffsetZ * DeltaMouse.Y;
    end;
  end;

  if Input.ButtonDown(mbLeft) or Input.ButtonDown(mbRight) then
    FOldMousePos := Input.MousePos;

  if Input.ScrolledUp then
    FDeltaOffset.Z := Location.OffsetZ - Location.OffsetZ * (1 + FZoomSpeed);
  if Input.ScrolledDown then
    FDeltaOffset.Z := Location.OffsetZ - Location.OffsetZ / (1 + FZoomSpeed);
end;

procedure TControlledCamera.Resize;
begin
  Aspect := Game.Aspect;
end;

procedure TControlledCamera.EnsureLimits;
begin
  Location.Pos := PosBounds.Clamp(Location.Pos);
  Location.PitchAngle := PitchLimit.Clamp(Location.PitchAngle);
  Location.OffsetZ := ZoomLimit.Clamp(Location.OffsetZ);
end;

function TControlledCamera.GetInputHandler: TInputHandler;
begin
  Result := Game.Input;
end;

procedure TControlledCamera.SetPosBounds(const Value: TBounds3);
begin
  if PosBounds = Value then
    Exit;
  FPosBounds := Value;
  Location.Pos := PosBounds.Clamp(Location.Pos);
end;

procedure TControlledCamera.SetPosLowerBound(const Value: TVector3);
begin
  if PosLowerBound = Value then
    Exit;
  FPosBounds.C1 := Value;
  Location.Pos := PosLowerBound.Max(Location.Pos);
end;

procedure TControlledCamera.SetPosLowerBoundX(const Value: Single);
begin
  if PosLowerBoundX = Value then
    Exit;
  FPosBounds.C1.X := Value;
  Location.PosX := Max(PosLowerBoundX, Location.Pos.X);
end;

procedure TControlledCamera.SetPosLowerBoundY(const Value: Single);
begin
  if PosLowerBoundY = Value then
    Exit;
  FPosBounds.C1.Y := Value;
  Location.PosY := Max(PosLowerBoundY, Location.Pos.Y);
end;

procedure TControlledCamera.SetPosLowerBoundZ(const Value: Single);
begin
  if PosLowerBoundZ = Value then
    Exit;
  FPosBounds.C1.Z := Value;
  Location.PosZ := Max(PosLowerBoundZ, Location.Pos.Z);
end;

procedure TControlledCamera.SetPosUpperBound(const Value: TVector3);
begin
  if PosUpperBound = Value then
    Exit;
  FPosBounds.C2 := Value;
  Location.Pos := PosLowerBound.Min(Location.Pos);
end;

procedure TControlledCamera.SetPosUpperBoundX(const Value: Single);
begin
  if PosUpperBoundX = Value then
    Exit;
  FPosBounds.C2.X := Value;
  Location.PosX := Min(PosLowerBoundX, Location.Pos.X);
end;

procedure TControlledCamera.SetPosUpperBoundY(const Value: Single);
begin
  if PosUpperBoundY = Value then
    Exit;
  FPosBounds.C2.Y := Value;
  Location.PosY := Min(PosLowerBoundY, Location.Pos.Y);
end;

procedure TControlledCamera.SetPosUpperBoundZ(const Value: Single);
begin
  if PosUpperBoundZ = Value then
    Exit;
  FPosBounds.C2.Z := Value;
  Location.PosZ := Min(PosLowerBoundZ, Location.Pos.Z);
end;

procedure TControlledCamera.SetPitchLimit(const Value: TBounds1);
begin
  if PitchLimit = Value then
    Exit;
  FPitchLimit := Value;
  Location.PitchAngle := PitchLimit.Clamp(Location.PitchAngle);
end;

procedure TControlledCamera.SetPitchLowerLimit(const Value: Single);
begin
  if PitchLowerLimit = Value then
    Exit;
  FPitchLimit.C1 := Value;
  Location.PitchAngle := Max(Location.PitchAngle, PitchLowerLimit);
end;

procedure TControlledCamera.SetPitchUpperLimit(const Value: Single);
begin
  if PitchUpperLimit = Value then
    Exit;
  FPitchLimit.C2 := Value;
  Location.PitchAngle := Min(Location.PitchAngle, PitchUpperLimit);
end;

procedure TControlledCamera.SetZoomLimit(const Value: TBounds1);
begin
  if Value.C1 <= 0 then
    raise ECameraInvalidZoomLimit.Create;
  if ZoomLimit = Value then
    Exit;
  FZoomLimit := Value;
  Location.OffsetZ := ZoomLimit.Clamp(Location.OffsetZ);
end;

procedure TControlledCamera.SetZoomLowerLimit(const Value: Single);
begin
  if Value <= 0 then
    raise ECameraInvalidZoomLimit.Create;
  if ZoomLowerLimit = Value then
    Exit;
  FZoomLimit.C1 := Value;
  Location.OffsetZ := Max(Location.OffsetZ, ZoomLowerLimit);
end;

procedure TControlledCamera.SetZoomUpperLimit(const Value: Single);
begin
  if ZoomUpperLimit = Value then
    Exit;
  FZoomLimit.C2 := Value;
  Location.OffsetZ := Min(Location.OffsetZ, ZoomUpperLimit);
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

constructor TControlledCamera.Create(AGame: TGLGame; AFOV, ANearClip, AFarClip: Single);
begin
  inherited Create(AFOV, AGame.Aspect, ANearClip, AFarClip);
  FGame := AGame;
  Game.OnUpdate.Add(Update);
  Game.OnRender.Add(Render);
  Game.OnResize.Add(Resize);

  MouseSensitivity := DefaultMouseSensitivity;
  ZoomSpeed := DefaultZoomSpeed;

  PosBounds := DefaultPosBounds;
  PitchLimit := DefaultPitchLimit;
  ZoomLimit := DefaultZoomLimit;
end;

procedure TControlledCamera.Update;
begin
  if (Input.AsyncKeyDown(VK_CONTROL) or Input.AsyncKeyDown(VK_SHIFT)) and
      Input.ButtonUp(mbLeft) and Input.ButtonUp(mbRight) then
    FMoving := True
  else if (Input.AsyncKeyUp(VK_CONTROL) and Input.AsyncKeyUp(VK_SHIFT)) then
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
  Factor := 1 - Exp(-FSmoothSpeed * Game.DeltaTime);

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

constructor TSmoothControlledCamera.Create(AGLGame: TGLGame; AFOV, ANearClip, AFarClip: Single);
begin
  inherited;
  FSmoothSpeed := DefaultSmoothSpeed;
end;

end.
