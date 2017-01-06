unit TimeManager;

interface

uses
  Windows;

type

  { TDeltaTimer }

  TDeltaTimer = record
  private
    FLastTime: Int64;

    FFPS, FDeltaTime, FUpdateTime, FSeconds: Single;
    FUpdateSpeed, FUpdateInterval: Single; // update speed: 0 = no update; inf = raw

    procedure SetUpdateSpeed(const Value: Single);
    procedure SetUpdateInterval(const Value: Single);

  public
    procedure Init(AUpdateSpeed: Single = 4; AUpdateInterval: Single = 0.5);

    function Update: Boolean;

    property UpdateSpeed: Single read FUpdateSpeed write SetUpdateSpeed;
    property UpdateInterval: Single read FUpdateInterval write SetUpdateInterval;
    property DeltaTime: Single read FDeltaTime;
    property FPS: Single read FFPS;
    property Seconds: Single read FSeconds;

    procedure ForceFPSUpdate;
  end;

  TTimeFormat = (
    tfNanoseconds,
    tfMicroseconds,
    tfMilliseconds,
    tfSeconds,
    tfMinutes,
    tfHours,
    tfDays,
    tfWeeks,
    tfYears
  );

procedure StartTimer;
function StopTimer: Single;
procedure StopTimerAndOutput(ATimeFormat: TTimeFormat = tfSeconds);
function StopTimerGetString(ATimeFormat: TTimeFormat = tfSeconds): String;
function ConvertSecondsTo(ASeconds: Single; ATimeFormat: TTimeFormat): Single;

const
  TimeFormatString: array [TTimeFormat] of String = (
    'nanoseconds',
    'microseconds',
    'milliseconds',
    'seconds',
    'minutes',
    'hours',
    'days',
    'weeks',
    'years'
  );

implementation

uses
  SysUtils;

var
  Frequency: Int64;
  MeassureStart: Int64;

{ LOCAL }

procedure InitFrequency;
begin
  QueryPerformanceFrequency(Frequency);
end;

{ GLOBAL }

procedure StartTimer;
begin
  QueryPerformanceCounter(MeassureStart);
end;

function StopTimer: Single;
var
  MeassureStop: Int64;
begin
  QueryPerformanceCounter(MeassureStop);
  Result := (MeassureStop - MeassureStart) / Frequency;
end;

procedure StopTimerAndOutput(ATimeFormat: TTimeFormat);
begin
  WriteLn(Format('Meassured Time: %.3f ' +
                  TimeFormatString[ATimeFormat],
                  [ConvertSecondsTo(StopTimer, ATimeFormat)]));
end;

function StopTimerGetString(ATimeFormat: TTimeFormat): String;
begin
  Result := Format('%.3f ' + TimeFormatString[ATimeFormat], [ConvertSecondsTo(StopTimer, ATimeFormat)]);
end;

function ConvertSecondsTo(ASeconds: Single; ATimeFormat: TTimeFormat): Single;
begin
  Result := ASeconds;
  // smaller
  if ATimeFormat <= tfMilliseconds then
    Result := Result * 1000;
  if ATimeFormat <= tfMicroseconds then
    Result := Result * 1000;
  if ATimeFormat <= tfNanoseconds then
    Result := Result * 1000;

  // bigger
  if ATimeFormat >= tfMinutes then
    Result := Result / 60;
  if ATimeFormat >= tfHours then
    Result := Result / 60;

  // handle separated
  if ATimeFormat = tfWeeks then
    Result := Result / 7;
  if ATimeFormat = tfYears then
    Result := Result / 365;

end;

{ TGLTimer }

procedure TDeltaTimer.Init(AUpdateSpeed: Single = 4; AUpdateInterval: Single = 0.5);
begin
  FUpdateSpeed := AUpdateSpeed;
  FUpdateInterval := AUpdateInterval;
  Update;
  FSeconds := 0;
end;

procedure TDeltaTimer.SetUpdateSpeed(const Value: Single);
begin
  FUpdateSpeed := Value;
end;

procedure TDeltaTimer.SetUpdateInterval(const Value: Single);
begin
  FUpdateInterval := Value;
end;

function TDeltaTimer.Update: Boolean;
var
  F: Single;
  NewTime: Int64;
begin
  QueryPerformanceCounter(NewTime);
  FDeltaTime := (NewTime - FLastTime) / Frequency;
  FLastTime := NewTime;

  FSeconds := FSeconds + FDeltaTime;
  F := Exp(-UpdateSpeed * FDeltaTime);
  FFPS := F * FFPS + (1 - F) / FDeltaTime;

  FUpdateTime := FUpdateTime + DeltaTime;
  if FUpdateTime >= UpdateInterval then
  begin
    FUpdateTime := 0;
    Exit(True);
  end;
  Result := False;
end;

procedure TDeltaTimer.ForceFPSUpdate;
begin
  FUpdateTime := FUpdateInterval;
end;

begin
  InitFrequency;
  StartTimer;
end.
