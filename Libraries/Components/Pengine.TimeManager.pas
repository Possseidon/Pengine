unit Pengine.TimeManager;

interface

uses
  Winapi.Windows,

  Pengine.EventHandling;

type

  TSeconds = type Single;

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

  TDeltaTimer = class
  public type

    TEventInfo = TSenderEventInfo<TDeltaTimer>;
    TEvent = TEvent<TEventInfo>;

  private
    FLastTime: Int64;

    FFPS: Single;
    FDeltaTime: TSeconds;
    FUpdateTime: TSeconds;
    FSeconds: TSeconds;
    FUpdateSpeed: Single;
    FUpdateInterval: TSeconds;

    FOnFPSUpdate: TEvent;

    procedure SetUpdateSpeed(const Value: Single);
    procedure SetUpdateInterval(const Value: TSeconds);
    function GetOnFPSUpdate: TEvent.TAccess;

  public
    constructor Create(AUpdateSpeed: Single = 4; AUpdateInterval: TSeconds = 0.5);

    procedure Update;

    /// <summary>0 = no update; inf = raw</summary>
    property UpdateSpeed: Single read FUpdateSpeed write SetUpdateSpeed;
    property UpdateInterval: TSeconds read FUpdateInterval write SetUpdateInterval;
    property DeltaTime: TSeconds read FDeltaTime;
    property FPS: Single read FFPS;
    property Time: TSeconds read FSeconds;

    property OnFPSUpdate: TEvent.TAccess read GetOnFPSUpdate;

    procedure ForceFPSUpdate;

  end;

  { TStopWatch }

  TStopWatch = record
  private
    FStart: Single;
  public
    procedure Start; inline;
    function Time(ATimeFormat: TTimeFormat = tfSeconds): Single;
  end;

procedure StartTimer;
function StopTimer: Single;
procedure StopTimerAndOutput(ATimeFormat: TTimeFormat = tfSeconds); inline;
function StopTimerGetString(ATimeFormat: TTimeFormat = tfSeconds): string; inline;
function ConvertSecondsTo(ASeconds: Single; ATimeFormat: TTimeFormat): Single; inline;

const
  TimeFormatString: array [TTimeFormat] of string = (
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

function StopTimerGetString(ATimeFormat: TTimeFormat): string;
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

constructor TDeltaTimer.Create(AUpdateSpeed: Single = 4; AUpdateInterval: TSeconds = 0.5);
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

procedure TDeltaTimer.SetUpdateInterval(const Value: TSeconds);
begin
  FUpdateInterval := Value;
end;

procedure TDeltaTimer.Update;
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
    FOnFPSUpdate.Execute(TEventInfo.Create(Self));
  end;
end;

procedure TDeltaTimer.ForceFPSUpdate;
begin
  FUpdateTime := FUpdateInterval;
end;

function TDeltaTimer.GetOnFPSUpdate: TEvent.TAccess;
begin
  Result := FOnFPSUpdate.Access;
end;

{ TStopWatch }

procedure TStopWatch.Start;
begin
  FStart := 0;
  FStart := Time;
end;

function TStopWatch.Time(ATimeFormat: TTimeFormat): Single;
var
  StartTime: Int64;
begin
  QueryPerformanceCounter(StartTime);
  Result := StartTime / Frequency - FStart;
  if ATimeFormat <> tfSeconds then
    Result := ConvertSecondsTo(Result, ATimeFormat);
end;

initialization
  InitFrequency;
  StartTimer;
end.
