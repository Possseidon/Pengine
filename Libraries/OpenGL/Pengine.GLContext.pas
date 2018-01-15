unit Pengine.GLContext;

interface

uses
  dglOpenGL,

  Winapi.Windows,

  System.Classes,
  System.SysUtils,
  System.Math,
  System.UITypes,

  Vcl.Controls,
  Vcl.Dialogs,

  Pengine.Renderbuffer,
  Pengine.GLState,
  Pengine.ResourceManager,
  Pengine.FBO,
  Pengine.GLEnums,
  Pengine.TimeManager,
  Pengine.IntMaths;

type

  TGLContext = class
  public type

    TRenderCallback = procedure of object;

  private
    FVSync: Boolean;
    FMaxDeltaTime: Single;
    FTimer: TDeltaTimer;
    FFPSLimit: Single;
    FClearMask: TGLAttribMaskFlags;
    FGLState: TGLState;
    FDC: HDC;
    FRC: HGLRC;

    FFBO: TFBO;
    FFBOBinding: TGLObjectBinding<TFBO>;

    FColorRenderbuffer: TRenderbufferMS;
    FDepthRenderbuffer: TRenderbufferMS;

    FSamples: Integer;
    FMaxSamples: Integer;
    FSize: TIntVector2;
    FRenderCallback: TRenderCallback;

    FGLDebugLogLevels: TGLDebugSeverities;
    FGLDebugRaiseLevels: TGLDebugSeverities;

    function GetDeltaTime: TSeconds;
    function GetFPS: Single;
    function GetFPSInt: Cardinal;
    function GetTime: TSeconds;

    procedure SetMultiSampled(Value: Boolean);
    procedure SetSamples(Value: Integer);
    procedure SetVSync(Value: Boolean);
    procedure SetFPSLimit(Value: Single);

    procedure InitGL;
    procedure FinalizeGL;

    class procedure DebugCallback(ASource, AType, AID, ASeverity: Cardinal; ALength: Integer; const AMessage: PAnsiChar; AUserdata: Pointer); static; stdcall;

    procedure SetSize(const Value: TIntVector2);

    function GetDebugOutput: Boolean;
    function GetDebugOutputSynced: Boolean;
    procedure SetDebugOutput(const Value: Boolean);
    procedure SetDebugOutputSynced(const Value: Boolean);
    procedure SetGLDebugLogLevels(const Value: TGLDebugSeverities);
    procedure SetGLDebugRaiseLevels(const Value: TGLDebugSeverities);
    function GetMultiSampled: Boolean;

  public
    constructor Create(ADC: HDC; ASize: TIntVector2; ARenderCallback: TRenderCallback);
    destructor Destroy; override;

    class function ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;

    procedure WaitForFPSLimit;

    procedure Update;
    procedure ForceFPSUpdate;

    procedure Render;

    procedure Clear(AMask: TGLAttribMaskFlags);

    property Size: TIntVector2 read FSize write SetSize;
    property VSync: Boolean read FVSync write SetVSync;
    property ClearMask: TGLAttribMaskFlags read FClearMask write FClearMask;
    property MaxDeltaTime: Single read FMaxDeltaTime write FMaxDeltaTime;

    property Timer: TDeltaTimer read FTimer;
    property DeltaTime: TSeconds read GetDeltaTime;
    property FPS: Single read GetFPS;
    property FPSLimit: Single read FFPSLimit write SetFPSLimit;
    property FPSInt: Cardinal read GetFPSInt;
    property Time: TSeconds read GetTime;

    property GLDebug: Boolean read GetDebugOutput write SetDebugOutput;
    property GLDebugSynced: Boolean read GetDebugOutputSynced write SetDebugOutputSynced;
    property GLDebugRaiseLevels: TGLDebugSeverities read FGLDebugRaiseLevels write SetGLDebugRaiseLevels;
    property GLDebugLogLevels: TGLDebugSeverities read FGLDebugLogLevels write SetGLDebugLogLevels;

    property GLState: TGLState read FGLState;

    property MultiSampled: Boolean read GetMultiSampled write SetMultiSampled;
    property MaxSamples: Integer read FMaxSamples;
    property Samples: Integer read FSamples write SetSamples;

  end;

implementation

{ TGLContext }

function TGLContext.GetDebugOutput: Boolean;
begin
  Result := GLState[stDebugOutput]
end;

function TGLContext.GetDebugOutputSynced: Boolean;
begin
  Result := GLState[stDebugOutputSynced];
end;

function TGLContext.GetDeltaTime: TSeconds;
begin
  Result := FTimer.DeltaTime;
end;

function TGLContext.GetFPS: Single;
begin
  Result := FTimer.FPS;
end;

function TGLContext.GetFPSInt: Cardinal;
begin
  Result := Floor(FTimer.FPS + 0.5);
end;

function TGLContext.GetMultiSampled: Boolean;
begin
  Result := FFBO <> nil;
end;

function TGLContext.GetTime: TSeconds;
begin
  Result := FTimer.Time;
end;

procedure TGLContext.InitGL;
begin
  FRC := CreateRenderingContextVersion(FDC, [opDoubleBuffered], 4, 2, True, 32, 32, 0, 0, 0, 0);
  ActivateRenderingContext(FDC, FRC);

  glDebugMessageCallback(DebugCallback, Self);

  FGLState := TGLState.Create;
end;

procedure TGLContext.FinalizeGL;
begin
  FGLState.Free;

  if FRC <> 0 then
  begin
    DeactivateRenderingContext;
    DestroyRenderingContext(FRC);
  end;
end;

procedure TGLContext.SetMultiSampled(Value: Boolean);
begin
  if MultiSampled = Value then
    Exit;

  if MultiSampled then
  begin
    FreeAndNil(FFBO);
    FreeAndNil(FColorRenderbuffer);
    FreeAndNil(FDepthRenderbuffer);
  end
  else
  begin
    FColorRenderbuffer := TRenderbufferMS.Create(GLState, Size, Samples, pfRGBA);
    FDepthRenderbuffer := TRenderbufferMS.Create(GLState, Size, Samples, pfDepthComponent);
    FFBO := TFBO.Create(GLState, IBounds2(FSize));
    FFBO.Add(TRenderbufferAttachment.Create(TColorAttachment.Create, FColorRenderbuffer));
    FFBO.Add(TRenderbufferAttachment.Create(TDepthAttachment.Create, FDepthRenderbuffer));
    FFBO.Complete;
  end;
end;

procedure TGLContext.SetSamples(Value: Integer);
begin
  if not (Value in IBounds1I(1, MaxSamples)) then
    raise Exception.Create('Unspported number of samples!');

  if (FSamples = Value) and MultiSampled then
    Exit;

  FSamples := Value;
  MultiSampled := False;
  MultiSampled := True;
end;

procedure TGLContext.SetSize(const Value: TIntVector2);
begin
  if Size = Value then
    Exit;
  FSize := Value;
  if MultiSampled then
  begin
    MultiSampled := False;
    MultiSampled := True;
  end;
  FGLState.SetScreenSize(Size);
end;

procedure TGLContext.SetVSync(Value: Boolean);
begin
  FVSync := Value;
  wglSwapIntervalEXT(Integer(FVSync));
end;

procedure TGLContext.SetDebugOutput(const Value: Boolean);
begin
  GLState[stDebugOutput] := Value;
end;

procedure TGLContext.SetDebugOutputSynced(const Value: Boolean);
begin
  GLState[stDebugOutputSynced] := Value;
end;

procedure TGLContext.SetFPSLimit(Value: Single);
begin
  if Value = 0 then
    raise Exception.Create('FPS Limit must be greater than zero!');
  if FFPSLimit = Value then
    Exit;
  FFPSLimit := Value;
end;

procedure TGLContext.SetGLDebugLogLevels(const Value: TGLDebugSeverities);
begin
  FGLDebugLogLevels := Value;
end;

procedure TGLContext.SetGLDebugRaiseLevels(const Value: TGLDebugSeverities);
begin
  FGLDebugRaiseLevels := Value;
end;

procedure TGLContext.WaitForFPSLimit;
begin
  if FPS > FPSLimit then
    Sleep(Max(1, Floor(1000 / FPSLimit)));
end;

procedure TGLContext.Update;
begin
  FTimer.Update;
end;

class function TGLContext.ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;
begin
  Result := MessageDlg(ATitle + sLineBreak + sLineBreak + AMessage, mtError, AButtons, 0, ADefault);
end;

procedure TGLContext.Clear(AMask: TGLAttribMaskFlags);
begin
  if MultiSampled then
    FFBO.Bind
  else
    TFBO.BindScreen(IBounds2(GLState.ScreenSize), FFBOBinding);
  glClear(ToGLBitfield(AMask));
end;

constructor TGLContext.Create(ADC: HDC; ASize: TIntVector2; ARenderCallback: TRenderCallback);
begin
  FDC := ADC;
  FRenderCallback := ARenderCallback;

  FTimer := TDeltaTimer.Create;

  InitGL;

  FFBOBinding := GLState.GLObjectBindings.Get<TFBO>;

  Size := ASize;

  glGetIntegerv(GL_MAX_SAMPLES, @FMaxSamples);
  FSamples := 1;

  VSync := True;
  FPSLimit := Infinity;

  MaxDeltaTime := 0.5;

  ClearMask := [amColor, amDepth];

  {$IFDEF DEBUG}
  GLDebug := True;
  GLDebugSynced := True;
  GLDebugLogLevels := [dmsLow, dmsMedium, dmsHigh];
  GLDebugRaiseLevels := [dmsHigh];
  {$ENDIF}
end;

class procedure TGLContext.DebugCallback(ASource, AType, AID, ASeverity: Cardinal; ALength: Integer; const AMessage: PAnsiChar; AUserdata: Pointer);

  function Formatted: string;
  begin
    Result := Format(
      'OpenGL [%s] %s (%s)' + sLineBreak + '%s',
      [GLDebugSeverityName[GLDebugSeverityToEnum(ASeverity)],
      GLDebugTypeName(TGLDebugType(AType)),
      GLDebugSourceName(TGLDebugSource(ASource)),
      AMessage]);
  end;

var
  Severity: TGLDebugSeverity;
  Self: TGLContext;
begin
  Self := TGLContext(AUserData);
  Severity := GLDebugSeverityToEnum(ASeverity);
  if Severity in Self.GLDebugLogLevels then
    OutputDebugString(@Formatted[1]);
  if Severity in Self.GLDebugRaiseLevels then
    raise Exception.Create(Formatted);
end;

destructor TGLContext.Destroy;
begin
  FFBO.Free;
  FColorRenderbuffer.Free;
  FDepthRenderbuffer.Free;
  FinalizeGL;
  FTimer.Free;
  inherited;
end;

procedure TGLContext.ForceFPSUpdate;
begin
  FTimer.ForceFPSUpdate;
end;

procedure TGLContext.Render;
begin
  Clear(ClearMask);

  FRenderCallback;

  if MultiSampled then
    FFBO.CopyToScreen([amColor]);

  SwapBuffers(FDC);
  glFlush;
end;

end.
