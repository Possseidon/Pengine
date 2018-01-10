unit Pengine.GLContext;

interface

uses
  dglOpenGL,

  Winapi.Windows,
  Winapi.Messages,

  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Math,

  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,

  Pengine.Renderbuffer,
  Pengine.GLState,
  Pengine.ResourceManager,
  Pengine.Color,
  Pengine.Texture,
  Pengine.FBO,
  Pengine.GLEnums,
  Pengine.InputHandler,
  Pengine.Collections,
  Pengine.Hasher,
  Pengine.TimeManager,
  Pengine.IntMaths,
  Pengine.GLGame;

type

  TGLContext = class
  public type

    TRenderCallback = procedure of object;

  private
    FMustUpdateFPS: Boolean;
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

    function GetDeltaTime: Single;
    function GetFPS: Single;
    function GetFPSInt: Cardinal;
    function GetSeconds: Single;

    procedure SetMultiSampled(Value: Boolean);
    procedure SetSamples(Value: Integer);
    procedure SetVSync(Value: Boolean);
    procedure SetFPSLimit(Value: Single);

    procedure InitGL;
    procedure FinalizeGL;

    procedure WaitForFPSLimit;

    class function ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;

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
    
    procedure Update;
    procedure ForceFPSUpdate;

    procedure Render;

    property Size: TIntVector2 read FSize write SetSize;
    property VSync: Boolean read FVSync write SetVSync;
    property ClearMask: TGLAttribMaskFlags read FClearMask write FClearMask;
    property MaxDeltaTime: Single read FMaxDeltaTime write FMaxDeltaTime;

    property Timer: TDeltaTimer read FTimer;
    property DeltaTime: Single read GetDeltaTime;
    property FPS: Single read GetFPS;
    property FPSLimit: Single read FFPSLimit write SetFPSLimit;
    property FPSInt: Cardinal read GetFPSInt;
    property Seconds: Single read GetSeconds;
    property MustUpdateFPS: Boolean read FMustUpdateFPS;

    property GLDebug: Boolean read GetDebugOutput write SetDebugOutput;
    property GLDebugSynced: Boolean read GetDebugOutputSynced write SetDebugOutputSynced;
    property GLDebugRaiseLevels: TGLDebugSeverities read FGLDebugRaiseLevels write SetGLDebugRaiseLevels;
    property GLDebugLogLevels: TGLDebugSeverities read FGLDebugLogLevels write SetGLDebugLogLevels;

    property GLState: TGLState read FGLState;

    property MultiSampled: Boolean read GetMultiSampled write SetMultiSampled;
    property MaxSamples: Integer read FMaxSamples;
    property Samples: Integer read FSamples write SetSamples;

  end;

  TGLForm = class(TForm)
  private
    FGame: TGLGame;
    FInput: TInputHandler;
    FContext: TGLContext;
    FFullscreen: Boolean;
    FOldWindowState: TWindowState;
    FRunning: Boolean;
    FDC: HDC;

    procedure ActivateHandler(Sender: TObject);
    procedure DeactivateHandler(Sender: TObject);
    function GetCursorVisible: Boolean;

    procedure SetFullscreen(Value: Boolean);
    procedure SetCursorVisible(Value: Boolean);

    procedure IdleHandler(Sender: TObject; var Done: Boolean);
    function GetGLState: TGLState;
    function GetDeltaTime: Single;

    procedure RenderCallback;

  protected
    procedure Resize; override;
    procedure Paint; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Fullscreen: Boolean read FFullscreen write SetFullscreen;
    property CursorVisible: Boolean read GetCursorVisible write SetCursorVisible;

    property Game: TGLGame read FGame;
    property Input: TInputHandler read FInput;
    
    procedure Start;
    procedure Pause;
    procedure Resume;

    procedure WndProc(var AMessage: TMessage); override;

    procedure Init; virtual;
    procedure Finalize; virtual;

    property Context: TGLContext read FContext;
    property GLState: TGLState read GetGLState;

    property DeltaTime: Single read GetDeltaTime;

  end;

implementation

const
  TaskbarWindowClass = 'Shell_TrayWnd';

{ TGLForm }

procedure TGLForm.ActivateHandler(Sender: TObject);
begin
  if Fullscreen then
  begin
    ShowWindow(FindWindow(TaskbarWindowClass, nil), SW_HIDE);
    BringToFront;
    // ShowWindow(Handle, SW_SHOW);
  end;
end;

procedure TGLForm.DeactivateHandler(Sender: TObject);
begin
  Input.ReleaseAll;
  if Fullscreen then
  begin
    ShowWindow(FindWindow(TaskbarWindowClass, nil), SW_SHOWNOACTIVATE);
    WindowState := wsMinimized;
    // ShowWindow(Handle, SW_HIDE);
  end;
end;

function TGLForm.GetCursorVisible: Boolean;
begin
  Result := Cursor <> -1;
end;

function TGLForm.GetDeltaTime: Single;
begin
  Result := Context.DeltaTime;
end;

function TGLForm.GetGLState: TGLState;
begin
  Result := Context.GLState;
end;

procedure TGLForm.SetFullscreen(Value: Boolean);
var
  Flags: LONG;
begin
  FFullscreen := Value;

  if FFullscreen then
  begin
    // activate fullscreen
    Flags := GetWindowLong(Handle, GWL_STYLE);
    Flags := Flags
      and not WS_CAPTION
      and not WS_THICKFRAME;
    SetWindowLong(Handle, GWL_STYLE, Flags);
    FOldWindowState := WindowState;
    WindowState := wsNormal;
    WindowState := wsMaximized;
    ShowWindow(FindWindow(TaskbarWindowClass, nil), SW_HIDE);
  end
  else
  begin
    // deactivate fullscreen
    Flags := GetWindowLong(Handle, GWL_STYLE);
    Flags := Flags
      or WS_CAPTION
      or WS_THICKFRAME;
    SetWindowLong(Handle, GWL_STYLE, Flags);
    WindowState := wsNormal;
    WindowState := FOldWindowState;
    ShowWindow(FindWindow(TaskbarWindowClass, nil), SW_SHOWNOACTIVATE);
  end;
end;

procedure TGLForm.SetCursorVisible(Value: Boolean);
begin
  Cursor := TCursor(Value) - 1;
end;

procedure TGLForm.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  FContext.Update;

  if FContext.DeltaTime <= FContext.MaxDeltaTime then
  begin
    try
      Game.Update;
    except
      on E: Exception do
      begin
        Pause;
        if TGLContext.ErrorBox('Game Update Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
          Halt
        else
          Resume;
      end;
    end;
  end;

  try
    FContext.Render;
    GLErrorMessage;
  except
    on E: Exception do
    begin
      Pause;
      if TGLContext.ErrorBox('Render Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
        Halt
      else
        Resume;
    end;
  end;

  FInput.NotifyChanges;

  FContext.WaitForFPSLimit;   
  Done := False;
end;

procedure TGLForm.RenderCallback;
begin
  Game.Render;
end;

procedure TGLForm.Resize;
begin
  inherited;
  if not FRunning then
    Exit;

  FContext.Size := IVec2(ClientWidth, ClientHeight);
  Game.Resize(FContext.Size);

  FContext.Render;
end;

procedure TGLForm.Paint;
begin
  if FContext <> nil then
    FContext.Render;
end;

constructor TGLForm.Create(TheOwner: TComponent);
begin
  inherited;

  try

    FDC := GetDC(Handle);
    FContext := TGLContext.Create(FDC, IVec2(ClientWidth, ClientHeight), RenderCallback);
    FInput := TInputHandler.Create(Self);
    FGame := TGLGame.Create(Context.GLState, Input, Context.Timer, IVec2(ClientWidth, ClientHeight));

    Constraints.MinWidth := 200;
    Constraints.MinHeight := 100;

    SendMessage(Handle, WM_KEYDOWN, VK_MENU, 0);
    SendMessage(Handle, WM_KEYDOWN, VK_RIGHT, 0);

    SendMessage(Handle, WM_KEYUP, VK_MENU, 0);
    SendMessage(Handle, WM_KEYUP, VK_RIGHT, 0);

    FFullscreen := False;

    Color := clBlack;

    TResourceManager.Init;
    Init;
    Start;
  except
    on E: EAbort do
    begin
      WindowState := wsMinimized;
      PostQuitMessage(0);
    end;
    on E: Exception do
    begin
      TGLContext.ErrorBox('Initialization Error!', E.Message, [mbOK], mbOK);
      WindowState := wsMinimized; // hide the window sneaky sneaky
      PostQuitMessage(0);
    end
  end;

  Resize;
end;

destructor TGLForm.Destroy;
begin
  if FContext = nil then
    Exit;

  try
    Finalize;
    TResourceManager.Finalize;
  except
    on E: Exception do
      TGLContext.ErrorBox('Finalization Error!', E.Message, [mbOK], mbOK);
  end;

  if Fullscreen then
    Fullscreen := False;

  FGame.Free;
  FInput.Free;
  FContext.Free;

  ReleaseDC(Handle, FDC);
  inherited;
end;

procedure TGLForm.Start;
begin
  Visible := True;
  FRunning := True;
  Application.OnDeactivate := DeactivateHandler;
  Application.OnActivate := ActivateHandler;
  Application.OnIdle := IdleHandler;
end;

procedure TGLForm.Pause;
begin
  Input.ReleaseAll;
  Application.OnIdle := nil;
end;

procedure TGLForm.Resume;
begin
  FContext.Update;
  Application.OnIdle := IdleHandler;
end;

procedure TGLForm.WndProc(var AMessage: TMessage);
var
  W: WPARAM;
begin
  case AMessage.msg of
    WM_SYSCOMMAND:
      begin
        if (AMessage.wParamlo = SC_KEYMENU) and (AMessage.wParamhi = 0) then
          AMessage.msg := WM_NULL;
      end;
    WM_CHAR:
      begin
        W := MakeWPARAM(AMessage.wParamlo, AMessage.wParamhi);
        if W = 0 then
          AMessage.Result := 1
        else
        begin
        // 0 - 31 = control charachters
        // 127 = Ctrl-Backspace
          if (W >= 32) and (W < 256) and (W <> 127) then
            FInput.PressChar(Char(W));
        end;
      end;
  end;

  inherited WndProc(AMessage);
end;

procedure TGLForm.Init;
begin
end;

procedure TGLForm.Finalize;
begin
end;

function TGLContext.GetDebugOutput: Boolean;
begin
  Result := GLState[stDebugOutput]
end;

function TGLContext.GetDebugOutputSynced: Boolean;
begin
  Result := GLState[stDebugOutputSynced];
end;

function TGLContext.GetDeltaTime: Single;
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

function TGLContext.GetSeconds: Single;
begin
  Result := FTimer.Seconds;
end;

procedure TGLContext.InitGL;
begin
  FRC := CreateRenderingContextVersion(FDC, [opDoubleBuffered], 4, 5, True, 32, 32, 0, 0, 0, 0);
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
  FMustUpdateFPS := FTimer.Update;
end;

class function TGLContext.ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;
begin
  Result := MessageDlg(ATitle + sLineBreak + sLineBreak + AMessage, mtError, AButtons, 0, ADefault);
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
  if MultiSampled then
  begin
    FFBO.Bind;
    glClear(ToGLBitfield(ClearMask));
    FRenderCallback;
    FFBO.CopyToScreen([amColor]);
  end
  else
  begin
    TFBO.BindScreen(GLState.ScreenSize, FFBOBinding);
    glClear(ToGLBitfield(ClearMask));
    FRenderCallback;
  end;
  SwapBuffers(FDC);
end;

end.
