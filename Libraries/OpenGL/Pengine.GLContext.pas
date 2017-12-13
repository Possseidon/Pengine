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

  Pengine.GLState,
  Pengine.ResourceManager,
  Pengine.Color,
  Pengine.Texture,
  Pengine.FBO,
  Pengine.GLEnums,
  Pengine.InputHandler,
  Pengine.Collections,
  Pengine.Hasher,
  Pengine.Shader,
  Pengine.TimeManager;

type

  TGLContext = class
  private
    FMustUpdateFPS: Boolean;
    FVSync: Boolean;
    FRunning: Boolean;
    FMaxDeltaTime: Single;
    FInput: TInputHandler;
    FTimer: TDeltaTimer;
    FFPSLimit: Single;
    FClearMask: TGLAttribMask;
    FState: TGLState;
    FDC: HDC;
    FRC: HGLRC;
    FFBO: TFBO;
    FMultiSampling: Boolean;
    FSamples: Cardinal;
    FMaxSamples: Cardinal;

    function GetDeltaTime: Single;
    function GetFPS: Single;
    function GetFPSInt: Cardinal;
    function GetSeconds: Single;

    procedure SetMultiSampling(Value: Boolean);
    procedure SetSamples(Value: Cardinal);
    procedure SetVSync(Value: Boolean);
    procedure SetFPSLimit(Value: Single);

    procedure InitGL;
    procedure FinalizeGL;

    procedure WaitForFPSLimit;

    procedure IdleHandler(Sender: TObject; var Done: Boolean);

    function ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;

    procedure Start;

    class procedure DebugCallback(ASource, AType, AID, ASeverity: Cardinal; ALength: Integer; const AMessage: PAnsiChar;
      AUserdata: Pointer); static; stdcall;

  public
    constructor Create(ADC: HDC);

    procedure ForceFPSUpdate;

    procedure Pause;
    procedure Resume;

    procedure Render;

    property VSync: Boolean read FVSync write SetVSync;
    property ClearMask: TGLAttribMask read FClearMask write FClearMask;
    property MaxDeltaTime: Single read FMaxDeltaTime write FMaxDeltaTime;

    property Input: TInputHandler read FInput;

    property DeltaTime: Single read GetDeltaTime;
    property FPS: Single read GetFPS;
    property FPSLimit: Single read FFPSLimit write SetFPSLimit;
    property FPSInt: Cardinal read GetFPSInt;
    property Seconds: Single read GetSeconds;
    property MustUpdateFPS: Boolean read FMustUpdateFPS;

    property State: TGLState read FState;

    property MultiSampling: Boolean read FMultiSampling write SetMultiSampling;
    property MaxSamples: Cardinal read FMaxSamples;
    property Samples: Cardinal read FSamples write SetSamples;

  end;

  TGLForm = class(TForm)
  private
    FContext: TGLContext;
    FFullscreen: Boolean;
    FOldWindowState: TWindowState;

    procedure ActivateHandler(Sender: TObject);
    procedure DeactivateHandler(Sender: TObject);
    function GetAspect: Single;
    function GetCursorVisible: Boolean;

    procedure SetFullscreen(Value: Boolean);
    procedure SetCursorVisible(Value: Boolean);

  protected
    procedure Resize; override;
    procedure Paint; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Fullscreen: Boolean read FFullscreen write SetFullscreen;
    property CursorVisible: Boolean read GetCursorVisible write SetCursorVisible;

    procedure WndProc(var AMessage: TMessage); override;

    procedure Init; virtual;
    procedure Finalize; virtual;

    procedure RenderGL; virtual;
    procedure UpdateGL; virtual;
    procedure ResizeGL; virtual;

    property Aspect: Single read GetAspect;

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
    //ShowWindow(Handle, SW_SHOW);
  end;
end;

procedure TGLForm.DeactivateHandler(Sender: TObject);
begin
  FInput.ReleaseAll;
  if Fullscreen then
  begin
    ShowWindow(FindWindow(TaskbarWindowClass, nil), SW_SHOWNOACTIVATE);
    WindowState := wsMinimized;
    //ShowWindow(Handle, SW_HIDE);
  end;
end;

function TGLForm.GetAspect: Single;
begin
  Result := ClientWidth / ClientHeight;
end;

function TGLForm.GetDeltaTime: Single;
begin
  Result := FTimer.DeltaTime;
end;

function TGLForm.GetFPS: Single;
begin
  Result := FTimer.FPS;
end;

function TGLForm.GetFPSInt: Cardinal;
begin
  Result := Floor(FTimer.FPS + 0.5);
end;

function TGLForm.GetSeconds: Single;
begin
  Result := FTimer.Seconds;
end;

function TGLForm.GetCursorVisible: Boolean;
begin
  Result := Cursor <> -1;
end;

procedure TGLForm.InitGL;
begin
  FDC := GetDC(Handle);
  FRC := CreateRenderingContextVersion(FDC, [opDoubleBuffered], 4, 3, True, 32, 32, 0, 0, 0, 0);
  ActivateRenderingContext(FDC, FRC);

  glDebugMessageCallback(DebugCallback, Self);

  FState := TGLState.Create;
end;

procedure TGLForm.FinalizeGL;
begin
  FState.Free;

  DeactivateRenderingContext;
  DestroyRenderingContext(FRC);
  ReleaseDC(Handle, FDC);
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

procedure TGLForm.SetMultiSampling(Value: Boolean);
begin
  if FMultiSampling = Value then
    Exit;
  FMultiSampling := Value;

  if FMultiSampling then
  begin
    FFBO := TFBO.Create(ClientWidth, ClientHeight);
    FFBO.EnableRenderBufferMS(fbaColor, pfRGBA, Samples);
    FFBO.EnableRenderBufferMS(fbaDepth, pfDepthComponent, Samples);
    if not FFBO.Finish then
      raise Exception.Create('Multisampling Framebuffer could not be created!');
  end
  else
    FFBO.Free;
end;

procedure TGLForm.SetSamples(Value: Cardinal);
begin
  if (Value < 1) or (Value > MaxSamples) then
    raise Exception.Create('Unspported Number of Samples!');

  if (FSamples = Value) and MultiSampling then
    Exit;
  FSamples := Value;

  if not MultiSampling then
    MultiSampling := True
  else
    FFBO.SetSamples(FSamples);
end;

procedure TGLForm.SetVSync(Value: Boolean);
begin
  FVSync := Value;
  wglSwapIntervalEXT(Integer(FVSync));
end;

procedure TGLForm.SetFPSLimit(Value: Single);
begin
  if Value = 0 then
    raise Exception.Create('FPS Limit must be greater than zero!');
  if FFPSLimit = Value then
    Exit;
  FFPSLimit := Value;
end;

procedure TGLForm.WaitForFPSLimit;
begin
  if FPS > FPSLimit then
    Sleep(Ceil(1000 / FPSLimit));
end;

procedure TGLForm.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  FMustUpdateFPS := FTimer.Update;

  if DeltaTime <= MaxDeltaTime then
  begin
    try
      UpdateGL;
    except
      on E: Exception do
      begin
        Pause;
        if ErrorBox('Game Update Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
        begin
          Close;
          Exit;
        end
        else
          Resume;
      end;
    end;
  end;

  try
    Render;
    GLErrorMessage;
  except
    on E: Exception do
    begin
      Pause;
      if ErrorBox('Render Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
      begin
        Close;
        Exit;
      end
      else
        Resume;
    end;
  end;

  Input.NotifyChanges;

  WaitForFPSLimit;

  Done := False;
end;

function TGLForm.ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;
begin
  Result := MessageDlg(ATitle + sLineBreak + sLineBreak + AMessage, mtError, AButtons, 0, ADefault);
end;

procedure TGLForm.Start;
begin
  Visible := True;
  FRunning := True;
  Application.OnDeactivate := DeactivateHandler;
  Application.OnActivate := ActivateHandler;
  Application.OnIdle := IdleHandler;
end;

class procedure TGLForm.DebugCallback(ASource, AType, AID, ASeverity: Cardinal; ALength: Integer;
  const AMessage: PAnsiChar; AUserdata: Pointer);
var
  S: string;
begin
  S := Format(
    'OpenGL [%s] %s (%s)' + sLineBreak + '%s',
    [GLDebugMessageSeverityName(TGLDebugMessageSeverity(ASeverity)),
    GLDebugMessageTypeName(TGLDebugMessageType(AType)),
    GLDebugMessageSourceName(TGLDebugMessageSource(ASource)),
    AMessage]);
  OutputDebugString(@S[1]);
end;

procedure TGLForm.Resize;
begin
  inherited;
  if not FRunning then
    Exit;

  ResizeGL;
  if MultiSampling then
    FFBO.Resize(ClientWidth, ClientHeight);

  Render;
end;

procedure TGLForm.Paint;
begin
  Render;
end;

constructor TGLForm.Create(TheOwner: TComponent);
begin
  inherited;

  InitGL;

  glGetIntegerv(GL_MAX_SAMPLES, @FMaxSamples);
  FSamples := 1;

  Constraints.MinWidth := 200;
  Constraints.MinHeight := 100;

  SendMessage(Handle, WM_KEYDOWN, VK_MENU, 0);
  SendMessage(Handle, WM_KEYDOWN, VK_RIGHT, 0);

  SendMessage(Handle, WM_KEYUP, VK_MENU, 0);
  SendMessage(Handle, WM_KEYUP, VK_RIGHT, 0);

  FInput := TInputHandler.Create(Self);
  FTimer.Init;

  VSync := True;
  FPSLimit := Infinity;
  FFullscreen := False;

  MaxDeltaTime := 0.5;

  ClearMask := amColorDepth;

  Color := clBlack;

  try
    TResourceManager.Init;
    GLErrorMessage;
    Init;
    GLErrorMessage;
    Start;
  except
    on E: EAbort do
    begin
      WindowState := wsMinimized;
      PostQuitMessage(0);
    end;
    on E: Exception do
    begin
      ErrorBox('Initialization Error!', E.Message, [mbOK], mbOK);
      WindowState := wsMinimized; // hide the window sneaky sneaky
      PostQuitMessage(0);
    end
  end;

  Resize;
end;

destructor TGLForm.Destroy;
begin
  try
    Finalize;
    TResourceManager.Finalize;
  except
    on E: Exception do
      ErrorBox('Finalization Error!', E.Message, [mbOK], mbOK);
  end;
  if Fullscreen then
    Fullscreen := False;
  Pause;
  FInput.Free;
  if MultiSampling then
    FFBO.Free;
  FinalizeGL;
  inherited;
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
            FInput.PressChar(AnsiChar(W));
        end;
      end;
  end;

  inherited WndProc(AMessage);
end;

procedure TGLForm.ForceFPSUpdate;
begin
  FTimer.ForceFPSUpdate;
end;

procedure TGLForm.Pause;
begin
  FInput.ReleaseAll;
  Application.OnIdle := nil;
end;

procedure TGLForm.Resume;
begin
  FTimer.Update;
  Application.OnIdle := IdleHandler;
end;

procedure TGLForm.Render;
begin
  if FMultiSampling then
  begin
    FFBO.Bind;
    glClear(Ord(ClearMask));
    RenderGL;
    FFBO.CopyToScreen(amColor);
  end
  else
  begin
    TFBO.BindScreen(ClientWidth, ClientHeight);
    glClear(Ord(ClearMask));
    RenderGL;
  end;
  SwapBuffers(FDC);
end;

procedure TGLForm.Init;
begin
end;

procedure TGLForm.Finalize;
begin
end;

procedure TGLForm.RenderGL;
begin
end;

procedure TGLForm.UpdateGL;
begin
end;

procedure TGLForm.ResizeGL;
begin
end;

end.
