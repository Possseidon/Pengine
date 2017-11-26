unit Pengine.GLContext;

interface

uses
  dglOpenGL, Forms, Controls, Windows, Classes, GLEnums, Color, InputHandler, TimeManager, Lists, Graphics, FBOManager,
  SysUtils, Dialogs, Shaders, ResourceManager
  {$IFNDEF FPC}
    , Messages, UITypes
  {$ENDIF}
    ;

type

  { TOpenGLState }

  // steps to add state:
  // 1) TState enum
  // 2) property
  // 3) Update Function
  // 4) Pop
  // 5) Copy
  // 6) InitDefaults

  TOpenGLState = class
  public type
    TState = (
      stClearColor,
      stDepthFunc,
      stCullFace,
      stDepthTest,
      stBlend,
      stBlendFunc,
      stDepthMask,
      stSeamlessCubemap,
      stDepthClamp,
      stDebugOutput,
      stDebugOutputSynced
      );
    TStates = set of TState;

  private
    FBlend: Boolean;
    FBlendFunc: TGLBlendingFunc;
    FClearColor: TColorRGBA;
    FCullFace: TGLCullFace;
    FDepthFunc: TGLCompareFunction;
    FChanges: TStates;
    FDepthMask: Boolean;
    FDepthTest: Boolean;
    FSeamlessCubemap: Boolean;
    FDepthClamp: Boolean;
    FDebugOutput: Boolean;
    FDebugOutputSynced: Boolean;

    procedure SetBlend(const Value: Boolean);
    procedure SetBlendFactorDest(const Value: TGLBlendingFactorDest);
    procedure SetBlendFactorSrc(const Value: TGLBlendingFactorSrc);
    procedure SetClearColor(const Value: TColorRGBA);
    procedure SetCullFace(const Value: TGLCullFace);
    procedure SetDepthFunc(const Value: TGLCompareFunction);
    procedure SetDepthMask(const Value: Boolean);
    procedure SetDepthTest(const Value: Boolean);
    procedure SetSeamlessCubemap(const Value: Boolean);
    procedure SetDepthClamp(const Value: Boolean);
    procedure SetDebugOutput(const Value: Boolean);
    procedure SetDebugOutputSynced(const Value: Boolean);

  public
    function Copy: TOpenGLState;
    property Changes: TStates read FChanges;

    property ClearColor: TColorRGBA read FClearColor write SetClearColor;
    property DepthFunc: TGLCompareFunction read FDepthFunc write SetDepthFunc;
    property CullFace: TGLCullFace read FCullFace write SetCullFace;
    property DepthTest: Boolean read FDepthTest write SetDepthTest;
    property Blend: Boolean read FBlend write SetBlend;
    property BlendFactorSrc: TGLBlendingFactorSrc read FBlendFunc.Src write SetBlendFactorSrc;
    property BlendFactorDest: TGLBlendingFactorDest read FBlendFunc.Dest write SetBlendFactorDest;
    property DepthMask: Boolean read FDepthMask write SetDepthMask;
    property SeamlessCubemap: Boolean read FSeamlessCubemap write SetSeamlessCubemap;
    property DepthClamp: Boolean read FDepthClamp write SetDepthClamp;
    property DebugOutput: Boolean read FDebugOutput write SetDebugOutput;
    property DebugOutputSynced: Boolean read FDebugOutputSynced write SetDebugOutputSynced;

    procedure UpdateClearColor;
    procedure UpdateDepthFunc;
    procedure UpdateCullFace;
    procedure UpdateDepthTest;
    procedure UpdateBlend;
    procedure UpdateBlendFunc;
    procedure UpdateDepthMask;
    procedure UpdateSeamlessCubemap;
    procedure UpdateDepthClamp;
    procedure UpdateDebugOutput;
    procedure UpdateDebugOutputSynced;
  end;

  { TGLForm }

  TGLForm = class(TForm)
  private
    FFullscreen: Boolean;
    FMustUpdateFPS: Boolean;
    FVSync: Boolean;

    FRunning: Boolean;

    FMaxDeltaTime: Single;

    FInput: TInputHandler;
    FTimer: TDeltaTimer;
    FFPSLimit: Single;

    FClearMask: TGLAttribMask;

    StateStack: TObjectStack<TOpenGLState>;

    FDC: HDC;
    FRC: HGLRC;

    FOldWindowState: TWindowState;

    FFBO: TFBO;

    FMultiSampling: Boolean;
    FSamples: Cardinal;
    FMaxSamples: Cardinal;

    procedure ActivateHandler(Sender: TObject);
    procedure DeactivateHandler(Sender: TObject);
    function GetAspect: Single;
    function GetDeltaTime: Single;
    function GetFPS: Single;
    function GetFPSInt: Cardinal;
    function GetSeconds: Single;
    function GetCursorVisible: Boolean;
    function GetState: TOpenGLState;

    procedure InitGL;
    procedure FinalizeGL;

    procedure SetFullscreen(Value: Boolean);
    procedure SetCursorVisible(Value: Boolean);
    procedure SetMultiSampling(Value: Boolean);
    procedure SetSamples(Value: Cardinal);
    procedure SetVSync(Value: Boolean);
    procedure SetFPSLimit(Value: Single);

    procedure InitDefaults;

    procedure WaitForFPSLimit;

    procedure IdleHandler(Sender: TObject; var Done: Boolean);

    function ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;

    procedure Start;

    class procedure DebugCallback(ASource, AType, AID, ASeverity: Cardinal; ALength: Integer; const AMessage: PAnsiChar;
      AUserdata: Pointer); static; stdcall;

  protected
    procedure Resize; override;
    procedure Paint; override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property VSync: Boolean read FVSync write SetVSync;
    property Fullscreen: Boolean read FFullscreen write SetFullscreen;
    property ClearMask: TGLAttribMask read FClearMask write FClearMask;
    property CursorVisible: Boolean read GetCursorVisible write SetCursorVisible;
    property MaxDeltaTime: Single read FMaxDeltaTime write FMaxDeltaTime;

    property Input: TInputHandler read FInput;

    property DeltaTime: Single read GetDeltaTime;
    property FPS: Single read GetFPS;
    property FPSLimit: Single read FFPSLimit write SetFPSLimit;
    property FPSInt: Cardinal read GetFPSInt;
    property Seconds: Single read GetSeconds;
    property MustUpdateFPS: Boolean read FMustUpdateFPS;

    property State: TOpenGLState read GetState;

    procedure WndProc(var AMessage: TMessage); override;

    procedure ForceFPSUpdate;

    procedure PushState;
    procedure PopState;

    procedure Pause;
    procedure Resume;

    procedure Render;

    procedure Init; virtual;
    procedure Finalize; virtual;

    procedure RenderFunc; virtual;
    procedure UpdateFunc; virtual;
    procedure ResizeFunc; virtual;

    property MultiSampling: Boolean read FMultiSampling write SetMultiSampling;
    property MaxSamples: Cardinal read FMaxSamples;
    property Samples: Cardinal read FSamples write SetSamples;

    property Aspect: Single read GetAspect;

  end;

implementation

uses
  Math;

const
  TaskbarWindowClass = 'Shell_TrayWnd';

{ TOpenGLState }

procedure TOpenGLState.SetClearColor(const Value: TColorRGBA);
begin
  if FClearColor = Value then
    Exit;
  FClearColor := Value;
  Include(FChanges, stClearColor);
  UpdateClearColor;
end;

procedure TOpenGLState.SetDebugOutput(const Value: Boolean);
begin
  if FDebugOutput = Value then
    Exit;
  FDebugOutput := Value;
  Include(FChanges, stDebugOutput);
  UpdateDebugOutput;
end;

procedure TOpenGLState.SetDebugOutputSynced(const Value: Boolean);
begin
  if FDebugOutputSynced = Value then
    Exit;
  FDebugOutputSynced := Value;
  Include(FChanges, stDebugOutputSynced);
  UpdateDebugOutput;
end;

procedure TOpenGLState.SetBlend(const Value: Boolean);
begin
  if FBlend = Value then
    Exit;
  FBlend := Value;
  Include(FChanges, stBlend);
  UpdateBlend;
end;

procedure TOpenGLState.SetBlendFactorDest(const Value: TGLBlendingFactorDest);
begin
  if FBlendFunc.Dest = Value then
    Exit;
  FBlendFunc.Dest := Value;
  Include(FChanges, stBlendFunc);
  UpdateBlendFunc;
end;

procedure TOpenGLState.SetBlendFactorSrc(const Value: TGLBlendingFactorSrc);
begin
  if FBlendFunc.Src = Value then
    Exit;
  FBlendFunc.Src := Value;
  Include(FChanges, stBlendFunc);
  UpdateBlendFunc;
end;

procedure TOpenGLState.SetCullFace(const Value: TGLCullFace);
begin
  if FCullFace = Value then
    Exit;
  FCullFace := Value;
  Include(FChanges, stCullFace);
  UpdateCullFace;
end;

procedure TOpenGLState.SetDepthClamp(const Value: Boolean);
begin
  if FDepthClamp = Value then
    Exit;
  FDepthClamp := Value;
  Include(FChanges, stDepthClamp);
  UpdateDepthClamp;
end;

procedure TOpenGLState.SetDepthFunc(const Value: TGLCompareFunction);
begin
  if FDepthFunc = Value then
    Exit;
  FDepthFunc := Value;
  Include(FChanges, stDepthFunc);
  UpdateDepthFunc;
end;

procedure TOpenGLState.SetDepthMask(const Value: Boolean);
begin
  if FDepthMask = Value then
    Exit;
  FDepthMask := Value;
  Include(FChanges, stDepthMask);
  UpdateDepthMask;
end;

procedure TOpenGLState.SetDepthTest(const Value: Boolean);
begin
  if FDepthTest = Value then
    Exit;
  FDepthTest := Value;
  Include(FChanges, stDepthTest);
  UpdateDepthTest;
end;

procedure TOpenGLState.SetSeamlessCubemap(const Value: Boolean);
begin
  if FSeamlessCubemap = Value then
    Exit;
  FSeamlessCubemap := Value;
  Include(FChanges, stSeamlessCubemap);
  UpdateSeamlessCubemap;
end;

function TOpenGLState.Copy: TOpenGLState;
begin
  Result := TOpenGLState.Create;

  Result.FClearColor := FClearColor;
  Result.FDepthFunc := FDepthFunc;
  Result.FBlend := FBlend;
  Result.FBlendFunc := FBlendFunc;
  Result.FDepthTest := FDepthTest;
  Result.FCullFace := FCullFace;
  Result.FDepthMask := FDepthMask;
  Result.FSeamlessCubemap := FSeamlessCubemap;
  Result.FDepthClamp := FDepthClamp;
  Result.FDebugOutput := FDebugOutput;
  Result.FDebugOutputSynced := FDebugOutputSynced;
end;

procedure TOpenGLState.UpdateClearColor;
begin
  glClearColor(ClearColor.R, ClearColor.G, ClearColor.B, ClearColor.A);
end;

procedure TOpenGLState.UpdateDebugOutput;
begin
  if DebugOutput then
    glEnable(GL_DEBUG_OUTPUT)
  else
    glDisable(GL_DEBUG_OUTPUT);
end;

procedure TOpenGLState.UpdateDebugOutputSynced;
begin
  if DebugOutputSynced then
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
  else
    glDisable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
end;

procedure TOpenGLState.UpdateDepthClamp;
begin
  if DepthClamp then
    glEnable(GL_DEPTH_CLAMP)
  else
    glDisable(GL_DEPTH_CLAMP);
end;

procedure TOpenGLState.UpdateDepthFunc;
begin
  glDepthFunc(Ord(DepthFunc));
end;

procedure TOpenGLState.UpdateCullFace;
begin
  glCullFace(Ord(CullFace));
end;

procedure TOpenGLState.UpdateDepthTest;
begin
  if DepthTest then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
end;

procedure TOpenGLState.UpdateBlend;
begin
  if Blend then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);
end;

procedure TOpenGLState.UpdateBlendFunc;
begin
  glBlendFunc(Ord(BlendFactorSrc), Ord(BlendFactorDest));
end;

procedure TOpenGLState.UpdateDepthMask;
begin
  glDepthMask(DepthMask);
end;

procedure TOpenGLState.UpdateSeamlessCubemap;
begin
  if SeamlessCubemap then
    glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS)
  else
    glDisable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
end;

{ TGLForm }

function TGLForm.GetDeltaTime: Single;
begin
  Result := FTimer.DeltaTime;
end;

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

function TGLForm.GetAspect: Single;
begin
  Result := ClientWidth / ClientHeight;
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

function TGLForm.GetState: TOpenGLState;
begin
  Result := StateStack.Top;
end;

procedure TGLForm.InitGL;
begin
  FDC := GetDC(Handle);
  FRC := CreateRenderingContextVersion(FDC, [opDoubleBuffered], 4, 3, True, 32, 32, 0, 0, 0, 0);
  ActivateRenderingContext(FDC, FRC);

  glDebugMessageCallback(DebugCallback, Self);

  StateStack := TObjectStack<TOpenGLState>.Create;
  StateStack.Push(TOpenGLState.Create);
  InitDefaults;
end;

procedure TGLForm.Start;
begin
  Visible := True;
  FRunning := True;
  Application.OnDeactivate := DeactivateHandler;
  Application.OnActivate := ActivateHandler;
  Application.OnIdle := IdleHandler;
end;

procedure TGLForm.Paint;
begin
  Render;
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

procedure TGLForm.FinalizeGL;
begin
  StateStack.Free;

  DeactivateRenderingContext;
  DestroyRenderingContext(FRC);
  ReleaseDC(Handle, FDC);
end;

procedure TGLForm.SetFPSLimit(Value: Single);
begin
  if Value = 0 then
    raise Exception.Create('FPS Limit must be greater than zero!');
  if FFPSLimit = Value then
    Exit;
  FFPSLimit := Value;
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

procedure TGLForm.InitDefaults;
begin
  glEnable(GL_CULL_FACE); // Disable can be achieved with CullFace to cfBoth

  State.DepthFunc := cfLess; // default
  State.ClearColor := ColorTransparent; // default
  State.DepthTest := True;
  State.CullFace := cfBack; // default
  State.Blend := True;
  State.BlendFactorSrc := bfsSrcAlpha;
  State.BlendFactorDest := bfdOneMinusSrcAlpha;
  State.DepthMask := True; // default
  State.SeamlessCubemap := True;
  State.DepthClamp := True;
  State.DebugOutput := False; // default
  State.DebugOutputSynced := False; // default
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
      UpdateFunc;
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

procedure TGLForm.Render;
begin
  if FMultiSampling then
  begin
    FFBO.Bind;
    glClear(Ord(ClearMask));
    RenderFunc;
    FFBO.CopyToScreen(amColor);
  end
  else
  begin
    TFBO.BindScreen(ClientWidth, ClientHeight);
    glClear(Ord(ClearMask));
    RenderFunc;
  end;
  SwapBuffers(FDC);
end;

procedure TGLForm.Init;
begin
end;

procedure TGLForm.Finalize;
begin
end;

procedure TGLForm.RenderFunc;
begin
end;

procedure TGLForm.UpdateFunc;
begin
end;

procedure TGLForm.Resize;
begin
  inherited;
  if not FRunning then
    Exit;

  ResizeFunc;
  if MultiSampling then
    FFBO.Resize(ClientWidth, ClientHeight);

  Render;
end;

procedure TGLForm.ResizeFunc;
begin
end;

procedure TGLForm.ForceFPSUpdate;
begin
  FTimer.ForceFPSUpdate;
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

  {$IFNDEF FPC}
  Resize;
  {$ENDIF}
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

function TGLForm.ErrorBox(const ATitle, AMessage: String; AButtons: TMsgDlgButtons; ADefault: TMsgDlgBtn): TModalResult;
begin
  {$IFDEF FPC}
  Result := MessageDlg(ATitle, PChar(AMessage), mtError, AButtons, 0, ADefault);
  {$ELSE}
  Result := MessageDlg(ATitle + sLineBreak + sLineBreak + AMessage, mtError, AButtons, 0, ADefault);
  {$ENDIF}
end;

procedure TGLForm.PushState;
begin
  StateStack.Push(State.Copy);
end;

procedure TGLForm.PopState;
var
  S: TOpenGLState.TState;
  Changes: TOpenGLState.TStates;
begin
  Changes := State.Changes;
  StateStack.Pop;
  for S in Changes do
  begin
    case S of
      stClearColor:
        State.UpdateClearColor;
      stDepthFunc:
        State.UpdateDepthFunc;
      stCullFace:
        State.UpdateCullFace;
      stDepthTest:
        State.UpdateDepthTest;
      stBlend:
        State.UpdateBlend;
      stBlendFunc:
        State.UpdateBlendFunc;
      stDepthMask:
        State.UpdateDepthMask;
      stSeamlessCubemap:
        State.UpdateSeamlessCubemap;
      stDepthClamp:
        State.UpdateDepthClamp;
      stDebugOutput:
        State.UpdateDebugOutput;
      stDebugOutputSynced:
        State.UpdateDebugOutputSynced;
    else
      Assert(False);
    end;
  end;
end;

end.
