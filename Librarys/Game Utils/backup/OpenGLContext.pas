unit OpenGLContext;

interface

uses
  dglOpenGL, Forms, Controls, Windows, Classes, GLEnums, Color, InputHandler, TimeManager, Lists, Graphics, FBOManager,
  SysUtils, Dialogs;

type

  { TOpenGLState }

  // steps to add state:
  // 1) TState enum
  // 2) property
  // 3) Update Function
  // 4) Pop
  // 5) copy
  // 6) initialization (important for pop to main state)

  TOpenGLState = class
  public
    type
      TState = (
        stClearColor,
        stDepthFunc,
        stCullFace,
        stDepthTest,
        stBlend,
        stBlendFunc,
        stDepthMask,
        stSeamlessCubemap
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

    procedure SetBlend(AValue: Boolean);
    procedure SetBlendFactorDest(AValue: TGLBlendingFactorDest);
    procedure SetBlendFactorSrc(AValue: TGLBlendingFactorSrc);
    procedure SetClearColor(AValue: TColorRGBA);
    procedure SetCullFace(AValue: TGLCullFace);
    procedure SetDepthFunc(AValue: TGLCompareFunction);
    procedure SetDepthMask(AValue: Boolean);
    procedure SetDepthTest(AValue: Boolean);
    procedure SetSeamlessCubemap(AValue: Boolean);

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

    procedure UpdateClearColor;
    procedure UpdateDepthFunc;
    procedure UpdateCullFace;
    procedure UpdateDepthTest;
    procedure UpdateBlend;
    procedure UpdateBlendFunc;
    procedure UpdateDepthMask;
    procedure UpdateSeamlessCubemap;
  end;

  TCharPressEvent = procedure (Sender: TObject; PressedChar: AnsiChar) of object;

  { TGLForm }

  TGLForm = class (TForm)
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

    FOnCharPress: TCharPressEvent;

    FFBO: TFBO;

    FMultiSampling: Boolean;
    FSamples: Cardinal;
    FMaxSamples: Cardinal;

    procedure ActivateHandler(Sender: TObject);
    procedure DeactivateHandler(Sender: TObject);
    function GetDeltaTime: Single;
    function GetFPS: Single;
    function GetFPSInt: Cardinal;
    function GetSeconds: Single;
    function GetCursorVisible: Boolean;
    function GetState: TOpenGLState;

    procedure InitGL;
    procedure FinalizeGL;

    procedure SetFullscreen(AValue: Boolean);
    procedure SetCursorVisible(AValue: Boolean);
    procedure SetMultiSampling(AValue: Boolean);
    procedure SetSamples(AValue: Cardinal);
    procedure SetVSync(AValue: Boolean);
    procedure SetFPSLimit(AValue: Single);

    procedure InitDefaults;

    procedure WaitForFPSLimit;

    procedure IdleHandler(Sender: TObject; var Done: Boolean);

    procedure Start;

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
    procedure DoOnResize; override;

    procedure ForceFPSUpdate;

    procedure PushState;
    procedure PopState;

    procedure Pause;
    procedure Continue;

    procedure Render;

    procedure Init; virtual;
    procedure Finalize; virtual;

    procedure RenderFunc; virtual;
    procedure UpdateFunc; virtual;
    procedure ResizeFunc; virtual;

    property OnCharPress: TCharPressEvent read FOnCharPress write FOnCharPress;

    property MultiSampling: Boolean read FMultiSampling write SetMultiSampling;
    property MaxSamples: Cardinal read FMaxSamples;
    property Samples: Cardinal read FSamples write SetSamples;

    const
      TaskbarWindowClass = 'Shell_TrayWnd';

  end;

implementation

uses
  Math;

{ TOpenGLState }

procedure TOpenGLState.SetClearColor(AValue: TColorRGBA);
begin
  if FClearColor = AValue then
    Exit;
  FClearColor := AValue;
  Include(FChanges, stClearColor);
  UpdateClearColor;
end;

procedure TOpenGLState.SetBlend(AValue: Boolean);
begin
  if FBlend = AValue then
    Exit;
  FBlend := AValue;
  Include(FChanges, stBlend);
  UpdateBlend;
end;

procedure TOpenGLState.SetBlendFactorDest(AValue: TGLBlendingFactorDest);
begin
  if FBlendFunc.Dest = AValue then
    Exit;
  FBlendFunc.Dest := AValue;
  Include(FChanges, stBlendFunc);
  UpdateBlendFunc;
end;

procedure TOpenGLState.SetBlendFactorSrc(AValue: TGLBlendingFactorSrc);
begin
  if FBlendFunc.Src = AValue then
    Exit;
  FBlendFunc.Src := AValue;
  Include(FChanges, stBlendFunc);
  UpdateBlendFunc;
end;

procedure TOpenGLState.SetCullFace(AValue: TGLCullFace);
begin
  if FCullFace = AValue then
    Exit;
  FCullFace := AValue;
  Include(FChanges, stCullFace);
  UpdateCullFace;
end;

procedure TOpenGLState.SetDepthFunc(AValue: TGLCompareFunction);
begin
  if FDepthFunc = AValue then
    Exit;
  FDepthFunc := AValue;
  Include(FChanges, stDepthFunc);
  UpdateDepthFunc;
end;

procedure TOpenGLState.SetDepthMask(AValue: Boolean);
begin
  if FDepthMask = AValue then
    Exit;
  FDepthMask := AValue;
  Include(FChanges, stDepthMask);
  UpdateDepthMask;
end;

procedure TOpenGLState.SetDepthTest(AValue: Boolean);
begin
  if FDepthTest = AValue then
    Exit;
  FDepthTest := AValue;
  Include(FChanges, stDepthTest);
  UpdateDepthTest;
end;

procedure TOpenGLState.SetSeamlessCubemap(AValue: Boolean);
begin
  if FSeamlessCubemap = AValue then
    Exit;
  FSeamlessCubemap := AValue;
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
end;

procedure TOpenGLState.UpdateClearColor;
begin
  glClearColor(FClearColor.R, FClearColor.G, FClearColor.B, FClearColor.A);
end;

procedure TOpenGLState.UpdateDepthFunc;
begin
  glDepthFunc(Ord(FDepthFunc));
end;

procedure TOpenGLState.UpdateCullFace;
begin
  glCullFace(Ord(FCullFace));
end;

procedure TOpenGLState.UpdateDepthTest;
begin
  if FDepthTest then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
end;

procedure TOpenGLState.UpdateBlend;
begin
  if FBlend then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);
end;

procedure TOpenGLState.UpdateBlendFunc;
begin
  glBlendFunc(Ord(FBlendFunc.Src), Ord(FBlendFunc.Dest));
end;

procedure TOpenGLState.UpdateDepthMask;
begin
  glDepthMask(FDepthMask);
end;

procedure TOpenGLState.UpdateSeamlessCubemap;
begin
  if FSeamlessCubemap then
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
    ShowWindow(Handle, SW_SHOW);
  end;
end;

procedure TGLForm.DeactivateHandler(Sender: TObject);
begin
  FInput.ReleaseAll;
  if Fullscreen then
  begin
    ShowWindow(FindWindow(TaskbarWindowClass, nil), SW_SHOWNOACTIVATE);
    ShowWindow(Handle, SW_HIDE);
  end;
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

procedure TGLForm.Pause;
begin
  FInput.ReleaseAll;
  Application.OnIdle := nil;
end;

procedure TGLForm.Continue;
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

procedure TGLForm.SetFPSLimit(AValue: Single);
begin
  if FFPSLimit = AValue then
    Exit;
  FFPSLimit := AValue;
end;

procedure TGLForm.SetFullscreen(AValue: Boolean);
var
  Flags: LONG;
begin
  FFullscreen := AValue;

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

procedure TGLForm.SetCursorVisible(AValue: Boolean);
begin
  Cursor := TCursor(AValue) - 1;
end;

procedure TGLForm.SetMultiSampling(AValue: Boolean);
begin
  if FMultiSampling = AValue then
    Exit;
  FMultiSampling := AValue;

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

procedure TGLForm.SetSamples(AValue: Cardinal);
begin
  if (AValue < 1) or (AValue > MaxSamples) then
    raise Exception.Create('Unspoorted Number of Samples!');

  if (FSamples = AValue) and MultiSampling then
    Exit;
  FSamples := AValue;

  if not MultiSampling then
    MultiSampling := True
  else
    FFBO.SetSamples(FSamples);
end;

procedure TGLForm.SetVSync(AValue: Boolean);
begin
  FVSync := AValue;
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
end;

procedure TGLForm.WaitForFPSLimit;
begin
  if FPS > FPSLimit then
    Sleep(Floor(1000 / FPSLimit));
end;

procedure TGLForm.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  FMustUpdateFPS := FTimer.Update;

  if DeltaTime <= MaxDeltaTime then
  begin
    try
      UpdateFunc;
      GLErrorMessage;
    except
      on E: Exception do
      begin
        Pause;
        if MessageDlg('Game Update Error!', PChar(E.Message), mtError, [mbIgnore, mbClose], 0, mbClose) = mrClose then
        begin
          Close;
          Exit;
        end
        else
          Continue;
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
      if MessageDlg('Render Error!', PChar(E.Message), mtError, [mbIgnore, mbClose], 0, mbClose) = mrClose then
      begin
        Close;
        Exit;
      end
      else
        Continue;
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
      Exit;
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

procedure TGLForm.DoOnResize;
begin
  inherited DoOnResize;
  if not FRunning then
    Exit;

  ResizeFunc;
  if MultiSampling then
    FFBO.Resize(ClientWidth, ClientHeight);
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
    GLErrorMessage;
    TFBO.BindScreen(ClientWidth, ClientHeight);
    GLErrorMessage;
    glClear(Ord(ClearMask));
    GLErrorMessage;
    RenderFunc;
    GLErrorMessage;
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
    Init;
    GLErrorMessage;
    Start;
  except
    on E: EAbort do
    begin
      WindowState := wsMinimized;
      Close;
    end;
    on E: Exception do
      if MessageDlg('Initialization Error!', PChar(E.Message), mtError, [mbIgnore, mbClose], 0, mbClose) = mrClose then
      begin
        WindowState := wsMinimized; // hide the window sneaky sneaky
        Close;
      end
      else
        Start;
  end;
end;

destructor TGLForm.Destroy;
begin
  try
    Finalize;
  except
    on E: Exception do
      MessageBox(0, PChar(E.Message), 'Finalization Error!', MB_ICONERROR);
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
    end;
  end;
end;

end.
