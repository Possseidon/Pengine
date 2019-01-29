unit Pengine.GLForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,
  System.SysUtils,

  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,

  Pengine.GLGame,
  Pengine.InputHandler,
  Pengine.GLContext,
  Pengine.GLState,
  Pengine.GLEnums,
  Pengine.IntMaths,
  Pengine.ResourceManager;

type

  TGLForm = class(TForm)
  private
    FGame: TGLGame;
    FInput: TInputHandler;
    FContext: TGLContext;
    FFullscreen: Boolean;
    FOldWindowStyle: NativeInt;
    FRunning: Boolean;
    FDC: HDC;
    FOldBoundsRect: TRect;

    procedure ActivateHandler(Sender: TObject);
    procedure DeactivateHandler(Sender: TObject);
    function GetCursorVisible: Boolean;

    procedure SetFullscreen(Value: Boolean);
    procedure SetCursorVisible(Value: Boolean);

    procedure IdleHandler(Sender: TObject; var Done: Boolean);
    function GetGLState: TGLState;
    function GetDeltaTime: Single;

    procedure RenderCallback;

    procedure ApplicationException(Sender: TObject; E: Exception);

  protected
    procedure Resize; override;
    procedure DoClose(var Action: TCloseAction); override;
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

{ TGLForm }

procedure TGLForm.ActivateHandler(Sender: TObject);
begin
  if Fullscreen then
  begin
    BringToFront;
    Resume;
  end;
end;

procedure TGLForm.DeactivateHandler(Sender: TObject);
begin
  Input.ReleaseAll;
  if Fullscreen then
  begin
    Pause;
    WindowState := wsMinimized;
  end;
end;

function TGLForm.GetCursorVisible: Boolean;
begin
  Result := Cursor <> -1;
end;

procedure TGLForm.SetFullscreen(Value: Boolean);
const
  FullscreenFlags: NativeUInt = WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;

begin
  if Fullscreen = Value then
    Exit;
  FFullscreen := Value;

  if FFullscreen then
  begin
    // activate fullscreen
    FOldWindowStyle := GetWindowLong(Handle, GWL_STYLE);
    SetWindowLong(Handle, GWL_STYLE, FullscreenFlags);
    FOldBoundsRect := BoundsRect;
    BoundsRect := Monitor.BoundsRect;
  end
  else
  begin
    // deactivate fullscreen
    SetWindowLong(Handle, GWL_STYLE, FOldWindowStyle);
    BoundsRect := FOldBoundsRect;
  end;
end;

procedure TGLForm.SetCursorVisible(Value: Boolean);
begin
  Cursor := TCursor(Value) - 1;
end;

procedure TGLForm.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  FContext.Update;

  try
    Game.Update;
  except
    on E: Exception do
    begin
      Pause;
      if TGLContext.ErrorBox('Game Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
      begin
        Close;
        Exit;
      end
      else
        Resume;
    end;
  end;

  try
    FContext.Render;
  except
    on E: Exception do
    begin
      Pause;
      if TGLContext.ErrorBox('Render Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
      begin
        Close;
        Exit;
      end
      else
        Resume;
    end;
  end;

  FInput.NotifyChanges;

  FContext.WaitForFPSLimit;
  Done := False;
end;

function TGLForm.GetGLState: TGLState;
begin
  Result := Context.GLState;
end;

function TGLForm.GetDeltaTime: Single;
begin
  Result := Context.DeltaTime;
end;

procedure TGLForm.RenderCallback;
begin
  Game.Render;
end;

procedure TGLForm.ApplicationException(Sender: TObject; E: Exception);
begin
  Pause;
  if TGLContext.ErrorBox('Game Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
    Close
  else
    Resume;
end;

procedure TGLForm.Resize;
begin
  inherited;
  if not FRunning then
    Exit;

  FContext.Size := IVec2(ClientWidth, ClientHeight);
  Game.Resize(FContext.Size);

  Context.Render;
end;

constructor TGLForm.Create(TheOwner: TComponent);
begin
  inherited;

  try
    FDC := GetDC(Handle);
    FContext := TGLContext.Create(FDC, IVec2(ClientWidth, ClientHeight), RenderCallback);
    FInput := TInputHandler.Create(Self);
    FGame := TGLGame.Create(Context, Input, Context.Timer, IVec2(ClientWidth, ClientHeight));

    Constraints.MinWidth := 200;
    Constraints.MinHeight := 100;

    SendMessage(Handle, WM_KEYDOWN, VK_MENU, 0);
    SendMessage(Handle, WM_KEYDOWN, VK_RIGHT, 0);

    SendMessage(Handle, WM_KEYUP, VK_MENU, 0);
    SendMessage(Handle, WM_KEYUP, VK_RIGHT, 0);

    FFullscreen := False;

    Color := clBlack;

    Application.OnException := ApplicationException;

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

  // DeactivateContext releases the Handle
  // ReleaseDC(Handle, FDC);
  inherited;
end;

procedure TGLForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  // prevent OpenGL from erroring
  Fullscreen := False;
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
  Context.Render;
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
          begin
            try
              FInput.PressChar(Char(W));
            except
              on E: Exception do
              begin
                Pause;
                if TGLContext.ErrorBox('Game Error!', E.Message, [mbIgnore, mbClose], mbClose) = mrClose then
                  Close
                else
                  Resume;
              end;
            end;
          end;
        end;
      end;
  end;

  inherited WndProc(AMessage);
end;

procedure TGLForm.Init;
begin
  // nothing by default
end;

procedure TGLForm.Finalize;
begin
  // nothing by default
end;

end.
