unit Pengine.InputHandler;

interface

uses
  Winapi.Windows,

  System.UITypes,
  System.Classes,
  System.Types,

  Vcl.Controls,
  Vcl.Forms,
  Vcl.Clipbrd,

  Pengine.Bitfield,
  Pengine.Vector,
  Pengine.EventHandling;

type

  TKey = type Byte;

  TKeyEventInfo = class(TEventInfo)
  private
    FKey: TKey;

  public
    constructor Create(AKey: TKey);

    property Key: TKey read FKey;

  end;

  TKeyEvent = TEvent<TKeyEventInfo>;

  TTypeEventInfo = class(TEventInfo)
  private
    FText: string;

  public
    constructor Create(AText: string);

    property Text: string read FText;

  end;

  TTypeEvent = TEvent<TTypeEventInfo>;

  TKeyboardInput = class
  private
    FKeys: TBitfield;
    FNotifyDown: TBitfield;
    FNotifyUp: TBitfield;
    FNotifyTyped: TBitfield;
    FTextBuffer: string;

    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnType: TTypeEvent;

    function GetOnKeyDown: TKeyEvent.TAccess;
    function GetOnKeyUp: TKeyEvent.TAccess;
    function GetOnType: TTypeEvent.TAccess;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetNotifyUp;
    procedure ResetNotifyDown;
    procedure ResetNotifyTyped;

    procedure ResetCharBuffer;

    procedure Reset;

    // WRITE
    procedure PressKey(AKey: TKey);
    procedure ReleaseKey(AKey: TKey);
    procedure PressChar(AChar: Char);
    procedure ReleaseAllKeys;

    // READ
    function KeyDown(AKey: TKey): Boolean;
    function KeyUp(AKey: TKey): Boolean;
    function AsyncKeyDown(AKey: TKey): Boolean;
    function AsyncKeyUp(AKey: TKey): Boolean;
    function KeyPressed(AKey: TKey): Boolean;
    function KeyReleased(AKey: TKey): Boolean;
    function KeyTyped(AKey: TKey): Boolean;

    property TextBuffer: string read FTextBuffer;

    function AnyKeyDown: Boolean;

    property OnKeyDown: TKeyEvent.TAccess read GetOnKeyDown;
    property OnKeyUp: TKeyEvent.TAccess read GetOnKeyUp;
    property OnType: TTypeEvent.TAccess read GetOnType;

  end;

  TButtonEventInfo = class(TEventInfo)
  private
    FButton: TMouseButton;

  public
    constructor Create(AButton: TMouseButton);

    property Button: TMouseButton read FButton;

  end;

  TButtonEvent = TEvent<TButtonEventInfo>;

  TScrollEventInfo = class(TEventInfo)
  private
    FScrolledUp: Boolean;

  public
    constructor Create(AScrolledUp: Boolean);

    property ScrolledUp: Boolean read FScrolledUp;

  end;

  TScrollEvent = TEvent<TScrollEventInfo>;

  TMouseInput = class
  private
    FButtons, FNotifyUp, FNotifyDown: TBitField;
    FScrolledUp, FScrolledDown: Boolean;
    FWidth, FHeight: Integer;
    FPos: TVector2;
    FPosNotify: Boolean;
    FOnScreen: Boolean;
    FOnScreenNotify: Boolean;
    FResizeNotify: Boolean;

    FOnButtonDown: TButtonEvent;
    FOnButtonUp: TButtonEvent;
    FOnMouseMove: TEvent;
    FOnEnterScreen: TEvent;
    FOnLeaveScreen: TEvent;
    FOnScroll: TScrollEvent;

    function GetOnButtonDown: TButtonEvent.TAccess;
    function GetOnButtonUp: TButtonEvent.TAccess;
    function GetOnMouseMove: TEvent.TAccess;
    function GetOnEnterScreen: TEvent.TAccess;
    function GetOnLeaveScreen: TEvent.TAccess;
    function GetOnScroll: TScrollEvent.TAccess;

  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;

    procedure ResetNotifyUp;
    procedure ResetNotifyDown;
    procedure ResetPosNotify;
    procedure ResetScroll;
    procedure ResetOnScreenNotify;
    procedure ResetResizeNotify;

    procedure Reset;

    // WRITE
    procedure PressButton(AButton: TMouseButton);
    procedure ReleaseButton(AButton: TMouseButton);
    procedure ReleaseAllButtons;
    procedure SetPosition(APos: TVector2);
    procedure ScrollUp;
    procedure ScrollDown;
    procedure Leave;
    procedure Enter;

    procedure Resize(AWidth, AHeight: Integer);

    // READ
    function ButtonDown(AButton: TMouseButton): Boolean;
    function ButtonPressed(AButton: TMouseButton): Boolean;
    function ButtonUp(AButton: TMouseButton): Boolean;
    function ButtonReleased(AButton: TMouseButton): Boolean;
    function AnyButtonDown: Boolean;
    function MousePos: TVector2;
    function MouseMoved: Boolean;
    function ScrolledUp: Boolean;
    function ScrolledDown: Boolean;
    function OnScreen: Boolean;
    function OnScreenChanged: Boolean;

    function Resized: Boolean;
    function Width: Integer;
    function Height: Integer;

    property OnButtonDown: TButtonEvent.TAccess read GetOnButtonDown;
    property OnButtonUp: TButtonEvent.TAccess read GetOnButtonUp;
    property OnMouseMove: TEvent.TAccess read GetOnMouseMove;
    property OnEnterScreen: TEvent.TAccess read GetOnEnterScreen;
    property OnLeaveScreen: TEvent.TAccess read GetOnLeaveScreen;
    property OnScroll: TScrollEvent.TAccess read GetOnScroll;

  end;

  TInputHandler = class
  private
    FForm: TForm;

    FMouse: TMouseInput;
    FKeyboard: TKeyboardInput;

    FOldKeyDown, FOldKeyUp: Vcl.Controls.TKeyEvent;
    FOldMouseDown, FOldMouseUp: Vcl.Controls.TMouseEvent;
    FOldMouseMove: Vcl.Controls.TMouseMoveEvent;
    FOldResize: TNotifyEvent;
    FOldMouseWheel: TMouseWheelEvent;

    function GetTextBuffer: string;
    function GetTextBufferEmpty: Boolean;

    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseWheelHandler(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ResizeHandler(Sender: TObject);

    function GetOnKeyDown: TKeyEvent.TAccess;
    function GetOnKeyUp: TKeyEvent.TAccess;
    function GetOnType: TTypeEvent.TAccess;

    function GetOnButtonDown: TButtonEvent.TAccess;
    function GetOnButtonUp: TButtonEvent.TAccess;
    function GetOnMouseMove: TEvent.TAccess;
    function GetOnEnterScreen: TEvent.TAccess;
    function GetOnLeaveScreen: TEvent.TAccess;
    function GetOnScroll: TScrollEvent.TAccess;

  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;

    procedure PressChar(AChar: Char);

    // Mouse
    function ButtonDown(AButton: TMouseButton): Boolean; inline;
    function ButtonPressed(AButton: TMouseButton): Boolean; inline;
    function ButtonUp(AButton: TMouseButton): Boolean; inline;
    function ButtonReleased(AButton: TMouseButton): Boolean; inline;

    function AnyButtonDown: Boolean; inline;

    function MousePos: TVector2; inline;
    function MouseMoved: Boolean; inline;

    function ScrolledUp: Boolean; inline;
    function ScrolledDown: Boolean; inline;
    function Scrolled: Boolean; inline;

    function MouseOnScreen: Boolean; inline;
    function MouseOnScreenChanged: Boolean; inline;
    function MouseLeftScreen: Boolean; inline;
    function MouseEnteredScreen: Boolean; inline;

    function Resized: Boolean; inline;
    function Width: Integer; inline;
    function Height: Integer; inline;

    property OnButtonDown: TButtonEvent.TAccess read GetOnButtonDown;
    property OnButtonUp: TButtonEvent.TAccess read GetOnButtonUp;
    property OnMouseMove: TEvent.TAccess read GetOnMouseMove;
    property OnEnterScreen: TEvent.TAccess read GetOnEnterScreen;
    property OnLeaveScreen: TEvent.TAccess read GetOnLeaveScreen;
    property OnScroll: TScrollEvent.TAccess read GetOnScroll;

    // Keyboard
    function KeyDown(AKey: TKey): Boolean; overload; inline;
    function KeyDown(AKey: Char): Boolean; overload; inline;

    function KeyUp(AKey: TKey): Boolean; overload; inline;
    function KeyUp(AKey: Char): Boolean; overload; inline;

    function AsyncKeyDown(AKey: TKey): Boolean; overload; inline;
    function AsyncKeyDown(AKey: Char): Boolean; overload; inline;

    function AsyncKeyUp(AKey: TKey): Boolean; overload; inline;
    function AsyncKeyUp(AKey: Char): Boolean; overload; inline;

    function KeyPressed(AKey: TKey): Boolean; overload; inline;
    function KeyPressed(AKey: Char): Boolean; overload; inline;

    function KeyReleased(AKey: TKey): Boolean; overload; inline;
    function KeyReleased(AKey: Char): Boolean; overload; inline;

    function KeyTyped(AKey: TKey): Boolean; overload; inline;
    function KeyTyped(AKey: Char): Boolean; overload; inline;

    property TextBuffer: string read GetTextBuffer;
    property TextBufferEmpty: Boolean read GetTextBufferEmpty;

    function AnyKeyDown: Boolean; inline;

    property OnKeyDown: TKeyEvent.TAccess read GetOnKeyDown;
    property OnKeyUp: TKeyEvent.TAccess read GetOnKeyUp;
    property OnType: TTypeEvent.TAccess read GetOnType;

    // Both
    function AnyAction: Boolean; inline;

    procedure NotifyChanges;

    procedure ReleaseAll;

  end;

implementation

const
  MouseButtonCount = Ord(High(TMouseButton)) + 1;

{ TInputHandler }

procedure TInputHandler.KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FKeyboard.PressKey(Key);
  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

function TInputHandler.GetTextBuffer: string;
begin
  Result := FKeyboard.TextBuffer;
end;

function TInputHandler.GetOnButtonDown: TButtonEvent.TAccess;
begin
  Result := FMouse.OnButtonDown;
end;

function TInputHandler.GetOnButtonUp: TButtonEvent.TAccess;
begin
  Result := FMouse.OnButtonUp;
end;

function TInputHandler.GetOnEnterScreen: TEvent.TAccess;
begin
  Result := FMouse.OnEnterScreen;
end;

function TInputHandler.GetOnKeyDown: TKeyEvent.TAccess;
begin
  Result := FKeyboard.OnKeyDown;
end;

function TInputHandler.GetOnKeyUp: TKeyEvent.TAccess;
begin
  Result := FKeyboard.OnKeyUp;
end;

function TInputHandler.GetOnLeaveScreen: TEvent.TAccess;
begin
  Result := FMouse.OnLeaveScreen;
end;

function TInputHandler.GetOnMouseMove: TEvent.TAccess;
begin
  Result := FMouse.OnMouseMove;
end;

function TInputHandler.GetOnScroll: TScrollEvent.TAccess;
begin
  Result := FMouse.OnScroll;
end;

function TInputHandler.GetOnType: TTypeEvent.TAccess;
begin
  Result := FKeyboard.OnType;
end;

function TInputHandler.GetTextBufferEmpty: Boolean;
begin
  Result := FKeyboard.TextBuffer <> '';
end;

procedure TInputHandler.KeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FKeyboard.ReleaseKey(Key);
  if Assigned(FOldKeyUp) then
    FOldKeyUp(Sender, Key, Shift);
end;

procedure TInputHandler.MouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouse.PressButton(Button);
  if Assigned(FOldMouseDown) then
    FOldMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TInputHandler.MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMouse.SetPosition(TVector2.Create(X, Y));
  if Assigned(FOldMouseMove) then
    FOldMouseMove(Sender, Shift, X, Y);
end;

procedure TInputHandler.MouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouse.ReleaseButton(Button);
  if Assigned(FOldMouseUp) then
    FOldMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TInputHandler.MouseWheelHandler(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  if WheelDelta > 0 then
    FMouse.ScrollUp
  else if WheelDelta < 0 then
    FMouse.ScrollDown;
  if Assigned(FOldMouseWheel) then
    FOldMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TInputHandler.ResizeHandler(Sender: TObject);
begin
  with TForm(Sender) do
    FMouse.Resize(ClientWidth, ClientHeight);
  if Assigned(FOldResize) then
    FOldResize(Sender);
end;

constructor TInputHandler.Create(AForm: TForm);
begin
  FForm := AForm;
  FMouse := TMouseInput.Create(FForm);
  FKeyboard := TKeyboardInput.Create;

  FOldKeyDown := FForm.OnKeyDown;
  FOldKeyUp := FForm.OnKeyUp;
  FOldMouseDown := FForm.OnMouseDown;
  FOldMouseUp := FForm.OnMouseUp;
  FOldMouseMove := FForm.OnMouseMove;
  FOldMouseWheel := FForm.OnMouseWheel;
  FOldResize := FForm.OnResize;

  FForm.OnKeyDown := KeyDownHandler;
  FForm.OnKeyUp := KeyUpHandler;
  FForm.OnMouseDown := MouseDownHandler;
  FForm.OnMouseUp := MouseUpHandler;
  FForm.OnMouseMove := MouseMoveHandler;
  FForm.OnMouseWheel := MouseWheelHandler;
  FForm.OnResize := ResizeHandler;
end;

destructor TInputHandler.Destroy;
begin
  FMouse.Free;
  FKeyboard.Free;
  inherited Destroy;
end;

procedure TInputHandler.PressChar(AChar: Char);
begin
  FKeyboard.PressChar(AChar);
end;

function TInputHandler.ButtonDown(AButton: TMouseButton): Boolean;
begin
  Result := FMouse.ButtonDown(AButton);
end;

function TInputHandler.ButtonPressed(AButton: TMouseButton): Boolean;
begin
  Result := FMouse.ButtonPressed(AButton);
end;

function TInputHandler.ButtonUp(AButton: TMouseButton): Boolean;
begin
  Result := FMouse.ButtonUp(AButton);
end;

function TInputHandler.ButtonReleased(AButton: TMouseButton): Boolean;
begin
  Result := FMouse.ButtonReleased(AButton);
end;

function TInputHandler.MousePos: TVector2;
begin
  Result := FMouse.MousePos;
end;

function TInputHandler.MouseMoved: Boolean;
begin
  Result := FMouse.MouseMoved;
end;

function TInputHandler.ScrolledUp: Boolean;
begin
  Result := FMouse.ScrolledUp;
end;

function TInputHandler.ScrolledDown: Boolean;
begin
  Result := FMouse.ScrolledDown;
end;

function TInputHandler.Scrolled: Boolean;
begin
  Result := ScrolledUp or ScrolledDown;
end;

function TInputHandler.MouseOnScreen: Boolean;
begin
  Result := FMouse.OnScreen;
end;

function TInputHandler.MouseOnScreenChanged: Boolean;
begin
  Result := FMouse.OnScreenChanged;
end;

function TInputHandler.MouseLeftScreen: Boolean;
begin
  Result := MouseOnScreenChanged and not MouseOnScreen;
end;

function TInputHandler.MouseEnteredScreen: Boolean;
begin
  Result := MouseOnScreenChanged and MouseOnScreen;
end;

function TInputHandler.Resized: Boolean;
begin
  Result := FMouse.Resized;
end;

function TInputHandler.Width: Integer;
begin
  Result := FMouse.Width;
end;

function TInputHandler.Height: Integer;
begin
  Result := FMouse.Height;
end;

function TInputHandler.KeyDown(AKey: TKey): Boolean;
begin
  Result := FKeyboard.KeyDown(AKey);
end;

function TInputHandler.KeyPressed(AKey: TKey): Boolean;
begin
  Result := FKeyboard.KeyPressed(AKey);
end;

function TInputHandler.KeyTyped(AKey: TKey): Boolean;
begin
  Result := FKeyboard.KeyTyped(AKey);
end;

function TInputHandler.KeyUp(AKey: TKey): Boolean;
begin
  Result := FKeyboard.KeyUp(AKey);
end;

function TInputHandler.KeyReleased(AKey: TKey): Boolean;
begin
  Result := FKeyboard.KeyReleased(AKey);
end;

function TInputHandler.AsyncKeyDown(AKey: TKey): Boolean;
begin
  Result := FKeyboard.AsyncKeyDown(AKey);
end;

function TInputHandler.AsyncKeyUp(AKey: TKey): Boolean;
begin
  Result := FKeyboard.AsyncKeyUp(AKey);
end;

function TInputHandler.KeyDown(AKey: Char): Boolean;
begin
  Result := KeyDown(Ord(AKey));
end;

function TInputHandler.KeyPressed(AKey: Char): Boolean;
begin
  Result := KeyPressed(Ord(AKey));
end;

function TInputHandler.KeyTyped(AKey: Char): Boolean;
begin
  Result := KeyTyped(Ord(AKey));
end;

function TInputHandler.KeyUp(AKey: Char): Boolean;
begin
  Result := KeyUp(Ord(AKey));
end;

function TInputHandler.KeyReleased(AKey: Char): Boolean;
begin
  Result := KeyReleased(Ord(AKey));
end;

function TInputHandler.AsyncKeyDown(AKey: Char): Boolean;
begin
  Result := FKeyboard.AsyncKeyDown(Ord(AKey));
end;

function TInputHandler.AsyncKeyUp(AKey: Char): Boolean;
begin
  Result := FKeyboard.AsyncKeyUp(Ord(AKey));
end;

function TInputHandler.AnyKeyDown: Boolean;
begin
  Result := FKeyboard.AnyKeyDown;
end;

function TInputHandler.AnyAction: Boolean;
begin
  Result := AnyKeyDown or AnyButtonDown or Scrolled;
end;

function TInputHandler.AnyButtonDown: Boolean;
begin
  Result := FMouse.AnyButtonDown;
end;

procedure TInputHandler.NotifyChanges;
var
  R: TRect;
begin
  FMouse.Reset;
  FKeyboard.Reset;

  R.BottomRight := FForm.ClientToScreen(FForm.ClientRect.BottomRight);
  R.TopLeft := FForm.ClientToScreen(FForm.ClientRect.TopLeft);
  if PtInRect(R, Mouse.CursorPos) then
  begin
    if not FMouse.OnScreen then
      FMouse.Enter;
  end
  else
  begin
    if FMouse.OnScreen then
      FMouse.Leave;
  end;
end;

procedure TInputHandler.ReleaseAll;
begin
  FMouse.ReleaseAllButtons;
  FKeyboard.ReleaseAllKeys;
end;

{ TMouseInput }

constructor TMouseInput.Create(AForm: TForm);
var
  P: TPoint;
begin
  FButtons := TBitField.Create(MouseButtonCount);
  FNotifyUp := TBitField.Create(MouseButtonCount);
  FNotifyDown := TBitField.Create(MouseButtonCount);
  FWidth := AForm.ClientWidth;
  FHeight := AForm.ClientHeight;
  P := AForm.ScreenToClient(Mouse.CursorPos);
  SetPosition(Vec2(P.X, P.Y));
end;

destructor TMouseInput.Destroy;
begin
  FButtons.Free;
  FNotifyUp.Free;
  FNotifyDown.Free;
  inherited;
end;

procedure TMouseInput.ResetNotifyUp;
begin
  FNotifyUp.Clear;
end;

procedure TMouseInput.ResetNotifyDown;
begin
  FNotifyDown.Clear;
end;

procedure TMouseInput.ResetPosNotify;
begin
  FPosNotify := False;
end;

procedure TMouseInput.ResetScroll;
begin
  FScrolledDown := False;
  FScrolledUp := False;
end;

procedure TMouseInput.ResetOnScreenNotify;
begin
  FOnScreenNotify := False;
end;

procedure TMouseInput.ResetResizeNotify;
begin
  FResizeNotify := False;
end;

procedure TMouseInput.Reset;
begin
  ResetNotifyDown;
  ResetNotifyUp;
  ResetPosNotify;
  ResetScroll;
  ResetOnScreenNotify;
  ResetResizeNotify;
end;

procedure TMouseInput.PressButton(AButton: TMouseButton);
begin
  FButtons[Ord(AButton)] := True;
  FNotifyDown[Ord(AButton)] := True;
  FOnButtonDown.Execute(TButtonEventInfo.Create(AButton));
end;

procedure TMouseInput.ReleaseButton(AButton: TMouseButton);
begin
  FButtons[Ord(AButton)] := False;
  FNotifyUp[Ord(AButton)] := True;
  FOnButtonUp.Execute(TButtonEventInfo.Create(AButton));
end;

procedure TMouseInput.ReleaseAllButtons;
var
  Button: Integer;
begin
  for Button in FButtons do
    FOnButtonUp.Execute(TButtonEventInfo.Create(TMouseButton(Button)));
  FButtons.Clear;
  FNotifyUp.Fill;
end;

procedure TMouseInput.SetPosition(APos: TVector2);
begin
  FPos.X := (APos.X * 2 - FWidth) / FHeight; // -aspect <-> +aspect
  FPos.Y := 1 - APos.Y / FHeight * 2;       // -1      <-> +1
  FPosNotify := True;
  FOnMouseMove.Execute;
end;

procedure TMouseInput.ScrollUp;
begin
  FScrolledUp := True;
  FOnScroll.Execute(TScrollEventInfo.Create(True));
end;

procedure TMouseInput.ScrollDown;
begin
  FScrolledDown := True;                           
  FOnScroll.Execute(TScrollEventInfo.Create(False));
end;

procedure TMouseInput.Leave;
begin
  ReleaseAllButtons;
  FOnScreen := False;
  FOnScreenNotify := True;
  FOnLeaveScreen.Execute;
end;

procedure TMouseInput.Enter;
begin
  FOnScreen := True;
  FOnScreenNotify := True;
  FOnEnterScreen.Execute;
end;

function TMouseInput.GetOnButtonDown: TButtonEvent.TAccess;
begin
  Result := FOnButtonDown.Access;
end;

function TMouseInput.GetOnButtonUp: TButtonEvent.TAccess;
begin
  Result := FOnButtonUp.Access;
end;

function TMouseInput.GetOnEnterScreen: TEvent.TAccess;
begin
  Result := FOnEnterScreen.Access;
end;

function TMouseInput.GetOnLeaveScreen: TEvent.TAccess;
begin
  Result := FOnLeaveScreen.Access;
end;

function TMouseInput.GetOnMouseMove: TEvent.TAccess;
begin
  Result := FOnMouseMove.Access;
end;

function TMouseInput.GetOnScroll: TScrollEvent.TAccess;
begin
  Result := FOnScroll.Access;
end;

procedure TMouseInput.Resize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FResizeNotify := True;
end;

function TMouseInput.ButtonDown(AButton: TMouseButton): Boolean;
begin
  Result := FButtons[Ord(AButton)];
end;

function TMouseInput.ButtonPressed(AButton: TMouseButton): Boolean;
begin
  Result := FNotifyDown[Ord(AButton)];
end;

function TMouseInput.ButtonUp(AButton: TMouseButton): Boolean;
begin
  Result := not FButtons[Ord(AButton)];
end;

function TMouseInput.ButtonReleased(AButton: TMouseButton): Boolean;
begin
  Result := FNotifyUp[Ord(AButton)];
end;

function TMouseInput.AnyButtonDown: Boolean;
begin
  Result := FButtons.Ones > 0;
end;

function TMouseInput.MousePos: TVector2;
begin
  Result := FPos;
end;

function TMouseInput.MouseMoved: Boolean;
begin
  Result := FPosNotify;
end;

function TMouseInput.ScrolledUp: Boolean;
begin
  Result := FScrolledUp;
end;

function TMouseInput.ScrolledDown: Boolean;
begin
  Result := FScrolledDown;
end;

function TMouseInput.OnScreen: Boolean;
begin
  Result := FOnScreen;
end;

function TMouseInput.OnScreenChanged: Boolean;
begin
  Result := FOnScreenNotify;
end;

function TMouseInput.Resized: Boolean;
begin
  Result := FResizeNotify;
end;

function TMouseInput.Width: Integer;
begin
  Result := FWidth;
end;

function TMouseInput.Height: Integer;
begin
  Result := FHeight;
end;

{ TKeyboardInput }

constructor TKeyboardInput.Create;
begin
  FKeys := TBitField.Create($100);
  FNotifyUp := TBitField.Create($100);
  FNotifyDown := TBitField.Create($100);
  FNotifyTyped := TBitField.Create($100);
end;

destructor TKeyboardInput.Destroy;
begin
  FKeys.Free;
  FNotifyUp.Free;
  FNotifyDown.Free;
  FNotifyTyped.Free;
  inherited;
end;

function TKeyboardInput.GetOnKeyDown: TKeyEvent.TAccess;
begin
  Result := FOnKeyDown.Access;
end;

function TKeyboardInput.GetOnKeyUp: TKeyEvent.TAccess;
begin
  Result := FOnKeyUp.Access;
end;

function TKeyboardInput.GetOnType: TTypeEvent.TAccess;
begin
  Result := FOnType.Access;
end;

procedure TKeyboardInput.ResetNotifyUp;
begin
  FNotifyUp.Clear;
end;

procedure TKeyboardInput.ResetNotifyDown;
begin
  FNotifyDown.Clear;
end;

procedure TKeyboardInput.ResetNotifyTyped;
begin
  FNotifyTyped.Clear;
end;

procedure TKeyboardInput.ResetCharBuffer;
begin
  FTextBuffer := '';
end;

procedure TKeyboardInput.Reset;
begin
  ResetNotifyUp;
  ResetNotifyDown;
  ResetNotifyTyped;
  ResetCharBuffer;
end;

procedure TKeyboardInput.ReleaseAllKeys;
var
  Key: TKey;
begin
  for Key in FKeys do                                         
    FOnKeyUp.Execute(TKeyEventInfo.Create(Key));
  FKeys.Clear;
  FNotifyUp.Fill;
end;

procedure TKeyboardInput.PressKey(AKey: TKey);
begin
  FNotifyDown[AKey] := not FKeys[AKey];
  FKeys[AKey] := True;
  FNotifyTyped[AKey] := True;

  if (AKey = Ord('V')) and (AsyncKeyDown(VK_CONTROL)) and (Clipboard.AsText <> '') then
  begin
    FTextBuffer := FTextBuffer + Clipboard.AsText;
    FOnType.Execute(TTypeEventInfo.Create(Clipboard.AsText));
  end;

  FOnKeyDown.Execute(TKeyEventInfo.Create(AKey));
end;

procedure TKeyboardInput.ReleaseKey(AKey: TKey);
begin
  FNotifyUp[AKey] := FKeys[AKey];
  FKeys[AKey] := False;
  FOnKeyUp.Execute(TKeyEventInfo.Create(AKey));
end;

procedure TKeyboardInput.PressChar(AChar: Char);
begin
  FTextBuffer := FTextBuffer + AChar;
  FOnType.Execute(TTypeEventInfo.Create(AChar));
end;

function TKeyboardInput.KeyDown(AKey: TKey): Boolean;
begin
  Result := FKeys[AKey];
end;

function TKeyboardInput.AsyncKeyDown(AKey: TKey): Boolean;
begin
  Result := (GetAsyncKeyState(AKey) and (1 shl 15)) <> 0;
end;

function TKeyboardInput.KeyPressed(AKey: TKey): Boolean;
begin
  Result := FNotifyDown[AKey];
end;

function TKeyboardInput.KeyTyped(AKey: TKey): Boolean;
begin
  Result := FNotifyTyped[AKey];
end;

function TKeyboardInput.KeyUp(AKey: TKey): Boolean;
begin
  Result := not FKeys[AKey];
end;

function TKeyboardInput.AsyncKeyUp(AKey: TKey): Boolean;
begin
  Result := (GetAsyncKeyState(AKey) and (1 shl 15)) = 0;
end;

function TKeyboardInput.KeyReleased(AKey: TKey): Boolean;
begin
  Result := FNotifyUp[AKey];
end;

function TKeyboardInput.AnyKeyDown: Boolean;
begin
  Result := FKeys.Ones > 0;
end;

{ TKeyEventInfo }

constructor TKeyEventInfo.Create(AKey: TKey);
begin
  FKey := AKey;
end;

{ TTypeEventInfo }

constructor TTypeEventInfo.Create(AText: string);
begin
  FText := AText;
end;

{ TButtonEventInfo }

constructor TButtonEventInfo.Create(AButton: TMouseButton);
begin
  FButton := AButton;
end;

{ TScrollEventInfo }

constructor TScrollEventInfo.Create(AScrolledUp: Boolean);
begin
  FScrolledUp := AScrolledUp;
end;

end.
