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

  Pengine.BitField,
  Pengine.Vector;

type

  { TKeyboardInput }

  TKeyboardInput = class
  private
    FKeys, FNotifyDown, FNotifyUp, FNotifyTyped: TBitField;
    FCharBuffer: AnsiString;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetNotifyUp;
    procedure ResetNotifyDown;
    procedure ResetNotifyTyped;

    procedure ResetCharBuffer;

    procedure Reset;

    procedure ReleaseAllKeys;

    // WRITE
    procedure PressKey(AKey: Byte);
    procedure ReleaseKey(AKey: Byte);
    procedure PressChar(AChar: AnsiChar);

    // READ
    function KeyDown(AKey: Byte): Boolean;
    function AsyncKeyDown(AKey: Byte): Boolean;
    function KeyPressed(AKey: Byte): Boolean; // only once
    function KeyTyped(AKey: Byte): Boolean;   // with key repeat

    function KeyUp(AKey: Byte): Boolean;
    function AsyncKeyUp(AKey: Byte): Boolean;
    function KeyReleased(AKey: Byte): Boolean;

    property CharBuffer: AnsiString read FCharBuffer;

    function AnyKeyDown: Boolean;
  end;

  { TMouseInput }

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
  end;

  { TInputHandler }

  TInputHandler = class
  private
    FForm: TForm;

    FMouse: TMouseInput;
    FKeyboard: TKeyboardInput;

    FOldKeyDown, FOldKeyUp: TKeyEvent;
    FOldMouseDown, FOldMouseUp: TMouseEvent;
    FOldMouseMove: TMouseMoveEvent;
    FOldResize: TNotifyEvent;
    FOldMouseWheel: TMouseWheelEvent;

    function GetAnsiCharBuffer: AnsiString;
    function GetCharInBuffer: Boolean;

    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnResize(Sender: TObject);

  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;

    procedure PressChar(AChar: AnsiChar);

    // FMouse
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

    // FKeyboard
    function KeyDown(AKey: Byte): Boolean; overload; inline;
    function KeyPressed(AKey: Byte): Boolean; overload; inline;
    function KeyTyped(AKey: Byte): Boolean; overload; inline;
    function KeyUp(AKey: Byte): Boolean; overload; inline;
    function KeyReleased(AKey: Byte): Boolean; overload; inline;
    function AsyncKeyDown(AKey: Byte): Boolean; overload; inline;
    function AsyncKeyUp(AKey: Byte): Boolean; overload; inline;

    function KeyDown(AKey: Char): Boolean; overload; inline;
    function KeyPressed(AKey: Char): Boolean; overload; inline;
    function KeyTyped(AKey: Char): Boolean; overload; inline;
    function KeyUp(AKey: Char): Boolean; overload; inline;
    function KeyReleased(AKey: Char): Boolean; overload; inline;
    function AsyncKeyDown(AKey: Char): Boolean; overload; inline;
    function AsyncKeyUp(AKey: Char): Boolean; overload; inline;

    property AnsiCharBuffer: AnsiString read GetAnsiCharBuffer;
    property CharInBuffer: Boolean read GetCharInBuffer;

    function AnyKeyDown: Boolean; inline;

    // Both
    function AnyAction: Boolean; inline;

    procedure NotifyChanges;

    procedure ReleaseAll;
  end;

implementation

const
  MouseButtonCount = Ord(High(TMouseButton)) + 1;

{ TInputHandler }

procedure TInputHandler.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FKeyboard.PressKey(Key);
  if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

function TInputHandler.GetAnsiCharBuffer: AnsiString;
begin
  Result := FKeyboard.CharBuffer;
end;

function TInputHandler.GetCharInBuffer: Boolean;
begin
  Result := FKeyboard.CharBuffer <> '';
end;

procedure TInputHandler.OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FKeyboard.ReleaseKey(Key);
  if Assigned(FOldKeyUp) then
    FOldKeyUp(Sender, Key, Shift);
end;

procedure TInputHandler.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouse.PressButton(Button);
  if Assigned(FOldMouseDown) then
    FOldMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TInputHandler.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMouse.SetPosition(TVector2.Create(X, Y));
  if Assigned(FOldMouseMove) then
    FOldMouseMove(Sender, Shift, X, Y);
end;

procedure TInputHandler.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouse.ReleaseButton(Button);
  if Assigned(FOldMouseUp) then
    FOldMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TInputHandler.OnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  if WheelDelta > 0 then
    FMouse.ScrollUp
  else if WheelDelta < 0 then
    FMouse.ScrollDown;
  if Assigned(FOldMouseWheel) then
    FOldMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

procedure TInputHandler.OnResize(Sender: TObject);
begin
  with TForm(Sender) do
    FMouse.Resize(ClientWidth, ClientHeight);
  if Assigned(FOldResize) then
    FOldResize(Sender);
end;

constructor TInputHandler.Create(AForm: TForm);
begin
  FForm := AForm;
  FMouse := TMouseInput.Create(AForm);
  FKeyboard := TKeyboardInput.Create;

  FOldKeyDown := AForm.OnKeyDown;
  FOldKeyUp := AForm.OnKeyUp;
  FOldMouseDown := AForm.OnMouseDown;
  FOldMouseUp := AForm.OnMouseUp;
  FOldMouseMove := AForm.OnMouseMove;
  FOldMouseWheel := AForm.OnMouseWheel;
  FOldResize := AForm.OnResize;

  AForm.OnKeyDown := OnKeyDown;
  AForm.OnKeyUp := OnKeyUp;
  AForm.OnMouseDown := OnMouseDown;
  AForm.OnMouseUp := OnMouseUp;
  AForm.OnMouseMove := OnMouseMove;
  AForm.OnMouseWheel := OnMouseWheel;
  AForm.OnResize := OnResize;
end;

destructor TInputHandler.Destroy;
begin
  FMouse.Free;
  FKeyboard.Free;
  inherited Destroy;
end;

procedure TInputHandler.PressChar(AChar: AnsiChar);
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

function TInputHandler.KeyDown(AKey: Byte): Boolean;
begin
  Result := FKeyboard.KeyDown(AKey);
end;

function TInputHandler.KeyPressed(AKey: Byte): Boolean;
begin
  Result := FKeyboard.KeyPressed(AKey);
end;

function TInputHandler.KeyTyped(AKey: Byte): Boolean;
begin
  Result := FKeyboard.KeyTyped(AKey);
end;

function TInputHandler.KeyUp(AKey: Byte): Boolean;
begin
  Result := FKeyboard.KeyUp(AKey);
end;

function TInputHandler.KeyReleased(AKey: Byte): Boolean;
begin
  Result := FKeyboard.KeyReleased(AKey);
end;

function TInputHandler.AsyncKeyDown(AKey: Byte): Boolean;
begin
  Result := FKeyboard.AsyncKeyDown(AKey);
end;

function TInputHandler.AsyncKeyUp(AKey: Byte): Boolean;
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
begin
  FButtons := TBitField.Create(MouseButtonCount);
  FNotifyUp := TBitField.Create(MouseButtonCount);
  FNotifyDown := TBitField.Create(MouseButtonCount);
  FWidth := AForm.ClientWidth;
  FHeight := AForm.ClientHeight;
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
end;

procedure TMouseInput.ReleaseButton(AButton: TMouseButton);
begin
  FButtons[Ord(AButton)] := False;
  FNotifyUp[Ord(AButton)] := True;
end;

procedure TMouseInput.ReleaseAllButtons;
begin
  FButtons.Clear;
  FNotifyUp.Fill;
end;

procedure TMouseInput.SetPosition(APos: TVector2);
begin
  FPos.X := (APos.X * 2 - FWidth) / FHeight; // -aspect <-> +aspect
  FPos.Y := 1 - APos.Y / FHeight * 2;       // -1      <-> +1
  FPosNotify := True;
end;

procedure TMouseInput.ScrollUp;
begin
  FScrolledUp := True;
end;

procedure TMouseInput.ScrollDown;
begin
  FScrolledDown := True;
end;

procedure TMouseInput.Leave;
begin
  ReleaseAllButtons;
  FOnScreen := False;
  FOnScreenNotify := True;
end;

procedure TMouseInput.Enter;
begin
  FOnScreen := True;
  FOnScreenNotify := True;
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
  FCharBuffer := '';
end;

procedure TKeyboardInput.Reset;
begin
  ResetNotifyUp;
  ResetNotifyDown;
  ResetNotifyTyped;
  ResetCharBuffer;
end;

procedure TKeyboardInput.ReleaseAllKeys;
begin
  FKeys.Clear;
  FNotifyUp.Fill;
end;

procedure TKeyboardInput.PressKey(AKey: Byte);
var
  Buffer: WideString;
  Text, TextNoNull: AnsiString;
  C: AnsiChar;
  Len: Integer;
begin
  FNotifyDown[AKey] := not FKeys[AKey];
  FKeys[AKey] := True;
  FNotifyTyped[AKey] := True;

  if (AKey = Ord('V')) and (AsyncKeyDown(VK_CONTROL)) then
  begin
    Len := Length(Clipboard.AsText) * 2;
    if Len <> 0 then
    begin
      SetLength(Buffer, Len);
      SetLength(Text, Len);
      ZeroMemory(@Buffer[1], Len);
      ZeroMemory(@Text[1], Len);
      Clipboard.GetTextBuf(@Buffer[1], Len);
      //{$IFDEF FPC}
      //Utf8ToUnicode(@Text[1], @Buffer[1], Len);
      //{$ELSE}
      Utf8ToUnicode(@Text[1], Len, @Buffer[1], Len);
      //{$ENDIF}
      TextNoNull := '';
      for C in Text do
        if C <> #0 then
          TextNoNull := TextNoNull + C;
      FCharBuffer := FCharBuffer + TextNoNull;
    end;
  end;
end;

procedure TKeyboardInput.ReleaseKey(AKey: Byte);
begin
  FNotifyUp[AKey] := FKeys[AKey];
  FKeys[AKey] := False;
end;

procedure TKeyboardInput.PressChar(AChar: AnsiChar);
begin
  FCharBuffer := FCharBuffer + AChar;
end;

function TKeyboardInput.KeyDown(AKey: Byte): Boolean;
begin
  Result := FKeys[AKey];
end;

function TKeyboardInput.AsyncKeyDown(AKey: Byte): Boolean;
begin
  Result := (GetAsyncKeyState(AKey) and (1 shl 15)) <> 0;
end;

function TKeyboardInput.KeyPressed(AKey: Byte): Boolean;
begin
  Result := FNotifyDown[AKey];
end;

function TKeyboardInput.KeyTyped(AKey: Byte): Boolean;
begin
  Result := FNotifyTyped[AKey];
end;

function TKeyboardInput.KeyUp(AKey: Byte): Boolean;
begin
  Result := not FKeys[AKey];
end;

function TKeyboardInput.AsyncKeyUp(AKey: Byte): Boolean;
begin
  Result := (GetAsyncKeyState(AKey) and (1 shl 15)) = 0;
end;

function TKeyboardInput.KeyReleased(AKey: Byte): Boolean;
begin
  Result := FNotifyUp[AKey];
end;

function TKeyboardInput.AnyKeyDown: Boolean;
begin
  Result := FKeys.Ones > 0;
end;

end.
