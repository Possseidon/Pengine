unit GUI;

interface

uses
  Shaders, VAOManager, FontManager, Lists, GLEnums, VectorGeometry, Controls, SysUtils, Color,
  Windows, Math, TextureManager, dglOpenGL, OpenGLContext, MathUtils, Clipbrd
  {$IFDEF FPC}
  {$ELSE}
  , AnsiStrings
  , UITypes
  {$ENDIF}
  ;

type

  { EInvalidCursorTime }

  EInvalidCursorTime = class (Exception)
  public
    constructor Create(ATime: Single);
  end;

  TMouseButtons = set of TMouseButton;

  { TGLControl }

  TGLControl = class;
  TGLBasicContainerControl = class;
  TGUI = class;

  TGLControlClass = class of TGLControl;

  TGLControl = class abstract
  strict private
    FParentControl: TGLBasicContainerControl;

    FFont: TBMPFontItem;

    FMouseHovering: Boolean;
    FMouseEntered: Boolean;
    FMouseLeft: Boolean;

    FMouseDown: TMouseButtons;
    FMousePressed: TMouseButtons;
    FMouseReleased: TMouseButtons;

    FVisible: Boolean;

    FPos: TGVector2;
    FDepth: Single;

    FXOrigin: THAlignment;
    FYOrigin: TVAlignment;

    function GetMousePos: TGVector2;
    procedure SetDepth(AValue: Single);
    procedure SetFont(AValue: TBMPFontItem);
    procedure SetVisible(AValue: Boolean);

    function GetFont: TBMPFontItem;

  strict protected
    FGUIVAO: TAutoUpdateVAO;
    FFontVAO: TAutoUpdateVAO;
    FTexturePage: TTexturePage;
    FGLForm: TGLForm;
    FGUI: TGUI;

    function MouseInBounds: Boolean;

    procedure SetPos(AValue: TGVector2); virtual;
    procedure SetPosX(AValue: Single); virtual;
    procedure SetPosY(AValue: Single); virtual;

    procedure SetXOrigin(AValue: THAlignment); virtual;
    procedure SetYOrigin(AValue: TVAlignment); virtual;

    function BoundsFromSize(ASize: TGVector2): TGBounds2;

    type
      TData = record
        Pos: TGVector3;
        TexCoord: TTexCoord2;
        Color: TColorRGBA;
        Border: TGBounds2;
      end;

  public
    constructor Create(AParentControl: TGLBasicContainerControl); virtual;
    destructor Destroy; override;

    // deletes from ParentControl and frees
    procedure Delete;

    function GetGUIVAOSize: Cardinal; virtual;
    function GetFontVAOSize: Cardinal; virtual;

    procedure AddToGUIVAO; virtual;
    procedure AddToFontVAO; virtual;

    function ClientToGUI(APoint: TGVector2): TGVector2; overload;
    function ClientToGUI(ABounds: TGBounds2): TGBounds2; overload;

    function ClientToGUIX(APoint: Single): Single; overload;
    function ClientToGUIX(ABounds: TGBounds1): TGBounds1; overload;

    function ClientToGUIY(APoint: Single): Single; overload;
    function ClientToGUIY(ABounds: TGBounds1): TGBounds1; overload;

    function GUIToClient(APoint: TGVector2): TGVector2; overload;
    function GUIToClient(ABounds: TGBounds2): TGBounds2; overload;

    function GetBounds: TGBounds2; virtual; abstract;

    function GetRealBounds: TGBounds2;

    function GetRealPos: TGVector2;

    procedure Update; virtual;

    property MouseHovering: Boolean read FMouseHovering;
    property MouseEntered: Boolean read FMouseEntered;
    property MouseLeft: Boolean read FMouseLeft;

    property MouseDown: TMouseButtons read FMouseDown;
    property MousePressed: TMouseButtons read FMousePressed;
    property MouseReleased: TMouseButtons read FMouseReleased;
    property MousePos: TGVector2 read GetMousePos;

    property Pos: TGVector2 read FPos write SetPos;
    property PosX: Single read FPos.X write SetPosX;
    property PosY: Single read FPos.Y write SetPosY;

    property Visible: Boolean read FVisible write SetVisible;

    property XOrigin: THAlignment read FXOrigin write SetXOrigin;
    property YOrigin: TVAlignment read FYOrigin write SetYOrigin;

    property Depth: Single read FDepth write SetDepth;

    property ParentControl: TGLBasicContainerControl read FParentControl;

    procedure Focus;
    procedure UnFocus;
    function HasFocus: Boolean;

    property Font: TBMPFontItem read GetFont write SetFont;
    function UsingParentFont: Boolean;
    procedure UseParentFont;

    property TexturePage: TTexturePage read FTexturePage;

  end;

  { TGLBasicContainerControl }

  TGLBasicContainerControl = class abstract (TGLControl)
  strict private
    FSize: TGVector2;

    procedure SetSize(AValue: TGVector2);
    procedure SetWidth(AValue: Single);
    procedure SetHeight(AValue: Single);

  strict protected
    FSubControls: TObjectArray<TGLControl>;

    function GetAspect: Single;

    function GetInnerBounds: TGBounds2; virtual;
    function GetCutInnerBounds: TGBounds2;

  public
    constructor Create(AParentControl: TGLBasicContainerControl); override;
    destructor Destroy; override;

    property Size: TGVector2 read FSize write SetSize;
    property Width: Single read FSize.X write SetWidth;
    property Height: Single read FSize.Y write SetHeight;

    procedure Update; override;

    function GetFontVAOSize: Cardinal; override;
    function GetGUIVAOSize: Cardinal; override;

    procedure AddToGUIVAO; override;
    procedure AddToFontVAO; override;

    property Aspect: Single read GetAspect;

    procedure AddControl(AControl: TGLControl);
    procedure DelControl(AControl: TGLControl);

  end;

  { TGLContainerControl }

  TGLContainerControl = class (TGLBasicContainerControl)
  public
    function GetBounds: TGBounds2; override;

    function GetEnumerator: TObjectArray<TGLControl>.TIterator;
  end;

  { TGLLabel }

  TGLLabel = class (TGLControl)
  strict private
    FTextDisplay: TTextDisplay2D;
    FHeight: Single;

    function GetColor: TColorRGBA;
    function GetItalic: Boolean;
    function GetText: AnsiString;

    procedure SetColor(AValue: TColorRGBA);
    procedure SetHeight(AValue: Single);
    procedure SetItalic(AValue: Boolean);
    procedure SetText(AValue: AnsiString);

  public
    constructor Create(AParentControl: TGLBasicContainerControl); override;
    destructor Destroy; override;

    function GetFontVAOSize: Cardinal; override;
    procedure AddToFontVAO; override;

    function GetBounds: TGBounds2; override;

    property Text: AnsiString read GetText write SetText;
    property Height: Single read FHeight write SetHeight;
    property Italic: Boolean read GetItalic write SetItalic;
    property Color: TColorRGBA read GetColor write SetColor;

  end;

  { TGLLabelList }

  TGLLabelList = class (TGLControl)
  strict private
    FTextDisplays: TObjectArray<TTextDisplay2D>;

    FLineHeight: Single;
    FLineSpacing: Single;
    FItalic: Boolean;
    FColor: TColorRGBA;

    function GetLine(I: Cardinal): AnsiString;
    function GetLineCount: Cardinal;
    function GetTextDisplay(I: Cardinal): TTextDisplay2D;

    procedure SetColor(AValue: TColorRGBA);
    procedure SetItalic(AValue: Boolean);

    procedure SetLine(I: Cardinal; AValue: AnsiString);
    procedure SetLineHeight(AValue: Single);
    procedure SetLineSpacing(AValue: Single);

    property TextDisplays[I: Cardinal]: TTextDisplay2D read GetTextDisplay;

  public
    constructor Create(AParentControl: TGLBasicContainerControl); override;
    destructor Destroy; override;

    function GetFontVAOSize: Cardinal; override;
    procedure AddToFontVAO; override;

    function GetBounds: TGBounds2; override;

    procedure AddLine(AText: AnsiString);
    procedure DelLine(AIndex: Cardinal);
    procedure DelAllLines;

    property Lines[I: Cardinal]: AnsiString read GetLine write SetLine;
    property LineCount: Cardinal read GetLineCount;

    property LineHeight: Single read FLineHeight write SetLineHeight;
    property LineSpacing: Single read FLineSpacing write SetLineSpacing;
    property Italic: Boolean read FItalic write SetItalic;
    property Color: TColorRGBA read FColor write SetColor;

  end;

  TButtonState = (
    bsDefault,
    bsHover,
    bsPressed,
    bsDisabled
  );

  { TGLButton }

  TGLButton = class (TGLControl)
  strict private
    FTextDisplay: TTextDisplay2D;

    FButtonColor: TColorRGBA;

    FHeight: Single;
    FWidth: Cardinal;
    FTextScale: Single;
    FTexture: TTextureID;

    FWasPressed: Boolean;

    FState: TButtonState;

    function GetCaption: AnsiString;
    function GetEnabled: Boolean;
    function GetPressed: Boolean;
    function GetTextColor: TColorRGBA;
    function GetTexture: String;

    procedure SetCaption(AValue: AnsiString);
    procedure SetButtonColor(AValue: TColorRGBA);
    procedure SetEnabled(AValue: Boolean);
    procedure SetTextColor(AValue: TColorRGBA);
    procedure SetHeight(AValue: Single);
    procedure SetTextScale(AValue: Single);
    procedure SetTexture(AValue: String);
    procedure SetWidth(AValue: Cardinal);
    procedure SetState(AValue: TButtonState);

    property State: TButtonState read FState write SetState;

  public
    constructor Create(AParentControl: TGLBasicContainerControl); override;
    destructor Destroy; override;

    function GetFontVAOSize: Cardinal; override;
    function GetGUIVAOSize: Cardinal; override;

    procedure AddToFontVAO; override;
    procedure AddToGUIVAO; override;

    function GetBounds: TGBounds2; override;

    property Height: Single read FHeight write SetHeight;
    property Width: Cardinal read FWidth write SetWidth;

    property TextColor: TColorRGBA read GetTextColor write SetTextColor;
    property TextScale: Single read FTextScale write SetTextScale;
    property Caption: AnsiString read GetCaption write SetCaption;

    property Texture: String read GetTexture write SetTexture;

    property ButtonColor: TColorRGBA read FButtonColor write SetButtonColor;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    // button pressed AND released
    property Pressed: Boolean read GetPressed;

    procedure Update; override;

    const
      DefaultTextureName = 'button';

  end;

  { TGLTextfield }

  TGLTextfield = class (TGLControl)
  strict private
    FTextDisplay: TTextDisplay2D;

    FCursorPos: Integer; // can't use FTextDisplay's because that is just visually
    FOffsetSkippedChars: Integer;
    FText: AnsiString;
    FHeight: Single;
    FTextfieldColor: TColorRGBA;
    FWidth: Cardinal;
    FTextScale: Single;

    FTexture: TTextureID;

    FEnabled: Boolean;

    FCursorOnTime: Single;  // time in seconds, the cursor is visible
    FCursorOffTime: Single; // time in seconds, the cursor is invisible
    FCursorTime: Single;    // current time between 0 and on+off

    FOffset: Single;

    FTextChanged: Boolean;

    function GetTextColor: TColorRGBA;
    function GetTexture: String;
    function GetVisibleArea: TGBounds1;
    procedure SetCursorOffTime(AValue: Single);
    procedure SetCursorOnTime(AValue: Single);
    procedure SetCursorPos(AValue: Integer);

    function GetCursorVisible: Boolean;
    procedure SetCursorVisible(const AValue: Boolean);

    procedure SetEnabled(AValue: Boolean);
    procedure SetHeight(AValue: Single);
    procedure SetText(AValue: AnsiString);
    procedure SetTextColor(AValue: TColorRGBA);
    procedure SetTextfieldColor(AValue: TColorRGBA);
    procedure SetTextScale(AValue: Single);
    procedure SetTexture(AValue: String);
    procedure SetWidth(AValue: Cardinal);

    property CursorVisible: Boolean read GetCursorVisible write SetCursorVisible;

    procedure UpdateCursorPosOffset;

  public
    constructor Create(AParentControl: TGLBasicContainerControl); override;
    destructor Destroy; override;

    function GetFontVAOSize: Cardinal; override;
    function GetGUIVAOSize: Cardinal; override;

    procedure AddToFontVAO; override;
    procedure AddToGUIVAO; override;

    function GetBounds: TGBounds2; override;

    property Height: Single read FHeight write SetHeight;
    property Width: Cardinal read FWidth write SetWidth;

    property TextColor: TColorRGBA read GetTextColor write SetTextColor;
    property TextScale: Single read FTextScale write SetTextScale;
    property Text: AnsiString read FText write SetText;

    property CursorPos: Integer read FCursorPos write SetCursorPos;
    property Offset: Single read FOffset;
    property VisibleArea: TGBounds1 read GetVisibleArea;

    property CursorOnTime: Single read FCursorOnTime write SetCursorOnTime;
    property CursorOffTime: Single read FCursorOffTime write SetCursorOffTime;

    property Texture: String read GetTexture write SetTexture;

    property TextfieldColor: TColorRGBA read FTextfieldColor write SetTextfieldColor;

    property Enabled: Boolean read FEnabled write SetEnabled;

    procedure Update; override;

    property TextChanged: Boolean read FTextChanged;

    const
      DefaultTextureName = 'textfield';
  end;

  TCheckboxState = (cbUnchecked, cbChecked, cbGrayed);

  { TGLCheckbox }

  TGLCheckbox = class (TGLControl)
  private
    FHeight: Single;

    FState: TCheckboxState;
    FEnabled: Boolean;
    FGrayable: Boolean;

    FBoxColor: TColorRGBA;
    FCheckColor: TColorRGBA;

    FTexture: TTextureID;

    FChanged: Boolean;

    function GetChecked: Boolean;
    function GetGrayed: Boolean;
    function GetTexture: String;
    function GetUnchecked: Boolean;
    procedure SetBoxColor(AValue: TColorRGBA);
    procedure SetCheckColor(AValue: TColorRGBA);

    procedure SetEnabled(AValue: Boolean);
    procedure SetHeight(AValue: Single);
    procedure SetState(AValue: TCheckboxState);
    procedure SetTexture(AValue: String);

  public
    constructor Create(AParentControl: TGLBasicContainerControl); override;

    function GetGUIVAOSize: Cardinal; override;
    procedure AddToGUIVAO; override;

    function GetBounds: TGBounds2; override;

    property Height: Single read FHeight write SetHeight;

    property Texture: String read GetTexture write SetTexture;

    property State: TCheckboxState read FState write SetState;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Grayable: Boolean read FGrayable write FGrayable;

    property Unchecked: Boolean read GetUnchecked;
    property Checked: Boolean read GetChecked;
    property Grayed: Boolean read GetGrayed;

    property BoxColor: TColorRGBA read FBoxColor write SetBoxColor;
    property CheckColor: TColorRGBA read FCheckColor write SetCheckColor;

    property Changed: Boolean read FChanged;

    procedure Update; override;

    const
      DefaultTextureName = 'checkbox';

  end;

  { TGLBasicTrackbar }

  TGLBasicTrackbar = class abstract (TGLControl)
  private
    FHeight: Single;
    FWidth: Cardinal;

    FEnabled: Boolean;

    FColor: TColorRGBA;

    FTexture: TTextureID;

    FState: TButtonState;

    FMouseDownOffset: Single;

    function GetTexture: String;
    procedure SetColor(AValue: TColorRGBA);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHeight(AValue: Single);
    procedure SetTexture(AValue: String);
    procedure SetWidth(AValue: Cardinal);
    procedure SetState(AValue: TButtonState);

    property State: TButtonState read FState write SetState;

    function BoundsOf(APosition: Single): TGBounds2;

  protected
    FTrackPosChanged: Boolean;

    function GetVisiblePosition: Single; virtual; abstract;
    function GetVisibleTicks: Cardinal; virtual; abstract;
    procedure SetVisiblePosition(APosition: Single); virtual; abstract;

  public
    constructor Create(AParentControl: TGLBasicContainerControl); override;

    function GetGUIVAOSize: Cardinal; override;
    procedure AddToGUIVAO; override;

    function GetBounds: TGBounds2; override;

    property Height: Single read FHeight write SetHeight;
    property Width: Cardinal read FWidth write SetWidth;

    property Texture: String read GetTexture write SetTexture;

    property Enabled: Boolean read FEnabled write SetEnabled;

    property Color: TColorRGBA read FColor write SetColor;

    procedure Update; override;

    property TrackPosChanged: Boolean read FTrackPosChanged;

    const
      DefaultTextureName = 'trackbar';
  end;

  { TGLTrackbarFixed }

  TGLTrackbarFixed = class (TGLBasicTrackbar)
  private
    FRange: TIntBounds1;
    FTrackPos: Integer;

    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
    procedure SetTrackPos(AValue: Integer);
    procedure SetRange(AValue: TIntBounds1);

  protected
    function GetVisiblePosition: Single; override;
    function GetVisibleTicks: Cardinal; override;
    procedure SetVisiblePosition(APosition: Single); override;

  public
    property Range: TIntBounds1 read FRange write SetRange;
    property Min: Integer read FRange.C1 write SetMin;
    property Max: Integer read FRange.C2 write SetMax;
    property TrackPos: Integer read FTrackPos write SetTrackPos;

  end;

  { TGLTrackbarSmooth }

  TGLTrackbarSmooth = class (TGLBasicTrackbar)
  private
    FRange: TGBounds1;
    FTrackPos: Single;
    FTicks: Cardinal;

    procedure SetMax(AValue: Single);
    procedure SetMin(AValue: Single);
    procedure SetTicks(AValue: Cardinal);
    procedure SetTrackPos(AValue: Single);
    procedure SetRange(AValue: TGBounds1);

  protected
    function GetVisiblePosition: Single; override;
    function GetVisibleTicks: Cardinal; override;
    procedure SetVisiblePosition(APosition: Single); override;

  public
    property Range: TGBounds1 read FRange write SetRange;
    property Min: Single read FRange.C1 write SetMin;
    property Max: Single read FRange.C2 write SetMax;
    property TrackPos: Single read FTrackPos write SetTrackPos;

    property Ticks: Cardinal read FTicks write SetTicks;

  end;

  { TGLProgressbar }

  TGLProgressbar = class (TGLControl)

  end;

  { TGUI }

  TGUI = class (TGLContainerControl)
  strict private
    type

      { TGUIVAO }

      TGUIVAO = class (TAutoUpdateVAO)
      private
        FGUI: TGUI;

      protected
        procedure BeforeRender; override;
        procedure BuildVAO; override;

      public
        constructor Create(AShader: TShader; AGUI: TGUI);

      end;

      { TFontVAO }

      TFontVAO = class (TAutoUpdateVAO)
      private
        FGUI: TGUI;

      protected
        procedure BeforeRender; override;
        procedure BuildVAO; override;

      public
        constructor Create(AShader: TShader; AGUI: TGUI);

      end;

    const
      TextureUniformName = 'tex';

  strict private
    FFocusedControl: TGLControl;
    FFonts: TBMPFontList;

    procedure SetAspect(AValue: Single);
    procedure SetFocusedControl(AValue: TGLControl);

  public
    constructor Create(AShader: TShader; AGLForm: TGLForm; AFont: TBMPFontItem; AAspect: Single); reintroduce;
    destructor Destroy; override;

    procedure AddToFontVAO; override;
    procedure AddToGUIVAO; override;

    function GetFontVAOSize: Cardinal; override;
    function GetGUIVAOSize: Cardinal; override;

    procedure AddTextureFromFile(const AFileName: String); overload;
    procedure AddTextureFromFile(const AFileName, AName: String); overload;
    procedure AddTextureFromResource(const AResourceName: String); overload;
    procedure AddTextureFromResource(const AResourceName, AName: String); overload;

    procedure AddFont(AFont: TBMPFontItem);
    procedure DelFont(AFont: TBMPFontItem);
    function FontExists(AFont: TBMPFontItem): Boolean;

    property FocusedControl: TGLControl read FFocusedControl write SetFocusedControl;

    property Aspect: Single read GetAspect write SetAspect;

    procedure Render;
  end;

implementation

{ TGLTrackbarSmooth }

procedure TGLTrackbarSmooth.SetMax(AValue: Single);
begin
  if FRange.High = AValue then
    Exit;
  FRange.High := AValue;
  FTrackPos := Math.Min(FRange.High, FTrackPos);
  FGUIVAO.NotifyChanges;
end;

procedure TGLTrackbarSmooth.SetMin(AValue: Single);
begin
  if FRange.Low = AValue then
    Exit;
  FRange.Low := AValue;
  FTrackPos := Math.Max(FRange.Low, FTrackPos);
  FGUIVAO.NotifyChanges;
end;

procedure TGLTrackbarSmooth.SetTicks(AValue: Cardinal);
begin
  if FTicks = AValue then
    Exit;
  FTicks := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLTrackbarSmooth.SetTrackPos(AValue: Single);
begin
  AValue := Range.EnsureRange(AValue);
  if FTrackPos = AValue then
    Exit;
  FTrackPos := AValue;
  FTrackPosChanged := True;
  FGUIVAO.NotifyChanges;
end;

procedure TGLTrackbarSmooth.SetRange(AValue: TGBounds1);
begin
  if FRange = AValue then
    Exit;
  FRange := AValue;
  FTrackPos := FRange.EnsureRange(FTrackPos);
  FGUIVAO.NotifyChanges;
end;

function TGLTrackbarSmooth.GetVisiblePosition: Single;
begin
  Result := (TrackPos - Range.Low) / Range.Length;
end;

function TGLTrackbarSmooth.GetVisibleTicks: Cardinal;
begin
  Result := FTicks;
end;

procedure TGLTrackbarSmooth.SetVisiblePosition(APosition: Single);
begin
  TrackPos := FRange[APosition];
end;

{ TGLTrackbarFixed }

procedure TGLTrackbarFixed.SetMax(AValue: Integer);
begin
  if FRange.High = AValue then
    Exit;
  FRange.High := AValue;
  FTrackPos := Math.Min(Range.High, FTrackPos);
  FGUIVAO.NotifyChanges;
end;

procedure TGLTrackbarFixed.SetMin(AValue: Integer);
begin
  if FRange.Low = AValue then
    Exit;
  FRange.Low := AValue;
  FTrackPos := Math.Max(Range.Low, FTrackPos);
  FGUIVAO.NotifyChanges;
end;

procedure TGLTrackbarFixed.SetTrackPos(AValue: Integer);
begin
  AValue := Range.EnsureRange(AValue);
  if FTrackPos = AValue then
    Exit;
  FTrackPos := AValue;
  FTrackPosChanged := True;
  FGUIVAO.NotifyChanges;
end;

procedure TGLTrackbarFixed.SetRange(AValue: TIntBounds1);
begin
  if FRange = AValue then
    Exit;
  FRange := AValue;
  FTrackPos := Range.EnsureRange(FTrackPos);
  FGUIVAO.NotifyChanges;
end;

function TGLTrackbarFixed.GetVisiblePosition: Single;
begin
  Result := (TrackPos - Range.Low) / Range.Length;
end;

function TGLTrackbarFixed.GetVisibleTicks: Cardinal;
begin
  Result := Range.Length + 1;
end;

procedure TGLTrackbarFixed.SetVisiblePosition(APosition: Single);
begin
  TrackPos := Range[APosition];
end;

{ TGLBasicTrackbar }

procedure TGLBasicTrackbar.SetState(AValue: TButtonState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  FGUIVAO.NotifyChanges;
end;

function TGLBasicTrackbar.BoundsOf(APosition: Single): TGBounds2;
var
  B: TGBounds2;
begin
  B := GetRealBounds;
  Result.Vertical := B.Vertical;
  Result.C1.X := B.Horizontal[APosition * (Width - 0.5) / Width];
  Result.C2.X := Result.C1.X + Height / Width;
end;

function TGLBasicTrackbar.GetTexture: String;
begin
  Result := FTexturePage.TextureNames[FTexture];
end;

procedure TGLBasicTrackbar.SetColor(AValue: TColorRGBA);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLBasicTrackbar.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  if AValue then
    State := bsDefault
  else
    State := bsDisabled;
  FGUIVAO.NotifyChanges;
end;

procedure TGLBasicTrackbar.SetHeight(AValue: Single);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLBasicTrackbar.SetTexture(AValue: String);
begin
  FTexture := FTexturePage.TextureIDs[AValue];
  FGUIVAO.NotifyChanges;
end;

procedure TGLBasicTrackbar.SetWidth(AValue: Cardinal);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  FGUIVAO.NotifyChanges;
end;

constructor TGLBasicTrackbar.Create(AParentControl: TGLBasicContainerControl);
begin
  inherited Create(AParentControl);
  Width := 1;
  Height := 1;
  Texture := DefaultTextureName;
  Enabled := True;
  Color := ColorWhite;
end;

function TGLBasicTrackbar.GetGUIVAOSize: Cardinal;
begin
  // ticks + (width + 1) + trackcursor
  Result := (GetVisibleTicks + Width + 2) * 6;
end;

procedure TGLBasicTrackbar.AddToGUIVAO;
var
  B, TexB: TGBounds2;
  Data: TData;
  X: Integer;
  I: TQuadSide;
  T: Cardinal;
begin
  // BackgroundLine
  B := GetRealBounds;
  Data.Color := Color;
  Data.Pos.Z := Depth;
  for X := 0 to Width do
  begin
    TexB.Vertical := TGBounds1.Create((1 + Ord(FEnabled)) / 4, (2 + Ord(FEnabled)) / 4);
    if X = 0 then
      TexB.Horizontal := TGBounds1.Create(0, 0.25)
    else if X = Width then
      TexB.Horizontal := TGBounds1.Create(0.75, 1)
    else
      TexB.Horizontal := TGBounds1.Create(0.25, 0.75);

    TexB := TexturePage.GetTexBounds(FTexture, TexB);
    Data.Border := FTexturePage.HalfPixelInset(TexB);
    for I := Low(TQuadSide) to High(TQuadSide) do
    begin
      if X = 0 then
        Data.Pos.X := B.C1.X + (B.Width / Width) * QuadTexCoords[I].X * 0.5
      else if X = Width then
        Data.Pos.X := B.C1.X + (B.Width / Width) * (QuadTexCoords[I].X * 0.5 + X - 0.5)
      else
        Data.Pos.X := B.C1.X + (B.Width / Width) * (QuadTexCoords[I].X + X - 0.5);

      Data.Pos.Y := B.C1.Y + QuadTexCoords[I].Y * B.Height;

      Data.TexCoord := TexB[QuadTexCoords[I]];

      FGUIVAO.AddVertex(Data);
    end;
  end;

  // Ticks
  T := GetVisibleTicks;
  for X := 0 to T - 1 do
  begin
    B := BoundsOf(X / (T - 1));

    if (X = 0) or (X = T - 1) then
      TexB.Horizontal := TGBounds1.Create(0.5 - Ord(Enabled) / 2, 0.75 - Ord(Enabled) / 2)
    else
      TexB.Horizontal := TGBounds1.Create(0.75 - Ord(Enabled) / 2, 1 - Ord(Enabled) / 2);
    TexB.Vertical := TGBounds1.Create(0, 0.25);

    TexB := FTexturePage.GetTexBounds(FTexture, TexB);
    Data.Border := TexturePage.HalfPixelInset(TexB);

    for I := Low(TQuadSide) to High(TQuadSide) do
    begin
      Data.TexCoord := TexB[QuadTexCoords[I]];
      Data.Pos := B[QuadTexCoords[I]];

      FGUIVAO.AddVertex(Data);
    end;
  end;

  // TrackCursor
  B := BoundsOf(GetVisiblePosition);
  TexB.Horizontal := TGBounds1.Create(Ord(State) / 4, 0.25 + Ord(State) / 4);
  TexB.Vertical := TGBounds1.Create(0.75, 1);
  TexB := FTexturePage.GetTexBounds(FTexture, TexB);
  Data.Border := FTexturePage.HalfPixelInset(TexB);
  for I := Low(TQuadSide) to High(TQuadSide) do
  begin
    Data.Pos := B[QuadTexCoords[I]];

    Data.TexCoord := TexB[QuadTexCoords[I]];

    FGUIVAO.AddVertex(Data);
  end;
end;

function TGLBasicTrackbar.GetBounds: TGBounds2;
begin
  Result := BoundsFromSize(TGVector2.Create(Width * Height, Height));
end;

procedure TGLBasicTrackbar.Update;

  function GetPos: Single;
  begin
    Result := MousePos.X / (Width - 0.5) * Height + 0.5;
  end;

begin
  inherited Update;
  FTrackPosChanged := False;
  if not Enabled then
    Exit;

  if State <> bsPressed then
    if FGLForm.Input.MousePos in BoundsOf(GetVisiblePosition) then
    begin
      State := bsHover;
    end
    else
    begin
      State := bsDefault;
    end;

  if (State = bsHover) and (mbLeft in MousePressed) then
  begin
    State := bsPressed;
    FMouseDownOffset := GetPos - GetVisiblePosition;
  end;

  if State = bsPressed then
  begin
    SetVisiblePosition(GetPos - FMouseDownOffset);
  end;

  if FGLForm.Input.ButtonReleased(mbLeft) then
    State := bsDefault;

end;

{ EInvalidCursorTime }

constructor EInvalidCursorTime.Create(ATime: Single);
begin
  CreateFmt('CursorTime-Sum is %f and must be greater than zero!', [ATime]);
end;

{ TGLCheckbox }

procedure TGLCheckbox.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLCheckbox.SetHeight(AValue: Single);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLCheckbox.SetBoxColor(AValue: TColorRGBA);
begin
  if FBoxColor = AValue then
    Exit;
  FBoxColor := AValue;
  FGUIVAO.NotifyChanges;
end;

function TGLCheckbox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

function TGLCheckbox.GetGrayed: Boolean;
begin
  Result := State = cbGrayed;
end;

function TGLCheckbox.GetTexture: String;
begin
  Result := FTexturePage.TextureNames[FTexture];
end;

function TGLCheckbox.GetUnchecked: Boolean;
begin
  Result := State = cbUnchecked;
end;

procedure TGLCheckbox.SetCheckColor(AValue: TColorRGBA);
begin
  if FCheckColor = AValue then
    Exit;
  FCheckColor := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLCheckbox.SetState(AValue: TCheckboxState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
end;

procedure TGLCheckbox.SetTexture(AValue: String);
begin
  FTexture := FTexturePage.TextureIDs[AValue];
  FGUIVAO.NotifyChanges;
end;

constructor TGLCheckbox.Create(AParentControl: TGLBasicContainerControl);
begin
  inherited Create(AParentControl);
  BoxColor := ColorWhite;
  CheckColor := ColorGray;
  Enabled := True;
  Texture := DefaultTextureName;
end;

function TGLCheckbox.GetGUIVAOSize: Cardinal;
begin
  if State = cbUnchecked then
    Result := 6
  else
    Result := 12;
end;

procedure TGLCheckbox.AddToGUIVAO;
var
  S: TQuadSide;
  Data: TData;
  B, TexB: TGBounds2;
begin
  B := GetRealBounds;
  // Background
  Data.Color := BoxColor;
  TexB.Horizontal := TGBounds1.Create(0, 0.5);
  TexB.Vertical := TGBounds1.Create(Ord(FEnabled) / 2, 0.5 + Ord(FEnabled) / 2);
  TexB := FTexturePage.GetTexBounds(FTexture, TexB);
  Data.Border := FTexturePage.HalfPixelInset(TexB);
  for S := Low(TQuadSide) to High(TQuadSide) do
  begin
    Data.Pos := B[QuadTexCoords[S]];
    Data.Pos.Z := Depth;
    Data.TexCoord := TexB[QuadTexCoords[S]];
    FGUIVAO.AddVertex(Data);
  end;
  // Checked/Grayed
  if not Unchecked then
  begin
    TexB.Horizontal := TGBounds1.Create(0.5, 1);
    TexB.Vertical := TGBounds1.Create(1 - Ord(FState) / 2, 1.5 - Ord(FState) / 2);
    TexB := FTexturePage.GetTexBounds(FTexture, TexB);
    Data.Border := FTexturePage.HalfPixelInset(TexB);
    Data.Color := CheckColor;
    for S := Low(TQuadSide) to High(TQuadSide) do
    begin
      Data.Pos := B[QuadTexCoords[S]];
      Data.Pos.Z := Depth;
      Data.TexCoord := TexB[QuadTexCoords[S]];
      FGUIVAO.AddVertex(Data);
    end;
  end;
end;

function TGLCheckbox.GetBounds: TGBounds2;
begin
  Result := BoundsFromSize(TGVector2.Create(Height, Height));
end;

procedure TGLCheckbox.Update;
begin
  inherited Update;
  FChanged := False;
  if not Enabled then
    Exit;
  if mbLeft in MousePressed then
  begin
    State := TCheckboxState((Ord(State) + 1) mod (Ord(High(TCheckboxState)) + 1));
    if Grayed and not Grayable then
      State := cbUnchecked;
    FChanged := True;
  end;
end;

{ TGLTextfield }

function TGLTextfield.GetCursorVisible: Boolean;
begin
  Result := FTextDisplay.CursorVisible;
end;

function TGLTextfield.GetTextColor: TColorRGBA;
begin
  Result := FTextDisplay.Color;
end;

function TGLTextfield.GetTexture: String;
begin
  Result := FTexturePage.TextureNames[FTexture];
end;

function TGLTextfield.GetVisibleArea: TGBounds1;
begin
  Result.Low := FOffset;
  Result.High := Result.Low + (Width - 2) / TextScale + 2;
end;

procedure TGLTextfield.SetCursorOffTime(AValue: Single);
var
  S: Single;
begin
  S := AValue + FCursorOnTime;
  if S <= 0 then
    raise EInvalidCursorTime.Create(S);
  FCursorOffTime := AValue;
end;

procedure TGLTextfield.SetCursorOnTime(AValue: Single);
var
  S: Single;
begin
  S := AValue + FCursorOffTime;
  if S <= 0 then
    raise EInvalidCursorTime.Create(S);
  FCursorOnTime := AValue;
end;

procedure TGLTextfield.SetCursorPos(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, Length(Text));
  if FCursorPos = AValue then
    Exit;
  FCursorPos := AValue;
  FCursorTime := 0;

  UpdateCursorPosOffset;

  FFontVAO.NotifyChanges;
end;

procedure TGLTextfield.SetCursorVisible(const AValue: Boolean);
begin
  if FTextDisplay.CursorVisible = AValue then
    Exit;
  FTextDisplay.CursorVisible := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLTextfield.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  if not FEnabled and HasFocus then
    UnFocus;
  FGUIVAO.NotifyChanges;
end;

procedure TGLTextfield.SetHeight(AValue: Single);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLTextfield.SetText(AValue: AnsiString);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  FCursorTime := 0;
  FTextChanged := True;
  UpdateCursorPosOffset;

  FFontVAO.NotifyChanges;
end;

procedure TGLTextfield.SetTextColor(AValue: TColorRGBA);
begin
  if FTextDisplay.Color = AValue then
    Exit;
  FTextDisplay.Color := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLTextfield.SetTextfieldColor(AValue: TColorRGBA);
begin
  if FTextfieldColor = AValue then
    Exit;
  FTextfieldColor := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLTextfield.SetTextScale(AValue: Single);
begin
  if FTextScale = AValue then
    Exit;
  FTextScale := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLTextfield.SetTexture(AValue: String);
begin
  FTexture := FTexturePage.TextureIDs[AValue];
  FGUIVAO.NotifyChanges;
end;

procedure TGLTextfield.SetWidth(AValue: Cardinal);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;

  UpdateCursorPosOffset;

  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLTextfield.UpdateCursorPosOffset;
var
  C: Integer;
  Longer: Boolean;
begin
  if Text = '' then
  begin
    FTextDisplay.Text := '';
    FOffset := 0;
    FTextDisplay.CursorPos := 0;
    FOffsetSkippedChars := 0;
    Exit;
  end;
  FTextDisplay.Text := FText;
  FTextDisplay.CursorPos := CursorPos;

  if FTextDisplay.GetCursorOffset > VisibleArea then
  begin
    FOffset := FTextDisplay.GetCursorOffset - VisibleArea.Length;
    FOffsetSkippedChars := FTextDisplay.IndexFromOffset(FOffset, haLeft);

    FTextDisplay.Text := FText;
    C := FTextDisplay.IndexFromOffset(VisibleArea.High, haLeft) - FOffsetSkippedChars;
    FTextDisplay.Text := Copy(FText, FOffsetSkippedChars + 1, C);

    while FTextDisplay.Width > VisibleArea.Length do
    begin
      Inc(FOffsetSkippedChars);
      C := FTextDisplay.IndexFromOffset(VisibleArea.High, haLeft) - FOffsetSkippedChars;
      FTextDisplay.Text := Copy(FText, FOffsetSkippedChars + 1, C);
    end;

  end
  else if FTextDisplay.GetCursorOffset < VisibleArea then
  begin
    FOffset := FTextDisplay.GetCursorOffset;
    FOffsetSkippedChars := FTextDisplay.IndexFromOffset(FOffset, haLeft);
  end;

  FTextDisplay.Text := FText;
  C := FTextDisplay.IndexFromOffset(VisibleArea.High, haLeft) - FOffsetSkippedChars;
  FTextDisplay.Text := Copy(FText, FOffsetSkippedChars + 1, C);

  Longer := False;
  while (C < Length(FText)) and (FTextDisplay.Width <= VisibleArea.Length) do
  begin
    Longer := True;
    Inc(C);
    FTextDisplay.Text := Copy(FText, FOffsetSkippedChars + 1, C);
  end;
  if Longer then
  begin
    Dec(C);
    FTextDisplay.Text := Copy(FText, FOffsetSkippedChars + 1, C);
  end;
  FTextDisplay.CursorPos := CursorPos - FOffsetSkippedChars;

  FFontVAO.NotifyChanges;
end;

constructor TGLTextfield.Create(AParentControl: TGLBasicContainerControl);
begin
  inherited Create(AParentControl);
  FTextDisplay := TTextDisplay2D.Create(Font);
  FTextDisplay.Color := ColorGray;
  FTextDisplay.XOrigin := haLeft;
  FTextDisplay.YOrigin := vaCenter;
  TextScale := 0.75;
  Texture := DefaultTextureName;
  Width := 1;
  TextfieldColor := ColorWhite;
  Enabled := True;
  CursorOnTime := 0.5;
  CursorOffTime := 0.5;
end;

destructor TGLTextfield.Destroy;
begin
  FTextDisplay.Free;
  inherited Destroy;
end;

function TGLTextfield.GetFontVAOSize: Cardinal;
begin
  Result := inherited GetFontVAOSize + FTextDisplay.GetVAOSize;
end;

function TGLTextfield.GetGUIVAOSize: Cardinal;
begin
  Result := inherited GetGUIVAOSize + (Width + 1) * 6;
end;

procedure TGLTextfield.AddToFontVAO;
begin
  inherited AddToFontVAO;
  FTextDisplay.Font := Font;
  FTextDisplay.Height := GetRealBounds.Height * TextScale;
  FTextDisplay.Pos := ClientToGUI(GetBounds.LeftMid + UVecX * Height * (1 - TextScale));
  // for Offset testing
  //FTextDisplay.Pos := ClientToGUI(GetBounds.LeftMid + UVecX * Height * ((1 - TextScale) - Offset));
  FTextDisplay.Depth := Depth;
  FTextDisplay.AddToVAO(FFontVAO);
end;

procedure TGLTextfield.AddToGUIVAO;
var
  Data: TData;
  I: TQuadSide;
  X: Cardinal;
  B, TexB: TGBounds2;
begin
  inherited AddToGUIVAO;
  B := GetRealBounds;
  Data.Color := TextfieldColor;
  Data.Pos.Z := Depth;
  for X := 0 to Width do
  begin
    TexB.Vertical := TGBounds1.Create(Ord(FEnabled) / 2, (1 + Ord(FEnabled)) / 2);
    if X = 0 then
      TexB.Horizontal := TGBounds1.Create(0, 0.25)
    else if X = Width then
      TexB.Horizontal := TGBounds1.Create(0.75, 1)
    else
      TexB.Horizontal := TGBounds1.Create(0.25, 0.75);

    TexB := TexturePage.GetTexBounds(FTexture, TexB);
    Data.Border := FTexturePage.HalfPixelInset(TexB);
    for I := Low(TQuadSide) to High(TQuadSide) do
    begin
      if X = 0 then
        Data.Pos.X := B.C1.X + (B.Width / Width) * QuadTexCoords[I].X * 0.5
      else if X = Width then
        Data.Pos.X := B.C1.X + (B.Width / Width) * (QuadTexCoords[I].X * 0.5 + X - 0.5)
      else
        Data.Pos.X := B.C1.X + (B.Width / Width) * (QuadTexCoords[I].X + X - 0.5);

      Data.Pos.Y := B.C1.Y + QuadTexCoords[I].Y * B.Height;

      Data.TexCoord := TexB[QuadTexCoords[I]];

      FGUIVAO.AddVertex(Data);
    end;
  end;
end;

function TGLTextfield.GetBounds: TGBounds2;
begin
  Result := BoundsFromSize(TGVector2.Create(Width * Height, Height));
end;

procedure TGLTextfield.Update;

  const
    WordChars = ['a'..'z', 'A'..'Z', '0'..'9', '_', #$C0..#$D6, #$D8..#$F6, #$F8..#$FF];

  function StrLFind(AString: AnsiString; AStart: Integer): Integer;
  var
    I: Integer;
    FoundChar: Boolean;
  begin
    Result := 0;
    FoundChar := False;
    for I := AStart - 1 downto 0 do
    begin
      if (FoundChar) and not (AString[I + 1] in WordChars) then
        Exit(I + 1)
      else if AString[I + 1] in WordChars then
        FoundChar := True;
    end;
  end;

  function StrRFind(AString: AnsiString; AStart: Integer): Integer;
  var
    I: Integer;
    FoundSpace: Boolean;
  begin
    Result := Length(AString);
    FoundSpace := False;
    for I := AStart to Length(AString) - 1 do
    begin
      if FoundSpace and (AString[I + 1] in WordChars) then
        Exit(I - 1)
      else if not (AString[I + 1] in WordChars) then
        FoundSpace := True;
    end;
  end;

  function CountSpaces(AString: AnsiString; AStart: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := AStart to Length(AString) - 1 do
      if not (AString[I + 1] in WordChars) then
        Inc(Result)
      else
        Break;
  end;

var
  C: AnsiChar;
  S: AnsiString;
  L: Single;
begin
  inherited Update;
  FTextChanged := False;

  if not Enabled then
    Exit;

  if mbLeft in MouseDown then
  begin
    L := FTextDisplay.GetHBounds.Low;
    CursorPos := FTextDisplay.IndexFromOffset(
                   (FGLForm.Input.MousePos.X - L) / FTextDisplay.Height, haCenter) + FOffsetSkippedChars;
  end;

  if HasFocus then
  begin

    FCursorTime := FMod(FCursorTime + FGLForm.DeltaTime, 0, FCursorOnTime + FCursorOffTime);

    if FGLForm.Input.KeyTyped(VK_LEFT) then
    begin
      if FGLForm.Input.KeyDown(VK_CONTROL) then
      begin
        S := Copy(Text, 0, StrLFind(Text, CursorPos));
        CursorPos := Length(S);
      end
      else
        CursorPos := CursorPos - 1;
    end;
    if FGLForm.Input.KeyTyped(VK_RIGHT) then
    begin
      if FGLForm.Input.KeyDown(VK_CONTROL) then
        CursorPos := StrRFind(Text, CursorPos) + 1
      else
        CursorPos := CursorPos + 1;
    end;
    if FGLForm.Input.KeyTyped(VK_DELETE) then
    begin
      if FGLForm.Input.KeyDown(VK_CONTROL) then
        Text := Copy(Text, 0, CursorPos) + Copy(Text, StrRFind(Text, CursorPos + CountSpaces(Text, CursorPos)) + 2, Length(Text) - 1)
      else
        Text := Copy(Text, 0, CursorPos) + Copy(Text, CursorPos + 2, Length(Text) - 1);
    end;
    if FGLForm.Input.KeyTyped(VK_BACK) and (CursorPos > 0) then
    begin
      if FGLForm.Input.KeyDown(VK_CONTROL) then
      begin
        S := Copy(Text, 0, StrLFind(Text, CursorPos));
        Text := S + Copy(Text, CursorPos + 1, Length(Text));
        CursorPos := Length(S);
      end
      else
      begin
        Text := Copy(Text, 0, CursorPos - 1) + Copy(Text, CursorPos + 1, Length(Text) - 1);
        CursorPos := CursorPos - 1;
      end;
    end;
    if FGLForm.Input.KeyPressed(VK_HOME) then
      CursorPos := 0;
    if FGLForm.Input.KeyPressed(VK_END) then
      CursorPos := Length(Text);

    if FGLForm.Input.KeyDown(VK_CONTROL) and FGLForm.Input.KeyPressed('C') and (Text <> '') then
      Clipboard.AsText := Text;

    for C in FGLForm.Input.AnsiCharBuffer do
    begin
      Text := Copy(Text, 0, CursorPos) + C + Copy(Text, CursorPos + 1, Length(Text));
      CursorPos := CursorPos + 1;
    end;

    CursorVisible := FCursorTime < FCursorOnTime;

  end
  else
    CursorVisible := False;
end;

{ TGLContainerControl }

function TGLContainerControl.GetBounds: TGBounds2;
begin
  Result := BoundsFromSize(Size);
end;

function TGLContainerControl.GetEnumerator: TObjectArray<TGLControl>.TIterator;
begin
  Result := FSubControls.GetEnumerator;
end;

{ TGLButton }

function TGLButton.GetTextColor: TColorRGBA;
begin
  Result := FTextDisplay.Color;
end;

function TGLButton.GetTexture: String;
begin
  Result := FTexturePage.TextureNames[FTexture];
end;

procedure TGLButton.SetCaption(AValue: AnsiString);
begin
  if FTextDisplay.Text = AValue then
    Exit;
  FTextDisplay.Text := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLButton.SetButtonColor(AValue: TColorRGBA);
begin
  if FButtonColor = AValue then
    Exit;
  FButtonColor := AValue;
  FGUIVAO.NotifyChanges;
end;

procedure TGLButton.SetState(AValue: TButtonState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

function TGLButton.GetEnabled: Boolean;
begin
  Result := State <> bsDisabled;
end;

function TGLButton.GetPressed: Boolean;
begin
  Result := FWasPressed and (mbLeft in MouseReleased);
end;

function TGLButton.GetCaption: AnsiString;
begin
  Result := FTextDisplay.Text;
end;

procedure TGLButton.SetEnabled(AValue: Boolean);
begin
  if Enabled = AValue then
    Exit;
  if AValue then
    State := bsDefault
  else
    State := bsDisabled;
end;

procedure TGLButton.SetHeight(AValue: Single);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLButton.SetTextScale(AValue: Single);
begin
  if FTextScale = AValue then
    Exit;
  FTextScale := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLButton.SetTexture(AValue: String);
begin
  FTexture := FTexturePage.TextureIDs[AValue];
  FGUIVAO.NotifyChanges;
end;

procedure TGLButton.SetTextColor(AValue: TColorRGBA);
begin
  if FTextDisplay.Color = AValue then
    Exit;
  FTextDisplay.Color := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLButton.SetWidth(AValue: Cardinal);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

constructor TGLButton.Create(AParentControl: TGLBasicContainerControl);
begin
  inherited Create(AParentControl);
  FTextDisplay := TTextDisplay2D.Create(Font);
  FTextDisplay.XOrigin := haCenter;
  FTextDisplay.YOrigin := vaCenter;
  TextScale := 0.75;
  Texture := DefaultTextureName;
  Width := 1;
  ButtonColor := ColorWhite;
  Enabled := True;
end;

destructor TGLButton.Destroy;
begin
  FTextDisplay.Free;
  inherited Destroy;
end;

function TGLButton.GetFontVAOSize: Cardinal;
begin
  Result := FTextDisplay.GetVAOSize;
end;

function TGLButton.GetGUIVAOSize: Cardinal;
begin
  Result := 6 * (Width + 1);
end;

procedure TGLButton.AddToFontVAO;
var
  W: Single;
begin
  inherited AddToFontVAO;
  FTextDisplay.Font := Font;
  FTextDisplay.Height := GetRealBounds.Height * TextScale;
  if State = bsPressed then
    FTextDisplay.Height := FTextDisplay.Height * 0.97;
  W := Width / TextScale - (1 - TextScale) * 2;
  if (Caption <> '') and (FTextDisplay.Width > W) then
    FTextDisplay.Height := FTextDisplay.Height * (W / FTextDisplay.Width);
  FTextDisplay.Pos := ClientToGUI(GetBounds.Middle);
  FTextDisplay.Depth := Depth;
  FTextDisplay.AddToVAO(FFontVAO);
end;

procedure TGLButton.AddToGUIVAO;
var
  Data: TData;
  I: TQuadSide;
  X: Cardinal;
  B, TexB: TGBounds2;
begin
  B := GetRealBounds;
  Data.Color := ButtonColor;
  Data.Pos.Z := Depth;
  for X := 0 to Width do
  begin
    TexB.Vertical := TGBounds1.Create(0.75 - Ord(FState) / 4, 1 - Ord(FState) / 4);
    if X = 0 then
      TexB.Horizontal := TGBounds1.Create(0, 0.25)
    else if X = Width then
      TexB.Horizontal := TGBounds1.Create(0.75, 1)
    else
      TexB.Horizontal := TGBounds1.Create(0.25, 0.75);

    TexB := TexturePage.GetTexBounds(FTexture, TexB);
    Data.Border := FTexturePage.HalfPixelInset(TexB);
    for I := Low(TQuadSide) to High(TQuadSide) do
    begin
      if X = 0 then
        Data.Pos.X := B.C1.X + (B.Width / Width) * QuadTexCoords[I].X * 0.5
      else if X = Width then
        Data.Pos.X := B.C1.X + (B.Width / Width) * (QuadTexCoords[I].X * 0.5 + X - 0.5)
      else
        Data.Pos.X := B.C1.X + (B.Width / Width) * (QuadTexCoords[I].X + X - 0.5);

      Data.Pos.Y := B.C1.Y + QuadTexCoords[I].Y * B.Height;

      Data.TexCoord := TexB[QuadTexCoords[I]];

      FGUIVAO.AddVertex(Data);
    end;
  end;
end;

function TGLButton.GetBounds: TGBounds2;
begin
  Result := BoundsFromSize(TGVector2.Create(Width * Height, Height));
end;

procedure TGLButton.Update;
begin
  inherited Update;

  if not Enabled then
    Exit;

  if MouseEntered then
  begin
    if FWasPressed then
      State := bsPressed
    else
      State := bsHover;
  end;
  if MouseLeft then
    State := bsDefault;
  if mbLeft in MousePressed then
  begin
    FWasPressed := True;
    State := bsPressed;
  end;
  if mbLeft in MouseReleased then
    State := bsHover;

  if not FGLForm.Input.ButtonReleased(mbLeft) and FGLForm.Input.ButtonUp(mbLeft) then
    FWasPressed := False;
end;

{ TGLLabelList }

function TGLLabelList.GetTextDisplay(I: Cardinal): TTextDisplay2D;
begin
  Result := FTextDisplays[I];
end;

function TGLLabelList.GetLine(I: Cardinal): AnsiString;
begin
  Result := FTextDisplays[I].Text;
end;

function TGLLabelList.GetLineCount: Cardinal;
begin
  Result := FTextDisplays.Count;
end;

procedure TGLLabelList.SetColor(AValue: TColorRGBA);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLLabelList.SetItalic(AValue: Boolean);
begin
  if FItalic = AValue then
    Exit;
  FItalic := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLLabelList.SetLine(I: Cardinal; AValue: AnsiString);
begin
  FTextDisplays[I].Text := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLLabelList.SetLineHeight(AValue: Single);
begin
  if FLineHeight = AValue then
    Exit;
  FLineHeight := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLLabelList.SetLineSpacing(AValue: Single);
begin
  if FLineSpacing = AValue then
    Exit;
  FLineSpacing := AValue;
  FFontVAO.NotifyChanges;
end;

constructor TGLLabelList.Create(AParentControl: TGLBasicContainerControl);
begin
  inherited Create(AParentControl);
  FTextDisplays := TObjectArray<TTextDisplay2D>.Create;
  FColor := ColorWhite;
  FLineSpacing := 1 / 8;
end;

destructor TGLLabelList.Destroy;
begin
  FTextDisplays.Free;
  inherited Destroy;
end;

function TGLLabelList.GetFontVAOSize: Cardinal;
var
  Current: TTextDisplay2D;
begin
  Result := inherited GetFontVAOSize;
  for Current in FTextDisplays do
    Result := Result + Current.GetVAOSize;
end;

procedure TGLLabelList.AddToFontVAO;
var
  I: Integer;
  P: TGVector2;
  H: Single;
begin
  inherited AddToFontVAO;
  P := Pos;
  H := GetBounds.Height;
  case YOrigin of
    vaTop:
      P.Y := P.Y;
    vaBottom:
      P.Y := P.Y + H;
    vaCenter:
      P.Y := P.Y + H / 2;
  end;

  for I := 0 to FTextDisplays.Count - 1 do
  begin
    FTextDisplays[I].Font := Font;
    FTextDisplays[I].Pos := ClientToGUI(TGVector2.Create(P.X, P.Y - I * (LineHeight + LineSpacing * LineHeight)));
    FTextDisplays[I].Color := Color;
    FTextDisplays[I].XOrigin := XOrigin;
    FTextDisplays[I].Italic := Italic;
    FTextDisplays[I].Height := ClientToGUIY(TGBounds1.Create(0, LineHeight)).Length;
    FTextDisplays[I].Depth := Depth;
    FTextDisplays[I].AddToVAO(FFontVAO);
  end;
end;

function TGLLabelList.GetBounds: TGBounds2;
  function BoundsFromDisplay(ATextDisplay: TTextDisplay2D): TGBounds1;
  var
    W: Single;
  begin
    Result.C1 := 0;
    Result.C2 := 0;
    W := ATextDisplay.Width * LineHeight / FGUI.Aspect;
    case XOrigin of
      haLeft:
      begin
        Result.Low := Pos.X;
        Result.High := Pos.X + W;
      end;
      haRight:
      begin
        Result.Low := Pos.X - W;
        Result.High := Pos.X;
      end;
      haCenter:
      begin
        W := W / 2;
        Result.Low := Pos.X - W;
        Result.High := Pos.X + W;
      end;
    end;
  end;

var
  I: Cardinal;
  B: TGBounds1;
  H: Single;
begin
  if FTextDisplays.Count >= 1 then
  begin
    B := BoundsFromDisplay(TextDisplays[0]);
    Result.C1.X := B.C1;
    Result.C2.X := B.C2;
    if FTextDisplays.Count >= 2 then
      for I := 1 to FTextDisplays.Count - 1 do
      begin
        B := BoundsFromDisplay(TextDisplays[I]);
        Result.C1.X := Min(Result.C1.X, B.C1);
        Result.C2.X := Max(Result.C2.X, B.C2);
      end;
  end
  else
  begin
    Result.C1.X := Pos.X;
    Result.C2.X := Pos.X;
  end;

  H := Max(0, FTextDisplays.Count * LineHeight + (FTextDisplays.Count - 1) * LineSpacing * LineHeight);
  case YOrigin of
    vaTop:
    begin
      Result.C1.Y := Pos.Y - H;
      Result.C2.Y := Pos.Y;
    end;
    vaBottom:
    begin
      Result.C1.Y := Pos.Y;
      Result.C2.Y := Pos.Y + H;
    end;
    vaCenter:
    begin
      H := H / 2;
      Result.C1.Y := Pos.Y - H;
      Result.C2.Y := Pos.Y + H;
    end;
  end;
end;

procedure TGLLabelList.AddLine(AText: AnsiString);
var
  NewDisplay: TTextDisplay2D;
begin
  NewDisplay := TTextDisplay2D.Create(Font);
  NewDisplay.Text := AText;
  NewDisplay.Color := Color;
  FTextDisplays.Add(NewDisplay);
end;

procedure TGLLabelList.DelLine(AIndex: Cardinal);
begin
  FTextDisplays.Del(AIndex);
end;

procedure TGLLabelList.DelAllLines;
begin
  FTextDisplays.DelAll;
end;

{ TGLBasicContainerControl }

procedure TGLBasicContainerControl.SetHeight(AValue: Single);
begin
  if FSize.Y = AValue then
    Exit;
  FSize.Y := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

function TGLBasicContainerControl.GetInnerBounds: TGBounds2;
begin
  Result := GetBounds;
end;

function TGLBasicContainerControl.GetCutInnerBounds: TGBounds2;
var
  ABorder: TGBounds2;
begin
  Result := GetInnerBounds;
  ABorder.Right := ParentControl.Aspect;
  ABorder.Left := -ABorder.Right;
  ABorder.Bottom := -1;
  ABorder.Top := 1;
  Result.EnsureRange(ABorder, Result);
end;

procedure TGLBasicContainerControl.SetSize(AValue: TGVector2);
begin
  if FSize = AValue then
    Exit;
  FSize := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

function TGLBasicContainerControl.GetAspect: Single;
begin
  Result := Size.X / Size.Y;
end;

procedure TGLBasicContainerControl.SetWidth(AValue: Single);
begin
  if FSize.X = AValue then
    Exit;
  FSize.X := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

constructor TGLBasicContainerControl.Create(AParentControl: TGLBasicContainerControl);
begin
  inherited Create(AParentControl);
  FSubControls := TObjectArray<TGLControl>.Create(True);
end;

destructor TGLBasicContainerControl.Destroy;
var
  Control: TGLControl;
begin
  for Control in FSubControls.IterReversed do
    Control.Free;
  FSubControls.Free;
  inherited Destroy;
end;

procedure TGLBasicContainerControl.Update;
var
  Control: TGLControl;
begin
  inherited Update;
  for Control in FSubControls do
  begin
    if not Control.Visible then
      Continue;
    Control.Update;
  end;
end;

function TGLBasicContainerControl.GetFontVAOSize: Cardinal;
var
  Control: TGLControl;
begin
  Result := inherited GetFontVAOSize;
  for Control in FSubControls do
    if Control.Visible then
      Result := Result + Control.GetFontVAOSize;
end;

function TGLBasicContainerControl.GetGUIVAOSize: Cardinal;
var
  Control: TGLControl;
begin
  Result := inherited GetGUIVAOSize;
  for Control in FSubControls do
    if Control.Visible then
      Result := Result + Control.GetGUIVAOSize;
end;

procedure TGLBasicContainerControl.AddToGUIVAO;
var
  Control: TGLControl;
begin
  inherited AddToGUIVAO;
  for Control in FSubControls do
    if Control.Visible then
      Control.AddToGUIVAO;
end;

procedure TGLBasicContainerControl.AddToFontVAO;
var
  Control: TGLControl;
begin
  inherited AddToFontVAO;
  for Control in FSubControls do
    if Control.Visible then
      Control.AddToFontVAO;
end;

procedure TGLBasicContainerControl.AddControl(AControl: TGLControl);
begin
  FSubControls.Add(AControl);
end;

procedure TGLBasicContainerControl.DelControl(AControl: TGLControl);
begin
  FSubControls.DelObject(AControl);
end;

{ TGUI.TFontVAO }

procedure TGUI.TFontVAO.BeforeRender;
begin
  inherited BeforeRender;
  FGUI.FFonts.Uniform(Shader, TextureUniformName);
end;

procedure TGUI.TFontVAO.BuildVAO;
var
  Size: Cardinal;
begin
  Size := FGUI.GetFontVAOSize;

  if GetSize <> Size then
    Generate(Size, buDynamicDraw);

  Map(baWriteOnly);
  FGUI.AddToFontVAO;
  Unmap;
end;

constructor TGUI.TFontVAO.Create(AShader: TShader; AGUI: TGUI);
begin
  inherited Create(AShader);
  FGUI := AGUI;
end;

{ TGUI.TGUIVAO }

procedure TGUI.TGUIVAO.BeforeRender;
begin
  inherited BeforeRender;
  FGUI.TexturePage.Uniform(Shader, TextureUniformName);
end;

procedure TGUI.TGUIVAO.BuildVAO;
var
  Size: Cardinal;
begin
  Size := FGUI.GetGUIVAOSize;

  if GetSize <> Size then
    Generate(Size, buDynamicDraw);

  Map(baWriteOnly);
  FGUI.AddToGUIVAO;
  Unmap;
end;

constructor TGUI.TGUIVAO.Create(AShader: TShader; AGUI: TGUI);
begin
  inherited Create(AShader);
  FGUI := AGUI;
end;

{ TGLLabel }

function TGLLabel.GetText: AnsiString;
begin
  Result := FTextDisplay.Text;
end;

procedure TGLLabel.SetColor(AValue: TColorRGBA);
begin
  if FTextDisplay.Color = AValue then
    Exit;
  FTextDisplay.Color := AValue;
  FFontVAO.NotifyChanges;
end;

function TGLLabel.GetColor: TColorRGBA;
begin
  Result := FTextDisplay.Color;
end;

function TGLLabel.GetItalic: Boolean;
begin
  Result := FTextDisplay.Italic;
end;

procedure TGLLabel.SetHeight(AValue: Single);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLLabel.SetItalic(AValue: Boolean);
begin
  if FTextDisplay.Italic = AValue then
    Exit;
  FTextDisplay.Italic := AValue;
  FFontVAO.NotifyChanges;
end;

procedure TGLLabel.SetText(AValue: AnsiString);
begin
  if FTextDisplay.Text = AValue then
    Exit;
  FTextDisplay.Text := AValue;
  FFontVAO.NotifyChanges;
end;

constructor TGLLabel.Create(AParentControl: TGLBasicContainerControl);
begin
  inherited;
  FTextDisplay := TTextDisplay2D.Create(Font);
  Height := 1;
end;

destructor TGLLabel.Destroy;
begin
  FTextDisplay.Free;
  inherited;
end;

function TGLLabel.GetFontVAOSize: Cardinal;
begin
  Result := FTextDisplay.GetVAOSize;
end;

procedure TGLLabel.AddToFontVAO;
begin
  FTextDisplay.Font := Font;
  FTextDisplay.XOrigin := XOrigin;
  FTextDisplay.YOrigin := YOrigin;
  FTextDisplay.Pos := GetRealPos;
  FTextDisplay.Height := GetRealBounds.Height;
  FTextDisplay.Depth := Depth;
  FTextDisplay.AddToVAO(FFontVAO);
end;

function TGLLabel.GetBounds: TGBounds2;
begin
  Result := BoundsFromSize(TGVector2.Create(FTextDisplay.Width * Height, Height));
end;

{ TGLControl }

function TGLControl.GetRealPos: TGVector2;
begin
  Result := ClientToGUI(Pos);
end;

procedure TGLControl.SetPos(AValue: TGVector2);
begin
  if FPos = AValue then
    Exit;
  FPos := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLControl.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLControl.SetDepth(AValue: Single);
begin
  if FDepth = AValue then
    Exit;
  FDepth := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

function TGLControl.GetMousePos: TGVector2;
begin
  Result := GUIToClient(FGLForm.Input.MousePos);
end;

function TGLControl.GetFont: TBMPFontItem;
begin
  if FFont <> nil then
    Exit(FFont);
  Result := FParentControl.Font;
end;

procedure TGLControl.SetFont(AValue: TBMPFontItem);
begin
  if Pointer(FFont) = Pointer(AValue) then
    Exit;
  FFont := AValue;

  if not FGUI.FontExists(AValue) then
    FGUI.AddFont(AValue);

  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLControl.SetPosX(AValue: Single);
begin
  if AValue = FPos.X then
    Exit;
  FPos.X := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLControl.SetPosY(AValue: Single);
begin
  if AValue = FPos.Y then
    Exit;
  FPos.Y := AValue;
  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGLControl.SetXOrigin(AValue: THAlignment);
begin
  if FXOrigin = AValue then
    Exit;
  FXOrigin := AValue;
end;

procedure TGLControl.SetYOrigin(AValue: TVAlignment);
begin
  if FYOrigin = AValue then
    Exit;
  FYOrigin := AValue;
end;

function TGLControl.BoundsFromSize(ASize: TGVector2): TGBounds2;
begin
  case XOrigin of
    haLeft:
    begin
      Result.Left := Pos.X;
      Result.Right := Pos.X + ASize.X;
    end;
    haRight:
    begin
      Result.Left := Pos.X - ASize.X;
      Result.Right := Pos.X;
    end;
    haCenter:
    begin
      ASize.X := ASize.X / 2;
      Result.Left := Pos.X - ASize.X;
      Result.Right := Pos.X + ASize.X;
    end;
  end;

  case YOrigin of
    vaTop:
    begin
      Result.Bottom := Pos.Y - ASize.Y;
      Result.Top := Pos.Y;
    end;
    vaBottom:
    begin
      Result.Bottom := Pos.Y;
      Result.Top := Pos.Y + ASize.Y;
    end;
    vaCenter:
    begin
      ASize.Y := ASize.Y / 2;
      Result.Bottom := Pos.Y - ASize.Y;
      Result.Top := Pos.Y + ASize.Y;
    end;
  end;
end;

function TGLControl.MouseInBounds: Boolean;
begin
  Result := (FGLForm.Input.MousePos in GetRealBounds) and FGLForm.Input.MouseOnScreen;
end;

constructor TGLControl.Create(AParentControl: TGLBasicContainerControl);
begin
  if AParentControl <> nil then
  begin
    FParentControl := AParentControl;
    FGLForm := FParentControl.FGLForm;
    FGUIVAO := FParentControl.FGUIVAO;
    FFontVAO := FParentControl.FFontVAO;
    FTexturePage := FParentControl.FTexturePage;
    FGUIVAO.NotifyChanges;
    FFontVAO.NotifyChanges;
    XOrigin := haCenter;
    YOrigin := vaCenter;
    FParentControl.AddControl(Self);
    while AParentControl.ParentControl <> nil do
      AParentControl := AParentControl.ParentControl;
    FGUI := AParentControl as TGUI;
  end;
  Visible := True;
end;

destructor TGLControl.Destroy;
begin
  if FParentControl <> nil then
  begin
    FParentControl.DelControl(Self);
    FParentControl.FGUIVAO.NotifyChanges;
  end;
  inherited;
end;

procedure TGLControl.Delete;
begin
  FParentControl.DelControl(Self);
end;

function TGLControl.GetGUIVAOSize: Cardinal;
begin
  Result := 0;
end;

function TGLControl.GetFontVAOSize: Cardinal;
begin
  Result := 0;
end;

procedure TGLControl.AddToGUIVAO;
begin
  // nothing by default
end;

procedure TGLControl.AddToFontVAO;
begin
  // nothing by default
end;

function TGLControl.ClientToGUI(APoint: TGVector2): TGVector2;
var
  B: TGBounds2;
  W, M: Single;
begin
  Result := APoint;
  if FParentControl <> nil then
  begin
    B := FParentControl.GetBounds;
    W := B.Width;

    M := B.Horizontal.Middle;
    B.Right := M + B.Height / 2;
    B.Left := M - B.Height / 2;

    case XOrigin of
      haLeft:
        B.Translate(-UVecX * W / 2);
      haRight:
        B.Translate(+UVecX * W / 2);
    end;

    case YOrigin of
      vaTop:
        B.Translate(+UVecY * B.Height / 2);
      vaBottom:
        B.Translate(-UVecY * B.Height / 2);
    end;

    B.C1 := B.Middle;
    Result := FParentControl.ClientToGUI(B[Result]);
  end;
end;

function TGLControl.ClientToGUI(ABounds: TGBounds2): TGBounds2;
begin
  Result.C1 := ClientToGUI(ABounds.C1);
  Result.C2 := ClientToGUI(ABounds.C2);
end;

function TGLControl.ClientToGUIX(APoint: Single): Single;
var
  B: TGBounds1;
begin
  Result := APoint;
  if FParentControl <> nil then
  begin
    B := FParentControl.GetBounds.Horizontal;

    case XOrigin of
      haLeft:
        B.Translate(-B.Length / 2);
      haRight:
        B.Translate(+B.Length / 2);
    end;

    B.C1 := B.Middle;
    Result := FParentControl.ClientToGUIX(B[Result]);
  end;
end;

function TGLControl.ClientToGUIX(ABounds: TGBounds1): TGBounds1;
begin
  Result.C1 := ClientToGUIX(ABounds.C1);
  Result.C2 := ClientToGUIX(ABounds.C2);
end;

function TGLControl.ClientToGUIY(APoint: Single): Single;
var
  B: TGBounds1;
begin
  Result := APoint;
  if FParentControl <> nil then
  begin
    B := FParentControl.GetBounds.Vertical;

    case YOrigin of
      vaTop:
        B.Translate(+B.Length / 2);
      vaBottom:
        B.Translate(-B.Length / 2);
    end;

    B.C1 := B.Middle;
    Result := FParentControl.ClientToGUIY(B[Result]);
  end;
end;

function TGLControl.ClientToGUIY(ABounds: TGBounds1): TGBounds1;
begin
  Result.C1 := ClientToGUIY(ABounds.C1);
  Result.C2 := ClientToGUIY(ABounds.C2);
end;

function TGLControl.GUIToClient(APoint: TGVector2): TGVector2;
var
  B: TGBounds2;
  W, M: Single;
begin
  Result := APoint;
  if FParentControl <> nil then
    Result := FParentControl.GUIToClient(Result);

  B := GetBounds;
  W := B.Width;

  M := B.Horizontal.Middle;
  B.Right := M + B.Height / 2;
  B.Left := M - B.Height / 2;

  case XOrigin of
    haLeft:
      B.Translate(-UVecX * W / 2);
    haRight:
      B.Translate(+UVecX * W / 2);
  end;

  case YOrigin of
    vaTop:
      B.Translate(+UVecY * B.Height / 2);
    vaBottom:
      B.Translate(-UVecY * B.Height / 2);
  end;

  B.C1 := B.Middle;
  Result := B.FindPoint(Result);
end;

function TGLControl.GUIToClient(ABounds: TGBounds2): TGBounds2;
begin
  Result.C1 := GUIToClient(ABounds.C1);
  Result.C2 := GUIToClient(ABounds.C2);
end;

function TGLControl.GetRealBounds: TGBounds2;
begin
  Result := ClientToGUI(GetBounds);
end;

procedure TGLControl.Update;
var
  HoveredBefore: Boolean;
  B: TMouseButton;
begin
  HoveredBefore := FMouseHovering;
  FMouseHovering := MouseInBounds and not FGLForm.Input.MouseLeftScreen;

  FMouseLeft := HoveredBefore and not FMouseHovering;
  FMouseEntered := FMouseHovering and not HoveredBefore;

  FMousePressed := [];
  FMouseReleased := [];

  if MouseHovering then
  begin
    for B := Low(TMouseButton) to High(TMouseButton) do
    begin
      if FGLForm.Input.ButtonPressed(B) then
      begin
        Include(FMousePressed, B);
        Include(FMouseDown, B);
      end;
      if FGLForm.Input.ButtonReleased(B) then
      begin
        Include(FMouseReleased, B);
        Exclude(FMouseDown, B);
      end;
    end;
  end
  else
    FMouseDown := [];

  if MousePressed <> [] then
    Focus;
end;

procedure TGLControl.Focus;
begin
  FGUI.FocusedControl := Self;
end;

procedure TGLControl.UnFocus;
begin
  if HasFocus then
    FGUI.FocusedControl := nil;
end;

function TGLControl.UsingParentFont: Boolean;
begin
  Result := FFont = nil;
end;

procedure TGLControl.UseParentFont;
begin
  if FParentControl <> nil then
    FFont := nil;
end;

function TGLControl.HasFocus: Boolean;
begin
  Result := Pointer(FGUI.FocusedControl) = Pointer(Self);
end;

{ TGUI }

procedure TGUI.SetAspect(AValue: Single);
begin
  AValue := AValue * Height;
  if Width = AValue then
    Exit;
  Width := AValue;

  FGUIVAO.NotifyChanges;
  FFontVAO.NotifyChanges;
end;

procedure TGUI.SetFocusedControl(AValue: TGLControl);
begin
  FFocusedControl := AValue;
end;

procedure TGUI.AddToFontVAO;
begin
  if Visible then
    inherited AddToFontVAO;
end;

procedure TGUI.AddToGUIVAO;
begin
  if Visible then
    inherited AddToGUIVAO;
end;

function TGUI.GetFontVAOSize: Cardinal;
begin
  if Visible then
    Exit(inherited GetFontVAOSize);
  Result := 0;
end;

function TGUI.GetGUIVAOSize: Cardinal;
begin
  if Visible then
    Exit(inherited GetGUIVAOSize);
  Result := 0;
end;

constructor TGUI.Create(AShader: TShader; AGLForm: TGLForm; AFont: TBMPFontItem; AAspect: Single);
begin
  FGUIVAO := TGUIVAO.Create(AShader, Self);
  FFontVAO := TFontVAO.Create(AShader, Self);
  FGLForm := AGLForm;
  FTexturePage := TTexturePage.Create;
  FFonts := TBMPFontList.Create;
  FGUI := Self;
  Font := AFont;
  Width := 2 * AAspect;
  Height := 2;
  XOrigin := haCenter;
  YOrigin := vaCenter;
  inherited Create(nil);
end;

destructor TGUI.Destroy;
begin
  FGUIVAO.Free;
  FFontVAO.Free;
  FTexturePage.Free;
  FFonts.Free;
  inherited;
end;

procedure TGUI.AddTextureFromFile(const AFileName: String);
begin
  FTexturePage.AddTextureFromFile(AFileName);
  FTexturePage.BuildPage(1, False);
  FGUIVAO.NotifyChanges;
end;

procedure TGUI.AddTextureFromFile(const AFileName, AName: String);
begin
  FTexturePage.AddTextureFromFile(AFileName, AName);
  FTexturePage.BuildPage(1, False);
  FGUIVAO.NotifyChanges;
end;

procedure TGUI.AddTextureFromResource(const AResourceName: String);
begin
  FTexturePage.AddTextureFromResource(AResourceName);
  FTexturePage.BuildPage(1, False);
  FGUIVAO.NotifyChanges;
end;

procedure TGUI.AddTextureFromResource(const AResourceName, AName: String);
begin
  FTexturePage.AddTextureFromResource(AResourceName, AName);
  FTexturePage.BuildPage(1, False);
  FGUIVAO.NotifyChanges;
end;

procedure TGUI.AddFont(AFont: TBMPFontItem);
begin
  FFonts.Add(AFont);
end;

procedure TGUI.DelFont(AFont: TBMPFontItem);
begin
  FFonts.Del(AFont);
end;

function TGUI.FontExists(AFont: TBMPFontItem): Boolean;
begin
  Result := FFonts.FontExists(AFont);
end;

procedure TGUI.Render;
begin
  glClear(Ord(amDepth));
  FGLForm.Push;
  FGLForm.State.DepthFunc := cfLEqual;
  FGUIVAO.Render;
  FFontVAO.Render;
  FGLForm.Pop;
end;

end.

