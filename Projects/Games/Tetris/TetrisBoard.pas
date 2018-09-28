unit TetrisBoard;

interface

uses
  Winapi.Windows,

  Pengine.SpriteSystem,
  Pengine.IntMaths,
  Pengine.Color,
  Pengine.GUI,
  Pengine.Vector,
  Pengine.Collections,
  Pengine.TimeManager,
  Pengine.InputHandler;

type

  TTetrisBoard = class;

  TTetrisBlock = class
  private
    FTetrisBoard: TTetrisBoard;
    FPos: TIntVector2;
    FSprite: TSprite;

    function GetColor: TColorRGB;
    procedure SetPos(const Value: TIntVector2);

  public
    constructor Create(ATetrisBoard: TTetrisBoard; APos: TIntVector2);

    property TetrisBoard: TTetrisBoard read FTetrisBoard;

    function Exists: Boolean;

    procedure Clear;
    procedure Fill(AColor: TColorRGB);

    /// <remarks>Use fill as "SetColor"</remarks>
    property Color: TColorRGB read GetColor;
    property Pos: TIntVector2 read FPos write SetPos;

  end;

  TTetrisPieceTemplate = class
  protected
    function GetColor: TColorRGB; virtual; abstract;
    function GetShape(AIndex: Integer): TIntVector2; virtual; abstract;
    function GetShapeCount: Integer; virtual; abstract;

  public
    property Shape[AIndex: Integer]: TIntVector2 read GetShape;
    property ShapeCount: Integer read GetShapeCount;
    property Color: TColorRGB read GetColor;
    function OddSymmetry: TCoordAxes2; virtual; abstract;

  end;

  TTetrisBasicPieceRec = record
    Color: TColorRGB;
    Shape: array [0 .. 3] of TIntVector2;
    OddSymmetry: TCoordAxes2;
  end;

  TTetrisBasicPiece = class(TTetrisPieceTemplate)
  private
    FData: TTetrisBasicPieceRec;

  protected
    function GetColor: TColorRGB; override;
    function GetShape(AIndex: Integer): TIntVector2; override;
    function GetShapeCount: Integer; override;

  public
    constructor Create(AData: TTetrisBasicPieceRec);

    function OddSymmetry: TCoordAxes2; override;

  end;

  TDropPiece = class
  public type

    TBlocks = TObjectArray<TTetrisBlock>;

  private
    FTetrisBoard: TTetrisBoard;
    FTemplate: TTetrisPieceTemplate;
    FPos: TIntVector2;
    FUpside: TBasicDir2;
    FBlocks: TBlocks;

    function GetTile(AIndex: Integer): TIntVector2;

    procedure Move(AAmount: TIntVector2);
    procedure Rotate(AAmount: Integer);
    
    procedure UpdateBlocks;
    
  public
    constructor Create(ATetrisBoard: TTetrisBoard; ATemplate: TTetrisPieceTemplate);
    destructor Destroy; override;

    function Advance: Boolean;
    procedure AddToBoard;

    property TetrisBoard: TTetrisBoard read FTetrisBoard;

    property Template: TTetrisPieceTemplate read FTemplate;
    property Pos: TIntVector2 read FPos;
    property Upside: TBasicDir2 read FUpside;

    property Tiles[AIndex: Integer]: TIntVector2 read GetTile;

    function CanMove(AAmount: TIntVector2): Boolean;
    function TryMove(AAmount: TIntVector2): Boolean;
    function CanRotate(AAmount: Integer): Boolean;
    function TryRotate(AAmount: Integer): Boolean;

    function ValidatePosition: Boolean;

  end;

  TTetrisBoard = class(TControl)
  public const

    DefaultSize: TIntVector2 = (X: 10; Y: 20);

  public type

    TPieceTemplates = TObjectArray<TTetrisPieceTemplate>;

  private
    FMap: array of array of TTetrisBlock;
    FSize: TIntVector2;
    FDropPiece: TDropPiece;
    FPieceTemplates: TPieceTemplates;
    FStepTime: TSeconds;
    FStepInterval: TSeconds;

    procedure ResetMap;
    procedure InitMap;

    procedure SetSize(const Value: TIntVector2);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    function GetBlock(APos: TIntVector2): TTetrisBlock;

    procedure SetStepInterval(const Value: TSeconds);

    procedure GameUpdate;
    procedure KeyDown(AInfo: TKeyEventInfo);

  protected
    function GetAspect: Single; override;

  public
    constructor Create(AParent: TControl); override;
    destructor Destroy; override;

    property Size: TIntVector2 read FSize write SetSize;
    property Width: Integer read FSize.X write SetWidth;
    property Height: Integer read FSize.Y write SetHeight;

    property Blocks[APos: TIntVector2]: TTetrisBlock read GetBlock; default;

    property PieceTemplates: TPieceTemplates read FPieceTemplates;
    procedure AddDefaultTemplates;

    procedure Step;

    property StepInterval: TSeconds read FStepInterval write SetStepInterval;

  end;

const

  DefaultTetrisPieces: array [0 .. 6] of TTetrisBasicPieceRec = (
    // O (yellow)
    (Color: (R: 1; G: 1; B: 0);
    Shape: ((X: -1; Y: -1), (X: 0; Y: -1), (X: -1; Y: 0), (X: 0; Y: 0));
    OddSymmetry: []),
    // L (orange)
    (Color: (R: 1; G: 0.5; B: 0);
    Shape: ((X: -1; Y: -1), (X: 0; Y: -1), (X: 1; Y: -1), (X: 1; Y: 0));
    OddSymmetry: [caX]),
    // J (blue)
    (Color: (R: 0; G: 0; B: 1);
    Shape: ((X: -1; Y: -1), (X: 0; Y: -1), (X: 1; Y: -1), (X: -1; Y: 0));
    OddSymmetry: [caX]),
    // T (purple)
    (Color: (R: 1; G: 0; B: 1);
    Shape: ((X: -1; Y: -1), (X: 0; Y: -1), (X: 1; Y: -1), (X: 0; Y: 0));
    OddSymmetry: [caX, caY]),
    // S (green)
    (Color: (R: 0; G: 1; B: 0);
    Shape: ((X: -1; Y: -1), (X: 0; Y: -1), (X: 0; Y: 0), (X: 1; Y: 0));
    OddSymmetry: [caX]),
    // Z (red)
    (Color: (R: 1; G: 0; B: 0);
    Shape: ((X: -1; Y: 0), (X: 0; Y: 0), (X: 0; Y: -1), (X: 1; Y: -1));
    OddSymmetry: [caX]),
    // I (cyan)
    (Color: (R: 0; G: 1; B: 1);
    Shape: ((X: -2; Y: 0), (X: -1; Y: 0), (X: 0; Y: 0), (X: 1; Y: 0));
    OddSymmetry: [caY])
  );

implementation

{ TTetrisBoard }

procedure TTetrisBoard.AddDefaultTemplates;
var
  Rec: TTetrisBasicPieceRec;
begin
  for Rec in DefaultTetrisPieces do
    PieceTemplates.Add(TTetrisBasicPiece.Create(Rec));
end;

constructor TTetrisBoard.Create(AParent: TControl);
begin
  inherited;
  Size := DefaultSize;
  FPieceTemplates := TPieceTemplates.Create;
  Game.OnUpdate.Add(GameUpdate);
  Input.OnKeyTyped.Add(KeyDown);
  FStepInterval := 0.5;
end;

destructor TTetrisBoard.Destroy;
begin
  FPieceTemplates.Free;
  FDropPiece.Free;
  ResetMap;
  inherited;
end;

procedure TTetrisBoard.GameUpdate;
begin
  FStepTime := FStepTime - Game.DeltaTime;
  if FStepTime < 0 then
  begin
    FStepTime := FStepTime + StepInterval;
    Step;
  end;
end;

function TTetrisBoard.GetAspect: Single;
begin
  Result := Width / Height;
end;

function TTetrisBoard.GetBlock(APos: TIntVector2): TTetrisBlock;
begin
  Result := FMap[APos.X, APos.Y];
end;

procedure TTetrisBoard.InitMap;
var
  Pos: TIntVector2;
begin
  for Pos in Size do
    FMap[Pos.X, Pos.Y] := TTetrisBlock.Create(Self, Pos);
end;

procedure TTetrisBoard.KeyDown(AInfo: TKeyEventInfo);
begin
  if FDropPiece <> nil then
  begin 
    case AInfo.Key of
      VK_LEFT:
        FDropPiece.TryMove(IVec2(-1, 0));
      VK_RIGHT:
        FDropPiece.TryMove(IVec2(1, 0));
      VK_DOWN:
        FDropPiece.TryMove(IVec2(0, -1));
      VK_UP:
        FDropPiece.TryRotate(1);
    end;
  end;
end;

procedure TTetrisBoard.ResetMap;
var
  Pos: TIntVector2;
begin
  for Pos in Size do
    FMap[Pos.X, Pos.Y].Free;
end;

procedure TTetrisBoard.SetHeight(const Value: Integer);
begin
  if Height = Value then
    Exit;
  Size := IVec2(Width, Value);
end;

procedure TTetrisBoard.SetSize(const Value: TIntVector2);
begin
  if Size = Value then
    Exit;
  ResetMap;
  FSize := Value;
  NotifyAspectChanged;
  SetLength(FMap, Width, Height);
  InitMap;
end;

procedure TTetrisBoard.SetStepInterval(const Value: TSeconds);
begin
  if StepInterval = Value then
    Exit;
  FStepInterval := Value;
  FStepTime := FStepInterval;
end;

procedure TTetrisBoard.SetWidth(const Value: Integer);
begin
  if Width = Value then
    Exit;
  Size := IVec2(Value, Height);
end;

procedure TTetrisBoard.Step;
begin
  if FDropPiece <> nil then
  begin
    if not FDropPiece.Advance then
    begin
      FDropPiece.AddToBoard;
      FDropPiece.Free;
      FDropPiece := nil;
    end;
  end
  else
    FDropPiece := TDropPiece.Create(Self, FPieceTemplates[Random(FPieceTemplates.Count)]);
end;

{ TTetrisBlockData }

procedure TTetrisBlock.Clear;
begin
  FSprite.Color := ColorTransparent;
end;

constructor TTetrisBlock.Create(ATetrisBoard: TTetrisBoard; APos: TIntVector2);
begin
  FTetrisBoard := ATetrisBoard;
  FSprite := TetrisBoard.Add<TSprite>('block');
  Clear;
  FSprite.Location.Parent := TetrisBoard.Location;
  FSprite.Location.Scale := 1 / TetrisBoard.Height;
  Pos := APos;
end;

function TTetrisBlock.Exists: Boolean;
begin
  Result := FSprite.Color <> ColorTransparent;
end;

procedure TTetrisBlock.Fill(AColor: TColorRGB);
begin
  FSprite.Color := AColor;
end;

function TTetrisBlock.GetColor: TColorRGB;
begin
  Result := FSprite.Color;
end;

procedure TTetrisBlock.SetPos(const Value: TIntVector2);
begin
  FPos := Value;
  FSprite.Location.Pos := (TVector2(Pos) + 0.5) / TetrisBoard.Height - Vec2(TetrisBoard.Aspect, 1) / 2;
end;

{ TDropPiece }

procedure TDropPiece.AddToBoard;
var
  I: Integer;
  Pos: TIntVector2;
begin
  for I := 0 to Template.ShapeCount - 1 do
  begin
    Pos := Tiles[I];
    if Pos in TetrisBoard.Size then
      TetrisBoard[Pos].Fill(Template.Color);
  end;
end;

function TDropPiece.Advance: Boolean;
begin
  Result := TryMove(IVec2(0, -1));
end;

function TDropPiece.CanMove(AAmount: TIntVector2): Boolean;
var
  OldPos: TIntVector2;
begin
  OldPos := Pos;
  Move(AAmount);
  Result := ValidatePosition;
  FPos := OldPos;
end;

function TDropPiece.CanRotate(AAmount: Integer): Boolean;
var
  OldUpside: TBasicDir2;
begin
  OldUpside := Upside;
  Rotate(AAmount);
  Result := ValidatePosition;
  FUpside := OldUpside;
end;

constructor TDropPiece.Create(ATetrisBoard: TTetrisBoard; ATemplate: TTetrisPieceTemplate);
var
  I: Integer;
  TetrisBlock: TTetrisBlock;
begin
  FUpside := bdUp;
  FTetrisBoard := ATetrisBoard;
  FTemplate := ATemplate;
  FPos.X := TetrisBoard.Width div 2;
  FPos.Y := TetrisBoard.Height;
  FBlocks := TBlocks.Create;
  for I := 0 to FTemplate.ShapeCount - 1 do
  begin
    TetrisBlock := TTetrisBlock.Create(TetrisBoard, Tiles[I]);
    TetrisBlock.Fill(Template.Color);
    FBlocks.Add(TetrisBlock);
  end;
end;

destructor TDropPiece.Destroy;
begin
  FBlocks.Free;
  inherited;
end;

function TDropPiece.GetTile(AIndex: Integer): TIntVector2;
begin
  case FUpside of
    bdLeft:
      Result := Pos + Template.Shape[AIndex].Cross;
    bdRight:
      Result := Pos - Template.Shape[AIndex].Cross;
    bdDown:
      Result := Pos - Template.Shape[AIndex];
    bdUp:
      Result := Pos + Template.Shape[AIndex];
  end;
end;

procedure TDropPiece.Move(AAmount: TIntVector2);
begin
  FPos := Pos + AAmount;
end;

procedure TDropPiece.Rotate(AAmount: Integer);
begin
  FUpside := RotateDir(Upside, AAmount);
end;

procedure TDropPiece.UpdateBlocks;
var
  I: Integer;
begin
  for I := 0 to Template.ShapeCount - 1 do
    FBlocks[I].Pos := Tiles[I];
end;

function TDropPiece.TryMove(AAmount: TIntVector2): Boolean;
begin
  Result := CanMove(AAmount);
  if Result then
  begin
    Move(AAmount);
    UpdateBlocks;
  end;
end;

function TDropPiece.TryRotate(AAmount: Integer): Boolean;
begin
  Result := CanRotate(AAmount);
  if Result then
  begin
    Rotate(AAmount);
    UpdateBlocks;
  end;
end;

function TDropPiece.ValidatePosition: Boolean;
var
  I: Integer;
  P: TIntVector2;
begin
  for I := 0 to Template.ShapeCount - 1 do
  begin
    P := Tiles[I];
    if not (P in TetrisBoard.Size) then
      Exit(False);
    if TetrisBoard[P].Exists then
      Exit(False);
  end;
  Result := True;
end;

{ TTetrisBasicPiece }

constructor TTetrisBasicPiece.Create(AData: TTetrisBasicPieceRec);
begin
  FData := AData;
end;

function TTetrisBasicPiece.GetColor: TColorRGB;
begin
  Result := FData.Color;
end;

function TTetrisBasicPiece.GetShape(AIndex: Integer): TIntVector2;
begin
  Result := FData.Shape[AIndex];
end;

function TTetrisBasicPiece.GetShapeCount: Integer;
begin
  Result := 4;
end;

function TTetrisBasicPiece.OddSymmetry: TCoordAxes2;
begin
  Result := FData.OddSymmetry;
end;

end.

