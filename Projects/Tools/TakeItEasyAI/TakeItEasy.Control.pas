unit TakeItEasy.Control;

interface

uses
  Vcl.Controls,

  Pengine.GUI,
  Pengine.Texture,
  Pengine.TextureAtlas,
  Pengine.SpriteSystem,
  Pengine.Vector,
  Pengine.Color,
  Pengine.Collections,
  Pengine.InputHandler,

  TakeItEasy.Game,
  Pengine.Utility;

type

  TTakeItEasyControl = class(TControl)
  public const

    DefaultTileTextureName = 'tie_tile';

    DefaultTileBackgroundName = 'tie_tile_background';

    DefaultLineTextureName = 'tie_line';

    DefaultValueTextureName = 'tie_values';

    RowColors: array [TTakeItEasy.TRowValueZero] of TColorRGBA = (
      (R: 0.0; G: 0.0; B: 0.0; A: 0.0), // - 0 - transparent
      (R: 0.6; G: 0.6; B: 0.7; A: 1.0), // - 1 - gray
      (R: 0.9; G: 0.7; B: 0.8; A: 1.0), // - 2 - pink
      (R: 0.9; G: 0.5; B: 0.7; A: 1.0), // - 3 - magenta
      (R: 0.1; G: 0.7; B: 0.9; A: 1.0), // - 4 - light blue
      (R: 0.0; G: 0.5; B: 0.6; A: 1.0), // - 5 - cyan
      (R: 0.8; G: 0.2; B: 0.2; A: 1.0), // - 6 - red
      (R: 0.6; G: 0.7; B: 0.4; A: 1.0), // - 7 - light green
      (R: 0.9; G: 0.6; B: 0.2; A: 1.0), // - 8 - orange
      (R: 0.9; G: 0.8; B: 0.4; A: 1.0) // -- 9 - yellow
      );

  public type

    TPiece = class
    private
      FControl: TTakeItEasyControl;
      FPiece: TTakeItEasy.TPiece;
      FTile: TSprite;
      FLines: array [TTakeItEasy.TRowDirection] of TSprite;
      FValues: array [TTakeItEasy.TRowDirection] of TSprite;

      function GetLocation: TLocation2;
      function GetScale: TVector2;
      procedure SetScale(const Value: TVector2);

    public
      constructor Create(AControl: TTakeItEasyControl; APiece: TTakeItEasy.TPiece);
      destructor Destroy; override;

      property Control: TTakeItEasyControl read FControl;

      property Tile: TSprite read FTile;
      property Piece: TTakeItEasy.TPiece read FPiece;

      property Location: TLocation2 read GetLocation;
      property Scale: TVector2 read GetScale write SetScale;

    end;

    TPlaceBehavior = class(TSprite.TBehavior)
    private
      FPiece: TPiece;
      FDragging: Boolean;
      FStartMousePos: TVector2;
      FStartTilePos: TVector2;
      FValidPos: Boolean;
      FPos: TTakeItEasy.TBoard.TPiecePos;

    protected
      procedure AddEvents; override;
      procedure RemoveEvents; override;

      procedure ButtonDown(AInfo: TButtonEventInfo);
      procedure ButtonUp(AInfo: TButtonEventInfo);
      procedure MouseMove;

    public
      constructor Create(APiece: TPiece); reintroduce;

      property Piece: TPiece read FPiece;

    end;

    TPlayer = class(TTakeItEasy.TPlayer)
    private
      FControl: TTakeItEasyControl;
      FTurn: TTakeItEasy.TPlayer.TTurn;
      FPiece: TPiece;

      procedure SetControl(const Value: TTakeItEasyControl);

    protected
      function BeginPlacePiece(APiece: TTakeItEasy.TPiece): TTakeItEasy.TPlayer.TTurn; override;

      property Control: TTakeItEasyControl read FControl write SetControl;

    public
      destructor Destroy; override;

      procedure TurnDone;

    end;

    TBackground = array [TTakeItEasy.TBoard.TPiecePos] of TSprite;

    TPieces = TObjectArray<TPiece>;

  private
    FBoard: TTakeItEasy.TBoard;
    FPlayer: TPlayer;
    FPieces: TPieces;
    FBackground: TBackground;
    FTileTexture: TTexTile;
    FTileBackgroundTexture: TTexTile;
    FLineTexture: TTexTile;
    FValueTexture: TTexTile;

    procedure SetBoard(const Value: TTakeItEasy.TBoard);

    procedure SetPlayer(const Value: TPlayer);

    procedure BoardPlacePiece(AInfo: TTakeItEasy.TBoard.TPlacePieceEventInfo);
    procedure BoardReset;

    function GetPieces: TPieces.TReader;

  protected
    function GetAspect: Single; override;

  public
    constructor Create(AParent: TControl); override;
    destructor Destroy; override;

    property TileTexture: TTexTile read FTileTexture;
    property TileBackgroundTexture: TTexTile read FTileBackgroundTexture;
    property LineTexture: TTexTile read FLineTexture;
    property ValueTexture: TTexTile read FValueTexture;

    property Board: TTakeItEasy.TBoard read FBoard write SetBoard;
    property Player: TPlayer read FPlayer write SetPlayer;

    property Pieces: TPieces.TReader read GetPieces;

  end;

implementation

{ TTakeItEasyControl }

procedure TTakeItEasyControl.BoardPlacePiece(AInfo: TTakeItEasy.TBoard.TPlacePieceEventInfo);
var
  Piece: TPiece;
begin
  Piece := FPieces.Add(TPiece.Create(Self, AInfo.Piece));
  Piece.Location.Pos := TTakeItEasy.TBoard.GetOffset(AInfo.Pos) * 0.2;
end;

procedure TTakeItEasyControl.BoardReset;
begin
  FPieces.Clear;
end;

constructor TTakeItEasyControl.Create(AParent: TControl);
var
  Pos: TTakeItEasy.TBoard.TPiecePos;
begin
  inherited;
  FPieces := TPieces.Create;

  FTileTexture := TextureAtlas[DefaultTileTextureName];
  FTileBackgroundTexture := TextureAtlas[DefaultTileBackgroundName];
  FLineTexture := TextureAtlas[DefaultLineTextureName];
  FValueTexture := TextureAtlas[DefaultValueTextureName];

  for Pos := Low(Pos) to High(Pos) do
  begin
    FBackground[Pos] := Add<TSprite>(TileBackgroundTexture);
    FBackground[Pos].Location.Scale := 0.2;
    FBackground[Pos].Location.Offset := TTakeItEasy.TBoard.GetOffset(Pos);
  end;
end;

destructor TTakeItEasyControl.Destroy;
begin
  FPieces.Free;
  inherited;
end;

function TTakeItEasyControl.GetAspect: Single;
begin
  Result := 1;
end;

function TTakeItEasyControl.GetPieces: TPieces.TReader;
begin
  Result := FPieces.Reader;
end;

procedure TTakeItEasyControl.SetBoard(const Value: TTakeItEasy.TBoard);
var
  PiecePos: TTakeItEasy.TBoard.TPiecePos;
  Piece: TPiece;
begin
  if Board = Value then
    Exit;

  FPieces.Clear;

  if Board <> nil then
  begin
    Board.OnPlacePiece.Remove(BoardPlacePiece);
    Board.OnReset.Remove(BoardReset);
  end;

  FBoard := Value;

  if Board <> nil then
  begin
    for PiecePos := Low(PiecePos) to High(PiecePos) do
    begin
      if Board[PiecePos].IsEmpty then
        Continue;
      Piece := FPieces.Add(TPiece.Create(Self, Board[PiecePos]));
      Piece.Location.Pos := TTakeItEasy.TBoard.GetOffset(PiecePos);
    end;
    Board.OnPlacePiece.Add(BoardPlacePiece);
    Board.OnReset.Add(BoardReset);
  end;
end;

procedure TTakeItEasyControl.SetPlayer(const Value: TPlayer);
begin
  if Player = Value then
    Exit;
  if Player <> nil then
  begin
    Board := nil;
  end;
  FPlayer := Value;
  if Player <> nil then
  begin
    Board := Player.Board;
    Player.Control := Self;
  end;
end;

{ TTakeItEasyControl.TPlayer }

function TTakeItEasyControl.TPlayer.BeginPlacePiece(APiece: TTakeItEasy.TPiece): TTakeItEasy.TPlayer.TTurn;
begin
  FPiece.Free;
  FPiece := TPiece.Create(Control, APiece);
  FPiece.Location.Pos := Vec2(0.375, -0.45);
  FPiece.Scale := 1.0;
  TPlaceBehavior.Create(FPiece);
  FTurn := TTurn.Create(Self);
  Result := FTurn;
end;

destructor TTakeItEasyControl.TPlayer.Destroy;
begin
  FPiece.Free;
  FTurn.Free;
  inherited;
end;

procedure TTakeItEasyControl.TPlayer.SetControl(const Value: TTakeItEasyControl);
begin
  Assert(Control = nil, 'Can only set player control once.');
  FControl := Value;
end;

procedure TTakeItEasyControl.TPlayer.TurnDone;
begin
  FTurn.Free;
  FTurn := nil;
end;

{ TTakeItEasyControl.TPiece }

constructor TTakeItEasyControl.TPiece.Create(AControl: TTakeItEasyControl; APiece: TTakeItEasy.TPiece);
var
  RowDir: TTakeItEasy.TRowDirection;
begin
  FControl := AControl;
  FPiece := APiece;

  FTile := Control.Add<TSprite>(Control.TileTexture);
  FTile.Location.Scale := 0.2;

  for RowDir := Low(RowDir) to High(RowDir) do
  begin
    FLines[RowDir] := Control.Add<TSprite>(Control.LineTexture);
    FLines[RowDir].Location.Rotation := Ord(RowDir) * 120;
    FLines[RowDir].Location.Parent := FTile.Location;
    FLines[RowDir].Color := RowColors[APiece[RowDir]];
    TSprite.TAnimationBeavior.Create(FLines[RowDir], 16, 0.5);

    FValues[RowDir] := Control.Add<TSprite>(Control.ValueTexture.SubTiles[APiece[RowDir] - 1]);
    FValues[RowDir].Location.Parent := FTile.Location;
    FValues[RowDir].Location.Scale := 0.25;
    FValues[RowDir].Location.Offset := Vec2(0, 1.3).Rotate(Ord(RowDir) * 120);
    // TSprite.TAnimationBeavior.Create(FValues[RowDir], 10, 1).Frame := APiece[RowDir] - 1;
  end;
end;

destructor TTakeItEasyControl.TPiece.Destroy;
var
  RowDir: TTakeItEasy.TRowDirection;
begin
  for RowDir := Low(RowDir) to High(RowDir) do
  begin
    FLines[RowDir].Remove;
    FValues[RowDir].Remove;
  end;
  FTile.Remove;
  inherited;
end;

function TTakeItEasyControl.TPiece.GetLocation: TLocation2;
begin
  Result := FTile.Location;
end;

function TTakeItEasyControl.TPiece.GetScale: TVector2;
begin
  Result := Location.Scale / 0.2;
end;

procedure TTakeItEasyControl.TPiece.SetScale(const Value: TVector2);
begin
  Location.Scale := Value * 0.2;
end;

{ TTakeItEasyControl.TPlaceBehavior }

procedure TTakeItEasyControl.TPlaceBehavior.AddEvents;
begin
  Game.Input.OnButtonDown.Add(ButtonDown);
  Game.Input.OnButtonUp.Add(ButtonUp);
  Game.Input.OnMouseMove.Add(MouseMove);
end;

procedure TTakeItEasyControl.TPlaceBehavior.ButtonDown(AInfo: TButtonEventInfo);
begin
  case AInfo.Button of
    mbLeft:
      if Sprite.Hittest(Game.Input.MousePos) then
      begin
        FDragging := True;
        FStartMousePos := FPiece.Control.Bounds.InvPoint[Game.Input.MousePos];
        FStartTilePos := Sprite.Location.Pos;
        Sprite.Color := TColorRGB.Gray(1.4);
      end;
  end;
end;

procedure TTakeItEasyControl.TPlaceBehavior.ButtonUp(AInfo: TButtonEventInfo);
begin
  case AInfo.Button of
    mbLeft:
      begin
        if FValidPos then
        begin
          Piece.Control.Board.PlacePiece(FPos, Piece.Piece);
          Piece.Control.Player.TurnDone;
          Remove;
        end
        else if FDragging then
        begin
          Sprite.Color := TColorRGB.Gray(1.2);
        end;
        FDragging := False;
      end;
  end;
end;

constructor TTakeItEasyControl.TPlaceBehavior.Create(APiece: TPiece);
begin
  inherited Create(APiece.Tile);
  FPiece := APiece;
end;

procedure TTakeItEasyControl.TPlaceBehavior.MouseMove;
var
  NewMousePos: TVector2;
  Pos: TTakeItEasy.TBoard.TPiecePos;
begin
  if FDragging then
  begin
    NewMousePos := FPiece.Control.Bounds.InvPoint[Game.Input.MousePos];
    Sprite.Location.Pos := FStartTilePos + (NewMousePos - FStartMousePos);
    FValidPos := False;
    for Pos := Low(Pos) to High(Pos) do
    begin
      if not Piece.Control.Board[Pos].IsEmpty then
        Continue;

      if Sprite.Location.Pos.DistanceTo(TTakeItEasy.TBoard.GetOffset(Pos) * 0.2) < 0.05 then
      begin
        Sprite.Location.Pos := TTakeItEasy.TBoard.GetOffset(Pos) * 0.2;
        FPos := Pos;
        FValidPos := True;
        Break;
      end;
    end;
  end
  else if Sprite.Hittest(Game.Input.MousePos) then
  begin
    Sprite.Color := TColorRGB.Gray(1.2);
  end
  else
  begin
    Sprite.Color := ColorWhite;
  end;
end;

procedure TTakeItEasyControl.TPlaceBehavior.RemoveEvents;
begin
  Game.Input.OnButtonDown.Remove(ButtonDown);
  Game.Input.OnButtonUp.Remove(ButtonUp);
  Game.Input.OnMouseMove.Remove(MouseMove);
end;

end.
