unit TakeItEasy.Game;

interface

uses
  System.SysUtils,
  System.Threading,

  Pengine.Collections,
  Pengine.Equaller,
  Pengine.Hasher,
  Pengine.EventHandling,
  Pengine.Vector,
  Pengine.IntMaths;

type

  {
    -       11
    -     6    15
    -  2    10    18
    -     5    14
    -  1     9    17
    -     4    13
    -  0     8    16
    -     3    12
    -        7
 }

  TTakeItEasy = class
  public type

    TState = (
      gsGameOver,
      gsTurnEnd,
      gsWaitingForInput
      );

    TRowDirection = (
      rdVertical,
      rdDiagRight,
      rdDiagLeft
      );

    TRowIndex = 0 .. 4;

    TRowValueZero = 0 .. 9;

    TRowValue = 1 .. 9;

  public const

    RowValues: array [TRowDirection, 0 .. 2] of TRowValue = (
      (1, 5, 9),
      (2, 6, 7),
      (3, 4, 8)
      );

  public type

    TPiece = record
    private
      FValues: array [TRowDirection] of TRowValueZero;

      function GetRow(ARowDirection: TRowDirection): TRowValueZero;

    public
      constructor Create(AVertical, ADiagRight, ADiagLeft: TRowValue);

      class operator Equal(A, B: TPiece): Boolean;

      class function Empty: TPiece; static;
      function IsEmpty: Boolean;

      property Rows[ARowDirection: TRowDirection]: TRowValueZero read GetRow; default;

    end;

    TPieceEqualler = class(TEqualler<TPiece>)
    public
      class function Equal(const AValue1, AValue2: TPiece): Boolean; override;

    end;

    TPieceHasher = class(THasher<TPiece, TPieceEqualler>)
    public
      class function GetHash(const AValue: TPiece): Cardinal; override;

    end;

    TBoard = class
    public const

      FieldCount = 3 + 4 + 5 + 4 + 3;

    public type

      TPiecePos = 0 .. FieldCount - 1;

      TPiecePosArray = System.TArray<TPiecePos>;

      TEventInfo = TSenderEventInfo<TBoard>;

      TEvent = TEvent<TEventInfo>;

      TPlacePieceEventInfo = class(TEventInfo)
      private
        FPos: TPiecePos;

        function GetPiece: TPiece;

      public
        constructor Create(ASender: TBoard; APos: TPiecePos);

        property Pos: TPiecePos read FPos;
        property Piece: TPiece read GetPiece;

      end;

      TPlacePieceEvent = TEvent<TPlacePieceEventInfo>;

    public const

      RowLength: array [TRowIndex] of Integer = (3, 4, 5, 4, 3);

      VisualPiecePos: array [TPiecePos] of TIntVector2 = (
        (X: - 2; Y: + 0),
        (X: - 2; Y: + 1),
        (X: - 2; Y: + 2),

        (X: - 1; Y: - 1),
        (X: - 1; Y: + 0),
        (X: - 1; Y: + 1),
        (X: - 1; Y: + 2),

        (X: + 0; Y: - 2),
        (X: + 0; Y: - 1),
        (X: + 0; Y: + 0),
        (X: + 0; Y: + 1),
        (X: + 0; Y: + 2),

        (X: + 1; Y: - 2),
        (X: + 1; Y: - 1),
        (X: + 1; Y: + 0),
        (X: + 1; Y: + 1),

        (X: + 2; Y: - 2),
        (X: + 2; Y: - 1),
        (X: + 2; Y: + 0)
        );

    private
    class var

      FPiecePos: array [TRowDirection, TRowIndex] of TPiecePosArray;
      FRowIndices: array [TRowDirection, TPiecePos] of TRowIndex;

    private
      FFieldsLeft: Integer;
      FMap: array [TPiecePos] of TPiece;
      FRows: array [TRowDirection, TRowIndex] of Integer;
      FOnPlacePiece: TPlacePieceEvent;
      FOnReset: TEvent;

      class function GetPiecePos(ADir: TRowDirection; AIndex: TRowIndex; APos: Integer): TPiecePos; static;
      class function GetRowIndex(ADir: TRowDirection; APos: TPiecePos): TRowIndex; static;

      function GetPiece(ADir: TRowDirection; ARow: TRowIndex; AIndex: Integer): TPiece; overload;
      function GetPiece(APiecePos: TPiecePos): TPiece; overload;
      function GetFieldsFilled: Integer;

    public
      class constructor Create;
      constructor Create;

      procedure Reset;

      class property PiecePos[ADir: TRowDirection; AIndex: TRowIndex; APos: Integer]: TPiecePos read GetPiecePos;
      class property RowIndices[ADir: TRowDirection; APos: TPiecePos]: TRowIndex read GetRowIndex;
      class function GetOffset(APos: TPiecePos): TVector2;

      property Map[ADir: TRowDirection; ARow: TRowIndex; AIndex: Integer]: TPiece read GetPiece; default;
      property Map[APiecePos: TPiecePos]: TPiece read GetPiece; default;

      procedure PlacePiece(ADir: TRowDirection; ARow: TRowIndex; AIndex: Integer; APiece: TPiece); overload;
      procedure PlacePiece(APos: TPiecePos; APiece: TPiece); overload;

      property FieldsLeft: Integer read FFieldsLeft;
      property FieldsFilled: Integer read GetFieldsFilled;
      function IsEmpty: Boolean;
      function IsFilled: Boolean;

      function CalculateScore: Integer;

      function OnPlacePiece: TPlacePieceEvent.TAccess;
      function OnReset: TEvent.TAccess;

    end;

    TPlayer = class
    public type

      TEventInfo = TSenderEventInfo<TPlayer>;

      TEvent = TEvent<TEventInfo>;

      TTurn = class
      private
        FPlayer: TPlayer;
        FOnDone: TEvent;

      public
        constructor Create(APlayer: TPlayer);
        destructor Destroy; override;

        property Player: TPlayer read FPlayer;

        procedure Done;

        function OnDone: TEvent.TAccess;

      end;

    private
      FGame: TTakeItEasy;
      FIndex: Integer;
      FBoard: TBoard;

    protected
      /// <returns>A <c>TTurn</c> object or <c>nil</c> if the turn got finished immediately.</returns>
      function BeginPlacePiece(APiece: TPiece): TTurn; virtual; abstract;

    public
      constructor Create(AGame: TTakeItEasy); virtual;
      destructor Destroy; override;

      property Game: TTakeItEasy read FGame;
      property Index: Integer read FIndex;

      property Board: TBoard read FBoard;

    end;

    TPlayerClass = class of TPlayer;

    TPlayers = TObjectArray<TPlayer>;

    TPieces = TArray<TPiece>;

  private
    FState: TState;
    FPlayers: TPlayers;
    FPlayersWaiting: Integer;
    FPieces: TPieces;
    FPieceIndex: Integer;

    function GetCurrentPiece: TPiece;

    procedure PlayerTurnDone(AInfo: TPlayer.TEventInfo);
    function GetPlayers: TPlayers.TReader;

    procedure EndTurn;
    function GetPieces: TPieces.TReader;

    procedure GeneratePieces;

  public
    constructor Create;
    destructor Destroy; override;

    procedure RemovePlayers;
    function AddPlayer(APlayer: TPlayerClass): TPlayer; overload;
    function AddPlayer<T: TPlayer>: T; overload;

    property Players: TPlayers.TReader read GetPlayers;

    procedure Start;
    procedure Turn;
    procedure PlayAll;

    property State: TState read FState;

    property Pieces: TPieces.TReader read GetPieces;
    property CurrentPiece: TPiece read GetCurrentPiece;

  end;

implementation

{ TTakeItEasy.TPiece }

constructor TTakeItEasy.TPiece.Create(AVertical, ADiagRight, ADiagLeft: TRowValue);
begin
  FValues[rdVertical] := AVertical;
  FValues[rdDiagRight] := ADiagRight;
  FValues[rdDiagLeft] := ADiagLeft;
end;

class function TTakeItEasy.TPiece.Empty: TPiece;
begin
  FillChar(Result, SizeOf(TPiece), 0);
end;

class operator TTakeItEasy.TPiece.Equal(A, B: TPiece): Boolean;
begin
  Result := (A[rdDiagRight] = B[rdDiagRight]) and (A[rdDiagLeft] = B[rdDiagLeft]) and (A[rdVertical] = B[rdVertical]);
end;

function TTakeItEasy.TPiece.GetRow(ARowDirection: TRowDirection): TRowValueZero;
begin
  Result := FValues[ARowDirection];
end;

function TTakeItEasy.TPiece.IsEmpty: Boolean;
begin
  Result := FValues[rdDiagLeft] = 0;
end;

{ TTakeItEasy.TBoard }

function TTakeItEasy.TBoard.CalculateScore: Integer;
var
  Dir: TRowDirection;
  Index: TRowIndex;
begin
  if FFieldsLeft = 0 then
  begin
    Result := 0;
    for Dir := Low(TRowDirection) to High(TRowDirection) do
      for Index := Low(TRowIndex) to High(TRowIndex) do
        Inc(Result, FRows[Dir, Index] * RowLength[Index]);
  end
  else
  begin
    Result := 0;
  end;
end;

class constructor TTakeItEasy.TBoard.Create;

  function FindRow(ADir: TRowDirection; APos: TPiecePos): TRowIndex;
  var
    Index: TRowIndex;
    I: Integer;
  begin
    for Index := Low(TRowIndex) to High(TRowIndex) do
      for I := 0 to Length(FPiecePos[ADir, Index]) do
        if FPiecePos[ADir, Index][I] = APos then
          Exit(Index);
    raise Exception.Create('PiecePos not found.');
  end;

var
  Dir: TRowDirection;
  Index: TRowIndex;
  PiecePos: TPiecePos;
begin
  for Dir := Low(TRowDirection) to High(TRowDirection) do
    for Index := Low(TRowIndex) to High(TRowIndex) do
      SetLength(FPiecePos[Dir, Index], RowLength[Index]);

  {
    -       11
    -     6    15
    -  2    10    18
    -     5    14
    -  1     9    17
    -     4    13
    -  0     8    16
    -     3    12
    -        7
 }

  FPiecePos[rdDiagRight, 0] := TPiecePosArray.Create(7, 12, 16);
  FPiecePos[rdDiagRight, 1] := TPiecePosArray.Create(3, 8, 13, 17);
  FPiecePos[rdDiagRight, 2] := TPiecePosArray.Create(0, 4, 9, 14, 18);
  FPiecePos[rdDiagRight, 3] := TPiecePosArray.Create(1, 5, 10, 15);
  FPiecePos[rdDiagRight, 4] := TPiecePosArray.Create(2, 6, 11);

  FPiecePos[rdDiagLeft, 0] := TPiecePosArray.Create(7, 3, 0);
  FPiecePos[rdDiagLeft, 1] := TPiecePosArray.Create(12, 8, 4, 1);
  FPiecePos[rdDiagLeft, 2] := TPiecePosArray.Create(16, 13, 9, 5, 2);
  FPiecePos[rdDiagLeft, 3] := TPiecePosArray.Create(17, 14, 10, 6);
  FPiecePos[rdDiagLeft, 4] := TPiecePosArray.Create(18, 15, 11);

  FPiecePos[rdVertical, 0] := TPiecePosArray.Create(0, 1, 2);
  FPiecePos[rdVertical, 1] := TPiecePosArray.Create(3, 4, 5, 6);
  FPiecePos[rdVertical, 2] := TPiecePosArray.Create(7, 8, 9, 10, 11);
  FPiecePos[rdVertical, 3] := TPiecePosArray.Create(12, 13, 14, 15);
  FPiecePos[rdVertical, 4] := TPiecePosArray.Create(16, 17, 18);

  for Dir := Low(TRowDirection) to High(TRowDirection) do
    for PiecePos := Low(TPiecePos) to High(TPiecePos) do
      FRowIndices[Dir, PiecePos] := FindRow(Dir, PiecePos);
end;

constructor TTakeItEasy.TBoard.Create;
begin
  Reset;
end;

function TTakeItEasy.TBoard.GetFieldsFilled: Integer;
begin
  Result := FieldCount - FieldsLeft;
end;

class function TTakeItEasy.TBoard.GetOffset(APos: TPiecePos): TVector2;
begin
  Result :=
    Vec2(VisualPiecePos[APos].X, 0).Rotate(30) +
    Vec2(0, VisualPiecePos[APos].Y);
end;

function TTakeItEasy.TBoard.GetPiece(ADir: TRowDirection; ARow: TRowIndex; AIndex: Integer): TPiece;
begin
  Result := FMap[PiecePos[ADir, ARow, AIndex]];
end;

function TTakeItEasy.TBoard.GetPiece(APiecePos: TPiecePos): TPiece;
begin
  Result := FMap[APiecePos];
end;

class function TTakeItEasy.TBoard.GetPiecePos(ADir: TRowDirection; AIndex: TRowIndex; APos: Integer): TPiecePos;
begin
  Result := FPiecePos[ADir, AIndex][APos];
end;

class function TTakeItEasy.TBoard.GetRowIndex(ADir: TRowDirection; APos: TPiecePos): TRowIndex;
begin
  Result := FRowIndices[ADir, APos];
end;

function TTakeItEasy.TBoard.IsEmpty: Boolean;
begin
  Result := FieldsFilled = 0;
end;

function TTakeItEasy.TBoard.IsFilled: Boolean;
begin
  Result := FieldsLeft = 0;
end;

function TTakeItEasy.TBoard.OnPlacePiece: TPlacePieceEvent.TAccess;
begin
  Result := FOnPlacePiece.Access;
end;

function TTakeItEasy.TBoard.OnReset: TEvent.TAccess;
begin
  Result := FOnReset.Access;
end;

procedure TTakeItEasy.TBoard.PlacePiece(APos: TPiecePos; APiece: TPiece);
var
  Dir: TRowDirection;
begin
  Dec(FFieldsLeft);
  FMap[APos] := APiece;
  for Dir := Low(TRowDirection) to High(TRowDirection) do
  begin
    if FRows[Dir, RowIndices[Dir, APos]] = -1 then
      FRows[Dir, RowIndices[Dir, APos]] := APiece[Dir]
    else if FRows[Dir, RowIndices[Dir, APos]] <> APiece[Dir] then
      FRows[Dir, RowIndices[Dir, APos]] := 0;
  end;
  if FOnPlacePiece.HasHandler then
    FOnPlacePiece.Execute(TPlacePieceEventInfo.Create(Self, APos));
end;

procedure TTakeItEasy.TBoard.Reset;
var
  Pos: TPiecePos;
  RowDir: TRowDirection;
  Index: TRowIndex;
begin
  FFieldsLeft := FieldCount;
  for Pos := Low(Pos) to High(Pos) do
    FMap[Pos] := TPiece.Empty;
  for RowDir := Low(RowDir) to High(RowDir) do
    for Index := Low(Index) to High(Index) do
      FRows[RowDir, Index] := -1;
  if FOnReset.HasHandler then
    FOnReset.Execute(TEventInfo.Create(Self));
end;

procedure TTakeItEasy.TBoard.PlacePiece(ADir: TRowDirection; ARow: TRowIndex; AIndex: Integer; APiece: TPiece);
begin
  PlacePiece(PiecePos[ADir, ARow, AIndex], APiece);
end;

{ TTakeItEasy }

function TTakeItEasy.AddPlayer(APlayer: TPlayerClass): TPlayer;
begin
  Result := FPlayers.Add(APlayer.Create(Self));
end;

function TTakeItEasy.AddPlayer<T>: T;
begin
  Result := T.Create(Self);
  FPlayers.Add(Result);
end;

constructor TTakeItEasy.Create;
begin
  FPlayers := TPlayers.Create;
  FPieces := TPieces.Create;
  GeneratePieces;
end;

destructor TTakeItEasy.Destroy;
begin
  FPieces.Free;
  FPlayers.Free;
  inherited;
end;

procedure TTakeItEasy.EndTurn;
begin
  if FPieceIndex = TBoard.FieldCount - 1 then
    FState := gsGameOver
  else
    FState := gsTurnEnd;
end;

procedure TTakeItEasy.GeneratePieces;
var
  Pos: TIntVector3;
begin
  for Pos in IVec3(3) do
    FPieces.Add(TPiece.Create(
      RowValues[rdVertical, Pos.X],
      RowValues[rdDiagRight, Pos.Y],
      RowValues[rdDiagLeft, Pos.Z])
      );
end;

function TTakeItEasy.GetCurrentPiece: TPiece;
begin
  Result := FPieces[FPieceIndex];
end;

function TTakeItEasy.GetPieces: TPieces.TReader;
begin
  Result := FPieces.Reader;
end;

function TTakeItEasy.GetPlayers: TPlayers.TReader;
begin
  Result := FPlayers.Reader;
end;

procedure TTakeItEasy.PlayAll;
begin
  while State = gsTurnEnd do
    Turn;
end;

procedure TTakeItEasy.PlayerTurnDone(AInfo: TPlayer.TEventInfo);
begin
  Dec(FPlayersWaiting);
  if FPlayersWaiting = 0 then
    EndTurn;
end;

procedure TTakeItEasy.RemovePlayers;
begin
  FPlayers.Clear;
end;

procedure TTakeItEasy.Start;
var
  Player: TPlayer;
begin
  FState := gsTurnEnd;
  FPieces.Shuffle;
  FPieceIndex := -1;
  for Player in Players do
    Player.Board.Reset;
end;

procedure TTakeItEasy.Turn;
var
  Player: TPlayer;
  Turn: TPlayer.TTurn;
begin
  Assert(State = gsTurnEnd, 'Turn not over.');
  Inc(FPieceIndex);
  for Player in FPlayers do
  begin
    Turn := Player.BeginPlacePiece(CurrentPiece);
    if Turn <> nil then
    begin
      FState := gsWaitingForInput;
      Turn.OnDone.Add(PlayerTurnDone);
      Inc(FPlayersWaiting);
    end;
  end;
  if State = gsTurnEnd then
    EndTurn;
end;

{ TTakeItEasy.TPlayer }

constructor TTakeItEasy.TPlayer.Create(AGame: TTakeItEasy);
begin
  FGame := AGame;
  FBoard := TBoard.Create;
  FIndex := Game.Players.Count;
end;

destructor TTakeItEasy.TPlayer.Destroy;
begin
  FBoard.Free;
  inherited;
end;

{ TTakeItEasy.TPlayer.TTurn }

constructor TTakeItEasy.TPlayer.TTurn.Create(APlayer: TPlayer);
begin
  FPlayer := APlayer;
end;

destructor TTakeItEasy.TPlayer.TTurn.Destroy;
begin
  FOnDone.Execute(TEventInfo.Create(Player));
  inherited;
end;

procedure TTakeItEasy.TPlayer.TTurn.Done;
begin
  Free;
end;

function TTakeItEasy.TPlayer.TTurn.OnDone: TEvent.TAccess;
begin
  Result := FOnDone.Access;
end;

{ TTakeItEasy.TPieceHasher }

class function TTakeItEasy.TPieceHasher.GetHash(const AValue: TPiece): Cardinal;
begin
  Result := AValue[rdDiagRight] or AValue[rdDiagLeft] shl 4 or AValue[rdVertical] shl 8;
end;

{ TTakeItEasy.TPieceEqualler }

class function TTakeItEasy.TPieceEqualler.Equal(const AValue1, AValue2: TPiece): Boolean;
begin
  Result := AValue1 = AValue2;
end;

{ TTakeItEasy.TBoard.TPlacePieceEventInfo }

constructor TTakeItEasy.TBoard.TPlacePieceEventInfo.Create(ASender: TBoard; APos: TPiecePos);
begin
  inherited Create(ASender);
  FPos := APos;
end;

function TTakeItEasy.TBoard.TPlacePieceEventInfo.GetPiece: TPiece;
begin
  Result := Sender[Pos];
end;

end.
