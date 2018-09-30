unit Pengine.TextureAtlas;

interface

uses
  System.SysUtils,
  System.Math,
  System.RegularExpressions,
  System.IOUtils,
  System.JSON, // TODO: Replace with Pengine.JSON
  System.Generics.Collections,
  System.Classes,

  Winapi.Windows,

  Pengine.Color,
  Pengine.Collections,
  Pengine.GLState,
  Pengine.IntMaths,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.Texture,
  Pengine.EventHandling,
  Pengine.Vector,
  Pengine.Bitfield,
  Pengine.GLProgram,
  Pengine.CollectionInterfaces;

type

  ETextureAtlasTileExists = class(Exception)
  public
    constructor Create;
  end;

  ETextureAtlasTileNotFound = class(Exception)
  public
    constructor Create;
  end;

  ETextureAtlasSubTypesLocked = class(Exception)
  public
    constructor Create;
  end;

  ETextureAtlasInvalidInfoFile = class(Exception)
  end;

  TTextureAtlas = class
  public type

    TFreeRows = class
    public type

      TRows = TArray<TIntBounds1>;

    private
      FRows: TRows;
      FSize: Integer;

      function GetRows: TRows.TReader;

      procedure SetSize(const Value: Integer);

    public
      constructor Create(ASize: Integer = 0);
      destructor Destroy; override;

      property Rows: TRows.TReader read GetRows;
      property Size: Integer read FSize write SetSize;

      function Find(AStart, ASize: Integer; out APos: Integer): Boolean;

      procedure Fill(ARange: TIntBounds1);

      procedure Clear(ARange: TIntBounds1); overload;
      procedure Clear; overload;

    end;

    TFreeSpace = class
    public type

      TRows = TObjectArray<TFreeRows>;

    private
      FRows: TRows;
      FSize: TIntVector2;

      procedure SetSize(const Value: TIntVector2);

    public
      constructor Create; overload;
      destructor Destroy; override;

      property Size: TIntVector2 read FSize write SetSize;

      procedure Clear; overload;
      procedure Clear(ABounds: TIntBounds2); overload;
      function FindFill(ASize: TIntVector2; out APos: TIntVector2): Boolean;

      function Format: string;

    end;

    TInfo = class
    public type

      TMode = (tmNone, tmTextures, tmGrid, tmFont, tmRegions);
      TValues = TMap<string, Single, TStringHasher>;
      TRegions = TArray<TBounds2>;

    private
      FMode: TMode;
      // Grid
      FGrid: array [TCoordAxis2] of TSingleArray;
      // Font
      FWidths: array [Byte] of Single;
      // Regions
      // TODO: FRegions: TRegions;

      FValues: TValues;

      procedure LoadGrid(ATexture: TTextureData; ATextures: TArray<TTextureData>);
      procedure LoadFont(ATexture: TTextureData; ATextures: TArray<TTextureData>);
      procedure LoadRegions(ATexture: TTextureData; ATextures: TArray<TTextureData>);

    public
      constructor Create(AInfo: string);
      destructor Destroy; override;

      function GetTextureArray(ATexture: TTextureData): TArray<TTextureData>;

      function OwnValues: TValues;

    end;

    TTile = class
    public type

      TState = (
        tsInitial,
        tsPositioned,
        tsAdded
        );

      TSubTextures = TObjectArray<TTextureData>;

      TSubTiles = TRefArray<TTile>;

      TInfoValues = TMap<string, Single, TStringHasher>;

    public const

      CharSpacingPixelsIdentifier = 'CharSpacingPixels';

    private
      FAtlas: TTextureAtlas;
      FSubTiles: TSubTiles;
      FSubTileIndex: Integer;
      FName: string;
      FTexture: TTextureData;
      FSubTextures: TSubTextures;
      FState: TState;
      FPosition: TIntVector2;
      FInfoValues: TInfoValues;

      function GetSubTextures: TSubTextures.TReader;

      function GetSubTiles: TSubTiles.TReader;
      function GetSubTile(AIndex: Integer): TTile;

      function GetPosition: TIntVector2;

      function GetTexelBounds: TIntBounds2;
      function GetBounds: TBounds2;
      function GetBoundsHalfPixelInset: TBounds2;

      function GetSize: TIntVector2;
      function GetAspect: Single;

      function GetInfoValues: TInfoValues.TReader;

    public
      constructor Create(AAtlas: TTextureAtlas; AName: string; ATexture: TTextureData; AInfo: TInfo = nil); overload;
      constructor Create(AAtlas: TTextureAtlas; AName: string; ATextures: IIterable<TTextureData>); overload;
      destructor Destroy; override;

      property Atlas: TTextureAtlas read FAtlas;
      property Name: string read FName;

      property Texture: TTextureData read FTexture;
      property SubTextures: TSubTextures.TReader read GetSubTextures;

      function HasSubTiles: Boolean;
      property SubTiles: TSubTiles.TReader read GetSubTiles;
      property SubTilesHelper[AIndex: Integer]: TTile read GetSubTile; default;
      property SubTileIndex: Integer read FSubTileIndex;

      property Position: TIntVector2 read GetPosition;

      property TexelBounds: TIntBounds2 read GetTexelBounds;
      property Bounds: TBounds2 read GetBounds;
      property BoundsHalfPixelInset: TBounds2 read GetBoundsHalfPixelInset;

      property Size: TIntVector2 read GetSize;
      property Aspect: Single read GetAspect;

      property InfoValues: TInfoValues.TReader read GetInfoValues;

      function TextWidth(AText: string): Single;

    end;

    TTiles = TToObjectMap<string, TTile, TStringHasher>;

    TSubType = class
    private
      FTexture: TTexture2D;
      FDefaultColor: TColorRGBA;
      FSuffix: string;

    public
      constructor Create(AGLState: TGLState; ASuffix: string; ADefaultColor: TColorRGB);
      destructor Destroy; override;

      property DefaultColor: TColorRGBA read FDefaultColor;
      property Texture: TTexture2D read FTexture;
      property Suffix: string read FSuffix;

      procedure Uniform(ASampler: TGLProgram.TUniformSampler);

    end;

    TSubTypes = TObjectArray<TSubType>;

  private
    FTexture: TTexture2D;
    FSubTypes: TSubTypes;
    FSubTypesLocked: Boolean;
    FFreeSpace: TFreeSpace;
    FTiles: TTiles;
    FOnChanged: TEvent;

    function GetGLState: TGLState;
    function GetTile(AName: string): TTile;

    function GetMinSize: Integer;
    function GetSize: Integer;

    procedure Resize(ASize: TIntVector2);
    function GetSubTypes: TSubTypes.TReader;

    function GetOnChanged: TEvent.TAccess; inline;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    property GLState: TGLState read GetGLState;

    property Size: Integer read GetSize;

    function AddFromFile(AName: string; AFileName: string): TTile;
    function AddFromResource(AName: string; AResource: string): TTile;
    function Add(AName: string; ATexture: TTextureData; AInfo: TInfo = nil): TTile; overload;
    function Add(AName: string; ATextures: IIterable<TTextureData>): TTile; overload;
    procedure Remove(AName: string);

    function AddSubType(ASuffix: string; ASampler: TGLProgram.TUniformSampler; ADefaultColor: TColorRGBA): TSubType;
    property SubTypes: TSubTypes.TReader read GetSubTypes;

    property Texture: TTexture2D read FTexture;

    property Tiles[AName: string]: TTile read GetTile; default;

    function HalfPixelInset(ABounds: TBounds2): TBounds2;

    procedure Generate(ARegenerate: Boolean = False);

    procedure Uniform(ASampler: TGLProgram.TUniformSampler);

    property OnChanged: TEvent.TAccess read GetOnChanged;

  end;

  TTexTile = TTextureAtlas.TTile;

implementation

{ TTextureAtlas.TTile }

constructor TTextureAtlas.TTile.Create(AAtlas: TTextureAtlas; AName: string; ATexture: TTextureData; AInfo: TInfo);
var
  I: Integer;
  SubTile: TTile;
  Values: TInfoValues;
  Textures: TArray<TTextureData>;
begin
  if AInfo = nil then
  begin
    FAtlas := AAtlas;
    FName := AName;
    FTexture := ATexture;
    FSubTextures := TSubTextures.Create;
    for I := 0 to Atlas.SubTypes.MaxIndex do
      FSubTextures.Add(nil);
  end
  else
  begin
    Textures := AInfo.GetTextureArray(ATexture);
    Create(AAtlas, AName, Textures);
    Textures.Free;
    Values := AInfo.OwnValues;
    for SubTile in FSubTiles do
      SubTile.FInfoValues := Values;
  end;
end;

constructor TTextureAtlas.TTile.Create(AAtlas: TTextureAtlas; AName: string; ATextures: IIterable<TTextureData>);
var
  First: Boolean;
  Texture: TTextureData;
  SubTile: TTile;
  Index: Integer;
begin
  FSubTiles := TSubTiles.Create;
  if ATextures.CountOptimized then
    FSubTiles.Capacity := ATextures.Count;
  First := True;
  Index := 1;
  for Texture in ATextures do
  begin
    if First then
    begin
      Create(AAtlas, AName, Texture);
      FSubTiles.Add(Self);
      First := False;
      Continue;
    end;
    SubTile := FSubTiles.Add(TTile.Create(AAtlas, AName, Texture));
    SubTile.FSubTiles := FSubTiles;
    SubTile.FSubTileIndex := Index;
    Inc(Index);
  end;
end;

destructor TTextureAtlas.TTile.Destroy;
var
  I: Integer;
begin
  if HasSubTiles and (Self = FSubTiles[0]) then
  begin
    for I := 1 to FSubTiles.MaxIndex do
      FSubTiles[I].Free;
    FSubTiles.Free;
    FInfoValues.Free;
  end;
  FSubTextures.Free;
  FTexture.Free;
  inherited;
end;

function TTextureAtlas.TTile.GetAspect: Single;
begin
  Result := Size.X / Size.Y;
end;

function TTextureAtlas.TTile.GetBounds: TBounds2;
begin
  Result := TBounds2(TexelBounds) / Atlas.Size;
end;

function TTextureAtlas.TTile.GetBoundsHalfPixelInset: TBounds2;
begin
  Result := Atlas.HalfPixelInset(Bounds);
end;

function TTextureAtlas.TTile.GetInfoValues: TInfoValues.TReader;
begin
  Result := FInfoValues.Reader;
end;

function TTextureAtlas.TTile.GetPosition: TIntVector2;
begin
  if FState = tsInitial then
    Atlas.Generate;
  Result := FPosition;
end;

function TTextureAtlas.TTile.GetSize: TIntVector2;
begin
  Result := FTexture.Size;
end;

function TTextureAtlas.TTile.GetSubTextures: TSubTextures.TReader;
begin
  Result := FSubTextures.Reader;
end;

function TTextureAtlas.TTile.GetSubTile(AIndex: Integer): TTile;
begin
  Result := FSubTiles[AIndex];
end;

function TTextureAtlas.TTile.GetSubTiles: TSubTiles.TReader;
begin
  Result := FSubTiles.Reader;
end;

function TTextureAtlas.TTile.GetTexelBounds: TIntBounds2;
begin
  Result := IBounds2(Position, Position + Texture.Size);
end;

function TTextureAtlas.TTile.HasSubTiles: Boolean;
begin
  Result := FSubTiles <> nil;
end;

function TTextureAtlas.TTile.TextWidth(AText: string): Single;
var
  I: Integer;
  PixelSpacing: Single;
  Tile: TTile;
begin
  Result := 0;
  PixelSpacing := 0;
  if InfoValues <> nil then
    InfoValues.Get(CharSpacingPixelsIdentifier, PixelSpacing);
  if AText <> '' then
  begin
    for I := 1 to Length(AText) - 1 do
    begin
      Tile := SubTiles[Ord(AText[I])];
      Result := Result + (Tile.Size.X + PixelSpacing) / Tile.Size.Y;
    end;
    Result := Result + SubTiles[Ord(AText[Length(AText)])].Aspect;
  end;
end;

{ TTextureAtlas }

function TTextureAtlas.Add(AName: string; ATexture: TTextureData; AInfo: TInfo): TTile;
begin
  FSubTypesLocked := True;
  if FTiles.KeyExists(AName) then
    raise ETextureAtlasTileExists.Create;
  Result := TTile.Create(Self, AName, ATexture, AInfo);
  FTiles[AName] := Result;
end;

function TTextureAtlas.Add(AName: string; ATextures: IIterable<TTextureData>): TTile;
begin
  FSubTypesLocked := True;
  if FTiles.KeyExists(AName) then
    raise ETextureAtlasTileExists.Create;
  Result := TTile.Create(Self, AName, ATextures);
  FTiles[AName] := Result;
end;

function TTextureAtlas.AddFromFile(AName: string; AFileName: string): TTile;
var
  Data: TTextureData;
  I: Integer;
  FileName: string;
  Info: TInfo;
begin
  Data := TTextureData.CreateFromFile(AFileName);
  FileName := TRegex.Replace(AFileName, '\.\w+$', '.info');
  if TFile.Exists(FileName) then
  begin
    // load with .info file
    try
      Info := TInfo.Create(TFile.ReadAllText(FileName));
      Result := Add(AName, Data, Info);
      Info.Free;
    finally
      Data.Free;
    end;
  end
  else
  begin
    // load without .info file
    Result := Add(AName, Data);
    for I := 0 to FSubTypes.MaxIndex do
    begin
      FileName := TRegex.Replace(AFileName, '\.\w+$', '_' + TRegex.Escape(FSubTypes[I].Suffix) + '$0');
      if TFile.Exists(FileName) then
        Result.FSubTextures[I] := TTextureData.CreateFromFile(FileName);
    end;
  end;
end;

function TTextureAtlas.AddFromResource(AName: string; AResource: string): TTile;
var
  I: Integer;
  Resource: string;
  Info: TInfo;
  InfoText: TStrings;
  ResourceStream: TResourceStream;
  Data: TTextureData;
begin
  InfoText := nil;
  ResourceStream := nil;
  Info := nil;
  Data := TTextureData.CreateFromResource(AResource);
  Resource := AResource + '_INFO';
  if FindResource(HInstance, PWideChar(Resource), RT_RCDATA) <> 0 then
  begin
    // load with .info file
    try
      ResourceStream := TResourceStream.Create(HInstance, PWideChar(Resource), RT_RCDATA);
      InfoText := TStringList.Create;
      InfoText.LoadFromStream(ResourceStream);
      Info := TInfo.Create(InfoText.Text);
      Result := Add(AName, Data, Info);
    finally
      Info.Free;
      InfoText.Free;
      ResourceStream.Free;
      Data.Free;
    end;
  end
  else
  begin
    // load without .info file
    Result := Add(AName, Data);
    for I := 0 to FSubTypes.MaxIndex do
    begin
      Resource := AResource + '_' + FSubTypes[I].Suffix;
      if FindResource(HInstance, PWideChar(Resource), RT_RCDATA) <> 0 then
        Result.FSubTextures[I] := TTextureData.CreateFromResource(Resource);
    end;
  end;
end;

function TTextureAtlas.AddSubType(ASuffix: string; ASampler: TGLProgram.TUniformSampler; ADefaultColor: TColorRGBA): TSubType;
begin
  if FSubTypesLocked then
    raise ETextureAtlasSubTypesLocked.Create;
  Result := FSubTypes.Add(TSubType.Create(GLState, ASuffix, ADefaultColor));
  Result.Uniform(ASampler);
end;

constructor TTextureAtlas.Create(AGLState: TGLState);
begin
  FTexture := TTexture2D.Create(AGLState);
  FSubTypes := TSubTypes.Create;
  FTiles := TTiles.Create;
  FFreeSpace := TFreeSpace.Create;
end;

procedure TTextureAtlas.Remove(AName: string);
begin
  if not FTiles.TryRemove(AName) then
    raise ETextureAtlasTileNotFound.Create;
end;

destructor TTextureAtlas.Destroy;
begin
  FFreeSpace.Free;
  FTiles.Free;
  FSubTypes.Free;
  FTexture.Free;
  inherited;
end;

procedure TTextureAtlas.Generate(ARegenerate: Boolean);
var
  Tile, SubTile: TTile;
  MinSize: Integer;
  TileArray: TArray<TTile>;
  Changed: Boolean;

  procedure PositionTile(ATile: TTile);
  begin
    if ATile.FState >= tsPositioned then
      Exit;

    while not FFreeSpace.FindFill(ATile.Size, ATile.FPosition) do
    begin
      Resize(Size * 2);
      Changed := True;
    end;

    ATile.FState := tsPositioned;
  end;

  procedure AddTile(ATile: TTile);
  var
    I: Integer;
  begin
    if ATile.FState = tsAdded then
      Exit;

    FTexture.SubData[ATile.TexelBounds] := ATile.Texture.DataPointer;
    for I := 0 to FSubTypes.MaxIndex do
    begin
      if ATile.SubTextures[I] <> nil then
        FSubTypes[I].Texture.SubData[ATile.TexelBounds] := ATile.SubTextures[I].DataPointer
      else
        FSubTypes[I].Texture.Fill(ATile.TexelBounds, FSubTypes[I].DefaultColor);
    end;
    ATile.FState := tsAdded;
  end;

begin
  Changed := ARegenerate;

  if ARegenerate then
  begin
    for Tile in FTiles.Values do
      if Tile.HasSubTiles then
        for SubTile in Tile.SubTiles do
          SubTile.FState := tsInitial
      else
        Tile.FState := tsInitial;

    FFreeSpace.Clear;
  end;

  MinSize := GetMinSize;
  if MinSize > Size then
  begin
    Resize(MinSize);
    Changed := True;
  end;

  TileArray := FTiles.Values.ToArray;
  TileArray.Sort(
    function(ALeft, ARight: TTile): Boolean
    begin
      Result := ALeft.Size.Y > ARight.Size.Y;
    end
    );

  // find positions
  for Tile in TileArray do
    if Tile.HasSubTiles then
      for SubTile in Tile.SubTiles do
        PositionTile(SubTile)
    else
      PositionTile(Tile);

  // add image data
  for Tile in TileArray do
    if Tile.HasSubTiles then
      for SubTile in Tile.SubTiles do
        AddTile(SubTile)
    else
      AddTile(Tile);

  TileArray.Free;

  if Changed then
    FOnChanged.Execute;
end;

function TTextureAtlas.GetGLState: TGLState;
begin
  Result := FTexture.GLState;
end;

function TTextureAtlas.GetTile(AName: string): TTile;
begin
  if not FTiles.Get(AName, Result) then
    raise ETextureAtlasTileNotFound.Create;
end;

function TTextureAtlas.HalfPixelInset(ABounds: TBounds2): TBounds2;
begin
  Result := ABounds.Inset(0.5 / Size);
end;

function TTextureAtlas.GetMinSize: Integer;
var
  Tile, SubTile: TTile;
  Pixels: Integer;

  procedure IncPixels(ATile: TTile);
  begin
    Inc(Pixels, ATile.Texture.PixelCount);
    Result := Max(Result, 1 shl Ceil(Log2(ATile.Texture.Width)));
    Result := Max(Result, 1 shl Ceil(Log2(ATile.Texture.Height)));
  end;

begin
  // The result is sure to fit each texture separately or the amount of all pixels combined
  Pixels := 0;
  Result := 0;
  for Tile in FTiles.Values do
    if Tile.HasSubTiles then
      for SubTile in Tile.SubTiles do
        IncPixels(SubTile)
    else
      IncPixels(Tile);

  Result := Max(Result, 1 shl Ceil(Log2(Sqrt(Pixels))));
end;

function TTextureAtlas.GetOnChanged: TEvent.TAccess;
begin
  Result := FOnChanged.Access;
end;

function TTextureAtlas.GetSize: Integer;
begin
  Result := FTexture.Width;
end;

function TTextureAtlas.GetSubTypes: TSubTypes.TReader;
begin
  Result := FSubTypes.Reader;
end;

procedure TTextureAtlas.Resize(ASize: TIntVector2);
var
  Tile, SubTile: TTile;
  SubType: TSubType;

  procedure UpdateState(ATile: TTile);
  begin
    if ATile.FState > tsPositioned then
      ATile.FState := tsPositioned;
  end;

begin
  FFreeSpace.Size := ASize;

  FTexture.Size := ASize;
  for SubType in FSubTypes do
    SubType.Texture.Size := ASize;

  for Tile in FTiles.Values do
    if Tile.HasSubTiles then
      for SubTile in Tile.SubTiles do
        UpdateState(SubTile)
    else
      UpdateState(Tile);

end;

procedure TTextureAtlas.Uniform(ASampler: TGLProgram.TUniformSampler);
begin
  FTexture.Uniform(ASampler);
end;

{ ETextureAtlasDuplicateName }

constructor ETextureAtlasTileExists.Create;
begin
  inherited Create('A texture atlas tile with that name exist already.');
end;

{ ETextureAtlasTileNotFound }

constructor ETextureAtlasTileNotFound.Create;
begin
  inherited Create('The texture atlas tile could not be found.');
end;

{ TTextureAtlas.TFreeRows }

function TTextureAtlas.TFreeRows.GetRows: TRows.TReader;
begin
  Result := FRows.Reader;
end;

procedure TTextureAtlas.TFreeRows.SetSize(const Value: Integer);
var
  I: Integer;
begin
  if Size = Value then
    Exit;

  if Value > Size then
  begin
    // lengthened
    if not FRows.Empty and (FRows.Last.C2 = Size) then
      // lengthen the last row
      FRows.Last := IBounds1I(FRows.Last.C1, Value)
    else
      // add a new row with the required space
      FRows.Add(IBounds1(Size, Value));
  end
  else
  begin
    // shortened
    for I := FRows.MaxIndex downto 0 do
    begin
      FRows[I] := IBounds1(FRows[I].C1, Value).Clamp(FRows[I]);
      if FRows[I].Length <= 0 then
        FRows.RemoveLast
      else
        Break;
    end;
  end;

  FSize := Value;
end;

constructor TTextureAtlas.TFreeRows.Create(ASize: Integer);
begin
  FRows := TRows.Create;
  FSize := ASize;
  if FSize > 0 then
    FRows.Add(IBounds1(Size));
end;

destructor TTextureAtlas.TFreeRows.Destroy;
begin
  FRows.Free;
  inherited;
end;

function TTextureAtlas.TFreeRows.Find(AStart, ASize: Integer; out APos: Integer): Boolean;
var
  I: Integer;
  Row: TIntBounds1;
begin
  for I := 0 to FRows.MaxIndex do
  begin
    if AStart >= FRows[I].C2 then
      Continue;
    Row.C1 := Max(AStart, FRows[I].C1);
    Row.C2 := FRows[I].C2;
    if Row.Length >= ASize then
    begin
      APos := Row.C1;
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TTextureAtlas.TFreeRows.Fill(ARange: TIntBounds1);
var
  I: Integer;
  Left, Right: Boolean;
begin
  for I := 0 to FRows.MaxIndex do
  begin
    if not(ARange in FRows[I]) then
      Continue;

    Left := FRows[I].C1 = ARange.C1;
    Right := FRows[I].C2 = ARange.C2;

    if Left then
    begin
      if Right then
      begin
        // remove
        FRows.RemoveAt(I);
      end
      else
      begin
        // decrease cleared section to the right
        FRows[I] := IBounds1(ARange.C2, FRows[I].C2);
      end;
    end
    else
    begin
      if Right then
      begin
        // decrease cleared section to the left
        FRows[I] := IBounds1(FRows[I].C1, ARange.C1);
      end
      else
      begin
        // split
        FRows.Insert(IBounds1(ARange.C2, FRows[I].C2), I + 1);
        FRows[I] := IBounds1(FRows[I].C1, ARange.C1);
      end;
    end;

    Exit;
  end;

  Assert(False);
end;

procedure TTextureAtlas.TFreeRows.Clear(ARange: TIntBounds1);
var
  I: Integer;
  Row: TIntBounds1;
  Left, Right: Boolean;
begin
  for I := 0 to FRows.Count do
  begin
    if I = 0 then
      Row.C1 := 0
    else
      Row.C1 := FRows[I - 1].C2;
    Row.C2 := FRows[I].C1;

    if Row.Length = 0 then
      Continue;

    if not(ARange in Row) then
      Continue;

    Left := (Row.C1 = ARange.C1) and (I <> 0);
    Right := (Row.C2 = ARange.C2) and (I <> FRows.MaxIndex);

    if Left then
    begin
      if Right then
      begin
        // merge cleared sections
        FRows[I - 1] := IBounds1(FRows[I - 1].C1, FRows[I].C2);
        FRows.RemoveAt(I);
      end
      else
      begin
        // increase left cleared section to the right
        FRows[I - 1] := IBounds1(FRows[I - 1].C1, ARange.C2);
      end;
    end
    else
    begin
      if Right then
      begin
        // inscrease right cleared section to the left
        FRows[I - 1] := IBounds1(ARange.C1, FRows[I - 1].C2);
      end
      else
      begin
        // create new cleared section
        FRows.Insert(ARange, I);
      end;
    end;

    Break;
  end;

  Assert(False);
end;

procedure TTextureAtlas.TFreeRows.Clear;
begin
  FRows.Clear;
  FRows.Add(IBounds1(Size));
end;

{ TTextureAtlas.TFreeSpace }

procedure TTextureAtlas.TFreeSpace.SetSize(const Value: TIntVector2);
var
  Row: TFreeRows;
begin
  if not(FSize >= 0) then
    raise EBitfieldNegativeSize.Create;

  if Size = Value then
    Exit;

  FSize.X := Value.X;
  for Row in FRows do
    Row.Size := FSize.X;

  while FSize.Y > Value.Y do
  begin
    FRows.RemoveLast;
    Dec(FSize.Y);
  end;

  while FSize.Y < Value.Y do
  begin
    FRows.Add(TFreeRows.Create(FSize.X));
    Inc(FSize.Y);
  end;

end;

function TTextureAtlas.TFreeSpace.Format: string;
var
  Row: TFreeRows;
  Bounds: TIntBounds1;
begin
  Result := '|';
  for Row in FRows do
  begin
    for Bounds in Row.FRows do
      Result := Result + Bounds.ToString;
    Result := Result + '|';
  end;
end;

constructor TTextureAtlas.TFreeSpace.Create;
begin
  FRows := TRows.Create;
end;

destructor TTextureAtlas.TFreeSpace.Destroy;
begin
  FRows.Free;
  inherited;
end;

procedure TTextureAtlas.TFreeSpace.Clear;
var
  Row: TFreeRows;
begin
  for Row in FRows do
    Row.Clear;
end;

procedure TTextureAtlas.TFreeSpace.Clear(ABounds: TIntBounds2);
var
  Y: Integer;
begin
  for Y in ABounds.LineY do
    FRows[Y].Clear(ABounds.LineX);
end;

function TTextureAtlas.TFreeSpace.FindFill(ASize: TIntVector2; out APos: TIntVector2): Boolean;
var
  StartY, TestY, LowestX, X: Integer;
  FoundAll: Boolean;
begin
  // start at the top
  for StartY := 0 to Size.Y - ASize.Y do
  begin
    // find the lowest X, where it would fit
    LowestX := 0;
    repeat
      FoundAll := True;
      for TestY := StartY to StartY + ASize.Y - 1 do
      begin
        if not FRows[TestY].Find(LowestX, ASize.X, X) then
        begin
          // if it doesn't fit at all in any row, cancel and advance the outer loop
          LowestX := -1;
          Break;
        end;
        // fits somewhere
        if X > LowestX then
        begin
          // doesn't fit right at the current test position, try again at new position
          FoundAll := False;
          LowestX := X;
          Break;
        end;
      end;

      // advance outer loop, as it didn't fit in any row
      if LowestX = -1 then
        Break;

    until FoundAll;

    // advance outer loop, as it didn't fit in any row
    if LowestX = -1 then
      Continue;

    // set result
    APos := IVec2(LowestX, StartY);
    // fill it
    for TestY := StartY to StartY + ASize.Y - 1 do
      FRows[TestY].Fill(IBounds1(LowestX, LowestX + ASize.X));
    Exit(True);
  end;

  Result := False;
end;

{ TTextureAtlas.TSubType }

constructor TTextureAtlas.TSubType.Create(AGLState: TGLState; ASuffix: string; ADefaultColor: TColorRGB);
begin
  FTexture := TTexture2D.Create(AGLState);
  FSuffix := ASuffix;
  FDefaultColor := ADefaultColor;
end;

destructor TTextureAtlas.TSubType.Destroy;
begin
  FTexture.Free;
  inherited;
end;

procedure TTextureAtlas.TSubType.Uniform(ASampler: TGLProgram.TUniformSampler);
begin
  Texture.Uniform(ASampler);
end;

{ ETextureAtlasSubTypesLocked }

constructor ETextureAtlasSubTypesLocked.Create;
begin
  inherited Create('Texture atlas subtypes are not modifiable anymore.');
end;

{ TTextureAtlas.TInfo }

constructor TTextureAtlas.TInfo.Create(AInfo: string);
var
  Info: TJSONObject;
  BaseNode, WidthsNode: TJSONObject;
  PairNode: TJSONPair;
  NumberNode: TJSONNumber;
  Index: Integer;
  ArrayNode: TJSONArray;
  ValueNode: TJSONValue;
  Axis: TCoordAxis2;
  I: Integer;
begin
  Info := TJSONObject.ParseJSONValue(AInfo) as TJSONObject;
  if Info = nil then
    raise ETextureAtlasInvalidInfoFile.Create('The texture info file is not valid json.');
  try

    FMode := tmNone;

    if Info.TryGetValue<TJSONObject>('Grid', BaseNode) then
    begin
      if FMode <> tmNone then
        raise ETextureAtlasInvalidInfoFile.Create('Only one texture info mode can be used at once.');
      FMode := tmGrid;
      for Axis := Low(TCoordAxis2) to High(TCoordAxis2) do
      begin
        if BaseNode.TryGetValue<TJSONValue>(string(CoordAxisNames[Axis]), ValueNode) then
        begin
          FGrid[Axis] := TSingleArray.Create;
          if ValueNode is TJSONArray then
          begin
            ArrayNode := TJSONArray(ValueNode);
            for ValueNode in ArrayNode do
            begin
              if not (ValueNode is TJSONNumber) then
                raise ETextureAtlasInvalidInfoFile.CreateFmt('Expected number in Grid.%s.', [CoordAxisNames[Axis]]);
              FGrid[Axis].Add(TJSONNumber(ValueNode).AsDouble);
            end;
            if FGrid[Axis].Empty then
              raise ETextureAtlasInvalidInfoFile.CreateFmt('Expected number in Grid.%s.', [CoordAxisNames[Axis]]);
          end
          else if ValueNode is TJSONNumber then
          begin
            // leave FGrid[Axis] as nil to mark, that we used FTextures
            for I := 1 to TJSONNumber(ValueNode).AsInt do
              FGrid[Axis].Add(1);
          end
          else
            raise ETextureAtlasInvalidInfoFile.CreateFmt('Expected Grid.%s integer or array.', [CoordAxisNames[Axis]]);
        end
        else
          raise ETextureAtlasInvalidInfoFile.CreateFmt('Expected Grid.%s integer or array.', [CoordAxisNames[Axis]]);
      end;
    end;

    if Info.TryGetValue<TJSONObject>('Font', BaseNode) then
    begin
      if FMode <> tmNone then
        raise ETextureAtlasInvalidInfoFile.Create('Only one texture info mode can be used at once.');
      FMode := tmFont;
      if BaseNode.TryGetValue<TJSONObject>('Widths', WidthsNode) then
      begin
        for PairNode in WidthsNode do
        begin
          case PairNode.JsonString.Value.Length of
            1:
              Index := Ord(PairNode.JsonString.Value[1]);
            2:
              if not TryStrToInt(PairNode.JsonString.Value, Index) then
                raise ETextureAtlasInvalidInfoFile.Create('Invalid hex index for char width.');
          else
            raise ETextureAtlasInvalidInfoFile.Create('Only chars or hex indices are allowed.');
          end;

          if not(Index in IBounds1(256)) then
            raise ETextureAtlasInvalidInfoFile.Create('Only char indices from 00 to FF are allowed.');
          if not PairNode.JsonValue.TryGetValue<TJSONNumber>(NumberNode) or not(NumberNode.AsDouble in Bounds1(0, 1)) then
            raise ETextureAtlasInvalidInfoFile.Create('Only 0 < width <= 1 are allowed char widths.');
          FWidths[Index] := NumberNode.AsDouble;
        end;
      end;
    end;

    if Info.TryGetValue<TJSONObject>('Regions', BaseNode) then
    begin
      if FMode <> tmNone then
        raise ETextureAtlasInvalidInfoFile.Create('Only one texture info mode can be used at once.');
      FMode := tmRegions;
      raise ENotImplemented.Create('Texture info file with regions.');
    end;

    if Info.TryGetValue<TJSONObject>('Values', BaseNode) then
    begin
      FValues := TValues.Create;
      for PairNode in BaseNode do
        FValues[PairNode.JsonString.Value] := PairNode.JsonValue.GetValue<TJSONNumber>.AsDouble;
    end;

  finally
    Info.Free;
  end;
end;

destructor TTextureAtlas.TInfo.Destroy;
var
  Axis: TCoordAxis2;
begin
  for Axis := Low(TCoordAxis2) to High(TCoordAxis2) do
    FGrid[Axis].Free;
  FValues.Free;
  inherited;
end;

function TTextureAtlas.TInfo.GetTextureArray(ATexture: TTextureData): TArray<TTextureData>;
begin
  Result := TArray<TTextureData>.Create;
  case FMode of
    tmNone:
      raise ETextureAtlasInvalidInfoFile.Create('Either Grid, Font or Regions node expected in texture info file.');
    tmGrid:
      LoadGrid(ATexture, Result);
    tmFont:
      LoadFont(ATexture, Result);
    tmRegions: 
      LoadRegions(ATexture, Result);
  end;
end;

procedure TTextureAtlas.TInfo.LoadFont(ATexture: TTextureData; ATextures: TArray<TTextureData>);
var
  Pos, PixelPos: TIntVector2;
  I: Integer;
  Bounds: TIntBounds2;
  CharSize: TIntVector2;
begin
  ATextures.Capacity := IBounds2(16).Area;
  ATextures.ForceCount(ATextures.Capacity);
  CharSize := ATexture.Size div 16;
  for Pos in IBounds2(16) do
  begin
    I := Pos.X + (15 - Pos.Y) * 16;
    if FWidths[I] = 0 then
    begin
      for PixelPos in IBounds2I(1, CharSize) do
      begin
        if ATexture.Pixels[CharSize * (Pos + 1) - PixelPos.YX].A <> 0 then
          Break;
      end;
      FWidths[I] := 1 - (PixelPos.Y - 1) / (ATexture.Size.X div 16);
    end;
    Bounds := (Pos * CharSize).Bounds(CharSize);
    Bounds.LineY := (ATexture.Height - Bounds.LineY).Normalize;
    Bounds.C2.X := Bounds.C1.X + Floor((ATexture.Size.X div 16) * FWidths[I]);
    ATextures[I] := ATexture.CreateSubTexture(Bounds);
  end;
end;

procedure TTextureAtlas.TInfo.LoadGrid(ATexture: TTextureData; ATextures: TArray<TTextureData>);
var
  Size, GridPos: TIntVector2;
  Pos, Sum: TVector2;
  Bounds: TBounds2;
begin
  Size := IVec2(FGrid[caX].Count, FGrid[caY].Count);
  Sum := Vec2(FGrid[caX].Sum, FGrid[caY].Sum);
  ATextures.Capacity := Size.X * Size.Y;
  ATextures.ForceCount(ATextures.Capacity);
  Pos := 0;
  for GridPos in Size do
  begin
    if GridPos.X = 0 then
      Pos.X := 0;
    Bounds := Pos.Bounds(Vec2(FGrid[caX][GridPos.X], FGrid[caY][GridPos.Y]) / Sum);
    ATextures[GridPos.X + GridPos.Y * Size.X] := ATexture.CreateSubTexture(Bounds);
    Pos.X := Pos.X + FGrid[caX][GridPos.X] / Sum.X;
    if GridPos.X = Size.X - 1 then
      Pos.Y := Pos.Y + FGrid[caY][GridPos.Y] / Sum.Y;
  end;
end;

procedure TTextureAtlas.TInfo.LoadRegions(ATexture: TTextureData; ATextures: TArray<TTextureData>);
begin
  raise ENotImplemented.Create('Texture info file LoadRegions function.');
end;

function TTextureAtlas.TInfo.OwnValues: TValues;
begin
  Result := FValues;
  FValues := nil;
end;

end.
