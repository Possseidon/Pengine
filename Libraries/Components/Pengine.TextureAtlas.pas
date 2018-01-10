unit Pengine.TextureAtlas;

interface

uses
  System.SysUtils,
  System.Math,
  System.RegularExpressions,
  System.IOUtils,
  System.JSON,

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
  public
    constructor Create;
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

    TTile = class
    public type

      TState = (
        tsInitial,
        tsPositioned,
        tsAdded
        );

      TSubTextures = TObjectArray<TTextureData>;

      TSubTiles = TObjectArray<TTile>;

    private
      FAtlas: TTextureAtlas;
      FSubTiles: TSubTiles;
      FName: string;
      FTexture: TTextureData;
      FSubTextures: TSubTextures;
      FState: TState;
      FPosition: TIntVector2;
     
      function GetSubTextures: TSubTextures.TReader;

      function GetSubTiles: TSubTiles.TReader;

      function GetPosition: TIntVector2;

      function GetTexelBounds: TIntBounds2;
      function GetBounds: TBounds2;
      function GetBoundsHalfPixelInset: TBounds2;

      function GetSize: TIntVector2;
      function GetAspect: Single;

    public
      constructor Create(AAtlas: TTextureAtlas; AName: string; ATexture: TTextureData); overload;
      constructor Create(AAtlas: TTextureAtlas; AName: string; ATextures: IIterable<TTextureData>); overload;
      destructor Destroy; override;

      property Atlas: TTextureAtlas read FAtlas;
      property Name: string read FName;

      property Texture: TTextureData read FTexture;
      property SubTextures: TSubTextures.TReader read GetSubTextures;

      function HasSubTiles: Boolean;
      property SubTiles: TSubTiles.TReader read GetSubTiles;

      property Position: TIntVector2 read GetPosition;

      property TexelBounds: TIntBounds2 read GetTexelBounds;
      property Bounds: TBounds2 read GetBounds;
      property BoundsHalfPixelInset: TBounds2 read GetBoundsHalfPixelInset;

      property Size: TIntVector2 read GetSize;
      property Aspect: Single read GetAspect;

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

    TInfo = class
    private
      FTextures: TIntVector2;

    public
      constructor Create(AInfo: string);

      function GetTextureArray(ATexture: TTextureData): TArray<TTextureData>;
      
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
    function Add(AName: string; ATexture: TTextureData): TTile; overload;
    function Add(AName: string; ATextures: IIterable<TTextureData>): TTile; overload;
    procedure Del(AName: string);

    function AddSubType(ASuffix: string; ASampler: TGLProgram.TUniformSampler; ADefaultColor: TColorRGBA): TSubType;
    property SubTypes: TSubTypes.TReader read GetSubTypes;

    property Texture: TTexture2D read FTexture;

    property Tiles[AName: string]: TTile read GetTile; default;

    function HalfPixelInset(ABounds: TBounds2): TBounds2;

    procedure Generate(ARegenerate: Boolean = False);

    procedure Uniform(ASampler: TGLProgram.TUniformSampler);

    property OnChanged: TEvent.TAccess read GetOnChanged;

  end;

implementation

{ TTextureAtlas.TTile }

constructor TTextureAtlas.TTile.Create(AAtlas: TTextureAtlas; AName: string; ATexture: TTextureData);
var
  I: Integer;
begin
  FAtlas := AAtlas;
  FName := AName;
  FTexture := ATexture;
  FSubTextures := TSubTextures.Create;
  for I := 0 to Atlas.SubTypes.MaxIndex do
    FSubTextures.Add(nil);
end;

constructor TTextureAtlas.TTile.Create(AAtlas: TTextureAtlas; AName: string; ATextures: IIterable<TTextureData>);
var
  First: Boolean;
  Texture: TTextureData;
begin
  FSubTiles := TSubTiles.Create;
  if ATextures.CountOptimized then
    FSubTiles.Capacity := ATextures.Count;
  First := True;
  for Texture in ATextures do
  begin
    if First then
    begin
      Create(AAtlas, AName, Texture);
      FSubTiles.Add(Self);
      First := False;
      Continue;
    end;
    FSubTiles.Add(TTile.Create(AAtlas, AName, Texture)).FSubTiles := FSubTiles;
  end;
end;

destructor TTextureAtlas.TTile.Destroy;
begin
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

function TTextureAtlas.TTile.GetSubTiles: TSubTiles.TReader;
begin
  Result := FSubTiles.Reader;
end;

function TTextureAtlas.TTile.GetTexelBounds: TIntBounds2;
begin
  Result.Create(Position, Position + Texture.Size);
end;

function TTextureAtlas.TTile.HasSubTiles: Boolean;
begin
  Result := FSubTiles <> nil;
end;

{ TTextureAtlas }

function TTextureAtlas.Add(AName: string; ATexture: TTextureData): TTile;
begin
  FSubTypesLocked := True;
  if FTiles.KeyExists(AName) then
    raise ETextureAtlasTileExists.Create;
  Result := TTile.Create(Self, AName, ATexture);
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
      Result := Add(AName, Info.GetTextureArray(Data));
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
begin
  Resource := AResource + '_INFO';
  if FindResource(HInstance, PWideChar(Resource), RT_RCDATA) <> 0 then
  begin
    // load with .info file
    raise ENotImplemented.Create('TTextureAtlas.AddFromResource with .info file');
  end
  else
  begin
    // load without .info file
    Result := Add(AName, TTextureData.CreateFromResource(AResource));
    for I := 0 to FSubTypes.MaxIndex do
    begin
      Resource := AResource + '_' + FSubTypes[I].Suffix;
      if FindResource(HInstance, PWideChar(Resource), RT_RCDATA) <> 0 then
        Result.FSubTextures[I] := TTextureData.CreateFromResource(Resource);
    end;
  end;
end;

function TTextureAtlas.AddSubType(ASuffix: string; ASampler: TGLProgram.TUniformSampler; ADefaultColor: TColorRGBA)
  : TSubType;
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

procedure TTextureAtlas.Del(AName: string);
begin
  if not FTiles.TryDel(AName) then
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
  MinSize, I: Integer;
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

    FTexture.SubData(ATile.TexelBounds, ATile.Texture.DataPointer);
    for I := 0 to FSubTypes.MaxIndex do
    begin
      if ATile.SubTextures[I] <> nil then
        FSubTypes[I].Texture.SubData(ATile.TexelBounds, ATile.SubTextures[I].DataPointer)
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
      FRows.Last := TIntBounds1.Create(FRows.Last.C1, Value)
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
        FRows.DelLast
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
        FRows.DelAt(I);
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
        FRows.DelAt(I);
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
    FRows.DelLast;
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

{ ETextureAtlasInvalidInfoFile }

constructor ETextureAtlasInvalidInfoFile.Create;
begin
  inherited Create('The texture atlas info file format is not valid.');
end;

{ TTextureAtlas.TInfo }

constructor TTextureAtlas.TInfo.Create(AInfo: string);
var
  Info: TJSONObject;
  TexturesNode: TJSONObject;
  JSONNumber: TJSONNumber;
begin
  Info := TJSONObject.ParseJSONValue(AInfo) as TJSONObject;
  if (Info = nil) or not Info.TryGetValue<TJSONObject>('Textures', TexturesNode) then
    raise ETextureAtlasInvalidInfoFile.Create;
  if not TexturesNode.TryGetValue<TJSONNumber>('X', JSONNumber) then
    raise ETextureAtlasInvalidInfoFile.Create;
  FTextures.X := JSONNumber.AsInt;
  if not TexturesNode.TryGetValue<TJSONNumber>('Y', JSONNumber) then
    raise ETextureAtlasInvalidInfoFile.Create;
  FTextures.Y := JSONNumber.AsInt;
end;

function TTextureAtlas.TInfo.GetTextureArray(ATexture: TTextureData): TArray<TTextureData>;
var
  Pos, TextureSize: TIntVector2;
  Data: TArray<TColorRGBA.TBytes>;
begin
  Result := TArray<TTextureData>.Create;
  for Pos in FTextures do
  begin
    TextureSize := ATexture.Size div FTextures;
    Data := ATexture.SubData[IBounds2(ATexture.Size) div FTextures + Pos * TextureSize];
    Result.Add(TTextureData.Create(TextureSize)).Data := Data;
    Data.Free;    
  end;
end;

end.
