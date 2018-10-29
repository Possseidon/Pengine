unit Pengine.MC.Assets;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Types,

  GdiPlus,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Settings,
  Pengine.JSON,
  Pengine.IntMaths,

  Pengine.MC.Item,
  Pengine.MC.BlockState,
  Pengine.Vector,
  Pengine.Color,
  System.Math;

type

  // TODO: Split up Assets into multiple sub assets (texture, model, blockstate) to split up load

  // TODO: take .mcmeta into account
  TAssetsSettings = class;

  /// <summary>A single, square texture or an animation of square textures.</summary>
  TTexture = class
  public type

    TFrames = TArray<IGPBitmap>;

  private
    FName: string;
    FFrames: TFrames;
    FTintColor: TColorRGB;

    function GetFrames: TFrames.TReader;
    function GetImage: IGPBitmap;

  public
    constructor Create(AName: string; AImage: IGPBitmap); overload;
    constructor Create(AName: string; AImage: IGPBitmap; ATintColor: TColorRGB); overload;
    destructor Destroy; override;

    property Name: string read FName;
    property TintColor: TColorRGB read FTintColor;
    property Image: IGPBitmap read GetImage;
    property Frames: TFrames.TReader read GetFrames;

  end;

  /// <summary>A collection of all textures found in folders.</summary>
  TTextureCollection = class
  public type

    TTextureMap = TToObjectMap<string, TTexture, TStringHasher>;

  private
    FTextures: TTextureMap;

    function GetTextures: TTextureMap.TReader;

    procedure LoadRecursive(APath, AFilePrefix: string; AJTintIndex: TJObject);

  public
    constructor Create(APath: string);
    destructor Destroy; override;

    property Textures: TTextureMap.TReader read GetTextures;

    function Exists(AName: string): Boolean;
    function Get(AName: string; out ATexture: TTexture): Boolean;

  end;

  TTextureVariable = class
  public
    class function CreateTyped(ATextureCollection: TTextureCollection; AName: string): TTextureVariable;

    function Format: string; virtual; abstract;

  end;

  TTextureVariableDirect = class(TTextureVariable)
  private
    FTexture: TTexture;

  public
    constructor Create(ATexture: TTexture);

    property Texture: TTexture read FTexture;

    function Format: string; override;

  end;

  TTextureVariablePlaceholder = class(TTextureVariable)
  private
    FName: string;

  public
    constructor Create(AName: string);

    property Name: string read FName;

    function Format: string; override;

  end;

  TModelCollection = class;

  /// <summary>A model, eventually used for items or blockstates.</summary>
  TModel = class
  public type

    TBuiltinType = (
      btNone,
      btGenerated,
      btEntity,
      btCompass,
      btClock
      );

    TElement = class;

    /// <summary>A face with texture, uv and additional info.</summary>
    TFace = class
    private
      FElement: TElement;
      FTexture: TTextureVariablePlaceholder;
      FCullFace: TBasicDir3;
      FUVPx: TBounds2;
      FUV: TBounds2;
      FUVRotated: TAxisSystem2;
      FTintindex: Boolean;
      FRotation: Integer;

      function CalcUV: TBounds2;
      function CalcUVRotated: TAxisSystem2;

    public
      constructor Create(AElement: TElement; ADir: TBasicDir3; AJObject: TJObject);
      destructor Destroy; override;

      property Element: TElement read FElement;

      /// <summary>Resolves this faces texture name for a given model.</summary>
      function ResolveTexture(AModel: TModel; out AResolved: string): Boolean;

      /// <summary>The texture placeholder for this face.</summary>
      property Texture: TTextureVariablePlaceholder read FTexture;
      /// <summary>The side to use for culling.</summary>
      property CullFace: TBasicDir3 read FCullFace;
      /// <summary>UV-Coordinates in Pixels in top-bottom order. <c>[0, 16]</c></summary>
      property UVPx: TBounds2 read FUVPx;
      /// <summary>UV-Coordinates in bottom-top order. <c>[0, 1]</c></summary>
      property UV: TBounds2 read FUV;
      /// <summary>UV-Coordinates in bottom-top order with c>Rotation</c> applied. <c>[0, 1]</c></summary>
      property UVRotated: TAxisSystem2 read FUVRotated;
      /// <summary>The count of counter-clockwise UV-Rotations.</summary>
      property Rotation: Integer read FRotation;
      /// <summaryWether to tint this tile like grass or water.</summary>
      property Tintindex: Boolean read FTintindex;

    end;

    /// <summary>A cube and additional info, for which each side is optional.</summary>
    TElement = class
    public const

      FaceNames: array [TBasicDir3] of string = (
        'west',
        'east',
        'down',
        'up',
        'north',
        'south'
        );

    public type

      TRotation = class
      private
        FOriginPx: TVector3;
        FOrigin: TVector3;
        FAxis: TCoordAxis3;
        FAngle: Single;
        FRescale: Boolean;

        class function AxisFromName(AName: string): TCoordAxis3;

      public
        constructor Create(AJObject: TJObject);

        property OriginPx: TVector3 read FOriginPx;
        property Origin: TVector3 read FOrigin;
        property Axis: TCoordAxis3 read FAxis;
        property Angle: Single read FAngle;
        property Rescale: Boolean read FRescale;

      end;

    private
      FModel: TModel;
      FSizePx: TBounds3;
      FSize: TBounds3;
      FBounds: TAxisSystem3;
      FFaces: array [TBasicDir3] of TFace;
      FRotation: TRotation;
      FShade: Boolean;

      function GetFace(ADir: TBasicDir3): TFace;

      class function DirFromName(AName: string): TBasicDir3;

      function CalcSize: TBounds3;
      function CalcBounds: TAxisSystem3;

    public
      constructor Create(AModel: TModel; AJObject: TJObject);
      destructor Destroy; override;

      property Model: TModel read FModel;

      /// <summary>The size of this element in Pixels. <c>[0, 16]</c></summary>
      property SizePx: TBounds3 read FSizePx;
      /// <summary>The size of this element. <c>[0, 1]</c></summary>
      property Size: TBounds3 read FSize;
      property Bounds: TAxisSystem3 read FBounds;
      property Rotation: TRotation read FRotation;

      property Faces[ADir: TBasicDir3]: TFace read GetFace;
      function FaceExists(ADir: TBasicDir3): Boolean;
      function UsedFaces: TBasicDirs3;
      function FaceCount: Integer;

      property Shade: Boolean read FShade;

    end;

    TDisplay = class
    private
      FRotation: TVector3;
      FTranslation: TVector3;
      FScale: TVector3;

    public
      constructor Create(AJObject: TJObject);

      property Rotation: TVector3 read FRotation;
      property Translation: TVector3 read FTranslation;
      property Scale: TVector3 read FScale;

    end;

    TElements = TObjectArray<TElement>;

    TTextures = TToObjectMap<string, TTextureVariable, TStringHasher>;

  public const

    BuiltinTypeNames: array [TBuiltinType] of string = (
      '',
      'builtin/generated',
      'builtin/entity',
      'builtin/compass',
      'builtin/clock'
      );

  private
    FAssets: TAssetsSettings;
    FParent: TModel;
    FBuiltinType: TBuiltinType;
    FElements: TElements;
    FTextures: TTextures;
    FGUIDisplay: TDisplay;

    function GetTextures: TTextures.TReader;
    function GetElements: TElements.TReader;
    function GetTexturesRecursive: TTextures.TReader;
    function GetGUIDisplay: TDisplay;

  public
    constructor Create(AAssets: TAssetsSettings; AJObject: TJObject);
    destructor Destroy; override;

    property Assets: TAssetsSettings read FAssets;

    property Parent: TModel read FParent;
    property BuiltinType: TBuiltinType read FBuiltinType;
    property Elements: TElements.TReader read GetElements;
    property Textures: TTextures.TReader read GetTextures;
    property TexturesRecursive: TTextures.TReader read GetTexturesRecursive;

    property GUIDisplay: TDisplay read GetGUIDisplay;

    function ResolveTexture(var AName: string; out AResolved: string): Boolean;

    function IsBuiltinType(AType: TBuiltinType): Boolean;

  end;

  TBlockModel = class(TModel)
  private
    FAmbientOcclusion: Boolean;

  public
    constructor Create(AAssets: TAssetsSettings; AJObject: TJObject);

    property AmbientOcclusion: Boolean read FAmbientOcclusion;

  end;

  TItemModel = class(TModel)
  public const

    LayerPrefix = 'layer';

  private
    // FOverrides: TOverrides;

    function GetLayer(AIndex: Integer): TTextureVariable;
    function GetLayerCount: Integer;

  public
    constructor Create(AAssets: TAssetsSettings; AJObject: TJObject);

    // property Overrides: TOverrides.TReader read GetOverrides;

    property LayerCount: Integer read GetLayerCount;
    property Layers[AIndex: Integer]: TTextureVariable read GetLayer;
    function LayerName(AIndex: Integer): string;

  end;

  /// <summary>A collection of all models foun in folders.</summary>
  TModelCollection = class
  public type

    TModels = TToObjectMap<string, TModel, TStringHasher>;
    TItemModels = TToRefMap<string, TItemModel, TStringHasher>;
    TBlockModels = TToRefMap<string, TBlockModel, TStringHasher>;

  private
    FAssets: TAssetsSettings;
    FPath: string;
    FModels: TModels;
    FBlockModels: TBlockModels;
    FItemModels: TItemModels;

    function ExtractName(APath: string): string;

    procedure Load(APath: string);

    function LoadBlock(AFileName: string): TBlockModel;
    function LoadItem(AFileName: string): TItemModel;
    function LoadFromName(AName: string): TModel;

    function GetBlockModels: TBlockModels.TReader;
    function GetItemModels: TItemModels.TReader;
    function GetModels: TModels.TReader;

  public
    constructor Create(AAssets: TAssetsSettings);
    destructor Destroy; override;

    property Assets: TAssetsSettings read FAssets;

    property Path: string read FPath;

    property Models: TModels.TReader read GetModels;
    property BlockModels: TBlockModels.TReader read GetBlockModels;
    property ItemModels: TItemModels.TReader read GetItemModels;

  end;

  /// <summary>Adds additional information to a model, as used in blockstates.</summary>
  TVariant = class
  private
    FModel: TModel;
    FX: Integer;
    FY: Integer;
    FUVLock: Boolean;
    FWeight: Integer;

  public
    constructor Create(AAssets: TAssetsSettings; AJObject: TJObject);

    property Model: TModel read FModel;
    property X: Integer read FX;
    property Y: Integer read FY;
    property UVLock: Boolean read FUVLock;
    property Weight: Integer read FWeight;

  end;

  /// <summary>Defines relevant models for a certain blockstate.</summary>
  TBlockStateModel = class
  public type

    TVariants = TRefArray<TVariant>;

  public
    function GetVariants(AProperties: TBlockState.TProperties { ; APos: Integer } ): TVariants; virtual; abstract;

  end;

  /// <summary>A blockstate model using variants.</summary>
  TVariantModel = class(TBlockStateModel)
  public type

    TVariants = TObjectArray<TVariant>;

    TVariantMap = TObjectObjectMap<TBlockState.TProperties, TVariants, TBlockState.TPropertiesHasher>;

  private
    FVariantMap: TVariantMap;

    function GetVariantMap: TVariantMap.TReader;

  public
    constructor Create(AAssets: TAssetsSettings; AJObject: TJObject);
    destructor Destroy; override;

    property VariantMap: TVariantMap.TReader read GetVariantMap;

    function GetVariants(AProperties: TBlockState.TProperties): TBlockStateModel.TVariants; override;

  end;

  /// <summary>A blockstate model using multiple parts.</summary>
  TMultiPart = class

  end;

  TMultiPartModel = class(TBlockStateModel)
  public type

    TParts = TObjectArray<TMultiPart>;

  private
    FParts: TParts;

  public
    constructor Create(AAssets: TAssetsSettings; AJArray: TJArray);

  end;

  /// <summary>A collection of all block state models.</summary>
  TBlockStateModelCollection = class
  public type

    TModels = TToObjectMap<string, TBlockStateModel, TStringHasher>;

  private
    FAssets: TAssetsSettings;
    FModels: TModels;
    FPath: string;

    function GetModels: TModels.TReader;

    function Load(APath: string): TBlockStateModel;

  public
    constructor Create(AAssets: TAssetsSettings; APath: string);
    destructor Destroy; override;

    property Models: TModels.TReader read GetModels;

  end;

  /// <summary>Stores textures, models and block states.</summary>
  TAssetsSettings = class(TSettings)
  public const

    DefaultPath = 'Data\assets';

  private
    FPath: string;
    FTextures: TTextureCollection;
    FModelCollection: TModelCollection;
    FBlockStates: TBlockStateModelCollection;

    procedure SetPath(const Value: string);

  protected
    class function GetNameForVersion(AVersion: Integer): string; override;
    procedure DoReload; override;

  public
    destructor Destroy; override;

    class function GetTitle: string; override;

    procedure SetDefaults; override;

    property Path: string read FPath write SetPath;

    property Textures: TTextureCollection read FTextures;
    property ModelCollection: TModelCollection read FModelCollection;
    property BlockStates: TBlockStateModelCollection read FBlockStates;

    procedure DefineJStorage(ASerializer: TJSerializer); override;

  end;

implementation

{ TTexture }

constructor TTexture.Create(AName: string; AImage: IGPBitmap);
var
  I: Integer;
begin
  FName := AName;
  FFrames := TFrames.Create;
  if AImage.Width = AImage.Height then
  begin
    FFrames.Add(AImage);
  end
  else
  begin
    for I := 0 to AImage.Height div AImage.Width - 1 do
      FFrames.Add(AImage.Clone(0, Cardinal(I) * AImage.Width, AImage.Width, AImage.Width, AImage.PixelFormat));
  end;
  FTintColor := ColorWhite;
end;

constructor TTexture.Create(AName: string; AImage: IGPBitmap; ATintColor: TColorRGB);
begin
  Create(AName, AImage);
  FTintColor := ATintColor;
end;

destructor TTexture.Destroy;
begin
  FFrames.Free;
  inherited;
end;

function TTexture.GetFrames: TFrames.TReader;
begin
  Result := FFrames.Reader;
end;

function TTexture.GetImage: IGPBitmap;
begin
  Result := FFrames.First;
end;

{ TTextureCollection }

constructor TTextureCollection.Create(APath: string);
var
  TintIndexPath: string;
  JTintIndex: TJObject;
begin
  FTextures := TTextureMap.Create;

  TintIndexPath := TPath.Combine(APath, 'tintindex.json');
  if TFile.Exists(TintIndexPath) then
    JTintIndex := TJObject.CreateFromFile(TintIndexPath)
  else
    JTintIndex := nil;

  try
    LoadRecursive(APath, '', JTintIndex);

  finally
    JTintIndex.Free;

  end;
end;

destructor TTextureCollection.Destroy;
begin
  FTextures.Free;
  inherited;
end;

function TTextureCollection.Exists(AName: string): Boolean;
begin
  Result := Textures.KeyExists(AName);
end;

function TTextureCollection.Get(AName: string; out ATexture: TTexture): Boolean;
begin
  Result := Textures.Get(AName, ATexture);
end;

function TTextureCollection.GetTextures: TTextureMap.TReader;
begin
  Result := FTextures.Reader;
end;

procedure TTextureCollection.LoadRecursive(APath, AFilePrefix: string; AJTintIndex: TJObject);
var
  Path, Name: string;
  JTintIndex: TJArray;
  Color: TColorRGB;
begin
  for Path in TDirectory.GetFiles(APath, '*.png') do
  begin
    Name := AFilePrefix + ChangeFileExt(ExtractFileName(Path), '');
    JTintIndex := AJTintIndex[Name].AsArray;
    if JTintIndex.Exists then
    begin
      Color := TColorRGB.Create(JTintIndex[0], JTintIndex[1], JTintIndex[2]);
      FTextures[Name] := TTexture.Create(Name, TGPBitmap.Create(Path), Color);
    end
    else
      FTextures[Name] := TTexture.Create(Name, TGPBitmap.Create(Path));
  end;
  for Path in TDirectory.GetDirectories(APath) do
    LoadRecursive(Path, ExtractFileName(Path) + '/', AJTintIndex);
end;

{ TAssetsSettings }

procedure TAssetsSettings.DefineJStorage(ASerializer: TJSerializer);
begin
  inherited;
  with ASerializer do
  begin
    Define('path', FPath);
  end;
end;

destructor TAssetsSettings.Destroy;
begin
  FTextures.Free;
  FModelCollection.Free;
  FBlockStates.Free;
  inherited;
end;

procedure TAssetsSettings.DoReload;
begin
  FreeAndNil(FTextures);
  FreeAndNil(FModelCollection);
  FreeAndNil(FBlockStates);
  FTextures := TTextureCollection.Create(TPath.Combine(Path, 'textures'));
  FModelCollection := TModelCollection.Create(Self);
  FModelCollection.Load(TPath.Combine(Path, 'models'));
  FBlockStates := TBlockStateModelCollection.Create(Self, TPath.Combine(Path, 'blockstates'));
end;

class function TAssetsSettings.GetNameForVersion(AVersion: Integer): string;
begin
  Result := 'mc_assets';
end;

class function TAssetsSettings.GetTitle: string;
begin
  Result := 'Assets';
end;

procedure TAssetsSettings.SetDefaults;
begin
  FPath := DefaultPath;
end;

procedure TAssetsSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Reload;
end;

{ TModelCollection }

constructor TModelCollection.Create(AAssets: TAssetsSettings);
begin
  FAssets := AAssets;
  FModels := TModels.Create;
  FBlockModels := TBlockModels.Create;
  FItemModels := TItemModels.Create;
end;

destructor TModelCollection.Destroy;
begin
  FModels.Free;
  FBlockModels.Free;
  FItemModels.Free;
  inherited;
end;

function TModelCollection.ExtractName(APath: string): string;
begin
  Result := ChangeFileExt(APath.Substring(APath.LastIndexOfAny(['/', '\']) + 1), '');
end;

function TModelCollection.GetBlockModels: TBlockModels.TReader;
begin
  Result := FBlockModels.Reader;
end;

function TModelCollection.GetItemModels: TItemModels.TReader;
begin
  Result := FItemModels.Reader;
end;

function TModelCollection.GetModels: TModels.TReader;
begin
  Result := FModels.Reader;
end;

procedure TModelCollection.Load(APath: string);
var
  FileName: string;
begin
  FPath := APath;
  for FileName in TDirectory.GetFiles(TPath.Combine(APath, 'block'), '*.json') do
    LoadBlock(FileName);
  for FileName in TDirectory.GetFiles(TPath.Combine(APath, 'item'), '*.json') do
    LoadItem(FileName);
end;

function TModelCollection.LoadBlock(AFileName: string): TBlockModel;
var
  Name: string;
  JObject: TJObject;
begin
  Name := ExtractName(AFileName);
  if FBlockModels.Get(Name, Result) then
    Exit;
  if not TFile.Exists(AFileName) then
    raise Exception.Create('Block doesn''t exist.');
  JObject := TJObject.CreateFromFile(AFileName);
  try
    Result := TBlockModel.Create(Assets, JObject);
  finally
    JObject.Free;
  end;
  FModels['block/' + Name] := Result;
  FBlockModels[Name] := Result;
end;

function TModelCollection.LoadFromName(AName: string): TModel;
begin
  AName := AName + '.json';
  if AName.StartsWith('block/') then
    Exit(LoadBlock(TPath.Combine(Path, AName)));
  if AName.StartsWith('item/') then
    Exit(LoadItem(TPath.Combine(Path, AName)));
  raise Exception.Create('Model doesn''t exist.');
end;

function TModelCollection.LoadItem(AFileName: string): TItemModel;
var
  Name: string;
  JObject: TJObject;
begin
  Name := ExtractName(AFileName);
  if FItemModels.Get(Name, Result) then
    Exit;
  if not TFile.Exists(AFileName) then
    raise Exception.Create('Item doesn''t exist.');
  JObject := TJObject.CreateFromFile(AFileName);
  try
    Result := TItemModel.Create(Assets, JObject);
  finally
    JObject.Free;
  end;
  FModels['item/' + Name] := Result;
  FItemModels[Name] := Result;
end;

{ TBlockStateModelCollection }

constructor TBlockStateModelCollection.Create(AAssets: TAssetsSettings; APath: string);
var
  FileName: string;
begin
  FAssets := AAssets;
  FPath := APath;
  FModels := TModels.Create;
  for FileName in TDirectory.GetFiles(APath, '*.json') do
    Load(FileName);
end;

destructor TBlockStateModelCollection.Destroy;
begin
  FModels.Free;
  inherited;
end;

function TBlockStateModelCollection.GetModels: TModels.TReader;
begin
  Result := FModels.Reader;
end;

function TBlockStateModelCollection.Load(APath: string): TBlockStateModel;
var
  JObject, JVariants: TJObject;
  JMultipart: TJArray;
  Name: string;
begin
  JObject := TJObject.CreateFromFile(APath);
  Name := ChangeFileExt(ExtractFileName(APath), '');
  try
    JVariants := JObject['variants'].AsObject;
    if JVariants.Exists then
    begin
      Result := TVariantModel.Create(FAssets, JVariants);
      FModels[Name] := Result;
      Exit;
    end;
    JMultipart := JObject['multipart'].AsArray;
    if JMultipart.Exists then
    begin
      Result := TMultiPartModel.Create(FAssets, JMultipart);
      FModels[Name] := Result;
      Exit;
    end;
    raise Exception.Create('Blockstates can only be of type "variants" or "multipart".');
  finally
    JObject.Free;

  end;
end;

{ TModel }

constructor TModel.Create(AAssets: TAssetsSettings; AJObject: TJObject);
var
  JParent, JTextures, JElements: TJWrapper;
  B: TBuiltinType;
  JPair: TJPair;
  ParentName: string;
  Found: Boolean;
  JElement: TJValue;
  JDisplay: TJObject;
  JGUIDisplay: TJObject;
begin
  FAssets := AAssets;

  JParent := AJObject['parent'];
  if JParent.Exists then
  begin
    ParentName := JParent;
    Found := False;
    for B := Low(TBuiltinType) to High(TBuiltinType) do
    begin
      if ParentName = BuiltinTypeNames[B] then
      begin
        FBuiltinType := B;
        Found := True;
        Break;
      end;
    end;
    if not Found then
    begin
      if not Assets.ModelCollection.Models.Get(ParentName, FParent) then
        FParent := Assets.ModelCollection.LoadFromName(ParentName);
    end;
  end;

  JTextures := AJObject['textures'];
  if JTextures.Exists then
  begin
    FTextures := TTextures.Create;
    for JPair in JTextures do
      FTextures[JPair.Key] := TTextureVariable.CreateTyped(Assets.Textures, JPair.Value.AsString);
  end;

  JElements := AJObject['elements'];
  if JElements.Exists then
  begin
    FElements := TElements.Create;
    for JElement in JElements.AsArray do
      FElements.Add(TElement.Create(Self, JElement.AsObject));
  end;

  JDisplay := AJObject['display'].AsObject;
  if JDisplay.Exists then
  begin
    JGUIDisplay := JDisplay['gui'].AsObject;
    if JGUIDisplay.Exists then
      FGUIDisplay := TDisplay.Create(JDisplay['gui'].AsObject);
  end;
end;

destructor TModel.Destroy;
begin
  FGUIDisplay.Free;
  FElements.Free;
  FTextures.Free;
  inherited;
end;

function TModel.GetElements: TElements.TReader;
begin
  if FElements <> nil then
    Exit(FElements.Reader);
  if Parent <> nil then
    Exit(Parent.Elements);
  Result := nil;
end;

function TModel.GetGUIDisplay: TDisplay;
begin
  if FGUIDisplay <> nil then
    Exit(FGUIDisplay);
  if FParent <> nil then
    Exit(Parent.GUIDisplay);
  Result := nil;
end;

function TModel.GetTextures: TTextures.TReader;
begin
  if FTextures <> nil then
    Exit(FTextures.Reader);
  Result := nil;
end;

function TModel.GetTexturesRecursive: TTextures.TReader;
begin
  if FTextures <> nil then
    Exit(FTextures.Reader);
  if Parent <> nil then
    Exit(Parent.TexturesRecursive);
  Result := nil;
end;

function TModel.IsBuiltinType(AType: TBuiltinType): Boolean;
begin
  if BuiltinType = AType then
    Exit(True);
  if Parent <> nil then
    Exit(Parent.IsBuiltinType(AType));
  Result := False;
end;

function TModel.ResolveTexture(var AName: string; out AResolved: string): Boolean;
var
  Variable: TTextureVariable;
begin
  Result := False;
  if Parent <> nil then
    Result := Parent.ResolveTexture(AName, AResolved);
  if Textures = nil then
    Exit;
  if not Textures.Get(AName, Variable) then
    Exit;
  if Variable is TTextureVariableDirect then
  begin
    AResolved := TTextureVariableDirect(Variable).Texture.Name;
    Exit(True);
  end;
  AName := TTextureVariablePlaceholder(Variable).Name;
  Result := False;
end;

{ TModel.TElement }

constructor TModel.TElement.Create(AModel: TModel; AJObject: TJObject);
var
  JVec: TJWrapper;
  JPair: TJPair;
  FaceDir: TBasicDir3;
  JRotation: TJObject;
begin
  FModel := AModel;

  JVec := AJObject['from'];
  FSizePx.C1 := Vec3(JVec[0], JVec[1], JVec[2]);

  JVec := AJObject['to'];
  FSizePx.C2 := Vec3(JVec[0], JVec[1], JVec[2]);

  JRotation := AJObject['rotation'].AsObject;
  if JRotation.Exists then
    FRotation := TRotation.Create(JRotation);

  FShade := AJObject['shade'] or True;

  for JPair in AJObject['faces'] do
  begin
    FaceDir := DirFromName(JPair.Key);
    FFaces[FaceDir] := TFace.Create(Self, FaceDir, JPair.Value.AsObject);
  end;

  FSize := CalcSize;
  FBounds := CalcBounds;
end;

destructor TModel.TElement.Destroy;
var
  Face: TFace;
begin
  FRotation.Free;
  for Face in FFaces do
    Face.Free;
  inherited;
end;

class function TModel.TElement.DirFromName(AName: string): TBasicDir3;
begin
  for Result := Low(FaceNames) to High(FaceNames) do
    if AName = FaceNames[Result] then
      Exit;
  raise Exception.CreateFmt('Unknown face direction "%s".', [AName]);
end;

function TModel.TElement.FaceCount: Integer;
var
  Dir: TBasicDir3;
begin
  Result := 6;
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    if FFaces[Dir] = nil then
      Dec(Result);
end;

function TModel.TElement.FaceExists(ADir: TBasicDir3): Boolean;
begin
  Result := FFaces[ADir] <> nil;
end;

function TModel.TElement.GetFace(ADir: TBasicDir3): TFace;
begin
  Result := FFaces[ADir];
end;

function TModel.TElement.CalcBounds: TAxisSystem3;
begin
  Result.Create(Size);
  if Rotation = nil then
    Exit;
  Result := Result.Rotate(Rotation.Origin, Vec3Axis[Rotation.Axis], Rotation.Angle);
  if Rotation.Rescale then
  begin
    Result.S := Result[0.5];
    if Rotation.Axis <> caX then
      Result.DX := Result.DX / Cos(DegToRad(Rotation.Angle));
    if Rotation.Axis <> caY then
      Result.DY := Result.DY / Cos(DegToRad(Rotation.Angle));
    if Rotation.Axis <> caZ then
      Result.DZ := Result.DZ / Cos(DegToRad(Rotation.Angle));
    Result.S := Result.Point[-0.5];
  end;
end;

function TModel.TElement.CalcSize: TBounds3;
begin
  Result := SizePx / 16;
end;

function TModel.TElement.UsedFaces: TBasicDirs3;
var
  Dir: TBasicDir3;
begin
  Result := [Low(TBasicDir3) .. High(TBasicDir3)];
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    if FFaces[Dir] = nil then
      Exclude(Result, Dir);
end;

{ TVariantModel }

constructor TVariantModel.Create(AAssets: TAssetsSettings; AJObject: TJObject);
var
  Pair: TJPair;
  Parser: TBlockState.TPropertiesParser;
  Properties: TBlockState.TProperties;
  Variants: TVariants;
  JVariant: TJValue;
begin
  FVariantMap := TVariantMap.Create;
  for Pair in AJObject do
  begin
    Parser := TBlockState.TPropertiesParser.Create('[' + Pair.Key + ']', False);
    Properties := Parser.OwnParseResult;
    Parser.Free;

    Variants := TVariants.Create;
    FVariantMap[Properties] := Variants;

    if Pair.Value is TJArray then
    begin
      for JVariant in TJArray(Pair.Value) do
        Variants.Add(TVariant.Create(AAssets, JVariant.AsObject));
    end
    else
      Variants.Add(TVariant.Create(AAssets, Pair.Value.AsObject));
  end;
end;

destructor TVariantModel.Destroy;
begin
  FVariantMap.Free;
  inherited;
end;

function TVariantModel.GetVariantMap: TVariantMap.TReader;
begin
  Result := FVariantMap.Reader;
end;

function TVariantModel.GetVariants(AProperties: TBlockState.TProperties): TBlockStateModel.TVariants;
begin
  Result := TBlockStateModel.TVariants.Create;
  Result.Add(FVariantMap[AProperties].First); // TODO: Random using weight and position
end;

{ TItemModel }

constructor TItemModel.Create(AAssets: TAssetsSettings; AJObject: TJObject);
begin
  inherited;
  // TODO: Load
end;

function TItemModel.GetLayer(AIndex: Integer): TTextureVariable;
begin
  Result := TexturesRecursive[LayerName(AIndex)];
end;

function TItemModel.GetLayerCount: Integer;
var
  Tex: TTextures.TReader;
begin
  Result := 0;
  Tex := TexturesRecursive;
  if Tex <> nil then
    while Tex.KeyExists(LayerName(Result)) do
      Inc(Result);
end;

function TItemModel.LayerName(AIndex: Integer): string;
begin
  Result := LayerPrefix + AIndex.ToString;
end;

{ TBlockModel }

constructor TBlockModel.Create(AAssets: TAssetsSettings; AJObject: TJObject);
begin
  inherited;
  // TODO: Load
end;

{ TTextureVariable }

class function TTextureVariable.CreateTyped(ATextureCollection: TTextureCollection; AName: string): TTextureVariable;
begin
  if AName.StartsWith('#') then
    Result := TTextureVariablePlaceholder.Create(AName.Substring(1))
  else
    Result := TTextureVariableDirect.Create(ATextureCollection.Textures[AName]);
end;

{ TTextureVariableDirect }

constructor TTextureVariableDirect.Create(ATexture: TTexture);
begin
  FTexture := ATexture;
end;

function TTextureVariableDirect.Format: string;
begin
  Result := Texture.Name;
end;

{ TTextureVariablePlaceholder }

constructor TTextureVariablePlaceholder.Create(AName: string);
begin
  FName := AName;
end;

function TTextureVariablePlaceholder.Format: string;
begin
  Result := '#' + Name;
end;

{ TMultiPartModel }

constructor TMultiPartModel.Create(AAssets: TAssetsSettings; AJArray: TJArray);
begin

end;

{ TVariant }

constructor TVariant.Create(AAssets: TAssetsSettings; AJObject: TJObject);
begin
  FModel := AAssets.ModelCollection.Models[AJObject['model']];
  FX := (AJObject['x'] or 0) div 90;
  FY := (AJObject['y'] or 0) div 90;
  FUVLock := AJObject['uvlock'] or False;
  FWeight := AJObject['weight'] or 1;
end;

{ TModel.TFace }

constructor TModel.TFace.Create(AElement: TElement; ADir: TBasicDir3; AJObject: TJObject);
var
  JUV, JCullFace: TJWrapper;
begin
  FElement := AElement;

  JUV := AJObject['uv'];
  if JUV.Exists then
    FUVPx := Bounds2(Vec2(JUV[0], JUV[1]), Vec2(JUV[2], JUV[3]))
  else
    FUVPx := AElement.SizePx.Plane[ADir];

  JCullFace := AJObject['cullface'];
  if JCullFace.Exists then
    FCullFace := TElement.DirFromName(JCullFace)
  else
    FCullFace := ADir;

  FTexture := TTextureVariablePlaceholder.Create(AJObject['texture'].AsString.Substring(1));

  FRotation := (AJObject['rotation'] or 0) div 90;

  FTintindex := AJObject['tintindex'].Exists;

  FUV := CalcUV;
  FUVRotated := CalcUVRotated;
end;

destructor TModel.TFace.Destroy;
begin
  FTexture.Free;
  inherited;
end;

function TModel.TFace.CalcUV: TBounds2;
begin
  Result := UVPx / 16;
  Result.LineY := (1 - Result.LineY).Flip;
end;

function TModel.TFace.CalcUVRotated: TAxisSystem2;
begin
  Result.Create(UV);
  Result := Result.RotateCorners(Rotation);
end;

function TModel.TFace.ResolveTexture(AModel: TModel; out AResolved: string): Boolean;
var
  Name: string;
begin
  Name := Texture.Name;
  Result := AModel.ResolveTexture(Name, AResolved);
end;

{ TModel.TElement.TRotation }

class function TModel.TElement.TRotation.AxisFromName(AName: string): TCoordAxis3;
begin
  for Result := caX to caZ do
    if AName = CoordAxisNamesLow[Result] then
      Exit;
  raise Exception.CreateFmt('Unknown axis "%s".', [AName]);
end;

constructor TModel.TElement.TRotation.Create(AJObject: TJObject);
var
  JOrigin: TJArray;
begin
  JOrigin := AJObject['origin'].AsArray;
  FOriginPx := Vec3(JOrigin[0], JOrigin[1], JOrigin[2]);
  FOrigin := FOriginPx / 16;
  FAxis := AxisFromName(AJObject['axis']);
  FAngle := AJObject['angle'];
  FRescale := AJObject['rescale'] or False;
end;

{ TModel.TDisplay }

constructor TModel.TDisplay.Create(AJObject: TJObject);

  function GetVec(AName: string): TVector3;
  var
    JVec: TJArray;
  begin
    JVec := AJObject[AName].AsArray;
    Result.Create(JVec[0], JVec[1], JVec[2]);
  end;

begin
  FRotation := GetVec('rotation');
  FTranslation := GetVec('translation');
  FScale := GetVec('scale');
end;

end.
