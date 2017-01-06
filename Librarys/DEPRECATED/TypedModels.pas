unit TypedModels;

interface

uses
  ModelDefine, Lists, SysUtils, AdvancedFileStream, VAOManager, VectorGeometry, Matrix, TextureManager, Shaders,
  GLEnums, BitField, Dialogs;

type

  TModelType = (
    mtNone,
    mtDefault,
    mtBlock,
    mtAnimated
  );

  TBaseModelList = TStringObjectMap<TBaseModel>;
  TTextureLookup = TNotifyArray<String>;

  TSTLTriangle = packed record
    Normal: TGVector3;
    Vertex: array [TTriangleSide] of TGVector3;
    AttribCountDummy: Word; // nobody knows what this should do...
  end;

  { TTypedModel }
  TTypedModel = class;
  TModelClass = class of TTypedModel;
  TTypedModel = class
  public
    type
      TData = record
        Pos: TGVector3;
        Tex: TGVector2;
        Normal, Tangent, Bitangent: TGVector3;
        Border: TGBounds2;
      end;
  private
    FBaseModel: TBaseModel;
    FBaseModelName: String;
    FName: String;
    FBaseModelList: TBaseModelList;

    FTexturePage: TTexturePage;
    FTextureLookup: TTextureLookup;

    FModelChanged: Boolean;

  protected
    procedure SaveData(AStream: TAdvFileStream); virtual;
    procedure LoadData(AStream: TAdvFileStream; {%H-}AModelClass: TModelClass); virtual;

    function GetModelType: TModelType; virtual;

    function Changed: Boolean; virtual;

  public
    constructor Create(ATexturePage: TTexturePage; ABaseModelList: TBaseModelList = nil); virtual;
    destructor Destroy; override;

    function SaveToFile(AFileName: String): Boolean;
    function LoadFromFile(AFileName: String): Boolean;

    function LoadBaseModelFromFile(AFileName: String): Boolean;

    property TextureLookup: TTextureLookup read FTextureLookup;

    procedure AddToVAO(AVAO: TVAO); virtual;
    function GetVAOSize: Integer; virtual;

    procedure NotifyChanges; virtual;

    class function GetModelTypeFromFile(AFileName: String): TModelType;

    property BaseModel: TBaseModel read FBaseModel;
    property Name: String read FName;

    const
      FileExtension = '.tmd';

      AttribNamePos = 'vpos';
      AttribNameTexCoord = 'vtexcoord';
      AttribNameNormal = 'vnormal';
      AttribNameTangent = 'vtangent';
      AttribNameBitangent = 'vbitangent';

  end;

  TBlockModel = class;
  TBlockModelDisplay = class;

  TBlockConditionType = (
    bcLogic,          // logic operation with multiple subconditions
    bcSolid,          // check for side being solid
    bcEqual,          // check for blocktype to be equal
    bcEqualRotation,  // check for rotation to be equal (possible with 3 bcCheckDirs but slightly faster and more easy)
    bcCheckDir,       // check for a single direction to be equal/mirrored
    bcBlockName,      // check for a given blockname
    bcTag             // check for a single tag
  );

  TMirrorType = (
    mtEqual,
    mtMirrored,
    mtDontCare
  );

  { TBlockCondition }
  TBlockCondition = class;
  TBCLogic = class;
  TBlockConditionClass = class of TBlockCondition;
  TBlockCondition = class abstract
  private
    FParentCondition: TBCLogic;

  public
    constructor Create(AParentCondition: TBCLogic = nil); virtual;
    destructor Destroy; override;

    function GetType: TBlockConditionType; virtual; abstract;
    function Check(ABlock: TBlockModelDisplay): Boolean; virtual; abstract;
    procedure Assign(ACondition: TBlockCondition); virtual; abstract;

    class function LoadType(AStream: TAdvFileStream): TBlockConditionType;

    procedure Save(AStream: TAdvFileStream); virtual;
    procedure Load(AStream: TAdvFileStream); virtual; abstract;

    property ParentCondition: TBCLogic read FParentCondition;
  end;

  { TBCCheckSide }

  TBCCheckSide = class abstract (TBlockCondition)
  private
    FDirection: TGBasicDir3;
  public
    constructor Create(AParentCondition: TBCLogic = nil); override;

    property Direction: TGBasicDir3 read FDirection write FDirection;

    procedure Assign(ACondition: TBlockCondition); override;

    procedure Save(AStream: TAdvFileStream); override;
    procedure Load(AStream: TAdvFileStream); override;
  end;

  { TBCSolid }

  TBCSolid = class (TBCCheckSide)
  public
    function GetType: TBlockConditionType; override;
    function Check(ABlock: TBlockModelDisplay): Boolean; override;
  end;

  { TBCEqual }

  TBCEqual = class (TBCCheckSide)
  public
    function GetType: TBlockConditionType; override;
    function Check(ABlock: TBlockModelDisplay): Boolean; override;
  end;

  { TBCEqualRotation }

  TBCEqualRotation = class (TBCCheckSide)
  public
    function GetType: TBlockConditionType; override;
    function Check(ABlock: TBlockModelDisplay): Boolean; override;
  end;

  { TBCCheckDir }

  TBCCheckDir = class (TBCCheckSide)
  private
    FOtherDir: TGBasicDir3;
    FOwnDir: TGBasicDir3;
    FMirrored: TMirrorType;
  public
    constructor Create(AParentCondition: TBCLogic = nil); override;

    property OtherDir: TGBasicDir3 read FOtherDir write FOtherDir;
    property OwnDir: TGBasicDir3 read FOwnDir write FOwnDir;
    property Mirrored: TMirrorType read FMirrored write FMirrored;

    function GetType: TBlockConditionType; override;
    function Check(ABlock: TBlockModelDisplay): Boolean; override;
    procedure Assign(ACondition: TBlockCondition); override;

    procedure Save(AStream: TAdvFileStream); override;
    procedure Load(AStream: TAdvFileStream); override;
  end;

  { TBCBlockName }

  TBCBlockName = class (TBCCheckSide)
  private
    FBlockName: String;
    FBlockType: TBlockModel;

  public
    property BlockName: String read FBlockName write FBlockName;

    function GetType: TBlockConditionType; override;
    function Check(ABlock: TBlockModelDisplay): Boolean; override;
    procedure Assign(ACondition: TBlockCondition); override;

    procedure Save(AStream: TAdvFileStream); override;
    procedure Load(AStream: TAdvFileStream); override;

  end;

  TLogicOperation = (loNever, loAlways, loAllTrue, loAnyTrue, loAnyFalse, loAllFalse);

  { TBCTag }

  TBCTag = class (TBCCheckSide)
  private
    FTag: String;
  public
    property Tag: String read FTag write FTag;

    function GetType: TBlockConditionType; override;
    function Check(ABlock: TBlockModelDisplay): Boolean; override;
    procedure Assign(ACondition: TBlockCondition); override;

    procedure Save(AStream: TAdvFileStream); override;
    procedure Load(AStream: TAdvFileStream); override;
  end;

  { TBCLogic }

  TBCLogic = class (TBlockCondition)
  private
    FConditions: TObjectArray<TBlockCondition>;
    FOperation: TLogicOperation;

    function GetCondition(I: Integer): TBlockCondition;
    function GetConditionCount: Integer;
  public
    constructor Create(AParentCondition: TBCLogic = nil); override;
    destructor Destroy; override;

    property Operation: TLogicOperation read FOperation write FOperation;

    function GetType: TBlockConditionType; override;
    function Check(ABlock: TBlockModelDisplay): Boolean; override;
    procedure Assign(ACondition: TBlockCondition); override;

    property Conditions[I: Integer]: TBlockCondition read GetCondition; default;
    property ConditionCount: Integer read GetConditionCount;

    function AddCondition(AConditionType: TBlockConditionType): TBlockCondition;
    procedure DelCondition(AIndex: Integer); overload;
    procedure DelCondition(ACondition: TBlockCondition); overload;

    function MoveCondition(ACondition: TBlockCondition; AUp: Boolean): Boolean;


    function GetEnumerator: TObjectArray<TBlockCondition>.TIterator;

    procedure Save(AStream: TAdvFileStream); override;
    procedure Load(AStream: TAdvFileStream); override;
  end;

  { TBlockAction }

  TBlockAction = class
  private
    FHide: TBitField;
    FShow: TBitField;

    FDisplayName: String;
    FCondition: TBlockCondition;
  public
    constructor Create(AFaceCount: Cardinal; AConditionType: TBlockConditionType);
    destructor Destroy; override;

    property Hide: TBitField read FHide;
    property Show: TBitField read FShow;

    property DisplayName: String read FDisplayName write FDisplayName;
    property Condition: TBlockCondition read FCondition;

    procedure Assign(AAction: TBlockAction);
  end;

  { TBlockModel }

  TBlockModel = class (TTypedModel)
  private
    FSolidSides: TGBasicDirs3;
    FActions: TObjectArray<TBlockAction>;
    FTags: TTags;

    FBlockDisplay: TBlockModelDisplay;
    // gets generated when FBlockDisplay is set
    FVisibleFaces: TBitField;

    function GetAction(I: Integer): TBlockAction;
    function GetActionCount: Integer;
    function GetSolid(ADir: TGBasicDir3): Boolean;
    procedure SetSolid(ADir: TGBasicDir3; AValue: Boolean);

  protected
    function GetModelType: TModelType; override;

    procedure SaveData(AStream: TAdvFileStream); override;
    procedure LoadData(AStream: TAdvFileStream; AModelClass: TModelClass); override;

  public
    constructor Create(ATexturePage: TTexturePage; ABaseModelList: TBaseModelList = nil); override;
    destructor Destroy; override;

    procedure DelAction(I: Integer); overload;
    procedure DelAction(ABlockAction: TBlockAction); overload;
    function AddAction(AConditionType: TBlockConditionType): TBlockAction;

    function MoveAction(ABlockAction: TBlockAction; AUp: Boolean): Boolean;

    property Actions[I: Integer]: TBlockAction read GetAction;
    property ActionCount: Integer read GetActionCount;

    function ImportFromFile(const AFileName: String): Boolean;

    property Solid[ADir: TGBasicDir3]: Boolean read GetSolid write SetSolid;

    property Tags: TTags read FTags;

    function GetMaxVAOSize: Integer; // just calls old GetVAOSize

    procedure GetVisibleFaces(AVisibleFaces: TBitfield);

    property BlockDisplay: TBlockModelDisplay read FBlockDisplay;

    // Set the state with SetBlockDisplay before calling GetVAOSize or AddToVAO
    // BlockDisplay automatically resets after GetVAOSize or AddToVAO gets called
    procedure SetBlockDisplay(ABlockDisplay: TBlockModelDisplay);
    function GetVAOSize: Integer; override;
    procedure AddToVAO(AVAO: TVAO); override;
  end;

  { TAnimatedModel }

  TAnimatedModel = class (TTypedModel)
  private
  protected
    function GetModelType: TModelType; override;
  public
  end;

  // new TTypedModel descendants here

  TTypedModelList = TStringObjectMap<TTypedModel>;
  TBlockModelList = TStringObjectMap<TBlockModel>;

  { TTypedModelList }
  {
  TTypedModelList = class (TStringHashTable)
  private
    function GetModel(const AFileName: String): TTypedModel;
    procedure SetModel(const AFileName: String; AValue: TTypedModel);
  public
    property Models[AFileName: String]: TTypedModel read GetModel write SetModel; default;
  end;
  }
  { TBlockModelList }
  {
  TBlockModelList = class (TStringHashTable)
  private
    function GetModel(const AFileName: String): TBlockModel;
    procedure SetModel(const AFileName: String; AValue: TBlockModel);
  public
    property Models[AFileName: String]: TBlockModel read GetModel write SetModel; default;
  end;
  }
  { TModelDisplay }

  TModelDisplay = class (TAutoUpdateVAO)
  private
    FLocation: TLocation;

  protected
    FTypedModel: TTypedModel;

    procedure BuildVAO; override;
    function CheckForChanges: Boolean; override;
    procedure BeforeRender; override;

    function GetModelMatrix: TMatrix4; override;

  public
    constructor Create(AShader: TShader);
    destructor Destroy; override;

    procedure Load(
      AName: String;
      ATexturePage: TTexturePage;
      ATypedModelList: TTypedModelList = nil;
      ABaseModelList: TBaseModelList = nil);

    property Location: TLocation read FLocation;

  end;

  { TReferencedModelDisplay }

  TReferencedModelDisplay = class (TModelDisplay)
  private
    procedure SetTypedModel(AValue: TTypedModel);
  public
    destructor Destroy; override;

    property TypedModel: TTypedModel read FTypedModel write SetTypedModel;
  end;

  { TReferencedBlockModelDisplay }

  TReferencedBlockModelDisplay = class (TReferencedModelDisplay)
  private
    FModelDisplay: TBlockModelDisplay;

    function GetBlockModel: TBlockModel;
    function GetRotation: TBlockRotation;

    procedure SetBlockModel(AValue: TBlockModel);

  protected
    function CheckForChanges: Boolean; override;

    procedure BuildVAO; override;

    function GetBounds: TGBounds3; override;
  public
    constructor Create(AShader: TShader);
    destructor Destroy; override;

    property BlockModel: TBlockModel read GetBlockModel write SetBlockModel;
    property Rotation: TBlockRotation read GetRotation;

    function HasBounds: Boolean; override;
  end;

  { TModelDisplayList }

  TModelDisplayList = class

  end;

  { TBlockModelDisplay }
  // only use in TBlockModelDisplayList
  TBlockModelDisplay = class
  private
    FBlockModel: TBlockModel;

    FRotation: TBlockRotation;
    FPosition: TIntVector;
    FSideBlocks: array [TGBasicDir3] of TBlockModelDisplay;

    function GetSideBlock(ADir: TGBasicDir3): TBlockModelDisplay;
    function GetSideBlockI(ADir: TGBasicDir3): TBlockModelDisplay;
    function GetSideBlockR(ADir: TGBasicDir3): TBlockModelDisplay;

    function GetSolid(ADir: TGbasicDir3): Boolean;
    function GetSolidI(ADir: TGBasicDir3): Boolean;
    function GetSolidR(ADir: TGBasicDir3): Boolean;

    procedure SetSideBlock(ADir: TGBasicDir3; AValue: TBlockModelDisplay);

  public
    constructor Create(ABlockModel: TBlockModel; APosition: TIntVector);
    destructor Destroy; override;

    procedure AddToVAO(AVAO: TVAO);
    function GetVAOSize: Cardinal;

    property SideBlock[ADir: TGBasicDir3]: TBlockModelDisplay read GetSideBlock write SetSideBlock;
    property SideBlockR[ADir: TGBasicDir3]: TBlockModelDisplay read GetSideBlockR;
    property SideBlockI[ADir: TGBasicDir3]: TBlockModelDisplay read GetSideBlockI;

    property Solid[ADir: TGbasicDir3]: Boolean read GetSolid;
    property SolidR[ADir: TGBasicDir3]: Boolean read GetSolidR;
    property SolidI[ADir: TGBasicDir3]: Boolean read GetSolidI;

    function HasTag(const FTag: String): Boolean;

    property Rotation: TBlockRotation read FRotation;
    property Pos: TIntVector read FPosition;

    function ModelLoaded: Boolean;
    property Model: TBlockModel read FBlockModel;
  end;

  { TBlockModelDisplayList }

  TBlockModelDisplayList = class (TAutoUpdateVAO)
  private
    FLocation: TLocation;
    FSize: TIntVector;
    FVAOSize: Cardinal;
    FUpdateCounter: Cardinal;
    FBlockCount: Cardinal;

    // used for next SetBlock
    FRotation: TBlockRotation;

    FBlocks: array of array of array of TBlockModelDisplay;

    function GetBlock(APos: TIntVector): TBlockModel; overload;
    function GetBlock(AX, AY, AZ: Integer): TBlockModel; overload;
    procedure SetBlock(APos: TIntVector; AValue: TBlockModel); overload;
    procedure SetBlock(AX, AY, AZ: Integer; AValue: TBlockModel); overload;

    function GetBlockDisplay(AX, AY, AZ: Integer): TBlockModelDisplay; overload;
    function GetBlockDisplay(APos: TIntVector): TBlockModelDisplay; overload;

    function RangeCheck(APos: TIntVector): Boolean;
    procedure RangeCheckError(APos: TIntVector);

  protected
    procedure BuildVAO; override;
    procedure BeforeRender; override;

    function GetModelMatrix: TMatrix4; override;

    property Blocks[APos: TIntVector]: TBlockModelDisplay read GetBlockDisplay;

    function GetBounds: TGBounds3; override;

  public
    constructor Create(AShader: TShader; ASize: TIntVector);
    destructor Destroy; override;

    // Methods to call before setting a BlockType
    procedure SetBlockRotation(ARotation: TBlockRotation);

    property BlockTypes[APos: TIntVector]: TBlockModel read GetBlock write SetBlock; default;

    procedure GetBlockRotation(APos: TIntVector; ABlockRotation: TBlockRotation);

    function IsAir(APos: TIntVector): Boolean; overload;
    function IsAir(AX, AY, AZ: Integer): Boolean; overload;

    property Location: TLocation read FLocation;

    property Size: TIntVector read FSize;

    property BlockCount: Cardinal read FBlockCount;

    procedure BeginUpdate;
    procedure EndUpdate;

    // call this if a block model changes
    procedure RebuildAll;
    procedure ReplaceModel(AFrom, ATo: TBlockModel);

    function GetLastAir(ALine: TGLine; AMin, AMax: Single; out APos: TIntVector): Boolean;
    function GetFirstBlock(ALine: TGLine; AMin, AMax: Single; out APos: TIntVector): Boolean;
    function GetLastDir(ALine: TGLine; AMin, AMax: Single; out ADir: TGBasicDir): Boolean;

    procedure SetRealLocation(APos: TIntVector; ALocation: TLocation);
    function GetRealPos(APos: TIntVector): TGVector3;

    function SaveToSTL(AFilename: String): Boolean;

    function HasBounds: Boolean; override;
  end;

const
  LogicOperationStrs: array [TLogicOperation] of String = (
    'Never [FALSE]',
    'Always [TRUE]',
    'All True [AND]',
    'Any True [OR]',
    'Any False [NAND]',
    'All False [NOR]'
  );

  ModelClasses: array [TModelType] of TModelClass = (
    nil,
    TTypedModel,
    TBlockModel,
    TAnimatedModel
  );

  ConditionClasses: array [TBlockConditionType] of TBlockConditionClass = (
    TBCLogic,
    TBCSolid,
    TBCEqual,
    TBCEqualRotation,
    TBCCheckDir,
    TBCBlockName,
    TBCTag
  );

implementation

{ TBCCheckDir }

constructor TBCCheckDir.Create(AParentCondition: TBCLogic);
begin
  inherited Create(AParentCondition);
  FOtherDir := sdRight;
  FOwnDir := sdRight;
end;

function TBCCheckDir.GetType: TBlockConditionType;
begin
  Result := bcCheckDir;
end;

function TBCCheckDir.Check(ABlock: TBlockModelDisplay): Boolean;
var
  Other: TBlockModelDisplay;
begin
  Other := ABlock.SideBlockR[Direction];
  Result := (Other <> nil) and (
            (Mirrored = mtEqual) and
            (ABlock.Rotation.Convert(OwnDir) = Other.Rotation.Convert(OtherDir)) or
            (Mirrored = mtMirrored) and
            (ABlock.Rotation.Convert(OwnDir) = InvertBasicDir(Other.Rotation.Convert(OtherDir))) or
            (Mirrored = mtDontCare) and
            (AbsBasicDir(ABlock.Rotation.Convert(OwnDir)) = AbsBasicDir(Other.Rotation.Convert(OtherDir))));
end;

procedure TBCCheckDir.Assign(ACondition: TBlockCondition);
begin
  inherited Assign(ACondition);
  if ACondition is TBCCheckDir then with ACondition as TBCCheckDir do
  begin
    Self.OtherDir := OtherDir;
    Self.OwnDir := OwnDir;
    Self.Mirrored := Mirrored;
  end;
end;

procedure TBCCheckDir.Save(AStream: TAdvFileStream);
begin
  inherited Save(AStream);
  AStream.Write(Ord(OtherDir));
  AStream.Write(Ord(OwnDir));
  AStream.Write(Ord(Mirrored));
end;

procedure TBCCheckDir.Load(AStream: TAdvFileStream);
begin
  inherited Load(AStream);
  OtherDir := TGBasicDir(AStream.ReadInteger);
  OwnDir := TGBasicDir(AStream.ReadInteger);
  Mirrored := TMirrorType(AStream.ReadInteger);
end;

{ TBCEqualRotation }

function TBCEqualRotation.Check(ABlock: TBlockModelDisplay): Boolean;
var
  Other: TBlockModelDisplay;
begin
  Other := ABlock.SideBlockR[FDirection];
  Result := (Other <> nil) and
            ABlock.Rotation.Equal(Other.Rotation);
end;

function TBCEqualRotation.GetType: TBlockConditionType;
begin
  Result := bcEqualRotation;
end;

{ TReferencedBlockModelDisplay }

function TReferencedBlockModelDisplay.GetBlockModel: TBlockModel;
begin
  Result := FTypedModel as TBlockModel;
end;

function TReferencedBlockModelDisplay.GetRotation: TBlockRotation;
begin
  Result := FModelDisplay.Rotation;
end;

procedure TReferencedBlockModelDisplay.SetBlockModel(AValue: TBlockModel);
begin
  FTypedModel := AValue;
end;

function TReferencedBlockModelDisplay.CheckForChanges: Boolean;
begin
  Result := inherited CheckForChanges or (BlockModel <> nil) and Rotation.Changed;
end;

procedure TReferencedBlockModelDisplay.BuildVAO;
begin
  BlockModel.SetBlockDisplay(FModelDisplay);
  Generate(FTypedModel.GetVAOSize, buStaticDraw);
  Map(baWriteOnly);
  BlockModel.SetBlockDisplay(FModelDisplay);
  FTypedModel.AddToVAO(Self);
  Unmap;
end;

function TReferencedBlockModelDisplay.GetBounds: TGBounds3;
begin
  Result := TGBounds3.Create(Origin, UVecXYZ);
end;

constructor TReferencedBlockModelDisplay.Create(AShader: TShader);
begin
  inherited Create(AShader);
  FModelDisplay := TBlockModelDisplay.Create(nil, TIntVector.Create(0, 0, 0));
end;

destructor TReferencedBlockModelDisplay.Destroy;
begin
  FModelDisplay.Free;
  inherited Destroy;
end;

function TReferencedBlockModelDisplay.HasBounds: Boolean;
begin
  Result := True;
end;

{ TBlockModelList }
{
function TBlockModelList.GetModel(const AFileName: String): TBlockModel;
begin
  Result := TBlockModel(Data[AFileName]);
end;

procedure TBlockModelList.SetModel(const AFileName: String; AValue: TBlockModel);
begin
  Data[AFileName] := AValue;
end;
}
{ TBCCheckSide }

constructor TBCCheckSide.Create(AParentCondition: TBCLogic);
begin
  inherited Create(AParentCondition);
  FDirection := sdRight;
end;

procedure TBCCheckSide.Assign(ACondition: TBlockCondition);
begin
  // inherited; base is abstract
  if ACondition is TBCCheckSide then with ACondition as TBCCheckSide do
  begin
    Self.FDirection := FDirection;
  end;
end;

procedure TBCCheckSide.Save(AStream: TAdvFileStream);
begin
  inherited Save(AStream);
  AStream.Write(Ord(FDirection));
end;

procedure TBCCheckSide.Load(AStream: TAdvFileStream);
begin
  // inherited; abstract
  FDirection := TGBasicDir3(AStream.ReadInteger);
end;

{ TBCTag }

function TBCTag.Check(ABlock: TBlockModelDisplay): Boolean;
var
  OtherBlock: TBlockModelDisplay;
begin
  OtherBlock := ABlock.SideBlockR[Direction];
  Result := (OtherBlock <> nil) and
            OtherBlock.HasTag(FTag);
end;

function TBCTag.GetType: TBlockConditionType;
begin
  Result := bcTag;
end;

procedure TBCTag.Assign(ACondition: TBlockCondition);
begin
  inherited Assign(ACondition);
  if ACondition is TBCTag then with ACondition as TBCTag do
    Self.FTag := FTag;
end;

procedure TBCTag.Save(AStream: TAdvFileStream);
begin
  inherited Save(AStream);
  AStream.WriteAnsiString(FTag);
end;

procedure TBCTag.Load(AStream: TAdvFileStream);
begin
  inherited Load(AStream);
  FTag := AStream.ReadAnsiString;
end;

{ TBlockCondition }

constructor TBlockCondition.Create(AParentCondition: TBCLogic);
begin
  FParentCondition := AParentCondition;
end;

destructor TBlockCondition.Destroy;
begin
  inherited;
end;

class function TBlockCondition.LoadType(AStream: TAdvFileStream): TBlockConditionType;
begin
  Result := TBlockConditionType(AStream.ReadInteger);
end;

procedure TBlockCondition.Save(AStream: TAdvFileStream);
begin
  AStream.Write(Ord(GetType));
end;

{ TBCLogic }

function TBCLogic.GetCondition(I: Integer): TBlockCondition;
begin
  Result := FConditions[I] as TBlockCondition;
end;

function TBCLogic.GetConditionCount: Integer;
begin
  Result := FConditions.Count;
end;

constructor TBCLogic.Create(AParentCondition: TBCLogic);
begin
  inherited;
  FConditions := TObjectArray<TBlockCondition>.Create;
  FOperation := loAllTrue;
end;

destructor TBCLogic.Destroy;
begin
  FConditions.Free;
  inherited;
end;

procedure TBCLogic.Assign(ACondition: TBlockCondition);
var
  C: TBlockCondition;
begin
  if ACondition is TBCLogic then
  begin
    FConditions.DelAll;
    for TObject(C) in TBCLogic(ACondition).FConditions do
      TBlockCondition(FConditions.Add(ConditionClasses[C.GetType].Create(Self))).Assign(C);
  end;
end;

function TBCLogic.GetType: TBlockConditionType;
begin
  Result := bcLogic;
end;

function TBCLogic.AddCondition(AConditionType: TBlockConditionType): TBlockCondition;
begin
  Result := FConditions.Add(ConditionClasses[AConditionType].Create(Self)) as TBlockCondition;
end;

procedure TBCLogic.DelCondition(AIndex: Integer);
begin
  FConditions.Del(AIndex);
end;

procedure TBCLogic.DelCondition(ACondition: TBlockCondition);
begin
  FConditions.DelObject(ACondition);
end;

function TBCLogic.MoveCondition(ACondition: TBlockCondition; AUp: Boolean): Boolean;
var
  I, J: Integer;
begin
  I := FConditions.FindObject(ACondition);
  if AUp then
    J := I - 1
  else
    J := I + 1;
  if FConditions.RangeCheck(J) then
  begin
    FConditions.Swap(I, J);
    Exit(True);
  end;
  Result := False;
end;

function TBCLogic.Check(ABlock: TBlockModelDisplay): Boolean;
var
  C: TBlockCondition;
begin
  case Operation of
    loNever:
      Exit(False);
    loAlways:
      Exit(True);
    loAllTrue:
    begin
      Result := True;
      for TObject(C) in FConditions do
        if not C.Check(ABlock) then
          Exit(False);
    end;
    loAnyTrue:
    begin
      Result := False;
      for TObject(C) in FConditions do
        if C.Check(ABlock) then
          Exit(True);
    end;
    loAnyFalse:
    begin
      Result := False;
      for TObject(C) in FConditions do
        if not C.Check(ABlock) then
          Exit(True);
    end;
    loAllFalse:
    begin
      Result := True;
      for TObject(C) in FConditions do
        if C.Check(ABlock) then
          Exit(False);
    end;
  end;
end;

function TBCLogic.GetEnumerator: TObjectArray<TBlockCondition>.TIterator;
begin
  Result := FConditions.GetEnumerator;
end;

procedure TBCLogic.Save(AStream: TAdvFileStream);
var
  C: TBlockCondition;
begin
  inherited Save(AStream);
  AStream.Write(Ord(FOperation));
  AStream.Write(ConditionCount);
  for TObject(C) in FConditions do
    C.Save(AStream);
end;

procedure TBCLogic.Load(AStream: TAdvFileStream);
var
  C: TBlockCondition;
  Count, I: Integer;
begin
  // inherited; abstract
  FOperation := TLogicOperation(AStream.ReadInteger);
  Count := AStream.ReadInteger;
  for I := 0 to Count - 1 do
  begin
    C := ConditionClasses[TBlockCondition.LoadType(AStream)].Create(Self);
    C.Load(AStream);
    FConditions.Add(C);
  end;
end;

{ TBCBlockName }

function TBCBlockName.GetType: TBlockConditionType;
begin
  Result := bcBlockName;
end;

function TBCBlockName.Check(ABlock: TBlockModelDisplay): Boolean;
var
  OtherBlock: TBlockModelDisplay;
begin
  OtherBlock := ABlock.SideBlockR[FDirection];
  if OtherBlock = nil then
    Exit(False);
  if FBlockType <> nil then
    Exit((Pointer(OtherBlock.Model) = Pointer(FBlockType)));
  if FBlockName = ABlock.Model.Name then
  begin
    FBlockType := OtherBlock.Model;
    Exit(True);
  end;
  Result := False;
end;

procedure TBCBlockName.Assign(ACondition: TBlockCondition);
begin
  inherited Assign(ACondition);
  if ACondition is TBCBlockName then with ACondition as TBCBlockName do
  begin
    Self.FBlockName := FBlockName;
    Self.FBlockType := FBlockType;
  end;
end;

procedure TBCBlockName.Save(AStream: TAdvFileStream);
begin
  inherited Save(AStream);
  AStream.WriteAnsiString(FBlockName);
end;

procedure TBCBlockName.Load(AStream: TAdvFileStream);
begin
  inherited Load(AStream);
  FBlockName := AStream.ReadAnsiString;
  FBlockType := nil;
end;

{ TBCEqual }

function TBCEqual.GetType: TBlockConditionType;
begin
  Result := bcEqual;
end;

function TBCEqual.Check(ABlock: TBlockModelDisplay): Boolean;
var
  OtherBlock: TBlockModelDisplay;
begin
  OtherBlock := ABlock.SideBlockR[FDirection];
  Result := (OtherBlock <> nil) and
            (Pointer(OtherBlock.Model) = Pointer(ABlock.Model));
end;

{ TBCSolid }

function TBCSolid.GetType: TBlockConditionType;
begin
  Result := bcSolid;
end;

function TBCSolid.Check(ABlock: TBlockModelDisplay): Boolean;
var
  OtherBlock: TBlockModelDisplay;
  C: TGBasicDir3;
begin
  C := ABlock.Rotation.Convert(FDirection);
  OtherBlock := ABlock.SideBlock[C];
  Result := (OtherBlock <> nil) and
            (OtherBlock.SolidI[InvertBasicDir(C)]);
end;

{ TBlockAction }

constructor TBlockAction.Create(AFaceCount: Cardinal; AConditionType: TBlockConditionType);
begin
  FHide := TBitField.Create(AFaceCount);
  FShow := TBitField.Create(AFaceCount);
  FCondition := ConditionClasses[AConditionType].Create;
end;

destructor TBlockAction.Destroy;
begin
  FHide.Free;
  FShow.Free;
  FCondition.Free;
  inherited Destroy;
end;

procedure TBlockAction.Assign(AAction: TBlockAction);
begin
  Hide.Assign(AAction.Hide);
  Show.Assign(AAction.Show);
  DisplayName := AAction.DisplayName;
  Condition.Assign(AAction.Condition);
end;

{ TBlockModelDisplay }

function TBlockModelDisplay.GetSideBlock(ADir: TGBasicDir3): TBlockModelDisplay;
begin
  Result := FSideBlocks[ADir];
end;

function TBlockModelDisplay.GetSideBlockI(ADir: TGBasicDir3): TBlockModelDisplay;
begin
  Result := SideBlock[Rotation.ConvertBack(ADir)];
end;

function TBlockModelDisplay.GetSideBlockR(ADir: TGBasicDir3): TBlockModelDisplay;
begin
  Result := SideBlock[Rotation.Convert(ADir)];
end;

function TBlockModelDisplay.GetSolid(ADir: TGbasicDir3): Boolean;
begin
  Result := ModelLoaded and Model.Solid[ADir];
end;

function TBlockModelDisplay.GetSolidI(ADir: TGBasicDir3): Boolean;
begin
  Result := Solid[FRotation.ConvertBack(ADir)];
end;

function TBlockModelDisplay.GetSolidR(ADir: TGBasicDir3): Boolean;
begin
  Result := Solid[FRotation.Convert(ADir)];
end;

procedure TBlockModelDisplay.SetSideBlock(ADir: TGBasicDir3; AValue: TBlockModelDisplay);
begin
  FSideBlocks[ADir] := AValue;
  if AValue <> nil then
    AValue.FSideBlocks[InvertBasicDir(ADir)] := Self;
end;

procedure TBlockModelDisplay.AddToVAO(AVAO: TVAO);
begin
  if Model.BaseModel <> nil then
  begin
    Model.SetBlockDisplay(Self);
    Model.AddToVAO(AVAO);
  end;
end;

function TBlockModelDisplay.GetVAOSize: Cardinal;
begin
  if Model.BaseModel <> nil then
  begin
    Model.SetBlockDisplay(Self);
    Result := Model.GetVAOSize;
  end
  else
    Result := 0;
end;

function TBlockModelDisplay.HasTag(const FTag: String): Boolean;
begin
  Result := ModelLoaded and Model.Tags[FTag];
end;

function TBlockModelDisplay.ModelLoaded: Boolean;
begin
  Result := FBlockModel <> nil;
end;

constructor TBlockModelDisplay.Create(ABlockModel: TBlockModel; APosition: TIntVector);
begin
  FRotation := TBlockRotation.Create;
  FBlockModel := ABlockModel;
  FPosition := APosition;
end;

destructor TBlockModelDisplay.Destroy;
begin
  FRotation.Free;
  inherited Destroy;
end;

{ TBlockModelDisplayList }

function TBlockModelDisplayList.GetBlockDisplay(APos: TIntVector): TBlockModelDisplay;
begin
  if RangeCheck(APos) then
    Result := FBlocks[APos.X, APos.Y, APos.Z]
  else
    Result := nil;
end;

function TBlockModelDisplayList.GetBlock(APos: TIntVector): TBlockModel;
begin
  if Blocks[APos] <> nil then
    Result := Blocks[APos].Model
  else
    Result := nil;
end;

function TBlockModelDisplayList.GetBlock(AX, AY, AZ: Integer): TBlockModel;
begin
  Result := BlockTypes[TIntVector.Create(AX, AY, AZ)];
end;

procedure TBlockModelDisplayList.SetBlock(APos: TIntVector; AValue: TBlockModel);
var
  S: TGBasicDir;
  P: TIntVector;
begin
  RangeCheckError(APos);

  // check for same block
  if (Pointer(Blocks[APos]) = Pointer(AValue)) or
     (Blocks[APos] <> nil) and
     (Blocks[APos].Rotation.Equal(FRotation)) then
  begin
    FRotation := nil;
    Exit;
  end;

  if FUpdateCounter = 0 then
  begin
    // remove current VAOSize
    if Blocks[APos] <> nil then
      FVAOSize := FVAOSize - Blocks[APos].GetVAOSize;
    for S := sdRight to sdBack do
    begin
      P := APos + VecDir[S];
      if RangeCheck(P) and (Blocks[P] <> nil) then
        FVAOSize := FVAOSize - Blocks[P].GetVAOSize;
    end;
  end;

  // free block if not nil
  if Blocks[APos] <> nil then
  begin
    Blocks[APos].Free;
    Dec(FBlockCount);
  end;

  // update block and all surrounding ones
  if AValue <> nil then
  begin
    // add Block
    FBlocks[APos.X, APos.Y, APos.Z] := TBlockModelDisplay.Create(AValue, APos);
    Inc(FBlockCount);
    for S := sdRight to sdBack do
      Blocks[APos].SideBlock[S] := Blocks[APos + VecDir[S]];
  end
  else
  begin
    // del Block
    FBlocks[APos.X, APos.Y, APos.Z] := nil;
    for S := sdRight to sdBack do
    begin
      P := APos + VecDir[S];
      if RangeCheck(P) and (Blocks[P] <> nil) then
        Blocks[P].SideBlock[InvertBasicDir(S)] := nil;
    end;
  end;

  // update properties before doing VAO stuff
  if FRotation <> nil then
    Blocks[APos].Rotation.Assign(FRotation);

  if FUpdateCounter = 0 then
  begin
    // add new VAOSize back
    if AValue <> nil then
      FVAOSize := FVAOSize + Blocks[APos].GetVAOSize;
    for S := sdRight to sdBack do
    begin
      P := APos + VecDir[S];
      if RangeCheck(P) and (Blocks[P] <> nil) then
        FVAOSize := FVAOSize + Blocks[P].GetVAOSize;
    end;

    NotifyChanges;
  end;

  FRotation := nil;
end;

procedure TBlockModelDisplayList.SetBlock(AX, AY, AZ: Integer; AValue: TBlockModel);
begin
  BlockTypes[TIntVector.Create(AX, AY, AZ)] := AValue;
end;

function TBlockModelDisplayList.GetBlockDisplay(AX, AY, AZ: Integer): TBlockModelDisplay;
begin
  Result := Blocks[TIntVector.Create(AX, AY, AZ)];
end;

function TBlockModelDisplayList.RangeCheck(APos: TIntVector): Boolean;
begin
  Result := (APos >= 0) and (APos < FSize);
end;

procedure TBlockModelDisplayList.RangeCheckError(APos: TIntVector);
begin
  if not RangeCheck(APos) then
    raise Exception.Create('Range error!');
end;

procedure TBlockModelDisplayList.BuildVAO;
var
  X, Y, Z: Integer;
begin
  Generate(FVAOSize, buStaticDraw);
  Map(baWriteOnly);
  for X := 0 to FSize.X - 1 do
    for Y := 0 to FSize.Y - 1 do
      for Z := 0 to FSize.Z - 1 do
        if FBlocks[X, Y, Z] <> nil then
          FBlocks[X, Y, Z].AddToVAO(Self);
  Unmap;
end;

procedure TBlockModelDisplayList.BeforeRender;
begin
  inherited BeforeRender;
  //FCamera.SetModelLocation(FLocation);
end;

function TBlockModelDisplayList.GetModelMatrix: TMatrix4;
begin
  Result := FLocation.Matrix;
end;

function TBlockModelDisplayList.GetBounds: TGBounds3;
begin
  Result := TGBounds3.Create(Origin, Size);
end;

constructor TBlockModelDisplayList.Create(AShader: TShader; ASize: TIntVector);
begin
  inherited Create(AShader);
  SetLength(FBlocks, ASize.X, ASize.Y, ASize.Z);
  FSize := ASize;
  FLocation := TLocation.Create;
end;

destructor TBlockModelDisplayList.Destroy;
var
  X, Y, Z: Integer;
begin
  FLocation.Free;
  for X := 0 to FSize.X - 1 do
    for Y := 0 to FSize.Y - 1 do
      for Z := 0 to FSize.Z - 1 do
        if FBlocks[X, Y, Z] <> nil then
          FBlocks[X, Y, Z].Free;
  inherited;
end;

procedure TBlockModelDisplayList.SetBlockRotation(ARotation: TBlockRotation);
begin
  FRotation := ARotation;
end;

procedure TBlockModelDisplayList.GetBlockRotation(APos: TIntVector; ABlockRotation: TBlockRotation);
begin
  ABlockRotation.Assign(Blocks[APos].Rotation);
end;

function TBlockModelDisplayList.IsAir(APos: TIntVector): Boolean;
begin
  Result := IsAir(APos.X, APos.Y, APos.Z);
end;

function TBlockModelDisplayList.IsAir(AX, AY, AZ: Integer): Boolean;
begin
  // air outside DEFAULT
  Result := not RangeCheck(TIntVector.Create(AX, AY, AZ)) or (FBlocks[AX, AY, AZ] = nil);
  // Solid outside ONLY TESTING
  // Result := RangeCheck(TIntVector.Create(AX, AY, AZ)) and (FBlocks[AX, AY, AZ] = nil);
end;

procedure TBlockModelDisplayList.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TBlockModelDisplayList.EndUpdate;
begin
  if FUpdateCounter = 0 then
    raise Exception.Create('No update to end!');
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    RebuildAll;
end;

procedure TBlockModelDisplayList.RebuildAll;
var
  X, Y, Z: Integer;
begin
  FVAOSize := 0;
  for X := 0 to FSize.X - 1 do
    for Y := 0 to FSize.Y - 1 do
      for Z := 0 to FSize.Z - 1 do
        if FBlocks[X, Y, Z] <> nil then
          FVAOSize := FVAOSize + FBlocks[X, Y, Z].GetVAOSize;
  NotifyChanges;
end;

procedure TBlockModelDisplayList.ReplaceModel(AFrom, ATo: TBlockModel);
var
  X, Y, Z: Integer;
begin
   for X := 0 to FSize.X - 1 do
    for Y := 0 to FSize.Y - 1 do
      for Z := 0 to FSize.Z - 1 do
        if not IsAir(X, Y, Z) and (Pointer(AFrom) = Pointer(FBlocks[X, Y, Z].Model)) then
          BlockTypes[X, Y, Z] := ATo;
end;

function TBlockModelDisplayList.GetLastAir(ALine: TGLine; AMin, AMax: Single; out APos: TIntVector): Boolean;
var
  Last: TIntVector;
begin
  with TBlockRaycaster.Create(FLocation, FSize) do
  begin
    AddMin(AMin);
    AddMax(AMax);

    Start(ALine);

    if not IsAir(Current) then
    begin
      Free;
      Exit(False);
    end;

    while True do
    begin
      Last := Current;

      if not Next then
      begin
        Free;
        Exit(False);
      end;

      if not IsAir(Current) then
      begin
        Free;
        APos := Last;
        Exit(True);
      end;
    end;
  end;
end;

function TBlockModelDisplayList.GetFirstBlock(ALine: TGLine; AMin, AMax: Single; out APos: TIntVector): Boolean;
begin
  with TBlockRaycaster.Create(FLocation, FSize) do
  begin
    AddMin(AMin);
    AddMax(AMax);

    Start(ALine);

    repeat
      if not IsAir(Current) then
      begin
        APos := Current;
        Free;
        Exit(True);
      end;
    until not Next;
    Free;
    Result := False;
  end;
end;

function TBlockModelDisplayList.GetLastDir(ALine: TGLine; AMin, AMax: Single; out ADir: TGBasicDir): Boolean;
begin
  with TBlockRaycaster.Create(FLocation, FSize) do
  begin
    AddMin(AMin);
    AddMax(AMax);

    Start(ALine);

    repeat
      if not IsAir(Current) then
      begin
        ADir := LastDirection;
        Free;
        Exit(True);
      end;
    until not Next;
    Free;
    Result := False;
  end;
end;

procedure TBlockModelDisplayList.SetRealLocation(APos: TIntVector; ALocation: TLocation);
begin
  ALocation.Pos := GetRealPos(APos);
  ALocation.TurnAngle := FLocation.TurnAngle;
  ALocation.PitchAngle := FLocation.PitchAngle;
  ALocation.RollAngle := FLocation.RollAngle;
end;

function TBlockModelDisplayList.GetRealPos(APos: TIntVector): TGVector3;
begin
  Result := FLocation.Matrix * TGVector3(APos).ToVec4;
end;

function TBlockModelDisplayList.SaveToSTL(AFilename: String): Boolean;
const
  Header: String = 'Generated with BlockModelBuilder by Possseidon';

var
  Stream: TAdvFileStream;
  I, X, Y, Z: Integer;
  Triangles: TArray<TSTLTriangle>;
  T: TSTLTriangle;
  V: TTriangleSide;
  B: TBlockModelDisplay;
  RMatrix: TMatrix3;
  Tmp: Single;
  Faces: TBitfield;
begin
  Result := False;
  Stream := TAdvFileStream.Create(AFileName, omWrite);
  Triangles := TArray<TSTLTriangle>.Create;
  Faces := TBitField.Create;
  try
    // Generate Triangles
    T.AttribCountDummy := 0; // just a dummy...
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        for Z := 0 to Size.Z - 1 do
        begin
          B := Blocks[X, Y, Z];
          if B <> nil then
          begin
            B.Model.SetBlockDisplay(B);
            B.Model.GetVisibleFaces(Faces);
            RMatrix := B.Rotation.Matrix;
            for I in Faces do
            begin
              for V := Low(TTriangleSide) to High(TTriangleSide) do
              begin
                T.Vertex[V] := B.Model.BaseModel.Faces[I].GetPos(V);

                T.Vertex[V] := RMatrix * (T.Vertex[V] - 0.5) + 0.5;
                T.Vertex[V] := T.Vertex[V] + B.Pos;

                // Cycle X Y and Z around
                Tmp := T.Vertex[V].Z;
                T.Vertex[V].Z := T.Vertex[V].Y;
                T.Vertex[V].Y := T.Vertex[V].X;
                T.Vertex[V].X := Tmp;
              end;
              T.Normal := T.Vertex[0].VectorTo(T.Vertex[1]).Cross(T.Vertex[0].VectorTo(T.Vertex[2]));
              Triangles.Add(T);
            end;
          end;
        end;

    // 80 Byte Header
    if Length(Header) > 80 then
      raise Exception.CreateFmt('Header too long! %d > 80', [Length(Header)]);
    Stream.Write(Header[1], Length(Header));
    for I := 0 to 79 - Length(Header) do
      Stream.WriteByte(0);

    // Count
    Stream.Write(Triangles.Count, SizeOf(Cardinal));

    I := SizeOf(TSTLTriangle);

    if Triangles.Count > 0 then
      Stream.Write(Triangles.Ptr^, Triangles.Count * SizeOf(TSTLTriangle));

    Result := True;

  finally
    Stream.Free;
    Triangles.Free;
    Faces.Free;
  end;
end;

function TBlockModelDisplayList.HasBounds: Boolean;
begin
  Result := True;
end;

{ TReferencedModelDisplay }

procedure TReferencedModelDisplay.SetTypedModel(AValue: TTypedModel);
begin
  if Pointer(FTypedModel) = Pointer(AValue) then
    Exit;
  FTypedModel := AValue;
  NotifyChanges;
end;

destructor TReferencedModelDisplay.Destroy;
begin
  FTypedModel := nil;
  inherited Destroy;
end;

{ TModelDisplay }

procedure TModelDisplay.BuildVAO;
begin
  Generate(FTypedModel.GetVAOSize, buStaticDraw);
  Map(baWriteOnly);
  FTypedModel.AddToVAO(Self);
  Unmap;
end;

function TModelDisplay.CheckForChanges: Boolean;
begin
  if inherited CheckForChanges then
    Exit(True);
  if FTypedModel = nil then
    Exit(False);
  Result := FTypedModel.Changed;
  FTypedModel.NotifyChanges;
end;

procedure TModelDisplay.BeforeRender;
begin
  inherited BeforeRender;
  //FCamera.SetModelLocation(FLocation);
end;

function TModelDisplay.GetModelMatrix: TMatrix4;
begin
  Result := FLocation.Matrix;
end;

constructor TModelDisplay.Create(AShader: TShader);
begin
  inherited Create(AShader);
  FLocation := TLocation.Create;
end;

destructor TModelDisplay.Destroy;
begin
  FLocation.Free;
  if FTypedModel <> nil then
    FTypedModel.Free;
  inherited Destroy;
end;

procedure TModelDisplay.Load(AName: String; ATexturePage: TTexturePage; ATypedModelList: TTypedModelList;
  ABaseModelList: TBaseModelList);
begin
  if ATypedModelList <> nil then
    FTypedModel := ATypedModelList[AName]
  else
    FTypedModel := nil;
  if FTypedModel = nil then
  begin
    case TTypedModel.GetModelTypeFromFile(AName) of
      mtNone:
        raise Exception.Create('Unknown Model Type');
      mtDefault:
        FTypedModel := TTypedModel.Create(ATexturePage, ABaseModelList);
      mtBlock:
        FTypedModel := TBlockModel.Create(ATexturePage, ABaseModelList);
      mtAnimated:
        FTypedModel := TAnimatedModel.Create(ATexturePage, ABaseModelList);
    end;
    if ATypedModelList <> nil then
      ATypedModelList[AName] := FTypedModel;
  end;
  NotifyChanges;
end;

{ TAnimatedModel }

function TAnimatedModel.GetModelType: TModelType;
begin
  Result := mtAnimated;
end;

{ TBlockModel }

function TBlockModel.GetMaxVAOSize: Integer;
begin
  Result := inherited GetVAOSize;
end;

procedure TBlockModel.GetVisibleFaces(AVisibleFaces: TBitfield);
begin
  AVisibleFaces.Assign(FVisibleFaces);
end;

procedure TBlockModel.SetBlockDisplay(ABlockDisplay: TBlockModelDisplay);
var
  Action: TBlockAction;
begin
  if Pointer(ABlockDisplay) = Pointer(FBlockDisplay) then
    Exit;
  FBlockDisplay := ABlockDisplay;

  FVisibleFaces.SetSize(BaseModel.FaceCount);
  FVisibleFaces.Fill; // default render all

  if FBlockDisplay <> nil then
    for TObject(Action) in FActions do
    begin
      if Action.Condition.Check(ABlockDisplay) then
      begin
        // hide hide-bits, show show-bits
        FVisibleFaces.ClearBits(Action.Hide);
        FVisibleFaces.SetBits(Action.Show);
      end
      else
      begin
        // hide show-bits, show hide-bits
        FVisibleFaces.ClearBits(Action.Show);
        FVisibleFaces.SetBits(Action.Hide);
      end;
    end;

end;

procedure TBlockModel.AddToVAO(AVAO: TVAO);
var
  I: Integer;
  S: TTriangleSide;
  Data: TData;
  P: TGPlane;
  P1, P2, P3: TGPlane.TPointIntsecData;
  RMatrix: TMatrix3;
begin
  if FBlockDisplay <> nil then
    RMatrix := FBlockDisplay.Rotation.Matrix;
  for I in FVisibleFaces do
  begin
    P := FBaseModel.Faces[I].TexPlane;
    P.PointIntsec(Origin, P1);
    P.PointIntsec(UVecX, P2);
    P.PointIntsec(UVecY, P3);

    P := FBaseModel.Faces[I].Plane;

    Data.Tangent := P[P1.PlaneCoord].VectorTo(P[P2.PlaneCoord]).Normalize;
    Data.Bitangent := P[P1.PlaneCoord].VectorTo(P[P3.PlaneCoord]).Normalize;

    Data.Border := FTexturePage.HalfPixelInset(FTexturePage.GetBounds(FTextureLookup[FBaseModel.Faces[I].GetTexture]));

    if FBlockDisplay <> nil then
    begin
      Data.Tangent := RMatrix * Data.Tangent;
      Data.Bitangent := RMatrix * Data.Bitangent;
    end;

    for S := Low(TTriangleSide) to High(TTriangleSide) do
    begin
      Data.Pos := FBaseModel.Faces[I].GetPos(S);
      Data.Tex := FTexturePage.GetTexCoord(FTextureLookup[FBaseModel.Faces[I].GetTexture],
                                           FBaseModel.Faces[I].GetTexCoord(S));
      Data.Normal := FBaseModel.Faces[I].GetNormal(S);

      if FBlockDisplay <> nil then
      begin
        Data.Pos := RMatrix * (Data.Pos - 0.5) + 0.5;
        Data.Pos := Data.Pos + FBlockDisplay.Pos;
        Data.Normal := RMatrix * Data.Normal;
      end;

      AVAO.AddVertex(Data);
    end;
  end;
  SetBlockDisplay(nil);
end;

function TBlockModel.GetVAOSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I in FVisibleFaces do
    Inc(Result);
  Result := Result * 3;
  SetBlockDisplay(nil);
end;

function TBlockModel.GetSolid(ADir: TGBasicDir3): Boolean;
begin
  Result := ADir in FSolidSides;
end;

function TBlockModel.GetAction(I: Integer): TBlockAction;
begin
  Result := FActions[I] as TBlockAction;
end;

function TBlockModel.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

procedure TBlockModel.SetSolid(ADir: TGBasicDir3; AValue: Boolean);
begin
  if AValue then
    Include(FSolidSides, ADir)
  else
    Exclude(FSolidSides, ADir);
end;

function TBlockModel.GetModelType: TModelType;
begin
  Result := mtBlock;
end;

procedure TBlockModel.SaveData(AStream: TAdvFileStream);
var
  Action: TBlockAction;
begin
  inherited SaveData(AStream);
  AStream.Write(Byte(FSolidSides));
  AStream.Write(FTags);
  AStream.Write(ActionCount);
  for TObject(Action) in FActions do
  begin
    // Condition
    Action.Condition.Save(AStream);

    // Action
    AStream.Write(Action.Hide);
    AStream.Write(Action.Show);
    AStream.WriteAnsiString(Action.DisplayName);
  end;
end;

procedure TBlockModel.LoadData(AStream: TAdvFileStream; AModelClass: TModelClass);
var
  I: Integer;
  Action: TBlockAction;
begin
  inherited LoadData(AStream, AModelClass);

  FVisibleFaces.SetSize(BaseModel.FaceCount);
  FVisibleFaces.Fill;

  FActions.DelAll;
  FSolidSides := [];

  FTags.Clear;

  if not AModelClass.InheritsFrom(TBlockModel) then
    Exit;

  FSolidSides := TGBasicDirs3(AStream.ReadByte);
  AStream.Read(FTags);

  for I := 0 to AStream.ReadInteger - 1 do
  begin
    // Condition
    Action := AddAction(TBlockCondition.LoadType(AStream));
    Action.Condition.Load(AStream);
    // Action
    AStream.Read(Action.Hide);
    AStream.Read(Action.Show);
    Action.DisplayName := AStream.ReadAnsiString;
  end;
end;

constructor TBlockModel.Create(ATexturePage: TTexturePage; ABaseModelList: TBaseModelList);
begin
  inherited Create(ATexturePage, ABaseModelList);
  FActions := TObjectArray<TBlockAction>.Create;
  FTags := TTags.Create;
  FVisibleFaces := TBitField.Create;
end;

destructor TBlockModel.Destroy;
begin
  FActions.Free;
  FTags.Free;
  FVisibleFaces.Free;
  inherited Destroy;
end;

procedure TBlockModel.DelAction(I: Integer);
begin
  FActions.Del(I);
end;

procedure TBlockModel.DelAction(ABlockAction: TBlockAction);
begin
  FActions.DelObject(ABlockAction);
end;

function TBlockModel.AddAction(AConditionType: TBlockConditionType): TBlockAction;
begin
  Result := FActions.Add(TBlockAction.Create(GetMaxVAOSize, AConditionType)) as TBlockAction;
end;

function TBlockModel.MoveAction(ABlockAction: TBlockAction; AUp: Boolean): Boolean;
var
  I, J: Integer;
begin
  I := FActions.FindObject(ABlockAction);
  if AUp then
    J := I - 1
  else
    J := I + 1;
  if FActions.RangeCheck(J) then
  begin
    FActions.Swap(I, J);
    Exit(True);
  end;
  Result := False;
end;

function TBlockModel.ImportFromFile(const AFileName: String): Boolean;
var
  Tmp: TBlockModel;
  I: Integer;
  Action: TBlockAction;
begin
  Tmp := TBlockModel.Create(nil, FBaseModelList);
  if not Tmp.LoadFromFile(AFileName) then
  begin
    Tmp.Free;
    Exit(False);
  end;

  for I := 0 to Tmp.ActionCount - 1 do
  begin
    Action := AddAction(Tmp.Actions[I].Condition.GetType);
    Action.Assign(Tmp.Actions[I]);
  end;

  FSolidSides := Tmp.FSolidSides;
  FTags.Assign(Tmp.FTags);

  Result := True;

  Tmp.Free;
end;

{ TTypedModel }

function TTypedModel.GetVAOSize: Integer;
begin
  Result := FBaseModel.FaceCount * 3;
end;

procedure TTypedModel.NotifyChanges;
begin
  FModelChanged := False;
  FTextureLookup.NotifyChanges;
end;

class function TTypedModel.GetModelTypeFromFile(AFileName: String): TModelType;
var
  Stream: TAdvFileStream;
begin
  Result := mtNone;
  try
    Stream := TAdvFileStream.Create(AFileName, omRead);
    Result := TModelType(Stream.ReadInteger);
  finally
    Stream.Free;
  end;
end;

procedure TTypedModel.SaveData(AStream: TAdvFileStream);
var
  I: Integer;
begin
  AStream.Write(Ord(GetModelType));
  AStream.WriteAnsiString(FBaseModelName);
  AStream.Write(FTextureLookup.Count);
  for I := 0 to FTextureLookup.Count - 1 do
    AStream.WriteAnsiString(FTextureLookup[I]);
end;

procedure TTypedModel.LoadData(AStream: TAdvFileStream; AModelClass: TModelClass);
var
  TexCount, I: Integer;
begin
  LoadBaseModelFromFile(ConcatPaths([ExtractFilePath(AStream.FileName), AStream.ReadAnsiString]));
  TexCount := AStream.ReadInteger;

  FTextureLookup.DelAll;
  for I := 0 to TexCount - 1 do
  begin
    FTextureLookup.Add(AStream.ReadAnsiString);
    if (FTexturePage <> nil) and not FTexturePage.TextureExists(FTextureLookup[I]) then
      FTexturePage.AddTextureFromFile(
        ConcatPaths([ExtractFilePath(AStream.FileName), FTextureLookup[I]]), FTextureLookup[I]);
  end;

  if FTexturePage <> nil then
    FTexturePage.BuildPage(16, False);
end;

function TTypedModel.GetModelType: TModelType;
begin
  Result := mtDefault;
end;

function TTypedModel.Changed: Boolean;
begin
  Result := FModelChanged or FTextureLookup.Changed;
end;

procedure TTypedModel.AddToVAO(AVAO: TVAO);
var
  I: Integer;
  S: TTriangleSide;
  Data: TData;
  P: TGPlane;
  P1, P2, P3: TGPlane.TPointIntsecData;
begin
  for I := 0 to FBaseModel.FaceCount - 1 do
  begin
    P := FBaseModel.Faces[I].TexPlane;
    P.PointIntsec(Origin, P1);
    P.PointIntsec(UVecX, P2);
    P.PointIntsec(UVecY, P3);

    P := FBaseModel.Faces[I].Plane;

    Data.Tangent := P[P1.PlaneCoord].VectorTo(P[P2.PlaneCoord]).Normalize;
    Data.Bitangent := P[P1.PlaneCoord].VectorTo(P[P3.PlaneCoord]).Normalize;

    Data.Border := FTexturePage.HalfPixelInset(FTexturePage.GetBounds(FTextureLookup[FBaseModel.Faces[I].GetTexture]));

    for S := Low(TTriangleSide) to High(TTriangleSide) do
    begin
      Data.Pos := FBaseModel.Faces[I].GetPos(S);
      Data.Tex := FTexturePage.GetTexCoord(FTextureLookup[FBaseModel.Faces[I].GetTexture],
                                           FBaseModel.Faces[I].GetTexCoord(S));
      Data.Normal := FBaseModel.Faces[I].GetNormal(S);

      AVAO.AddVertex(Data);
    end;
  end;
end;

constructor TTypedModel.Create(ATexturePage: TTexturePage; ABaseModelList: TBaseModelList);
begin
  FTexturePage := ATexturePage;
  FBaseModelList := ABaseModelList;
  FTextureLookup := TTextureLookup.Create;
end;

destructor TTypedModel.Destroy;
begin
  if FBaseModelList = nil then
    FBaseModel.Free;
  FTextureLookup.Free;
  inherited Destroy;
end;

function TTypedModel.SaveToFile(AFileName: String): Boolean;
var
  Stream: TAdvFileStream;
begin
  Result := False;
  try
    Stream := TAdvFileStream.Create(AFileName, omWrite);

    SaveData(Stream);

    Result := True;
  finally
    Stream.Free;
  end;
end;

function TTypedModel.LoadFromFile(AFileName: String): Boolean;
var
  Stream: TAdvFileStream;
  ModelClass: TModelClass;
begin
  Result := False;
  try
    Stream := TAdvFileStream.Create(AFileName, omRead);

    ModelClass := ModelClasses[TModelType(Stream.ReadInteger)];
    if not (Self is ModelClass) then
      raise Exception.Create('Type conflict while loading Model');

    LoadData(Stream, ModelClass);

    FName := ExtractFileName(AFileName);
    FModelChanged := True;
    Result := True;
  finally
    Stream.Free;
  end;
end;

function TTypedModel.LoadBaseModelFromFile(AFileName: String): Boolean;
var
  NameOnly: String;
begin
  NameOnly := ExtractFileName(AFileName);
  if FBaseModelList <> nil then
  begin
    FBaseModel := FBaseModelList[NameOnly];
    if FBaseModel = nil then
    begin
      FBaseModel := TBaseModel.Create;
      Result := FBaseModel.LoadFromFile(AFileName);
      if not Result then
      begin
        FBaseModel.Free;
        Exit;
      end;
      FBaseModelList[NameOnly] := FBaseModel;
    end
    else
      Result := False;
  end
  else
  begin
    if FBaseModel = nil then
      FBaseModel := TBaseModel.Create;
    Result := FBaseModel.LoadFromFile(AFileName);
    if not Result then
      FBaseModel.Free;
  end;

  if Result then
    FBaseModelName := NameOnly
  else
    FBaseModelName := EmptyStr;

  FTextureLookup.DelAll;
  FModelChanged := True;
end;

{ TTypedModelList }
{
function TTypedModelList.GetModel(const AFileName: String): TTypedModel;
begin
   Result := Data[AFileName] as TTypedModel;
end;

procedure TTypedModelList.SetModel(const AFileName: String; AValue: TTypedModel);
begin
  Data[AFileName] := AValue;
end;
}
{ TBaseModelList }
{
function TBaseModelList.GetModel(AFileName: String): TBaseModel;
begin
  Result := GetEntry(AFileName) as TBaseModel;
end;

procedure TBaseModelList.SetModel(AFileName: String; AValue: TBaseModel);
begin
  SetEntry(AFileName, AValue);
end;
}
end.

