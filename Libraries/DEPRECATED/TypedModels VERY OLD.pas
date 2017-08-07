unit TypedModels;

interface

uses
  Classes, SysUtils, ModelDefine, AdvancedFileStream, TextureManager, VectorGeometry, CustomVAOs, Shaders, Lists,
  BasicTypeClasses, VAOManager, GLEnums, Camera, BitField;

type

  TModelType = (
    mtTextured,
    mtBlock,
    mtAnimated
  );

  { EModelTypeLoadConflict }

  EModelTypeLoadConflict = class (Exception)
  public
    constructor Create(ALoaded, AExpected: TModelType);
  end;

  TRenderModel = class;

  { TCustomModel }
  // abstract baseclass of all Models that extends the Raw Model Structure
  TCustomModel = class abstract
  protected
    type
      TData = record
        Pos: TGVector3;
        Tex: TTexCoord2;
        Normal: TGVector3;
      end;

  private
    FModel: TModel;
    FIsCopy: Boolean;
    FRenderModel: TRenderModel;

  protected
    class function GetModelType: TModelType; virtual; abstract;

    procedure AddToVAO(AVAO: TVAO); virtual; abstract;
    function GetVAOSize: Cardinal; virtual;

    procedure SaveData(const AFileStream: TAdvFileStream); virtual; abstract;
    procedure LoadData(const AFileStream: TAdvFileStream); virtual; abstract;

    procedure NotifyChanges;

    property IsCopy: Boolean read FIsCopy;
  public
    constructor Create(ARenderModel: TRenderModel);

    property Model: TModel read FModel;

    function SaveToFile(const AFilename, AModelFilename: String): Boolean;
    function LoadFromFile(const AFilename: String; AModelList: TModelList): Boolean; overload;
    function LoadFromFile(const AFilename: String; AModelList: TModelList; out AModelFilename: String): Boolean; overload;

    function LoadRawDataFromFile(const AFilename: String; const AModelList: TModelList): Boolean;

    procedure Assign(ACustomModel: TCustomModel); virtual; // Assigns Data loaded from File and sets copy-flag
  end;

  { TRenderModel }

  TRenderModel = class abstract
  private
    FVAO: TVAO;
    FChanged: Boolean;
    FCamera: TCamera;
    FLocation: TLocation;

    procedure InitVAO;
    procedure BuildVAO;

  protected
    property VAO: TVAO read FVAO;
    procedure AddDataToVAO; virtual; abstract;
    function GetVAOSize: Cardinal; virtual; abstract;
  public
    constructor Create(AShader: TShader; ACamera: TCamera);
    destructor Destroy; override;

    property Location: TLocation read FLocation;

    procedure NotifyChanges;

    procedure Render; virtual;
  end;

  { TSingleRenderModel }

  TSingleRenderModel = class (TRenderModel)
  private
    FModel: TCustomModel;
    procedure SetModel(AValue: TCustomModel);
  protected
    function GetVAOSize: Cardinal; override;
    procedure AddDataToVAO; override;
  public
    property Model: TCustomModel write SetModel;
  end;

  { TRenderModelList }

  TRenderModelList = class (TRenderModel)
  private
    FModels: TObjectList;
  protected
    function GetVAOSize: Cardinal; override;
    procedure AddDataToVAO; override;
  public
    constructor Create(AShader: TShader; ACamera: TCamera; AReferenceList: Boolean = True);
    destructor Destroy; override;

    procedure Add(AModel: TCustomModel);
    procedure Clear(ABuildBeforeClear: Boolean = False);
  end;

  { TTexturedModel }
  // bind local 12 texture IDs to the IDs of a TexturePage via name
  // doesn't have a position, can still get positioned via TRenderModel
  TTexturedModel = class (TCustomModel)
  private
    FTexturePage: TTexturePage;
    FTexNames: TStringHashTable;

    function GetTexName(S: String): TString;
    procedure SetTexName(S: String; AValue: TString);

    function GetData(AFace: TFace; APoint: TFacePointID): TData; virtual;

  protected
    class function GetModelType: TModelType; override;

    procedure SaveData(const AFileStream: TAdvFileStream); override;
    procedure LoadData(const AFileStream: TAdvFileStream); override;

    procedure AddToVAO(AVAO: TVAO); override;

  public
    constructor Create(ATexturePage: TTexturePage; AShader: TShader; ARenderModel: TRenderModel = nil);
    destructor Destroy; override;

    property TexNames[S: String]: TString read GetTexName write SetTexName;

    procedure Assign(ACustomModel: TCustomModel); override;
  end;

  { TPositionedModel }

  TPositionedModel = class (TTexturedModel)
  private
    FCamera: TCamera;
    FPos: TGVector3;
    procedure SetPos(AValue: TGVector3);
  protected
    function GetData(AFace: TFace; APoint: TFacePointID): TData; override;
  public
    constructor Create(ATexturePage: TTexturePage; AShader: TShader; ACamera: TCamera; ARenderModel: TRenderModel = nil);

    property Pos: TGVector3 read FPos write SetPos;
  end;

  { TBlockModel }
  // model of a static block, that can hide various sides
  TBlockModel = class (TPositionedModel)
  private
    FSolid: TBitField;
    FSideBlocks: array [TGBasicDir3] of TBlockModel;

    // direction in which a Face in FModel faces
    FFaceDir: array of TGBasicDir;



    function GetSolid(Side: TGBasicDir3): Boolean;
    procedure SetFaceDir(Face: Integer; AValue: TGBasicDir);
    procedure SetSideBlock(Side: TGBasicDir3; AValue: TBlockModel);
    procedure SetSolid(Side: TGBasicDir3; AValue: Boolean);
  protected
    class function GetModelType: TModelType; override;
    procedure AddToVAO(AVAO: TVAO); override;
    function GetVAOSize: Cardinal; override;
  public
    constructor Create(ATexturePage: TTexturePage; AShader: TShader; ACamera: TCamera; ARenderModel: TRenderModel = nil);
    destructor Destroy; override;

    property Solid[Side: TGBasicDir3]: Boolean write SetSolid;
    property FaceDir[Face: Integer]: TGBasicDir write SetFaceDir;

    property SideBlocks[Side: TGBasicDir3]: TBlockModel write SetSideBlock;


  end;

  { TAnimatedModel }
  // model with Animations (animation data in other class)
  TAnimatedModel = class (TTexturedModel)
  private

  protected
    class function GetModelType: TModelType; override;
  public

  end;

  function ModelTypeToString(AModelType: TModelType): String;

implementation

function ModelTypeToString(AModelType: TModelType): String;
begin
  case AModelType of
    mtTextured:
      Result := 'Textured Model';
    mtBlock:
      Result := 'Block Model';
    mtAnimated:
      Result := 'Animated Model';
    else
      Result := 'Unknown Model';
  end;
end;

{ TRenderModelList }

function TRenderModelList.GetVAOSize: Cardinal;
var
  Model: TCustomModel;
begin
  Result := 0;
  for TObject(Model) in FModels do
    Result := Result + Model.GetVAOSize;
end;

procedure TRenderModelList.AddDataToVAO;
var
  Model: TCustomModel;
begin
  for TObject(Model) in FModels do
    Model.AddToVAO(VAO);
end;

constructor TRenderModelList.Create(AShader: TShader; ACamera: TCamera; AReferenceList: Boolean);
begin
  inherited Create(AShader, ACamera);
  FModels := TObjectList.Create(AReferenceList);
end;

destructor TRenderModelList.Destroy;
begin
  FModels.Free;
  inherited Destroy;
end;

procedure TRenderModelList.Add(AModel: TCustomModel);
begin
  FModels.Add(AModel);
  NotifyChanges;
end;

procedure TRenderModelList.Clear(ABuildBeforeClear: Boolean);
begin
  if ABuildBeforeClear then
    BuildVAO;
  FModels.DelAll;
end;

{ TSingleRenderModel }

procedure TSingleRenderModel.SetModel(AValue: TCustomModel);
begin
  if Pointer(FModel) = Pointer(AValue) then
    Exit;
  FModel := AValue;
  NotifyChanges;
end;

function TSingleRenderModel.GetVAOSize: Cardinal;
begin
  Result := FModel.GetVAOSize;
end;

procedure TSingleRenderModel.AddDataToVAO;
begin
  FModel.AddToVAO(VAO);
end;

{ TRenderModel }

procedure TRenderModel.InitVAO;
begin
  FVAO.AddAttribute(3, dtFloat, 'vpos');
  FVAO.AddAttribute(2, dtFloat, 'vtexcoord');
  FVAO.AddAttribute(3, dtFloat, 'vnormal');
  FVAO.GenAttributes;
end;

procedure TRenderModel.BuildVAO;
begin
  FVAO.Generate(GetVAOSize, buStaticDraw);
  FVAO.Map(baWriteOnly);
  AddDataToVAO;
  FVAO.Unmap;
  FChanged := False;
end;

constructor TRenderModel.Create(AShader: TShader; ACamera: TCamera);
begin
  FVAO := TVAO.Create(AShader);
  InitVAO;
  FLocation := TLocation.Create;
  FCamera := ACamera;
end;

destructor TRenderModel.Destroy;
begin
  FLocation.Free;
  FVAO.Free;
  inherited Destroy;
end;

procedure TRenderModel.NotifyChanges;
begin
  FChanged := True;
end;

procedure TRenderModel.Render;
begin
  if FChanged then
    BuildVAO;
  if FLocation.Changed then
  begin
    FCamera.SetModelLocation(FLocation);
    FLocation.NotifyChanges;
  end;
  FCamera.Render;
  FVAO.Render;
end;

{ TPositionedModel }

procedure TPositionedModel.SetPos(AValue: TGVector3);
begin
  if FPos = AValue then
    Exit;
  FPos := AValue;
  NotifyChanges;
end;

function TPositionedModel.GetData(AFace: TFace; APoint: TFacePointID): TData;
begin
  Result := inherited GetData(AFace, APoint);
  Result.Pos := Result.Pos + FPos;
end;

constructor TPositionedModel.Create(ATexturePage: TTexturePage; AShader: TShader; ACamera: TCamera;
  ARenderModel: TRenderModel);
begin
  inherited Create(ATexturePage, AShader, ARenderModel);
  FCamera := ACamera;
end;

{ EModelTypeLoadConflict }

constructor EModelTypeLoadConflict.Create(ALoaded, AExpected: TModelType);
begin
  inherited Create(
    'Model Type conflict while loading! Got ' +
    ModelTypeToString(ALoaded) +
    ', expected ' +
    ModelTypeToString(AExpected)
  );
end;

{ TCustomModel }

function TCustomModel.GetVAOSize: Cardinal;
begin
  Result := FModel.FaceCount * 3;
end;

procedure TCustomModel.NotifyChanges;
begin
  if FRenderModel <> nil then
    FRenderModel.NotifyChanges;
end;

constructor TCustomModel.Create(ARenderModel: TRenderModel);
begin
  FRenderModel := ARenderModel;
  NotifyChanges;
end;

function TCustomModel.SaveToFile(const AFilename, AModelFilename: String): Boolean;
var
  Stream: TAdvFileStream;
begin
  Result := False;
  try
    Stream := TAdvFileStream.Create(AFilename, omWrite);

    // Model Type
    Stream.Write(Ord(GetModelType));

    // Model File
    Stream.WriteAnsiString(AModelFilename);

    SaveData(Stream);
    Result := True;
  finally
    Stream.Free;
  end;
end;

function TCustomModel.LoadFromFile(const AFilename: String; AModelList: TModelList): Boolean;
var
  S: String;
begin
  Result := LoadFromFile(AFilename, AModelList, S);
end;

function TCustomModel.LoadFromFile(const AFilename: String; AModelList: TModelList; out AModelFilename: String): Boolean;
var
  Stream: TAdvFileStream;
  ModelType: TModelType;
begin
  Result := False;
  try
    Stream := TAdvFileStream.Create(AFilename, omRead);

    // Model Type
    ModelType := TModelType(Stream.ReadInteger);
    if ModelType <> GetModelType then
      raise EModelTypeLoadConflict.Create(ModelType, GetModelType);

    // Model File
    AModelFilename := Stream.ReadAnsiString;
    LoadRawDataFromFile(ConcatPaths([ExtractFilePath(AFilename), AModelFilename]), AModelList);

    // Rest
    LoadData(Stream);

    NotifyChanges;
    Result := True;
  except

  end;
  Stream.Free;
end;

function TCustomModel.LoadRawDataFromFile(const AFilename: String; const AModelList: TModelList): Boolean;
begin
  FModel := TModel.Create;
  Result := FModel.LoadFromFile(AFilename);
  AModelList[AFilename] := FModel;
end;

procedure TCustomModel.Assign(ACustomModel: TCustomModel);
begin
  FIsCopy := True;
  FModel := ACustomModel.FModel;
end;

{ TAnimatedModel }

class function TAnimatedModel.GetModelType: TModelType;
begin
  Result := mtAnimated;
end;

{ TBlockModel }

function TBlockModel.GetSolid(Side: TGBasicDir3): Boolean;
begin
  Result := FSolid[Ord(Side)];
end;

procedure TBlockModel.SetFaceDir(Face: Integer; AValue: TGBasicDir);
begin
  if FFaceDir[Face] = AValue then
    Exit;
  FFaceDir[Face] := AValue;
end;

procedure TBlockModel.SetSideBlock(Side: TGBasicDir3; AValue: TBlockModel);
begin
  if Pointer(FSideBlocks[Side]) = Pointer(AValue) then
    Exit;
  FSideBlocks[Side] := AValue;
  NotifyChanges;
end;

procedure TBlockModel.SetSolid(Side: TGBasicDir3; AValue: Boolean);
begin
  FSolid[Ord(Side)] := AValue;
end;

class function TBlockModel.GetModelType: TModelType;
begin
  Result := mtBlock;
end;

procedure TBlockModel.AddToVAO(AVAO: TVAO);
var
  I: Integer;
  P: TTriangleSide;
begin
  for I := 0 to FModel.FaceCount - 1 do
  begin
    if (FFaceDir[I] <> sdNone) and
       (FSideBlocks[FFaceDir[I]].GetSolid(InvertBasicDir(FFaceDir[I]))) then
    begin
      for P := Low(TTriangleSide) to High(TTriangleSide) do
        AVAO.AddVertex(GetData(FModel.Faces[I], P));
    end;
  end;
end;

function TBlockModel.GetVAOSize: Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FModel.FaceCount - 1 do
    if (FFaceDir[I] <> sdNone) and
       (FSideBlocks[FFaceDir[I]].GetSolid(InvertBasicDir(FFaceDir[I]))) then
      Inc(Result);
  Result := Result * 3;
end;

constructor TBlockModel.Create(ATexturePage: TTexturePage; AShader: TShader; ACamera: TCamera;
  ARenderModel: TRenderModel);
begin
  inherited Create(ATexturePage, AShader, ACamera, ARenderModel);
  FSolid := TBitField.Create(6);
end;

destructor TBlockModel.Destroy;
begin
  FSolid.Free;
  inherited Destroy;
end;

{ TTexturedModel }

function TTexturedModel.GetTexName(S: String): TString;
begin
  Result := TString(FTexNames[S]);
end;

procedure TTexturedModel.SetTexName(S: String; AValue: TString);
begin
  FTexNames[S] := AValue;
  NotifyChanges;
end;

function TTexturedModel.GetData(AFace: TFace; APoint: TFacePointID): TData;
begin
  Result.Pos := AFace.GetPos(APoint);
  Result.Tex := FTexturePage.GetTexCoord(TString(FTexNames[FModel.TexLookup[AFace.GetTexture]]).Text,
                                         AFace.GetTexCoord(APoint));
  Result.Normal := AFace.GetNormal(APoint);
end;

class function TTexturedModel.GetModelType: TModelType;
begin
  Result := mtTextured;
end;

procedure TTexturedModel.SaveData(const AFileStream: TAdvFileStream);
var
  Pair: TStringHashTable.TPair;
  Found: Boolean;
  I: Integer;
begin
  AFileStream.Write(FTexNames.Count);
  for Pair in FTexNames do with AFileStream do
  begin
    AFileStream.WriteAnsiString(Pair.Key);
    Found := False;
    for I := 0 to FModel.FaceCount - 1 do
      if FModel.TexLookup[FModel.Faces[I].GetTexture] = Pair.Key then
      begin
        AFileStream.WriteAnsiString(TString(Pair.Data).Text);
        Found := True;
        Break;
      end;
    if not Found then
      AFileStream.WriteAnsiString('');
  end;
end;

procedure TTexturedModel.LoadData(const AFileStream: TAdvFileStream);
var
  I: Integer;
  Count: Cardinal;
  Key: String;
begin
  AFileStream.Read(Count);
  for I := 0 to Count - 1 do
  begin
    Key := AFileStream.ReadAnsiString;
    FTexNames[Key] := TString.Create(AFileStream.ReadAnsiString);
    if TString(FTexNames[Key]).Text = '' then
      Continue;
    if not FTexturePage.TextureExists(TString(FTexNames[Key]).Text) then
      FTexturePage.AddTexture(ConcatPaths([ExtractFilePath(AFileStream.FileName), TString(FTexNames[Key]).Text]), TString(FTexNames[Key]).Text);
    FTexturePage.BuildPage(16, False);
  end;
end;

procedure TTexturedModel.AddToVAO(AVAO: TVAO);
var
  I: Integer;
  P: TTriangleSide;
begin
  for I := 0 to FModel.FaceCount - 1 do
    for P := Low(TTriangleSide) to High(TTriangleSide) do
      AVAO.AddVertex(GetData(FModel.Faces[I], P));
end;

constructor TTexturedModel.Create(ATexturePage: TTexturePage; AShader: TShader; ARenderModel: TRenderModel);
begin
  inherited Create(ARenderModel);
  FTexturePage := ATexturePage;
  FTexturePage.Uniform(AShader, 'tex');
  FTexNames := TStringHashTable.Create;
end;

destructor TTexturedModel.Destroy;
begin
  if not IsCopy then
    FTexNames.Free;
  inherited Destroy;
end;

procedure TTexturedModel.Assign(ACustomModel: TCustomModel);
begin
  inherited Assign(ACustomModel);
  with ACustomModel as TTexturedModel do
  begin
    Self.FTexturePage := FTexturePage;
    Self.FTexNames.Free;
    Self.FTexNames := FTexNames;
  end;
end;

end.

