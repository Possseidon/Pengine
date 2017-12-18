unit Pengine.Light;

interface

uses
  dglOpenGL,

  System.SysUtils,

  Pengine.Collections,
  Pengine.UBO,
  Pengine.GLProgram,
  Pengine.Color,
  Pengine.Vector,
  Pengine.Matrix,
  Pengine.GLEnums,
  Pengine.Texture,
  Pengine.Camera,
  Pengine.FBO,
  Pengine.VAO,
  Pengine.GLState;

type

  TLightSystem = class;

  { TBaseLight }

  TBaseLight = class abstract
  private
    FIndex: Integer;
    FUBOOffset: Integer;
    FLightSystem: TLightSystem;

    FColor: TColorRGB;
    FIntensity: Single;

    procedure SetIndex(AValue: Integer);

    procedure SetColor(AValue: TColorRGB);
    procedure SetIntensity(AValue: Single);

    property Index: Integer read FIndex write SetIndex;

  protected
    procedure Send(ASingleOffset: Integer; ASingleCount: Integer; const AData);

    property LightSystem: TLightSystem read FLightSystem;

  public
    constructor Create(ALightSystem: TLightSystem);
    destructor Destroy; override;

    class function MaxLights: Integer; virtual; abstract;
    class function MajorOffset: Integer; virtual; abstract;
    class function CalcMajorOffset: Integer; virtual; abstract;
    class function DataSize: Integer; virtual; abstract;

    procedure DecIndexAndSendAll;

    procedure SendAllData; virtual;

    procedure SendColor;

    property Color: TColorRGB read FColor write SetColor;
    property Intensity: Single read FIntensity write SetIntensity;
  end;

  { TDirectionalLight }

  TDirectionalLight = class(TBaseLight)
  private
    FDirection: TVector3;

    class var
      FMajorOffset: Integer;

    class constructor Create;

  protected
    procedure SetDirection(AValue: TVector3); virtual;

  public
    constructor Create(ALightSystem: TLightSystem);

    class function MaxLights: Integer; override;
    class function CalcMajorOffset: Integer; override;
    class function MajorOffset: Integer; override;
    class function DataSize: Integer; override;

    property Direction: TVector3 read FDirection write SetDirection;

    procedure SendAllData; override;

    procedure SendDirection;
  end;

  { TPointLight }

  TPointLight = class(TBaseLight)
  private
    FAttenuation: Single;

    class var
      FMajorOffset: Integer;

    procedure SetAttenuation(AValue: Single);

    class constructor Create;

  protected
    FPosition: TVector3;

    procedure SetPosition(AValue: TVector3); virtual;

  public
    constructor Create(ALightSystem: TLightSystem);

    class function MaxLights: Integer; override;
    class function CalcMajorOffset: Integer; override;
    class function MajorOffset: Integer; override;
    class function DataSize: Integer; override;

    property Position: TVector3 read FPosition write SetPosition;
    property Attenuation: Single read FAttenuation write SetAttenuation;

    procedure SendAllData; override;

    procedure SendPosition;
    procedure SendAttenuation;
  end;

  { TSpotLight }

  TSpotLight = class(TPointLight)
  private

    class var
      FMajorOffset: Integer;

    function GetFullCutoff: Single;

    class constructor Create;

  protected
    FDirection: TVector3;
    FCutoff: Single;
    FCutoffBonus: Single;

    procedure SetCutoff(AValue: Single); virtual;
    procedure SetCutoffBonus(AValue: Single); virtual;
    procedure SetDirection(AValue: TVector3); virtual;

  public
    constructor Create(ALightSystem: TLightSystem);

    class function MaxLights: Integer; override;
    class function CalcMajorOffset: Integer; override;
    class function MajorOffset: Integer; override;
    class function DataSize: Integer; override;

    property Direction: TVector3 read FDirection write SetDirection;
    property Cutoff: Single read FCutoff write SetCutoff;
    property CutoffBonus: Single read FCutoffBonus write SetCutoffBonus;
    property FullCutoff: Single read GetFullCutoff;

    procedure SendAllData; override;

    procedure SendDirection;
    procedure SendCutoff;
    procedure SendCutoffBonus;
  end;

  { TDirectionalLightShaded }

  TDirectionalLightShaded = class(TDirectionalLight)
  private
    FFBO: TFBO;
    FCamera: TCamera;
    FCameraChanged: Boolean;

    function GetPosition: TVector3;
    function GetNearClip: Single;
    function GetFarClip: Single;
    function GetSize: Single;

    procedure SetPosition(AValue: TVector3);
    procedure SetNearClip(AValue: Single);
    procedure SetFarClip(AValue: Single);
    procedure SetSize(AValue: Single);

    procedure SendCamera;

  protected
    procedure SetDirection(AValue: TVector3); override;

  public
    constructor Create(ALightSystem: TLightSystem);
    destructor Destroy; override;

    procedure AddOccluder(ARenderable: IRenderable);
    procedure DelOccluder(ARenderable: IRenderable);

    procedure AddGLProgram(AGLProgram: TGLProgram);
    procedure DelGLProgram(AGLProgram: TGLProgram);

    procedure RenderShadows;

    property Position: TVector3 read GetPosition write SetPosition;

    property Size: Single read GetSize write SetSize;

    property NearClip: Single read GetNearClip write SetNearClip;
    property FarClip: Single read GetFarClip write SetFarClip;

  end;

  { TPointLightShaded }

  TPointLightShaded = class(TPointLight)
  private
    FFBOs: array [TGLCubeMapSide] of TFBO;
    FCameras: array [TGLCubeMapSide] of TCamera;
    FClipChanged: Boolean;

    function GetFarClip: Single;
    function GetNearClip: Single;
    procedure SetFarClip(AValue: Single);
    procedure SetNearClip(AValue: Single);

    procedure SendClip;

  protected
    procedure SetPosition(AValue: TVector3); override;

  public
    constructor Create(ALightSystem: TLightSystem);
    destructor Destroy; override;

    procedure AddOccluder(ARenderable: IRenderable);
    procedure DelOccluder(ARenderable: IRenderable);

    procedure AddGLProgram(AGLProgram: TGLProgram);
    procedure DelGLProgram(AGLProgram: TGLProgram);

    procedure RenderShadows;

    property NearClip: Single read GetNearClip write SetNearClip;
    property FarClip: Single read GetFarClip write SetFarClip;

  end;

  { TSpotLightShaded }

  TSpotLightShaded = class(TSpotLight)
  private
    FFBO: TFBO;
    FCamera: TCamera;
    FCameraChanged: Boolean;

    //function GetFarClip: Single;
    //function GetNearClip: Single;
    //procedure SetFarClip(AValue: Single);
    //procedure SetNearClip(AValue: Single);

    procedure SendCamera;

  protected
    procedure SetCutoff(AValue: Single); override;
    procedure SetCutoffBonus(AValue: Single); override;
    procedure SetDirection(AValue: TVector3); override;
    procedure SetPosition(AValue: TVector3); override;

  public
    constructor Create(ALightSystem: TLightSystem);
    destructor Destroy; override;

    procedure AddOccluder(ARenderable: IRenderable);
    procedure DelOccluder(ARenderable: IRenderable);

    procedure AddGLProgram(AGLProgram: TGLProgram);
    procedure DelGLProgram(AGLProgram: TGLProgram);

    procedure RenderShadows;

    //property NearClip: Single read GetNearClip write SetNearClip;
    //property FarClip: Single read GetFarClip write SetFarClip;

  end;

  { TLightSystem }

  TLightSystem = class
  private
    FGLState: TGLState;
    FGLProgram: TGLProgram;

    FAmbient: TColorRGB;

    FDepthOnlyUniform: TGLProgram.TUniform<Boolean>;

    FDirectionalLights: TRefArray<TDirectionalLight>;
    FPointLights: TRefArray<TPointLight>;
    FSpotLights: TRefArray<TSpotLight>;

    FDirectionalLightTexArray: TEmptyTexture2DArray;
    FPointLightTexArray: TEmptyTextureCubeMapArray;
    FSpotLightTexArray: TEmptyTexture2DArray;

    FUBO: TUBO;

    FShadowLightCount: Cardinal;

    procedure SetAmbient(AValue: TColorRGB);

    procedure SendAmbient;
    procedure SendDirectionalLightCount;
    procedure SendPointLightCount;
    procedure SendSpotLightCount;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    property UBO: TUBO read FUBO;
    property DirectionalLightTexArray: TEmptyTexture2DArray read FDirectionalLightTexArray;
    property PointLightTexArray: TEmptyTextureCubeMapArray read FPointLightTexArray;
    property SpotLightTexArray: TEmptyTexture2DArray read FSpotLightTexArray;

    function AddLight(ALight: TBaseLight): Integer;
    procedure DelLight(ALight: TBaseLight);

    property Ambient: TColorRGB read FAmbient write SetAmbient;

    procedure BindToGLProgram(AGLProgram: TGLProgram);

    procedure RenderShadows;

  end;

implementation

{ TDirectionalLightShaded }

function TDirectionalLightShaded.GetPosition: TVector3;
begin
  Result := FCamera.Location.Pos;
end;

function TDirectionalLightShaded.GetFarClip: Single;
begin
  Result := FCamera.FarClip;
end;

function TDirectionalLightShaded.GetSize: Single;
begin
  Result := 2 / FCamera.OrthoFactor;
end;

function TDirectionalLightShaded.GetNearClip: Single;
begin
  Result := FCamera.NearClip;
end;

procedure TDirectionalLightShaded.SendCamera;
var
  M: TMatrix4;
begin
  M := FCamera.Matrix[mtProjection] * FCamera.Location.Matrix;
  Send(8, 16, M);
  FCameraChanged := False;
end;

procedure TDirectionalLightShaded.SetSize(AValue: Single);
begin
  if Size = AValue then
    Exit;
  FCamera.OrthoFactor := 2 / AValue;
  FCameraChanged := True;
end;

procedure TDirectionalLightShaded.SetFarClip(AValue: Single);
begin
  if FarClip = AValue then
    Exit;
  FCamera.FarClip := AValue;
  FCameraChanged := True;
end;

procedure TDirectionalLightShaded.SetNearClip(AValue: Single);
begin
  if NearClip = AValue then
    Exit;
  FCamera.NearClip := AValue;
  FCameraChanged := True;
end;

procedure TDirectionalLightShaded.SetPosition(AValue: TVector3);
begin
  if Position = AValue then
    Exit;
  FCamera.Location.Pos := AValue;
  FCameraChanged := True;
end;

procedure TDirectionalLightShaded.SetDirection(AValue: TVector3);
begin
  AValue := AValue.Normalize;
  if AValue = FDirection then
    Exit;
  FDirection := AValue;
  FCamera.Location.Look := AValue;
  FCameraChanged := True;
  SendDirection;
end;

constructor TDirectionalLightShaded.Create(ALightSystem: TLightSystem);
const
  ShadedBool: Integer = 1;
begin
  FCamera := TCamera.Create(0, 1, -50, 50);
  inherited Create(ALightSystem);
  Size := 100;
  FFBO := TFBO.Create(1024, 1024);
  FFBO.EnableTexture2DLayer(fbaDepth, LightSystem.DirectionalLightTexArray, Index);
  if not FFBO.Finish then
    raise Exception.Create('IT DOESNT WORK!');
  Send(7, 1, ShadedBool);
end;

destructor TDirectionalLightShaded.Destroy;
begin
  FFBO.Free;
  inherited Destroy;
  FCamera.Free;
end;

procedure TDirectionalLightShaded.AddOccluder(ARenderable: IRenderable);
begin
  FCamera.AddRenderable(ARenderable);
end;

procedure TDirectionalLightShaded.DelOccluder(ARenderable: IRenderable);
begin
  FCamera.DelRenderable(ARenderable);
end;

procedure TDirectionalLightShaded.AddGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.AddUniforms(AGLProgram);
end;

procedure TDirectionalLightShaded.DelGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.DelUniforms(AGLProgram);
end;

procedure TDirectionalLightShaded.RenderShadows;
begin
  FFBO.Bind;
  glClear(Ord(amDepth));
  if FCameraChanged then
    SendCamera;
  FCamera.Render;
end;

{ TPointLightShaded }

function TPointLightShaded.GetFarClip: Single;
begin
  Result := FCameras[cmsPosX].FarClip;
end;

function TPointLightShaded.GetNearClip: Single;
begin
  Result := FCameras[cmsPosX].NearClip;
end;

procedure TPointLightShaded.SetFarClip(AValue: Single);
var
  Side: TGLCubeMapSide;
begin
  if FarClip = AValue then
    Exit;
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[cmsPosX].FarClip := AValue;
  FClipChanged := True;
end;

procedure TPointLightShaded.SetNearClip(AValue: Single);
var
  Side: TGLCubeMapSide;
begin
  if FarClip = AValue then
    Exit;
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[cmsPosX].FarClip := AValue;
  FClipChanged := True;
end;

procedure TPointLightShaded.SendClip;
begin
  Send(8, 1, FCameras[cmsPosX].NearClip);
  Send(9, 1, FCameras[cmsPosX].FarClip);
  FClipChanged := False;
end;

procedure TPointLightShaded.SetPosition(AValue: TVector3);
var
  Side: TGLCubeMapSide;
begin
  if AValue = FPosition then
    Exit;
  FPosition := AValue;
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].Location.Pos := AValue;
  SendPosition;
end;

constructor TPointLightShaded.Create(ALightSystem: TLightSystem);
const
  ShadedBool: Integer = 1;
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side] := TCamera.Create(90, 1, 0.1, 100);
  inherited Create(ALightSystem);
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
  begin
    FFBOs[Side] := TFBO.Create(1024, 1024);
    FFBOs[Side].EnableTextureCubeMapLayer(fbaDepth, ALightSystem.PointLightTexArray, Index, Side);
    if not FFBOs[Side].Finish then
      raise Exception.Create('IT DOESN''T WORK!');
  end;
  Send(7, 1, ShadedBool);

  FCameras[cmsPosX].Location.Look := Vec3(+1, 0, 0);
  FCameras[cmsPosX].Location.RollAngle := 180;//Scale := TGVector3.Create(-1, -1, 1);

  FCameras[cmsNegX].Location.Look := Vec3(-1, 0, 0);
  FCameras[cmsNegX].Location.RollAngle := 180;//Scale := TGVector3.Create(-1, -1, 1);

  FCameras[cmsPosY].Location.Look := Vec3(0, +1, 0);

  FCameras[cmsNegY].Location.Look := Vec3(0, -1, 0);

  FCameras[cmsPosZ].Location.Look := Vec3(0, 0, +1);
  FCameras[cmsPosZ].Location.RollAngle := 180;//Scale := TGVector3.Create(-1, -1, 1);

  FCameras[cmsNegZ].Location.Look := Vec3(0, 0, -1);
  FCameras[cmsNegZ].Location.RollAngle := 180;//Scale := TGVector3.Create(-1, -1, 1);

  FClipChanged := True;
end;

destructor TPointLightShaded.Destroy;
var
  Side: TGLCubeMapSide;
begin
  inherited Destroy;
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
  begin
    FFBOs[Side].Free;
    FCameras[Side].Free;
  end;
end;

procedure TPointLightShaded.AddOccluder(ARenderable: IRenderable);
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].AddRenderable(ARenderable);
end;

procedure TPointLightShaded.DelOccluder(ARenderable: IRenderable);
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].DelRenderable(ARenderable);
end;

procedure TPointLightShaded.AddGLProgram(AGLProgram: TGLProgram);
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].AddUniforms(AGLProgram);
end;

procedure TPointLightShaded.DelGLProgram(AGLProgram: TGLProgram);
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].DelUniforms(AGLProgram);
end;

procedure TPointLightShaded.RenderShadows;
var
  Side: TGLCubeMapSide;
begin
  if FClipChanged then
    SendClip;
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
  begin
    FFBOs[Side].Bind;
    glClear(Ord(amDepth));
    FCameras[Side].Render;
  end;
end;

{ TSpotLightShaded }

procedure TSpotLightShaded.SetCutoff(AValue: Single);
begin
  if FCutoff = AValue then
    Exit;
  FCutoff := AValue;
  FCamera.FOV := FullCutoff;
  FCameraChanged := True;
  SendCutoff;
end;

procedure TSpotLightShaded.SetCutoffBonus(AValue: Single);
begin
  if FCutoffBonus = AValue then
    Exit;
  FCutoffBonus := AValue;
  FCamera.FOV := FullCutoff;
  FCameraChanged := True;
  SendCutoffBonus;
end;

procedure TSpotLightShaded.SetDirection(AValue: TVector3);
begin
  AValue := AValue.Normalize;
  if AValue = FDirection then
    Exit;
  FDirection := AValue;
  FCamera.Location.Look := AValue;
  FCameraChanged := True;
  SendDirection;
end;

procedure TSpotLightShaded.SetPosition(AValue: TVector3);
begin
  if AValue = FPosition then
    Exit;
  FPosition := AValue;
  FCamera.Location.Pos := AValue;
  FCameraChanged := True;
  SendPosition;
end;

constructor TSpotLightShaded.Create(ALightSystem: TLightSystem);
const
  ShadedBool: Integer = 1;
begin
  FCamera := TCamera.Create(42, 1, 0.1, 100);
  inherited Create(ALightSystem);
  FCamera.FOV := FullCutoff;
  FFBO := TFBO.Create(1024, 1024);
  FFBO.EnableTexture2DLayer(fbaDepth, LightSystem.SpotLightTexArray, Index);
  if not FFBO.Finish then
    raise Exception.Create('IT DOESNT WORK!');
  Send(12, 1, ShadedBool);
end;

destructor TSpotLightShaded.Destroy;
begin
  FFBO.Free;
  inherited Destroy;
  FCamera.Free;
end;

procedure TSpotLightShaded.AddOccluder(ARenderable: IRenderable);
begin
  FCamera.AddRenderable(ARenderable);
end;

procedure TSpotLightShaded.DelOccluder(ARenderable: IRenderable);
begin
  FCamera.DelRenderable(ARenderable);
end;

procedure TSpotLightShaded.AddGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.AddUniforms(AGLProgram);
end;

procedure TSpotLightShaded.DelGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.DelUniforms(AGLProgram);
end;

procedure TSpotLightShaded.SendCamera;
var
  M: TMatrix4;
begin
  Send(13, 1, FCamera.FOV);
  //Send(14, 1, FCamera.NearClip);
  //Send(15, 1, FCamera.FarClip);
  M := FCamera.Matrix[mtProjection] * FCamera.Location.Matrix;
  Send(16, 16, M);
  FCameraChanged := False;
end;

{
function TSpotLightShaded.GetFarClip: Single;
begin
  Result := FCamera.FarClip;
end;

function TSpotLightShaded.GetNearClip: Single;
begin
  Result := FCamera.NearClip;
end;

procedure TSpotLightShaded.SetFarClip(AValue: Single);
begin
  if FarClip = AValue then
    Exit;
  FCamera.FarClip := AValue;
  FCameraChanged := True;
end;

procedure TSpotLightShaded.SetNearClip(AValue: Single);
begin
  if NearClip = AValue then
    Exit;
  FCamera.NearClip := AValue;
  FCameraChanged := True;
end;
}
procedure TSpotLightShaded.RenderShadows;
begin
  FFBO.Bind;
  glClear(Ord(amDepth));
  FCamera.Render;
  if FCameraChanged then
    SendCamera;
end;

{ TSpotLight }

procedure TSpotLight.SetCutoff(AValue: Single);
begin
  if FCutoff = AValue then
    Exit;
  FCutoff := AValue;
  SendCutoff;
end;

function TSpotLight.GetFullCutoff: Single;
begin
  Result := FCutoff + FCutoffBonus;
end;

procedure TSpotLight.SetCutoffBonus(AValue: Single);
begin
  if FCutoffBonus = AValue then
    Exit;
  FCutoffBonus := AValue;
  SendCutoffBonus;
end;

procedure TSpotLight.SetDirection(AValue: TVector3);
begin
  AValue := AValue.Normalize;
  if FDirection = AValue then
    Exit;
  FDirection := AValue;
  SendDirection;
end;

class constructor TSpotLight.Create;
begin
  FMajorOffset := CalcMajorOffset;
end;

constructor TSpotLight.Create(ALightSystem: TLightSystem);
begin
  inherited;
  FDirection := Vec3(0, 0, -1);
  FCutoff := 60;
  FCutoffBonus := 30;
  SendDirection;
  SendCutoff;
  SendCutoffBonus;
end;

procedure TSpotLight.SendAllData;
begin
  inherited;
  SendDirection;
  SendCutoff;
  SendCutoffBonus;
end;

procedure TSpotLight.SendDirection;
begin
  Send(8, 3, Direction);
end;

procedure TSpotLight.SendCutoff;
begin
  Send(7, 1, Cutoff);
end;

procedure TSpotLight.SendCutoffBonus;
begin
  Send(11, 1, CutoffBonus);
end;

class function TSpotLight.MaxLights: Integer;
begin
  Result := 16;
end;

class function TSpotLight.CalcMajorOffset: Integer;
begin
  Result := TPointLight.CalcMajorOffset +
    16 +
    TPointLight.MaxLights * TPointLight.DataSize;
end;

class function TSpotLight.MajorOffset: Integer;
begin
  Result := FMajorOffset;
end;

class function TSpotLight.DataSize: Integer;
begin
  Result := 32 * SizeOf(Single);
end;

{ TPointLight }

procedure TPointLight.SetAttenuation(AValue: Single);
begin
  if FAttenuation = AValue then
    Exit;
  FAttenuation := AValue;
  SendAttenuation;
end;

procedure TPointLight.SetPosition(AValue: TVector3);
begin
  if FPosition = AValue then
    Exit;
  FPosition := AValue;
  SendPosition;
end;

class constructor TPointLight.Create;
begin
  FMajorOffset := CalcMajorOffset;
end;

constructor TPointLight.Create(ALightSystem: TLightSystem);
begin
  inherited;
  SendPosition;
  SendAttenuation;
end;

procedure TPointLight.SendAllData;
begin
  inherited;
  SendPosition;
  SendAttenuation;
end;

procedure TPointLight.SendPosition;
begin
  Send(4, 3, Position);
end;

procedure TPointLight.SendAttenuation;
begin
  Send(3, 1, Attenuation);
end;

class function TPointLight.MaxLights: Integer;
begin
  Result := 16;
end;

class function TPointLight.CalcMajorOffset: Integer;
begin
  Result := TDirectionalLight.CalcMajorOffset +
    16 +
    TDirectionalLight.MaxLights * TDirectionalLight.DataSize;
end;

class function TPointLight.MajorOffset: Integer;
begin
  Result := FMajorOffset;
end;

class function TPointLight.DataSize: Integer;
begin
  Result := 12 * SizeOf(Single);
end;

{ TDirectionalLight }

procedure TDirectionalLight.SetDirection(AValue: TVector3);
begin
  AValue := AValue.Normalize; // yes, necessary
  if FDirection = AValue then
    Exit;
  FDirection := AValue;
  SendDirection;
end;

class constructor TDirectionalLight.Create;
begin
  FMajorOffset := CalcMajorOffset;
end;

constructor TDirectionalLight.Create(ALightSystem: TLightSystem);
begin
  inherited;
  FDirection := Vec3(0, -1, 0);
  SendDirection;
end;

procedure TDirectionalLight.SendAllData;
begin
  inherited;
  SendDirection;
end;

procedure TDirectionalLight.SendDirection;
begin
  Send(4, 3, Direction);
end;

class function TDirectionalLight.MaxLights: Integer;
begin
  Result := 4;
end;

class function TDirectionalLight.CalcMajorOffset: Integer;
begin
  Result := 0;
end;

class function TDirectionalLight.MajorOffset: Integer;
begin
  Result := FMajorOffset;
end;

class function TDirectionalLight.DataSize: Integer;
begin
  Result := 24 * SizeOf(Single);
end;

{ TBaseLight }

procedure TBaseLight.SetIndex(AValue: Integer);
begin
  if FIndex = AValue then
    Exit;
  FIndex := AValue;
end;

procedure TBaseLight.SetColor(AValue: TColorRGB);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  SendColor;
end;

procedure TBaseLight.SetIntensity(AValue: Single);
begin
  if FIntensity = AValue then
    Exit;
  FIntensity := AValue;
  SendColor;
end;

procedure TBaseLight.Send(ASingleOffset: Integer; ASingleCount: Integer; const AData);
begin
  FLightSystem.UBO.SubData(FUBOOffset + 16 + ASingleOffset * SizeOf(Single), ASingleCount * SizeOf(Single), AData);
end;

constructor TBaseLight.Create(ALightSystem: TLightSystem);
begin
  FLightSystem := ALightSystem;
  Index := ALightSystem.AddLight(Self);
  if Index >= MaxLights then
    raise Exception.Create('Too many ' + ClassName + ' lights!');

  FUBOOffset := MajorOffset + Index * DataSize;
  FColor := ColorWhite;
  FIntensity := 1;
  SendColor;
end;

destructor TBaseLight.Destroy;
begin
  FLightSystem.DelLight(Self);
  inherited;
end;

procedure TBaseLight.DecIndexAndSendAll;
begin
  Index := Index - 1;
  SendAllData;
end;

procedure TBaseLight.SendAllData;
begin
  SendColor;
end;

procedure TBaseLight.SendColor;
var
  Value: TColorRGB;
begin
  Value := Color * Intensity;
  Send(0, 3, Value);
end;

{ TLightSystem }

procedure TLightSystem.SetAmbient(AValue: TColorRGB);
begin
  if FAmbient = AValue then
    Exit;
  FAmbient := AValue;
  SendAmbient;
end;

procedure TLightSystem.SendAmbient;
begin
  UBO.SubData(0, SizeOf(TColorRGB), Ambient);
end;

procedure TLightSystem.SendDirectionalLightCount;
var
  LightCount: Integer;
begin
  LightCount := FDirectionalLights.Count;
  UBO.SubData(TDirectionalLight.MajorOffset + SizeOf(Single) * 3, SizeOf(Integer), LightCount);
end;

procedure TLightSystem.SendPointLightCount;
var
  LightCount: Integer;
begin
  LightCount := FPointLights.Count;
  UBO.SubData(TPointLight.MajorOffset, SizeOf(Integer), LightCount);
end;

procedure TLightSystem.SendSpotLightCount;
var
  LightCount: Integer;
begin
  LightCount := FSpotLights.Count;
  UBO.SubData(TSpotLight.MajorOffset, SizeOf(Integer), LightCount);
end;

constructor TLightSystem.Create(AGLState: TGLState);
begin
  FGLState := AGLState;

  FDirectionalLights := TRefArray<TDirectionalLight>.Create;
  FPointLights := TRefArray<TPointLight>.Create;
  FSpotLights := TRefArray<TSpotLight>.Create;

  FDirectionalLightTexArray := TEmptyTexture2DArray.Create(FGLState, 1024, 1024, TDirectionalLight.MaxLights, pfDepthComponent);
  FDirectionalLightTexArray.MagFilter := magLinear;
  FDirectionalLightTexArray.TextureCompareMode := tcmCompareRefToTexture;

  FPointLightTexArray := TEmptyTextureCubeMapArray.Create(FGLState, 1024, TPointLight.MaxLights, pfDepthComponent);
  FPointLightTexArray.MagFilter := magLinear;
  FPointLightTexArray.TextureCompareMode := tcmCompareRefToTexture;

  FSpotLightTexArray := TEmptyTexture2DArray.Create(FGLState, 1024, 1024, TSpotLight.MaxLights, pfDepthComponent);
  FSpotLightTexArray.MagFilter := magLinear;
  FSpotLightTexArray.TextureCompareMode := tcmCompareRefToTexture;

  FUBO := TUBO.Create(FGLState);
  FUBO.GLLabel := 'Light-Data';
  FUBO.Generate(TSpotLight.MajorOffset + 16 + TSpotLight.DataSize * TSpotLight.MaxLights, buStreamDraw);

  Ambient := 0.1;
end;

destructor TLightSystem.Destroy;
begin
  FDirectionalLights.Free;
  FPointLights.Free;
  FSpotLights.Free;
  FDirectionalLightTexArray.Free;
  FSpotLightTexArray.Free;
  FPointLightTexArray.Free;
  FUBO.Free;
  inherited Destroy;
end;

function TLightSystem.AddLight(ALight: TBaseLight): Integer;
begin
  if (ALight.ClassType = TDirectionalLight) or (ALight.ClassType = TDirectionalLightShaded) then
  begin
    Result := FDirectionalLights.Count;
    FDirectionalLights.Add(ALight as TDirectionalLight);
    SendDirectionalLightCount;
    if ALight.ClassType = TDirectionalLightShaded then
    begin
      Inc(FShadowLightCount);
      TDirectionalLightShaded(ALight).AddGLProgram(FGLProgram);
    end;
  end
  else if (ALight.ClassType = TPointLight) or (ALight.ClassType = TPointLightShaded) then
  begin
    Result := FPointLights.Count;
    FPointLights.Add(ALight as TPointLight);
    SendPointLightCount;
    if ALight.ClassType = TPointLightShaded then
    begin
      Inc(FShadowLightCount);
      TPointLightShaded(ALight).AddGLProgram(FGLProgram);
    end;
  end
  else if (ALight.ClassType = TSpotLight) or (ALight.ClassType = TSpotLightShaded) then
  begin
    Result := FSpotLights.Count;
    FSpotLights.Add(ALight as TSpotLight);
    SendSpotLightCount;
    if ALight.ClassType = TSpotLightShaded then
    begin
      Inc(FShadowLightCount);
      TSpotLightShaded(ALight).AddGLProgram(FGLProgram);
    end;
  end
  else
    raise Exception.Create('Unsupported Light Class!');
end;

procedure TLightSystem.DelLight(ALight: TBaseLight);
var
  Index, I: Integer;
begin
  if (ALight.ClassType = TDirectionalLight) or (ALight.ClassType = TDirectionalLightShaded) then
  begin
    Index := FDirectionalLights.Find(ALight as TDirectionalLight);
    FDirectionalLights.DelAt(Index);
    for I := Index to FDirectionalLights.Count - 1 do
      FDirectionalLights[I].DecIndexAndSendAll;
    SendDirectionalLightCount;
    if ALight.ClassType = TDirectionalLightShaded then
    begin
      Dec(FShadowLightCount);
      TDirectionalLightShaded(ALight).DelGLProgram(FGLProgram);
    end;
  end
  else if (ALight.ClassType = TPointLight) or (ALight.ClassType = TPointLightShaded) then
  begin
    Index := FPointLights.Find(ALight as TPointLight);
    FPointLights.DelAt(Index);
    for I := Index to FPointLights.Count - 1 do
      FPointLights[I].DecIndexAndSendAll;
    SendPointLightCount;
    if ALight.ClassType = TPointLightShaded then
    begin
      Dec(FShadowLightCount);
      TPointLightShaded(ALight).DelGLProgram(FGLProgram);
    end;
  end
  else if (ALight.ClassType = TSpotLight) or (ALight.ClassType = TSpotLightShaded) then
  begin
    Index := FSpotLights.Find(ALight as TSpotLight);
    FSpotLights.DelAt(Index);
    for I := Index to FSpotLights.Count - 1 do
      FSpotLights[I].DecIndexAndSendAll;
    SendSpotLightCount;
    if ALight.ClassType = TSpotLightShaded then
    begin
      Dec(FShadowLightCount);
      TSpotLightShaded(ALight).DelGLProgram(FGLProgram);
    end;
  end
  else
    raise Exception.Create('Unsupported Light Class!');
end;

procedure TLightSystem.BindToGLProgram(AGLProgram: TGLProgram);
var
  Light: TObject;
begin
  FGLProgram := AGLProgram;
  FDepthOnlyUniform := AGLProgram.Uniform<Boolean>('depthonly');

  FDirectionalLightTexArray.Uniform(AGLProgram.UniformSampler('directionalshadowmaps'));
  FSpotLightTexArray.Uniform(AGLProgram.UniformSampler('spotshadowmaps'));
  FPointLightTexArray.Uniform(AGLProgram.UniformSampler('pointshadowmaps'));

  FUBO.BindToGLProgram(AGLProgram, 'lightdata');

  for Light in FDirectionalLights do
    if Light is TDirectionalLightShaded then
      TDirectionalLightShaded(Light).AddGLProgram(AGLProgram);

  for Light in FSpotLights do
    if Light is TSpotLightShaded then
      TSpotLightShaded(Light).AddGLProgram(AGLProgram);

  for Light in FPointLights do
    if Light is TPointLightShaded then
      TPointLightShaded(Light).AddGLProgram(AGLProgram);
end;

procedure TLightSystem.RenderShadows;
var
  Light: TObject;
  OldFBO: TFBO;
begin
  if FShadowLightCount = 0 then
    Exit;

  OldFBO := nil;
  raise Exception.Create('TODO');

  FDepthOnlyUniform.Value := True;

  for Light in FDirectionalLights do
    if Light is TDirectionalLightShaded then
      TDirectionalLightShaded(Light).RenderShadows;

  for Light in FSpotLights do
    if Light is TSpotLightShaded then
      TSpotLightShaded(Light).RenderShadows;

  for Light in FPointLights do
    if Light is TPointLightShaded then
      TPointLightShaded(Light).RenderShadows;

  FDepthOnlyUniform.Value := False;

  if OldFBO = nil then
    OldFBO.Unbind
  else
    OldFBO.Bind;
end;

end.
