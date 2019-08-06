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
  Pengine.GLState,
  Pengine.GLGame,
  Pengine.GLContext,
  Pengine.IntMaths;

type

  TLightSystem = class;

  { TBaseLight }

  TBaseLight = class abstract
  private
    FLightSystem: TLightSystem;
    FIndex: Integer;
    FUBOOffset: Integer;
    FColor: TColorRGB;
    FIntensity: Single;

    procedure SetIndex(AValue: Integer);

    procedure SetColor(AValue: TColorRGB);
    procedure SetIntensity(AValue: Single);

    property Index: Integer read FIndex write SetIndex;

  protected
    procedure Send(AOffset: Pointer; ASize: Integer; const AData);

    property LightSystem: TLightSystem read FLightSystem;

  public
    constructor Create(ALightSystem: TLightSystem);
    destructor Destroy; override;

    class function GetMaxLights: Integer; virtual; abstract;
    class function GetDataOffset: Integer; virtual; abstract;
    class function GetDataSize: Integer; virtual; abstract;

    procedure DecIndexAndSendAll;

    procedure SendAllData; virtual;
    procedure SendColor;

    property Color: TColorRGB read FColor write SetColor;
    property Intensity: Single read FIntensity write SetIntensity;

  end;

  { TDirectionalLight }

  TDirectionalLight = class(TBaseLight)
  public const

    MaxLights = 4;

  public type

    TData = record
      Color: TColorRGB;
      Unused: Integer;
      Direction: TVector3;
      Shadowed: LongBool;
      Matrix: TMatrix4;
    end;

  private
    FDirection: TVector3;

  protected
    procedure SetDirection(AValue: TVector3); virtual;

  public
    constructor Create(ALightSystem: TLightSystem);

    class function GetMaxLights: Integer; override;
    class function GetDataOffset: Integer; override;
    class function GetDataSize: Integer; override;

    property Direction: TVector3 read FDirection write SetDirection;

    procedure SendAllData; override;
    procedure SendDirection;
  end;

  { TPointLight }

  TPointLight = class(TBaseLight)
  public const

    MaxLights = 16;

  public type

    TData = record
      Color: TColorRGB;
      Attenuation: Single;
      Position: TVector3;
      Shadowed: LongBool;
      NearClip: Single;
      FarClip: Single;
      Unused: array [0 .. 1] of Integer;
    end;

  private
    FAttenuation: Single;

    procedure SetAttenuation(AValue: Single);

  protected
    FPosition: TVector3;

    procedure SetPosition(AValue: TVector3); virtual;

  public
    constructor Create(ALightSystem: TLightSystem);

    class function GetMaxLights: Integer; override;
    class function GetDataOffset: Integer; override;
    class function GetDataSize: Integer; override;

    property Position: TVector3 read FPosition write SetPosition;
    property Attenuation: Single read FAttenuation write SetAttenuation;

    procedure SendAllData; override;

    procedure SendPosition;
    procedure SendAttenuation;

  end;

  { TSpotLight }

  TSpotLight = class(TPointLight)
  public const

    MaxLights = 16;

  public type

    TData = record
      Color: TColorRGB;
      Attenuation: Single;
      Position: TVector3;
      Cutoff: Single;
      Direction: TVector3;
      CutoffBonus: Single;
      Shadowed: WordBool;
      FOV: Single;
      Unused: array [0 .. 1] of Integer;
      Matrix: TMatrix4;
    end;

  private
    function GetFullCutoff: Single;

  protected
    FDirection: TVector3;
    FCutoff: Single;
    FCutoffBonus: Single;

    procedure SetCutoff(AValue: Single); virtual;
    procedure SetCutoffBonus(AValue: Single); virtual;
    procedure SetDirection(AValue: TVector3); virtual;

  public
    constructor Create(ALightSystem: TLightSystem);

    class function GetMaxLights: Integer; override;
    class function GetDataOffset: Integer; override;
    class function GetDataSize: Integer; override;

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
    procedure RemoveOccluder(ARenderable: IRenderable);

    procedure AddGLProgram(AGLProgram: TGLProgram);
    procedure RemoveGLProgram(AGLProgram: TGLProgram);

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
    procedure RemoveOccluder(ARenderable: IRenderable);

    procedure AddGLProgram(AGLProgram: TGLProgram);
    procedure RemoveGLProgram(AGLProgram: TGLProgram);

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

    // function GetFarClip: Single;
    // function GetNearClip: Single;
    // procedure SetFarClip(AValue: Single);
    // procedure SetNearClip(AValue: Single);

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
    procedure RemoveOccluder(ARenderable: IRenderable);

    procedure AddGLProgram(AGLProgram: TGLProgram);
    procedure RemoveGLProgram(AGLProgram: TGLProgram);

    procedure RenderShadows;

    // property NearClip: Single read GetNearClip write SetNearClip;
    // property FarClip: Single read GetFarClip write SetFarClip;

  end;

  // make sure to create before camera, so that shadows get rendered first
  TLightSystem = class
  public type

    TLightData = record
      Ambient: TColorRGB;
      DirectionalLightCount: Integer;
      DirectionalLights: array [0 .. TDirectionalLight.MaxLights - 1] of TDirectionalLight.TData;
      PointLightCount: Integer;
      Unused1: array [0 .. 2] of Integer;
      PointLights: array [0 .. TPointLight.MaxLights - 1] of TPointLight.TData;
      SpotLightCount: Integer;
      Unused2: array [0 .. 2] of Integer;
      SpotLights: array [0 .. TSpotLight.MaxLights - 1] of TSpotLight.TData;
    end;

    TUBO = TUBO<TLightData>;

  private
    FContext: TGLContext;
    FGLProgram: TGLProgram;

    FAmbient: TColorRGB;

    FDepthOnlyUniform: TGLProgram.TUniform<Boolean>;

    FDirectionalLights: TRefArray<TDirectionalLight>;
    FPointLights: TRefArray<TPointLight>;
    FSpotLights: TRefArray<TSpotLight>;

    FDirectionalLightTexArray: TTexture2DArray;
    FPointLightTexArray: TTextureCubeMapArray;
    FSpotLightTexArray: TTexture2DArray;

    FUBO: TUBO;

    FFBOBinding: TGLObjectBinding<TFBO>;

    FShadowLightCount: Cardinal;

    procedure SetAmbient(AValue: TColorRGB);

    procedure SendAmbient;
    procedure SendDirectionalLightCount;
    procedure SendPointLightCount;
    procedure SendSpotLightCount;
    function GetGLState: TGLState;

  public
    constructor Create(AContext: TGLContext);
    destructor Destroy; override;

    property Context: TGLContext read FContext;
    property GLState: TGLState read GetGLState;

    property UBO: TUBO read FUBO;
    property DirectionalLightTexArray: TTexture2DArray read FDirectionalLightTexArray;
    property PointLightTexArray: TTextureCubeMapArray read FPointLightTexArray;
    property SpotLightTexArray: TTexture2DArray read FSpotLightTexArray;

    function AddLight(ALight: TBaseLight): Integer;
    procedure RemoveLight(ALight: TBaseLight);

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
  Send(@TData(nil^).Matrix, SizeOf(TData(nil^).Matrix), M);
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
  ShadedBool: LongBool = True;
begin
  FCamera := TCamera.Create(0, 1, -50, 50);
  inherited Create(ALightSystem);
  Size := 100;
  FFBO := TFBO.Create(ALightSystem.GLState, IBounds2(IVec2(1024, 1024)));
  FFBO.GLLabel := AnsiString(Format('Directional-Light %d Shadow-FBO', [Index]));
  FFBO.Add(TTextureLayerAttachment.Create(TDepthAttachment.Create, ALightSystem.DirectionalLightTexArray, Index));
  FFBO.Complete;
  Send(@TData(nil^).Shadowed, SizeOf(TData(nil^).Shadowed), ShadedBool);
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

procedure TDirectionalLightShaded.RemoveOccluder(ARenderable: IRenderable);
begin
  FCamera.RemoveRenderable(ARenderable);
end;

procedure TDirectionalLightShaded.AddGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.AddUniforms(AGLProgram);
end;

procedure TDirectionalLightShaded.RemoveGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.RemoveUniforms(AGLProgram);
end;

procedure TDirectionalLightShaded.RenderShadows;
begin
  FFBO.Bind;
  glClear(ToGLBitfield([amDepth]));
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
  Send(@TData(nil^).NearClip, SizeOf(TData(nil^).NearClip), FCameras[cmsPosX].NearClip);
  Send(@TData(nil^).FarClip, SizeOf(TData(nil^).FarClip), FCameras[cmsPosX].FarClip);
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
    FFBOs[Side] := TFBO.Create(ALightSystem.GLState, IVec2(1024, 1024));
    FFBOs[Side].Add(TCubeMapLayerAttachment.Create(TDepthAttachment.Create, ALightSystem.PointLightTexArray,
      Side, Index));
    FFBOs[Side].Complete;
  end;
  Send(@TData(nil^).Shadowed, SizeOf(TData(nil^).Shadowed), ShadedBool);

  FCameras[cmsPosX].Location.Look := Vec3(+1, 0, 0);
  FCameras[cmsPosX].Location.RollAngle := 180; // Scale := TGVector3.Create(-1, -1, 1);

  FCameras[cmsNegX].Location.Look := Vec3(-1, 0, 0);
  FCameras[cmsNegX].Location.RollAngle := 180; // Scale := TGVector3.Create(-1, -1, 1);

  FCameras[cmsPosY].Location.Look := Vec3(0, +1, 0);

  FCameras[cmsNegY].Location.Look := Vec3(0, -1, 0);

  FCameras[cmsPosZ].Location.Look := Vec3(0, 0, +1);
  FCameras[cmsPosZ].Location.RollAngle := 180; // Scale := TGVector3.Create(-1, -1, 1);

  FCameras[cmsNegZ].Location.Look := Vec3(0, 0, -1);
  FCameras[cmsNegZ].Location.RollAngle := 180; // Scale := TGVector3.Create(-1, -1, 1);

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

procedure TPointLightShaded.RemoveOccluder(ARenderable: IRenderable);
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].RemoveRenderable(ARenderable);
end;

procedure TPointLightShaded.AddGLProgram(AGLProgram: TGLProgram);
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].AddUniforms(AGLProgram);
end;

procedure TPointLightShaded.RemoveGLProgram(AGLProgram: TGLProgram);
var
  Side: TGLCubeMapSide;
begin
  for Side := Low(TGLCubeMapSide) to High(TGLCubeMapSide) do
    FCameras[Side].RemoveUniforms(AGLProgram);
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
  FFBO := TFBO.Create(ALightSystem.GLState, IVec2(1024, 1024));
  FFBO.Add(TTextureLayerAttachment.Create(TDepthAttachment.Create, LightSystem.SpotLightTexArray, Index));
  FFBO.Complete;
  Send(@TData(nil^).Shadowed, SizeOf(TData(nil^).Shadowed), ShadedBool);
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

procedure TSpotLightShaded.RemoveOccluder(ARenderable: IRenderable);
begin
  FCamera.RemoveRenderable(ARenderable);
end;

procedure TSpotLightShaded.AddGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.AddUniforms(AGLProgram);
end;

procedure TSpotLightShaded.RemoveGLProgram(AGLProgram: TGLProgram);
begin
  FCamera.RemoveUniforms(AGLProgram);
end;

procedure TSpotLightShaded.SendCamera;
var
  M: TMatrix4;
begin
  Send(@TData(nil^).FOV, SizeOf(TData(nil^).FOV), FCamera.FOV);
  // Send(14, 1, FCamera.NearClip);
  // Send(15, 1, FCamera.FarClip);
  M := FCamera.Matrix[mtProjection] * FCamera.Location.Matrix;
  Send(@TData(nil^).Matrix, SizeOf(TData(nil^).Matrix), M);
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
  if FCameraChanged then
    SendCamera;
  FCamera.Render;
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
  Send(@TData(nil^).Direction, SizeOf(TData(nil^).Direction), Direction);
end;

procedure TSpotLight.SendCutoff;
begin
  Send(@TData(nil^).Cutoff, SizeOf(TData(nil^).Cutoff), Cutoff);
end;

procedure TSpotLight.SendCutoffBonus;
begin
  Send(@TData(nil^).CutoffBonus, SizeOf(TData(nil^).CutoffBonus), CutoffBonus);
end;

class function TSpotLight.GetMaxLights: Integer;
begin
  Result := MaxLights;
end;

class function TSpotLight.GetDataOffset: Integer;
begin
  Result := Integer(@TLightSystem.TLightData(nil^).SpotLights[0]);
end;

class function TSpotLight.GetDataSize: Integer;
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
  Send(@TData(nil^).Position, SizeOf(TData(nil^).Position), Position);
end;

procedure TPointLight.SendAttenuation;
begin
  Send(@TData(nil^).Attenuation, SizeOf(TData(nil^).Attenuation), Attenuation);
end;

class function TPointLight.GetMaxLights: Integer;
begin
  Result := 16;
end;

class function TPointLight.GetDataOffset: Integer;
begin
  Result := Integer(@TLightSystem.TLightData(nil^).PointLights[0]);
end;

class function TPointLight.GetDataSize: Integer;
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
  Send(@TData(nil^).Direction, SizeOf(TData(nil^).Direction), Direction);
end;

class function TDirectionalLight.GetMaxLights: Integer;
begin
  Result := MaxLights;
end;

class function TDirectionalLight.GetDataOffset: Integer;
begin
  Result := Integer(@TLightSystem.TLightData(nil^).DirectionalLights[0]);
end;

class function TDirectionalLight.GetDataSize: Integer;
begin
  Result := SizeOf(TData);
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

procedure TBaseLight.Send(AOffset: Pointer; ASize: Integer; const AData);
begin
  FLightSystem.UBO.SubData(FUBOOffset + Integer(AOffset), ASize, AData);
end;

constructor TBaseLight.Create(ALightSystem: TLightSystem);
begin
  FLightSystem := ALightSystem;
  Index := ALightSystem.AddLight(Self);
  if Index >= GetMaxLights then
    raise Exception.Create('Too many ' + ClassName + ' lights!');

  FUBOOffset := GetDataOffset + Index * GetDataSize;
  FColor := ColorWhite;
  FIntensity := 1;
  SendColor;
end;

destructor TBaseLight.Destroy;
begin
  FLightSystem.RemoveLight(Self);
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
  Send(nil, SizeOf(Value), Value);
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
  UBO.SubData(Integer(@TLightData(nil^).DirectionalLightCount), SizeOf(Integer), LightCount);
end;

procedure TLightSystem.SendPointLightCount;
var
  LightCount: Integer;
begin
  LightCount := FPointLights.Count;
  UBO.SubData(Integer(@TLightData(nil^).PointLightCount), SizeOf(Integer), LightCount);
end;

procedure TLightSystem.SendSpotLightCount;
var
  LightCount: Integer;
begin
  LightCount := FSpotLights.Count;
  UBO.SubData(Integer(@TLightData(nil^).SpotLightCount), SizeOf(Integer), LightCount);
end;

constructor TLightSystem.Create(AContext: TGLContext);
begin
  FContext := AContext;
  
  FDirectionalLights := TRefArray<TDirectionalLight>.Create;
  FPointLights := TRefArray<TPointLight>.Create;
  FSpotLights := TRefArray<TSpotLight>.Create;

  FDirectionalLightTexArray := TTexture2DArray.Create(GLState, IVec3(1024, 1024, TDirectionalLight.MaxLights),
    pfDepthComponent);
  FDirectionalLightTexArray.GLLabel := 'Directional-Light Texture-Array';
  FDirectionalLightTexArray.MinFilter := minLinear;
  FDirectionalLightTexArray.CompareMode := tcmCompareRefToTexture;

  FPointLightTexArray := TTextureCubeMapArray.Create(GLState, 1024, TPointLight.MaxLights, pfDepthComponent);
  FPointLightTexArray.GLLabel := 'Point-Light CubeMapTexture-Array';
  FPointLightTexArray.MinFilter := minLinear;
  FPointLightTexArray.CompareMode := tcmCompareRefToTexture;

  FSpotLightTexArray := TTexture2DArray.Create(GLState, IVec3(1024, 1024, TSpotLight.MaxLights), pfDepthComponent);
  FSpotLightTexArray.GLLabel := 'Spot-Light Texture-Array';
  FSpotLightTexArray.MinFilter := minLinear;
  FSpotLightTexArray.CompareMode := tcmCompareRefToTexture;

  FUBO := TUBO.Create(GLState, buStreamDraw);
  FUBO.GLLabel := 'Light-Data';

  FFBOBinding := GLState.GLObjectBindings.Get<TFBO>;

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

function TLightSystem.GetGLState: TGLState;
begin
  Result := Context.GLState;
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

procedure TLightSystem.RemoveLight(ALight: TBaseLight);
var
  Index, I: Integer;
begin
  if (ALight.ClassType = TDirectionalLight) or (ALight.ClassType = TDirectionalLightShaded) then
  begin
    Index := FDirectionalLights.Find(ALight as TDirectionalLight);
    FDirectionalLights.RemoveAt(Index);
    for I := Index to FDirectionalLights.Count - 1 do
      FDirectionalLights[I].DecIndexAndSendAll;
    SendDirectionalLightCount;
    if ALight.ClassType = TDirectionalLightShaded then
    begin
      Dec(FShadowLightCount);
      TDirectionalLightShaded(ALight).RemoveGLProgram(FGLProgram);
    end;
  end
  else if (ALight.ClassType = TPointLight) or (ALight.ClassType = TPointLightShaded) then
  begin
    Index := FPointLights.Find(ALight as TPointLight);
    FPointLights.RemoveAt(Index);
    for I := Index to FPointLights.Count - 1 do
      FPointLights[I].DecIndexAndSendAll;
    SendPointLightCount;
    if ALight.ClassType = TPointLightShaded then
    begin
      Dec(FShadowLightCount);
      TPointLightShaded(ALight).RemoveGLProgram(FGLProgram);
    end;
  end
  else if (ALight.ClassType = TSpotLight) or (ALight.ClassType = TSpotLightShaded) then
  begin
    Index := FSpotLights.Find(ALight as TSpotLight);
    FSpotLights.RemoveAt(Index);
    for I := Index to FSpotLights.Count - 1 do
      FSpotLights[I].DecIndexAndSendAll;
    SendSpotLightCount;
    if ALight.ClassType = TSpotLightShaded then
    begin
      Dec(FShadowLightCount);
      TSpotLightShaded(ALight).RemoveGLProgram(FGLProgram);
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

  OldFBO := FFBOBinding.BoundObject;

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
    TFBO.BindScreen(GLState.ScreenSize, FFBOBinding)
  else
    OldFBO.Bind;
end;

end.
