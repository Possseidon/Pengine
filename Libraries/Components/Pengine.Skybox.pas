unit Pengine.Skybox;

interface

uses
  System.SysUtils,

  Pengine.VAO,
  Pengine.Collections,
  Pengine.Color,
  Pengine.Shader,
  Pengine.GLEnums,
  Pengine.Vector,
  Pengine.UBO,
  Pengine.Camera,
  Pengine.GLState,
  Pengine.ResourceManager,
  Pengine.Interfaces,
  Pengine.CollectionInterfaces;

type

  TStripeData = record
    Color: TColorRGB;
    Pitch: Single;
  end;

  TStripe = class
  private
    FIndex: Integer;
    FColor: TColorRGB;
    FAngle: Single;
    FUBO: TUBO;
  public
    constructor Create(AColor: TColorRGB; AAngle: Single; AIndex: Integer; AUBO: TUBO);

    procedure SendAll;

    property Angle: Single read FAngle;
    property Color: TColorRGB read FColor;
  end;

  TSkyboxShaderBase = class(TShaderResource)
  public type

    TData = record
      Pos: TVector3;
    end;

  protected
    class function GetAttributeOrder: TShader.TAttributeOrder; override;

  end;

  TSkyboxShaderClass = class of TSkyboxShaderBase;

  TSkybox = class(TRenderable)
  public const

    MaxStripes = 16;
    UBOSize = SizeOf(TStripeData) * MaxStripes + SizeOf(Integer);

  private type

    TSkyboxVAO = class(TVAO)
    private
      FGLState: TGLState;

    protected
      procedure BeforeRender; override;
      procedure AfterRender; override;

    public
      constructor Create(AShader: TShader; AGLState: TGLState);

    end;

  private
    FVAO: TSkyboxVAO;
    FUBO: TUBO;
    FStripes: TRefArray<TStripe>;

    procedure BuildVAO;

  protected
    function GetVisible: Boolean; override;
    procedure SetVisible(const Value: Boolean);

  public
    constructor Create(AGLState: TGLState; AShaderClass: TSkyboxShaderClass);
    destructor Destroy; override;

    procedure AddStripe(AColor: TColorRGB; AAngle: Single);

    procedure Render; override;

    property Visible: Boolean read GetVisible write SetVisible;

  end;

implementation

{ TStripe }

constructor TStripe.Create(AColor: TColorRGB; AAngle: Single; AIndex: Integer; AUBO: TUBO);
begin
  FColor := AColor;
  FAngle := AAngle;
  FIndex := AIndex;
  FUBO := AUBO;
  SendAll;
end;

procedure TStripe.SendAll;
var
  Data: TStripeData;
begin
  Data.Color := FColor;
  Data.Pitch := (FAngle + 90) / 180 * Pi;
  FUBO.SubData(SizeOf(TStripeData) * FIndex, SizeOf(TStripeData), Data);
end;

{ TSkyboxShaderBase }

class function TSkyboxShaderBase.GetAttributeOrder: TShader.TAttributeOrder;
begin
  Result := ['vpos'];
end;

{ TSkybox }

{
procedure TSkybox.BuildVAO;
const
  PitchSteps = 30; // min 1
  TurnSteps = 120; // min 3

  procedure AddData(T, P: Single);
  var
    Data: TSkyboxShaderBase.TData;
  begin
    P := P * 90 / PitchSteps;
    T := T * 360 / TurnSteps;
    Data.Pos := TVectorDir.Create(T, P);
    Data.Pitch := P;
    AddVertex(Data);
  end;

var
  P, T: Integer;
begin
  Generate(12 * PitchSteps * TurnSteps, buStaticDraw);
  Map(baWriteOnly);

  for P := -PitchSteps to PitchSteps - 1 do
    for T := 0 to TurnSteps - 1 do
    begin
      AddData(T + 0, P + 1);
      AddData(T + 1, P + 1);
      AddData(T + 0, P + 0);
      AddData(T + 1, P + 0);
      AddData(T + 0, P + 0);
      AddData(T + 1, P + 1);
    end;

  Unmap;
end;
}

procedure TSkybox.BuildVAO;
var
  Data: TSkyboxShaderBase.TData;
  P: TPlane3;
  T: TTexCoord2;
begin
  FVAO.Generate(6 * 6, buStaticDraw);
  FVAO.Map(baWriteOnly);

  for P in CubePlanes do
  begin
    for T in QuadTexCoords do
    begin
      Data.Pos := P[T.YX] * 2 - 1;
      FVAO.AddVertex(Data);
    end;
  end;

  FVAO.Unmap;
end;

constructor TSkybox.Create(AGLState: TGLState; AShaderClass: TSkyboxShaderClass);
const
  Zero: Integer = 0;
begin
  FVAO := TSkyboxVAO.Create(AShaderClass.Data, AGLState);

  FUBO := TUBO.Create;
  FUBO.Generate(UBOSize, buStaticDraw);
  FUBO.SubData(SizeOf(TStripeData) * MaxStripes, SizeOf(Integer), Zero);
  FUBO.BindToShader(FVAO.Shader, 'stripedata');

  FStripes := TRefArray<TStripe>.Create(True);

  BuildVAO;
end;

destructor TSkybox.Destroy;
begin
  FVAO.Free;
  FStripes.Free;
  FUBO.Free;
  inherited Destroy;
end;

function TSkybox.GetVisible: Boolean;
begin
  Result := FVAO.Visible;
end;

procedure TSkybox.Render;
begin
  FVAO.Render;
end;

procedure TSkybox.SetVisible(const Value: Boolean);
begin
  FVAO.Visible := Value;
end;

procedure TSkybox.AddStripe(AColor: TColorRGB; AAngle: Single);
var
  StripeCount: Integer;
begin
  if not FStripes.Empty and (AAngle <= TStripe(FStripes.Last).Angle) then
    raise Exception.Create('Angles of Skybox Stripes must be ascending!');
  FStripes.Add(TStripe.Create(AColor, AAngle, FStripes.Count, FUBO));
  StripeCount := FStripes.Count;
  FUBO.SubData(SizeOf(TStripeData) * MaxStripes, SizeOf(Integer), StripeCount);
end;

{ TSkybox.TSkyboxVAO }

procedure TSkybox.TSkyboxVAO.BeforeRender;
begin
  inherited;
  FGLState.Push;
  FGLState[stDepthTest] := False;
  FGLState[stDepthMask] := False;
end;

procedure TSkybox.TSkyboxVAO.AfterRender;
begin
  FGLState.Pop;
  inherited;
end;

constructor TSkybox.TSkyboxVAO.Create(AShader: TShader; AGLState: TGLState);
begin
  inherited Create(AShader);
  FGLState := AGLState;
end;

end.

