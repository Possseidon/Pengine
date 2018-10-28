unit Pengine.Skybox;

interface

uses
  System.SysUtils,

  Pengine.VAO,
  Pengine.Collections,
  Pengine.Color,
  Pengine.GLProgram,
  Pengine.GLEnums,
  Pengine.Vector,
  Pengine.IntMaths,
  Pengine.UBO,
  Pengine.Camera,
  Pengine.GLState,
  Pengine.ResourceManager,
  Pengine.Interfaces,
  Pengine.CollectionInterfaces;

type

  TSkyboxGLProgramBase = class(TGLProgramResource)
  public type

    TData = record
      Pos: TVector3;
    end;

  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;

  end;

  TSkyboxGLProgramClass = class of TSkyboxGLProgramBase;

  TSkybox = class(TRenderable)
  public const

    MaxStripes = 16;

  private type

    TUBO = class;

    TStripe = class
    public type

      TData = packed record
        Color: TColorRGB;
        Pitch: Single;
      end;

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

    TData = packed record
      Stripes: array [0 .. MaxStripes - 1] of TStripe.TData;
      Count: Integer;
    end;

    TUBO = class(TUBO<TData>);

    TSkyboxVAO = class(TVAOMutable<TSkyboxGLProgramBase.TData>)
    protected
      procedure BeforeRender; override;
      procedure AfterRender; override;

    end;

    TStripes = TObjectArray<TStripe>;

  private
    FVAO: TSkyboxVAO;
    FUBO: TUBO;
    FStripes: TStripes;

    procedure BuildVAO;

  protected
    function GetVisible: Boolean; override;
    procedure SetVisible(const Value: Boolean);

  public
    constructor Create(ASkyboxGLProgram: TGLProgram);
    destructor Destroy; override;

    procedure AddStripe(AColor: TColorRGB; AAngle: Single);

    procedure Render; override;

    property Visible: Boolean read GetVisible write SetVisible;

  end;

implementation

{ TSkybox.TStripe }

constructor TSkybox.TStripe.Create(AColor: TColorRGB; AAngle: Single; AIndex: Integer; AUBO: TUBO);
begin
  FColor := AColor;
  FAngle := AAngle;
  FIndex := AIndex;
  FUBO := AUBO;
  SendAll;
end;

procedure TSkybox.TStripe.SendAll;
var
  Data: TData;
begin
  Data.Color := FColor;
  Data.Pitch := (FAngle + 90) / 180 * Pi;
  FUBO.SubData(SizeOf(TData) * FIndex, SizeOf(TData), Data);
end;

{ TSkyboxGLProgramBase }

class function TSkyboxGLProgramBase.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := ['vpos'];
end;

{ TSkybox }

procedure TSkybox.BuildVAO;
var
  Data: TSkyboxVAO.TData;
  P: TPlane3;
  T: TTexCoord2;
begin
  FVAO.VBO.Generate(6 * 6, buStaticDraw);
  with FVAO.VBO.Map do
  begin
    for P in CubePlanes do
    begin
      for T in QuadTexCoords do
      begin
        Data.Pos := P[T.YX] * 2 - 1;
        AddToBuffer(Data);
      end;
    end;
    Free;
  end;
end;

constructor TSkybox.Create(ASkyboxGLProgram: TGLProgram);
const
  Zero: Integer = 0;
begin
  FVAO := TSkyboxVAO.Create(ASkyboxGLProgram);
  FVAO.GLLabel := 'Skybox';

  FUBO := TUBO.Create(ASkyboxGLProgram.GLState, buStaticDraw);
  FUBO.GLLabel := 'Skybox Stripe-Data';
  FUBO.SubData(SizeOf(TData) - SizeOf(Integer), SizeOf(Integer), Zero);
  FUBO.BindToGLProgram(FVAO.GLProgram, 'stripedata');

  FStripes := TStripes.Create;

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
  FUBO.SubData(SizeOf(TData) - SizeOf(Integer), SizeOf(Integer), StripeCount);
end;

{ TSkybox.TSkyboxVAO }

procedure TSkybox.TSkyboxVAO.BeforeRender;
begin
  inherited;
  GLState.Push;
  GLState[stDepthTest] := False;
  GLState[stDepthMask] := False;
end;

procedure TSkybox.TSkyboxVAO.AfterRender;
begin
  GLState.Pop;
  inherited;
end;

end.
