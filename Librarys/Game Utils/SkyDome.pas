unit SkyDome;

interface

uses
  VAOManager, Lists, Color, Shaders, SysUtils, GLEnums, VectorGeometry, UBOManager, Camera, OpenGLContext;

type

  TStripeData = record
    Color: TColorRGB;
    Pitch: Single;
  end;

  { TStripe }

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


  { TSkyDome }

  TSkyDome = class (TVAO)
  private
    type

      TData = record
        Pos: TGVector3;
        Pitch: Single;
      end;

  private
    FUBO: TUBO;
    FGLForm: TGLForm;
    FStripes: TObjectArray<TStripe>;
    FCamera: TCamera;

    procedure BuildVAO;

  protected
    procedure BeforeRender; override;
    procedure AfterRender; override;

  public
    constructor Create(AGLForm: TGLForm; ACamera: TCamera; AShader: TShader);
    destructor Destroy; override;

    procedure AddStripe(AColor: TColorRGB; AAngle: Single);

    const
      MaxStripes = 16;
      UBOSize = SizeOf(TStripeData) * MaxStripes + SizeOf(Integer);
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
  Data.Pitch := FAngle;
  FUBO.SubData(SizeOf(TStripeData) * FIndex, SizeOf(TStripeData), Data);
end;

{ TSkyDome }

procedure TSkyDome.BuildVAO;
const
  PitchSteps = 30; // min 1
  TurnSteps = 120;  // min 3

  procedure AddData(T, P: Single);
  var
    Data: TData;
  begin
    P := P * 90 / PitchSteps;
    T := T * 360 / TurnSteps;
    Data.Pos := TGDirection.Create(T, P).ToVector;
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

procedure TSkyDome.BeforeRender;
begin
  inherited BeforeRender;
  FGLForm.PushState;
  FGLForm.State.DepthTest := False;
  FGLForm.State.DepthMask := False;
end;

procedure TSkyDome.AfterRender;
begin
  FGLForm.PopState;
  inherited AfterRender;
end;

constructor TSkyDome.Create(AGLForm: TGLForm; ACamera: TCamera; AShader: TShader);
const
  Zero: Integer = 0;
begin
  inherited Create(AShader);

  FCamera := ACamera;

  FGLForm := AGLForm;

  FUBO := TUBO.Create;
  FUBO.Generate(UBOSize, buStaticDraw);
  FUBO.SubData(SizeOf(TStripeData) * MaxStripes, SizeOf(Integer), Zero);
  FUBO.BindToShader(AShader, 'stripedata');

  FStripes := TObjectArray<TStripe>.Create;

  BuildVAO;
end;

destructor TSkyDome.Destroy;
begin
  FStripes.Free;
  FUBO.Free;
  inherited Destroy;
end;

procedure TSkyDome.AddStripe(AColor: TColorRGB; AAngle: Single);
begin
  if (FStripes.Last <> nil) and (AAngle <= TStripe(FStripes.Last).Angle) then
    raise Exception.Create('Angles of SkyDome Stripes must be ascending!');
  FStripes.Add(TStripe.Create(AColor, AAngle, FStripes.Count, FUBO));
  FUBO.SubData(SizeOf(TStripeData) * MaxStripes, SizeOf(Integer), FStripes.Count);
end;

end.

