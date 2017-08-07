unit Lights;

{$mode Delphi}

interface

uses
  dglOpenGL, Color, VectorGeometry, SysUtils;

type
  TGLLight = class
  private
    FLightIndex: Integer;

    FPosition: TGLVector;
    FAmbient: TGLColor;
    FDiffuse: TGLColor;
    FSpecular: TGLColor;
    FActive: Boolean;

  class var
    MaxLights: Integer;
    NextIndex: Integer;
    UsedIndices: array of Boolean;
    FLightCount: Integer;

    procedure SetPosition(const Value: TGLVector);
    procedure SetAmbient(const Value: TGLColor);
    procedure SetDiffuse(const Value: TGLColor);
    procedure SetSpecular(const Value: TGLColor);

    function GetOrthoLight: Boolean;
    procedure SetOrthoLight(const Value: Boolean);
    procedure SetActive(const Value: Boolean);
    procedure SetPositionX(const Value: Single);
    procedure SetPositionY(const Value: Single);
    procedure SetPositionZ(const Value: Single);
  public
    // Position
    property Position: TGLVector read FPosition write SetPosition;
    property PositionX: Single write SetPositionX;
    property PositionY: Single write SetPositionY;
    property PositionZ: Single write SetPositionZ;
    property OrthoLight: Boolean read GetOrthoLight write SetOrthoLight;

    // Color
    property Ambient: TGLColor read FAmbient write SetAmbient;
    property Diffuse: TGLColor read FDiffuse write SetDiffuse;
    property Specular: TGLColor read FSpecular write SetSpecular;

    property Active: Boolean read FActive write SetActive;

    // Light Index Handling
    property LightIndex: Integer read FLightIndex;
    class property LightCount: Integer read FLightCount;

    procedure Render;

    class procedure Init;
    class procedure Enable;
    class procedure Disable;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TGLLight }

constructor TGLLight.Create;
begin
  Assert(LightCount < MaxLights, 'Maximum of ' + IntToStr(MaxLights) + ' exceeded!');

  FLightIndex := NextIndex;
  Inc(FLightCount);

  UsedIndices[LightIndex] := True;

  while UsedIndices[NextIndex] do
    Inc(NextIndex);

  Position := TGLVector.New(0, 0, 1, 0);
  Ambient := TGLColor.New(0, 0, 0);
  Diffuse := TGLColor.New(1, 1, 1);
  Specular := TGLColor.New(1, 1, 1);
  Active := True;
end;

destructor TGLLight.Destroy;
begin
  UsedIndices[LightIndex] := False;
  if LightIndex < NextIndex then
    NextIndex := LightIndex;
  inherited;
end;

class procedure TGLLight.Disable;
begin
  glDisable(GL_LIGHTING);
end;

class procedure TGLLight.Enable;
begin
  glEnable(GL_LIGHTING);
end;

function TGLLight.GetOrthoLight: Boolean;
begin
  Result := Position.W = 0;
end;

class procedure TGLLight.Init;
begin
  glGetIntegerv(GL_MAX_LIGHTS, @MaxLights);
  SetLength(UsedIndices, MaxLights);
end;

procedure TGLLight.Render;
begin
  glLightfv(GL_LIGHT0 + LightIndex, GL_POSITION, @Position);
end;

procedure TGLLight.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if Value then
    glEnable(GL_LIGHT0 + LightIndex)
  else
    glDisable(GL_LIGHT0 + LightIndex);
end;

procedure TGLLight.SetAmbient(const Value: TGLColor);
begin
  FAmbient := Value;
  glLightfv(GL_LIGHT0 + LightIndex, GL_AMBIENT, @Value);
end;

procedure TGLLight.SetDiffuse(const Value: TGLColor);
begin
  FDiffuse := Value;
  glLightfv(GL_LIGHT0 + LightIndex, GL_DIFFUSE, @Value);
end;

procedure TGLLight.SetOrthoLight(const Value: Boolean);
begin
  if Value then
    FPosition.W := 0
  else
    FPosition.W := 1;
end;

procedure TGLLight.SetPosition(const Value: TGLVector);
begin
  FPosition := Value;
end;

procedure TGLLight.SetPositionX(const Value: Single);
begin
  FPosition.X := Value;
end;

procedure TGLLight.SetPositionY(const Value: Single);
begin
  FPosition.Y := Value;
end;

procedure TGLLight.SetPositionZ(const Value: Single);
begin
  FPosition.Z := Value;
end;

procedure TGLLight.SetSpecular(const Value: TGLColor);
begin
  FSpecular := Value;
  glLightfv(GL_LIGHT0 + LightIndex, GL_SPECULAR, @Value);
end;

end.
