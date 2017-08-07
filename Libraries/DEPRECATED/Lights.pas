unit Lights;

interface

uses
  Color, VectorGeometry, SysUtils, Lists, Shaders, UBOManager, GLEnums;

type

{
  4 pos         // w > ortho direction
  3 direction   // for ortho/cutoff
  3 color
  1 ambient
  1 diffuse
  1 specular
  1 cutoff
  1 cutoffbonus // smooth cutoff around default cutoff
  1 attenuation
}

  TLightData = record
    Pos: TGVector4;
    Dir: TGVector3;
    Attenuation: Single;
    Color: TColorRGB;
    Ambient, Diffuse, Specular: Single;
    Cutoff, CutoffBonus: Single;
  end;

  { TLight }

  TLight = class
  private
    FLocation: TLocation;
    FOrtho: Boolean;

    FUBO: TUBO;
    FIndex: Integer;

    FActive: Boolean;
    FChanged: Boolean;

    // order important for move
    FAttenuation: Single;
    FColor: TColorRGB;
    FAmbient, FDiffuse, FSpecular: Single;
    FCutoff, FCutoffBonus: Single;

    function GetChanged: Boolean;
    function GetData: TLightData;
    procedure OnLightChange(AChanges: TLocationChanges);
    procedure SetActive(AValue: Boolean);
    procedure SetAmbient(AValue: Single);
    procedure SetAttenuation(AValue: Single);
    procedure SetColor(AValue: TColorRGB);
    procedure SetCutoff(AValue: Single);
    procedure SetCutoffBonus(AValue: Single);
    procedure SetDiffuse(AValue: Single);
    procedure SetOrtho(AValue: Boolean);
    procedure SetSpecular(AValue: Single);

    procedure Send(AOffset, ASingles: Integer; const AData);
    procedure SendAll;

  public
    constructor Create;
    destructor Destroy; override;

    property Active: Boolean read FActive write SetActive;

    property Location: TLocation read FLocation;
    property Ortho: Boolean read FOrtho write SetOrtho;

    property Color: TColorRGB read FColor write SetColor;
    property Ambient: Single read FAmbient write SetAmbient;
    property Diffuse: Single read FDiffuse write SetDiffuse;
    property Specular: Single read FSpecular write SetSpecular;

    property Cutoff: Single read FCutoff write SetCutoff;
    property CutoffBonus: Single read FCutoffBonus write SetCutoffBonus;
    property Attenuation: Single read FAttenuation write SetAttenuation;

    property Data: TLightData read GetData;

    property Changed: Boolean read GetChanged;
    procedure NotifyChanges;

  end;

  { TLightSystem }

  TLightSystem = class
  private
    FLights: TObjectArray<TLight>;
    FUBO: TUBO;

    const
      MaxLights = 256;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddShader(AShader: TShader);

    procedure AddLight(ALight: TLight);
    procedure DelLight(ALight: TLight);

  end;

implementation

{ TLight }

procedure TLight.SetAmbient(AValue: Single);
begin
  if FAmbient = AValue then
    Exit;
  FAmbient := AValue;
  FChanged := FActive;
  Send(11, 1, FAmbient);
end;

function TLight.GetChanged: Boolean;
begin
  Result := FLocation.Changed or FChanged;
end;

function TLight.GetData: TLightData;
begin
  Result.Pos := FLocation.RealPosition;
  if FOrtho then
    Result.Pos.W := 0;
  Result.Dir := FLocation.Look;
  Result.Attenuation := FAttenuation;
  if FActive then
    Result.Color := FColor
  else
    Result.Color := TColorRGB.Create(0, 0, 0);
  Move(FAmbient, Result.Ambient, 5 * SizeOf(Single));
end;

procedure TLight.OnLightChange(AChanges: TLocationChanges);
var
  NewPos: TGVector3;
begin
  if [lcPosition, lcOffset, lcFreeTranslation] - AChanges <> [] then
  begin
    NewPos := FLocation.RealPosition;
    Send(0, 3, NewPos);
  end;
end;

procedure TLight.SetActive(AValue: Boolean);
var
  NewColor: TColorRGBA;
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  FChanged := True;
  if FActive then
    NewColor := ColorBlack
  else
    NewColor := FColor;
  Send(4, 3, NewColor);
end;

procedure TLight.SetAttenuation(AValue: Single);
begin
  if FAttenuation = AValue then
    Exit;
  FAttenuation := AValue;
  FChanged := FActive;
  Send(7, 1, FAttenuation);
end;

procedure TLight.SetColor(AValue: TColorRGB);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  FChanged := FActive;
  if FActive then
    Send(4, 3, FColor);
end;

procedure TLight.SetCutoff(AValue: Single);
begin
  if FCutoff = AValue then
    Exit;
  FCutoff := AValue;
  FChanged := FActive;
  Send(14, 1, FCutoff);
end;

procedure TLight.SetCutoffBonus(AValue: Single);
begin
  if FCutoffBonus = AValue then
    Exit;
  FCutoffBonus := AValue;
  FChanged := FActive;
  Send(15, 1, FCutoffBonus);
end;

procedure TLight.SetDiffuse(AValue: Single);
begin
  if FDiffuse = AValue then
    Exit;
  FDiffuse := AValue;
  FChanged := FActive;
  Send(12, 1, FDiffuse);
end;

procedure TLight.SetOrtho(AValue: Boolean);
var
  Value: Single;
begin
  if FOrtho = AValue then
    Exit;
  FOrtho := AValue;
  FChanged := True;
  if FOrtho then
    Value := 0.0
  else
    Value := 1.0;
  Send(4, 1, Value);
end;

procedure TLight.SetSpecular(AValue: Single);
begin
  if FSpecular = AValue then
    Exit;
  FSpecular := AValue;
  FChanged := FActive;
  Send(13, 1, FSpecular);
end;

procedure TLight.Send(AOffset, ASingles: Integer; const AData);
begin
  FUBO.SubData(SizeOf(TLightData) * FIndex + SizeOf(Single) * AOffset, SizeOf(Single) * ASingles, AData);
end;

constructor TLight.Create;
begin
  FLocation := TLocation.Create(True);
  FLocation.OnChange := OnLightChange;
  FActive := True;
  FColor := TColorRGB.Create(1, 1, 1);
  FOrtho := False;
  FAmbient := 0.1;
  FDiffuse := 0.9;
  FSpecular := 1.0;
  FCutoff := 180;
  FCutoffBonus := 0;
  FAttenuation := 0;
end;

destructor TLight.Destroy;
begin
  FLocation.Free;
  inherited Destroy;
end;

procedure TLight.NotifyChanges;
begin
  FChanged := False;
  FLocation.NotifyChanges;
end;

procedure TLight.SendAll;
begin
  FUBO.SubData(SizeOf(TLightData) * FIndex, SizeOf(TLightData), Data);
end;

{ TLightSystem }

constructor TLightSystem.Create;
const
  Zero: Integer = 0;
begin
  FLights := TObjectArray<TLight>.Create(True);
  FUBO := TUBO.Create;
  FUBO.Generate(SizeOf(TLightData) * MaxLights + SizeOf(Integer), buDynamicDraw);
  FUBO.SubData(SizeOf(TLightData) * MaxLights, SizeOf(Integer), Zero); // zero lights
end;

destructor TLightSystem.Destroy;
begin
  FLights.Free;
  FUBO.Free;
  inherited Destroy;
end;

procedure TLightSystem.AddShader(AShader: TShader);
begin
  FUBO.BindToShader(AShader, 'lightdata');
end;

procedure TLightSystem.AddLight(ALight: TLight);
begin
  if FLights.Count = MaxLights then
    raise Exception.Create('Maximum Lights reached!');
  ALight.FIndex := FLights.Count;
  ALight.FUBO := FUBO;
  ALight.SendAll;
  FLights.Add(ALight);
  FUBO.SubData(SizeOf(TLightData) * MaxLights, SizeOf(Integer), FLights.Count);
end;

procedure TLightSystem.DelLight(ALight: TLight);
var
  I: Integer;
begin
  for I := FLights.FindObject(ALight) + 1 to FLights.Count - 1 do
  with TLight(FLights[I]) do
  begin
    Dec(FIndex);
    SendAll;
  end;
  FLights.DelObject(ALight);
  FUBO.SubData(SizeOf(TLightData) * MaxLights, SizeOf(Integer), FLights.Count);
end;

end.
