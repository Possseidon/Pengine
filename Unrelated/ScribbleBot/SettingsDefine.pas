unit SettingsDefine;

interface

uses
  System.SysUtils,

  Pengine.IntMaths,
  Pengine.Color;

type

  TPalette = class
  private
    FArea: TIntBounds2;
    FSize: TIntVector2;
    FColorValues: array of array of TColorRGB;
    function GetColorValue(APos: TIntVector2): TColorRGB;
    procedure SetColorValue(APos: TIntVector2; const Value: TColorRGB);
    procedure SetSize(const Value: TIntVector2);

  public
    property Area: TIntBounds2 read FArea write FArea;
    property Size: TIntVector2 read FSize write SetSize;

    property ColorValues[APos: TIntVector2]: TColorRGB read GetColorValue write SetColorValue;

  end;

  TSettings = class
  private
    FDrawArea: TIntBounds2;
    FPalette: TPalette;

  public
    constructor Create;
    destructor Destroy; override;

    property DrawArea: TIntBounds2 read FDrawArea write FDrawArea;
    property Palette: TPalette read FPalette;


  end;

implementation

{ TSettings }

constructor TSettings.Create;
begin
  FPalette := TPalette.Create;
end;

destructor TSettings.Destroy;
begin
  FPalette.Free;
  inherited;
end;

{ TPalette }

function TPalette.GetColorValue(APos: TIntVector2): TColorRGB;
begin
  Result := FColorValues[APos.X, APos.Y];
end;

procedure TPalette.SetColorValue(APos: TIntVector2; const Value: TColorRGB);
begin
  if not (APos in Size) then
    raise EArgumentOutOfRangeException.Create('Color position out of range.');
  FColorValues[APos.X, APos.Y] := Value;
end;

procedure TPalette.SetSize(const Value: TIntVector2);
begin
  FSize := Value;
  SetLength(FColorValues, Size.X, Size.Y);
end;

end.

