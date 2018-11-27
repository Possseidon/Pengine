unit LineRider;

interface

uses
  Pengine.Vector,
  Pengine.Collections,
  Pengine.Interfaces,
  Pengine.JSON;

type

  TLineRider = class(TInterfaceBase, IJSerializable)
  public type

    TLine = class(TInterfaceBase, IJSerializable)
    public type

      TType = (
        ltNormal,
        ltBoost,
        ltDecoration
        );

    private
      FID: Integer;
      FLineType: TType;
      FPos1: TVector2;
      FPos2: TVector2;
      FFlipped: Boolean;
      FLeftExtended: Boolean;
      FRightExtended: Boolean;

    public
      property ID: Integer read FID write FID;
      property LineType: TType read FLineType write FLineType;
      property Pos1: TVector2 read FPos1 write FPos1;
      property Pos2: TVector2 read FPos2 write FPos2;
      property Flipped: Boolean read FFlipped write FFlipped;
      property LeftExtended: Boolean read FLeftExtended write FLeftExtended;
      property RightExtended: Boolean read FRightExtended write FRightExtended;

      function GetJVersion: Integer;
      procedure DefineJStorage(ASerializer: TJSerializer);

    end;

    TLines = TObjectArray<TLine>;

    TLinesSerializer = class(TJArraySerializer)
    private
      FLines: TLines;

    public
      constructor Create(ALines: TLines);

      procedure Serialize(AValue: TJArray); override;
      procedure Unserialize(AValue: TJArray); override;

    end;

  private
    FVersion: string;
    FCreator: string;
    FTrackLabel: string;
    FStartPosition: TVector2;
    FDuration: Integer;
    FDescription: string;
    FLines: TLines;

  public
    constructor Create;
    destructor Destroy; override;

    property TrackLabel: string read FTrackLabel write FTrackLabel;
    property Creator: string read FCreator write FCreator;
    property Description: string read FDescription write FDescription;
    property Version: string read FVersion write FVersion;
    property Duration: Integer read FDuration write FDuration;
    property StartPosition: TVector2 read FStartPosition write FStartPosition;

    property Lines: TLines read FLines;

    function GetJVersion: Integer;
    procedure DefineJStorage(ASerializer: TJSerializer);

  end;

implementation

{ TLineRider }

constructor TLineRider.Create;
begin
  FLines := TLines.Create;
end;

procedure TLineRider.DefineJStorage(ASerializer: TJSerializer);
begin
  with ASerializer do
  begin
    Define('label', FTrackLabel);
    Define('creator', FCreator);
    Define('description', FDescription);
    Define('duration', FDuration);
    Define('version', FVersion);
    if IsStoring then
    begin
      with Value.AddObject('startPosition') do
      begin
        AsObject['x'] := FStartPosition.X;
        AsObject['y'] := FStartPosition.Y;
      end;
    end
    else
    begin
      with Value['startPosition'] do
      begin
        FStartPosition.X := AsObject['x'];
        FStartPosition.Y := AsObject['y'];
      end;
    end;
    Define('lines', TLinesSerializer.Create(FLines));
  end;
end;

destructor TLineRider.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TLineRider.GetJVersion: Integer;
begin
  Result := 0;
end;

{ TLineRider.TLine }

procedure TLineRider.TLine.DefineJStorage(ASerializer: TJSerializer);
begin
  with ASerializer do
  begin
    Define('id', FID);
    if IsStoring then
      Value['type'] := Ord(FLineType)
    else
      FLineType := TType(Value['type'].AsInt);
    Define('x1', FPos1.X);
    Define('y1', FPos1.Y);
    Define('x2', FPos2.X);
    Define('y2', FPos2.Y);
    Define('flipped', FFlipped);
    Define('leftExtended', FLeftExtended);
    Define('rightExtended', FRightExtended);
  end;
end;

function TLineRider.TLine.GetJVersion: Integer;
begin
  Result := 0;
end;

{ TLineRider.TLinesSerializer }

constructor TLineRider.TLinesSerializer.Create(ALines: TLines);
begin
  FLines := ALines;
end;

procedure TLineRider.TLinesSerializer.Serialize(AValue: TJArray);
var
  Line: TLine;
begin
  for Line in FLines do
    AValue.Add(TJSerializer.Serialize(Line));
end;

procedure TLineRider.TLinesSerializer.Unserialize(AValue: TJArray);
var
  Line: TJValue;
begin
  for Line in AValue do
    TJSerializer.Unserialize(FLines.Add(TLine.Create), Line.AsObject);
end;

end.
