unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.Math,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  Lists, VectorGeometry, Vcl.ExtCtrls;

type

  TP2DObject = class abstract
  public type
    TBounds = TArrayList<TGVector2>;

  private
    FGravCenter: TGVector2;
    FGravCenterChanged: Boolean;

    FPos: TGVector2;
    FVelocity: TGVector2;

    FRotation: Single;
    FRotVelocity: Single;

    FMass: Single;

    FBounds: TBounds;
    FBoundsChanged: Boolean;

    function Transform(APoint: TGVector2): TGVector2;
    function Untransform(APoint: TGVector2): TGVector2;
    function GetBound(I: Integer): TGVector2;
    function GetSide(I: Integer): TGLine2;
    function GetBoundCount: Integer;
    procedure SetPos(const Value: TGVector2);
    procedure SetRotation(const Value: Single);
    function GetGravCenter: TGVector2;

  protected
    procedure BuildBounds(ABounds: TBounds); virtual; abstract;
    function CalcGravCenter: TGVector2; virtual; abstract;

    procedure NotifyBoundsChanged;
    procedure NotifyGravityCenterChanged;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single);

    property Pos: TGVector2 read FPos write SetPos;
    property Rotation: Single read FRotation write SetRotation;
    property GravCenter: TGVector2 read GetGravCenter;

    property BoundCount: Integer read GetBoundCount;
    property Bounds[I: Integer]: TGVector2 read GetBound;
    property Sides[I: Integer]: TGLine2 read GetSide;

    procedure ApplyForce(FFrom, FForce: TGVector2);

  end;

  TP2DRect = class(TP2DObject)
  private
    FSize: TGVector2;

    procedure SetHeight(const Value: Single);
    procedure SetWidth(const Value: Single);
    procedure SetSize(const Value: TGVector2);

  protected
    procedure BuildBounds(ABounds: TP2DObject.TBounds); override;
    function CalcGravCenter: TGVector2; override;

  public
    constructor Create;

    property Size: TGVector2 read FSize write SetSize;
    property Width: Single read FSize.X write SetWidth;
    property Height: Single read FSize.Y write SetHeight;

  end;

  TP2DWorld = class
  private
    FObjects: TObjectArray<TP2DObject>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Update(ADeltaTime: Single);

    property Objects: TObjectArray<TP2DObject> read FObjects;

  end;

  TfrmMain = class(TForm)
    tmrUpdate: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
  private
    FWorld: TP2DWorld;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}
{ TP2DWorld }

constructor TP2DWorld.Create;
begin
  FObjects := TObjectArray<TP2DObject>.Create;
end;

destructor TP2DWorld.Destroy;
begin
  FObjects.Free;
  inherited;
end;

procedure TP2DWorld.Update(ADeltaTime: Single);
var
  Obj: TP2DObject;
begin
  for Obj in FObjects do
    Obj.Update(ADeltaTime);
end;

{ TP2DObject }

procedure TP2DObject.ApplyForce(FFrom, FForce: TGVector2);
var
  AccRot: Single;
  F: TGVector2;
  R: TGVector2;
begin
  F := FForce.Rotate(-FRotation);
  R := GravCenter.VectorTo(FFrom);
  AccRot := R.ToVec3.Cross(F).Z;
  FRotVelocity := FRotVelocity + AccRot;
end;

constructor TP2DObject.Create;
begin
  FBounds := TBounds.Create;
  FBoundsChanged := True;
  FGravCenterChanged := True;
  FMass := 1;
  FPos := -UVecXY;
end;

destructor TP2DObject.Destroy;
begin
  FBounds.Free;
  inherited;
end;

function TP2DObject.GetBound(I: Integer): TGVector2;
begin
  if FBoundsChanged then
  begin
    BuildBounds(FBounds);
    FBoundsChanged := False;
  end;
  Result := Transform(FBounds[I]);
end;

function TP2DObject.GetBoundCount: Integer;
begin
  if FBoundsChanged then
  begin
    BuildBounds(FBounds);
    FBoundsChanged := False;
  end;
  Result := FBounds.Count;
end;

function TP2DObject.GetGravCenter: TGVector2;
begin
  if FGravCenterChanged then
  begin
    FGravCenter := CalcGravCenter;
    FGravCenterChanged := False;
  end;
  Result := FGravCenter;
end;

function TP2DObject.GetSide(I: Integer): TGLine2;
begin
  Result := Bounds[I].LineTo(Bounds[(I + 1) mod BoundCount]);
end;

procedure TP2DObject.NotifyBoundsChanged;
begin
  FBoundsChanged := True;
end;

procedure TP2DObject.NotifyGravityCenterChanged;
begin
  FGravCenterChanged := True;
end;

procedure TP2DObject.SetPos(const Value: TGVector2);
begin
  if FPos = Value then
    Exit;
  FPos := Value;
  NotifyBoundsChanged;
end;

procedure TP2DObject.SetRotation(const Value: Single);
begin
  if FRotation = Value then
    Exit;
  FRotation := Value;
  NotifyBoundsChanged;
end;

function TP2DObject.Transform(APoint: TGVector2): TGVector2;
begin
  Result := Pos + APoint.Rotate(Rotation);
end;

function TP2DObject.Untransform(APoint: TGVector2): TGVector2;
begin
  Result := (Pos - APoint).Rotate(-Rotation);
end;

procedure TP2DObject.Update(ADeltaTime: Single);
var
  R: Single;
  Offset: TGVector2;
begin
  R := FRotVelocity * ADeltaTime;
  FPos := FPos + FVelocity * ADeltaTime;
  FRotation := FRotation + R;
  Offset := Transform(GravCenter);
  FPos := FPos - Offset;
  FPos := FPos.Rotate(R);
  FPos := FPos + Offset;
end;

{ TP2DRect }

procedure TP2DRect.BuildBounds(ABounds: TP2DObject.TBounds);
var
  P: TGVector2;
begin
  for P in QuadTexCoords do
    ABounds.Add(P * FSize);
end;

function TP2DRect.CalcGravCenter: TGVector2;
begin
  Result := Size / 2;
end;

constructor TP2DRect.Create;
begin
  inherited;
  Size := UVecXY + 2;
end;

procedure TP2DRect.SetHeight(const Value: Single);
begin
  if FSize.Y = Value then
    Exit;
  FSize.Y := Value;
  NotifyBoundsChanged;
end;

procedure TP2DRect.SetSize(const Value: TGVector2);
begin
  if FSize = Value then
    Exit;
  FSize := Value;
  NotifyBoundsChanged;
end;

procedure TP2DRect.SetWidth(const Value: Single);
begin
  if FSize.X = Value then
    Exit;
  FSize.X := Value;
  NotifyBoundsChanged;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FWorld := TP2DWorld.Create;
  FWorld.Objects.Add(TP2DRect.Create);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FWorld.Free;
end;

procedure TfrmMain.FormPaint(Sender: TObject);

  function GetPoint(const APoint: TGVector2): TPoint;
  begin
    Result.X := ClientWidth div 2 + Floor(APoint.X * ClientHeight / 10 + 0.5);
    Result.Y := ClientHeight div 2 + Floor(APoint.Y * ClientHeight / 10 + 0.5);
    Result.Y := ClientHeight - Result.Y;
  end;

var
  Obj: TP2DObject;
  I: Integer;
  Poly: TArray<TPoint>;
begin
  Canvas.Pen.Width := 5;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := $FF9F7F;

  for Obj in FWorld.Objects do
  begin
    SetLength(Poly, Obj.BoundCount);
    for I := 0 to Obj.BoundCount - 1 do
      Poly[I] := GetPoint(Obj.Bounds[I]);
    Canvas.Polygon(Poly);
  end;
end;

procedure TfrmMain.tmrUpdateTimer(Sender: TObject);
begin
  FWorld.Objects[0].ApplyForce(TGVector2.Create(1, 1), UVecX * 5);

  FWorld.Update(tmrUpdate.Interval / 1000);
  Invalidate;
end;

end.
