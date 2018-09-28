unit HelperFrameBounds;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.ExtCtrls,

  Pengine.Utility,
  Pengine.Vector;

type
  TfrmHelperBounds = class(TFrame)
    cbRanged: TCheckBox;
    edtMin: TEdit;
    lbName: TLabel;
    edtMax: TEdit;
    procedure cbRangedClick(Sender: TObject);
    procedure edtMaxChange(Sender: TObject);
    procedure edtMinChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
  
    function GetBounds: TBounds1;
    function GetBoundsMax: Single;
    function GetBoundsMin: Single;
    procedure SetBounds(const Value: TBounds1); reintroduce;
    procedure SetBoundsMax(const Value: Single);
    procedure SetBoundsMin(const Value: Single);

    procedure SetMode(ARanged: Boolean);

    function GetFloat(AEdit: TEdit): Single;
    procedure SetFloat(AEdit: TEdit; AValue: Single);

  public
    property Bounds: TBounds1 read GetBounds write SetBounds;
    property BoundsMin: Single read GetBoundsMin write SetBoundsMin;
    property BoundsMax: Single read GetBoundsMax write SetBoundsMax;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    
  end;

implementation

{$R *.dfm}

{ TfrmHelperIntBounds }

procedure TfrmHelperBounds.cbRangedClick(Sender: TObject);
begin
  SetMode(cbRanged.Checked);
  if cbRanged.Checked then
    SetFloat(edtMax, GetFloat(edtMin));
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TfrmHelperBounds.GetBounds: TBounds1;
begin
  if cbRanged.Checked then
    Result := Bounds1(GetFloat(edtMin), GetFloat(edtMax))
  else
    Result := GetFloat(edtMin);
end;

function TfrmHelperBounds.GetBoundsMax: Single;
begin
  Result := Bounds.C2;
end;

function TfrmHelperBounds.GetBoundsMin: Single;
begin
  Result := Bounds.C1;
end;

function TfrmHelperBounds.GetFloat(AEdit: TEdit): Single;
begin
  if not Single.TryParse(AEdit.Text, Result, TFormatSettings.Invariant) then
    Result := 0;
end;

procedure TfrmHelperBounds.edtMaxChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TfrmHelperBounds.edtMinChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TfrmHelperBounds.SetBounds(const Value: TBounds1);
begin
  if Bounds = Value then
    Exit;
  cbRanged.Checked := Value.Length <> 0;
  SetMode(cbRanged.Checked );
  SetFloat(edtMin, Value.C1);
  SetFloat(edtMax, Value.C2);
end;

procedure TfrmHelperBounds.SetBoundsMax(const Value: Single);
begin
  Bounds := Bounds1(Bounds.C1, Value);
end;

procedure TfrmHelperBounds.SetBoundsMin(const Value: Single);
begin
  Bounds := Bounds1(Value, Bounds.C2);
end;

procedure TfrmHelperBounds.SetFloat(AEdit: TEdit; AValue: Single);
begin
  AEdit.Text := PrettyFloat(AValue);
end;

procedure TfrmHelperBounds.SetMode(ARanged: Boolean);
begin
  edtMax.Visible := ARanged;
end;

end.
