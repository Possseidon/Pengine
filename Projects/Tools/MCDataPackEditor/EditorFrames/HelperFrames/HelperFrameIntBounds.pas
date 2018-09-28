unit HelperFrameIntBounds;

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

  Pengine.IntMaths;

type
  TfrmHelperIntBounds = class(TFrame)
    cbRanged: TCheckBox;
    seMin: TSpinEdit;
    lbName: TLabel;
    seMax: TSpinEdit;
    procedure cbRangedClick(Sender: TObject);
    procedure seMaxChange(Sender: TObject);
    procedure seMinChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
  
    function GetBounds: TIntBounds1;
    function GetBoundsMax: Integer;
    function GetBoundsMin: Integer;
    procedure SetBounds(const Value: TIntBounds1); reintroduce;
    procedure SetBoundsMax(const Value: Integer);
    procedure SetBoundsMin(const Value: Integer);

    procedure SetMode(ARanged: Boolean);
    
  public
    property Bounds: TIntBounds1 read GetBounds write SetBounds;
    property BoundsMin: Integer read GetBoundsMin write SetBoundsMin;
    property BoundsMax: Integer read GetBoundsMax write SetBoundsMax;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    
  end;

implementation

{$R *.dfm}

{ TfrmHelperIntBounds }

procedure TfrmHelperIntBounds.cbRangedClick(Sender: TObject);
begin
  SetMode(cbRanged.Checked);
  if cbRanged.Checked then
    seMax.Value := seMin.Value;
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TfrmHelperIntBounds.GetBounds: TIntBounds1;
begin
  if cbRanged.Checked then
    Result := IBounds1I(seMin.Value, seMax.Value)
  else
    Result := seMin.Value;
end;

function TfrmHelperIntBounds.GetBoundsMax: Integer;
begin
  Result := Bounds.C2;
end;

function TfrmHelperIntBounds.GetBoundsMin: Integer;
begin
  Result := Bounds.C1;
end;

procedure TfrmHelperIntBounds.seMaxChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TfrmHelperIntBounds.seMinChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TfrmHelperIntBounds.SetBounds(const Value: TIntBounds1);
begin
  if Bounds = Value then
    Exit;
  cbRanged.Checked := Value.Length <> 0;
  SetMode(cbRanged.Checked );
  seMin.Value := Value.C1;
  seMax.Value := Value.C2;
end;

procedure TfrmHelperIntBounds.SetBoundsMax(const Value: Integer);
begin
  Bounds := IBounds1(Bounds.C1, Value);
end;

procedure TfrmHelperIntBounds.SetBoundsMin(const Value: Integer);
begin
  Bounds := IBounds1(Value, Bounds.C2);
end;

procedure TfrmHelperIntBounds.SetMode(ARanged: Boolean);
begin
  seMax.Visible := ARanged;
end;

end.
