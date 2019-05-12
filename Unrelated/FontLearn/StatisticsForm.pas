unit StatisticsForm;

interface

uses
  System.Classes,

  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,

  VclTee.TeeGDIPlus,
  VclTee.TeEngine,
  VclTee.Series,
  VclTee.TeeProcs,
  VclTee.Chart;

type

  TStatistics = array ['A' .. 'Z'] of Integer;

  TfrmStatistics = class(TForm)
    Chart1: TChart;
    Series1: THorizBarSeries;
  public
    procedure Show(AStatistics: TStatistics);

  end;

var
  frmStatistics: TfrmStatistics;

implementation

{$R *.dfm}

{ TfrmStatistics }

procedure TfrmStatistics.Show(AStatistics: TStatistics);
var
  I: Char;
begin
  Series1.BeginUpdate;
  Series1.Clear;
  for I := Low(TStatistics) to High(TStatistics) do
    Series1.Add(AStatistics[I], I);
  Series1.EndUpdate;
  ShowModal;
end;

end.
