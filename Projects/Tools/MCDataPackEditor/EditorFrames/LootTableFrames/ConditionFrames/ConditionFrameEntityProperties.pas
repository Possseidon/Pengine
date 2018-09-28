unit ConditionFrameEntityProperties;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmConditionEntityProperties = class(TFrame)
    GroupBox1: TGroupBox;
    cbTarget: TComboBox;
    lbTarget: TLabel;
    cbOnFire: TCheckBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.

