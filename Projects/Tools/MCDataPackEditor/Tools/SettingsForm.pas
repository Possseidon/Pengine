unit SettingsForm;

interface

uses
  System.SysUtils,

  Vcl.Forms,

  Pengine.Settings,

  Pengine.MC.Brigadier,
  Pengine.MC.BrigadierParser,
  Pengine.MC.BlockState,
  Pengine.MC.Item;

type

  TMainSettings = class(TRootSettings)
  public
    class function GetSubSettingClasses: System.TArray<Pengine.Settings.TSettingsClass>; override;

  end;

  TfrmSettings = class(TForm)
  private

  public


  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.dfm}

{ TMainSettings }

class function TMainSettings.GetSubSettingClasses: System.TArray<Pengine.Settings.TSettingsClass>;
begin
  Result := [
    TBrigadierSettings,
    TBlockSettings,
    TItemSettings,
    TFormatSettings
  ];
end;

end.
