unit Pengine.Settings;

interface

uses
  System.SysUtils,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher;

type

  ESettings = class(Exception);

  TSettingsClass = class of TSettings;

  TRootSettings = class;

  TSettings = class
  private
    FRoot: TRootSettings;

  protected
    constructor Create(ARoot: TRootSettings); virtual;

  public
    procedure AfterConstruction; override;

    class function GetTitle: string; virtual; abstract;
    class function GetDescription: string; virtual;

    property Root: TRootSettings read FRoot;

    procedure SetDefaults; virtual; 
    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(AOther: TSettings): Boolean; reintroduce; overload; virtual;
    /// <summary>Copies all settings.</summary>
    /// <remarks>The root is not changed.</remarks>
    procedure Assign(AFrom: TSettings); virtual;

    function Sub<T: TSettings>: T; overload;
    function Sub(ASettingsClass: TSettingsClass): TSettings; overload;

  end;

  TRootSettings = class(TSettings)
  public type

    TSubSettings = TClassObjectMap<TSettings>;

  private
    FSubSettings: TSubSettings;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    class function GetTitle: string; override;
    class function GetDescription: string; override;
    class function GetSettingsClasses: System.TArray<TSettingsClass>; virtual; abstract;

  end;

implementation

{ TSettings }

procedure TSettings.AfterConstruction;
begin
  SetDefaults;
end;

procedure TSettings.Assign(AFrom: TSettings);
begin
  if AFrom.ClassType <> Self.ClassType then
    raise ESettings.CreateFmt('Cannot assign settings "%s" to "%s".', [AFrom.GetTitle, GetTitle]);
end;

constructor TSettings.Create(ARoot: TRootSettings);
begin
  FRoot := ARoot;
end;

function TSettings.Equals(Obj: TObject): Boolean;
begin
  Result := Equals(Obj as TSettings);
end;

function TSettings.Equals(AOther: TSettings): Boolean;
begin
  if AOther.ClassType <> Self.ClassType then
    raise ESettings.CreateFmt('Cannot compare settings "%s" to "%s".', [AOther.GetTitle, GetTitle]);
  Result := True;
end;

class function TSettings.GetDescription: string;
begin
  Result := '-';
end;

procedure TSettings.SetDefaults;
begin
  // nothing by default
end;

function TSettings.Sub(ASettingsClass: TSettingsClass): TSettings;
begin
  if not Root.FSubSettings.Get(ASettingsClass, Result) then
    raise ESettings.CreateFmt('The setting "%s" is not available.', [ASettingsClass.GetTitle]);
end;

function TSettings.Sub<T>: T;
begin
  if not Root.FSubSettings.Get(T, TSettings(Result)) then
    raise ESettings.CreateFmt('The setting "%s" is not available.', [T.GetTitle]);
end;

{ TRootSettings }

constructor TRootSettings.Create;
var
  SettingsClass: TSettingsClass;
begin
  inherited Create(Self);
  FSubSettings := TSubSettings.Create;
  for SettingsClass in GetSettingsClasses do
    FSubSettings[SettingsClass] := SettingsClass.Create(Self);
end;

destructor TRootSettings.Destroy;
begin
  FSubSettings.Free;
  inherited;
end;

class function TRootSettings.GetDescription: string;
begin
  Result := 'The root, containing all other settings.';
end;

class function TRootSettings.GetTitle: string;
begin
  Result := 'Root';
end;

end.
