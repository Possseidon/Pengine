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
    class function GetTitle: string; virtual; abstract;
    class function GetDescription: string; virtual;

    property Root: TRootSettings read FRoot;

    procedure SetDefaults; virtual; 
    function Equals(Obj: TObject): Boolean; overload; override;
    function Equals(AOther: TSettings): Boolean; reintroduce; overload; virtual;
    /// <summary>Copies all settings.</summary>
    /// <remarks>The root is not changed.</remarks>
    procedure Assign(AFrom: TSettings); virtual;

    function Get<T: TSettings>: T; overload;
    function Get(ASettingsClass: TSettingsClass): TSettings; overload;

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

  end;

var
  RootSettings: TRootSettings;

implementation

{ TSettings }

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

function TSettings.Get(ASettingsClass: TSettingsClass): TSettings;
begin
  if not Root.FSubSettings.Get(ASettingsClass, Result) then
  begin
    Result := ASettingsClass.Create(Root);
    Root.FSubSettings[ASettingsClass] := Result;
    Result.SetDefaults;
  end;
end;

function TSettings.Get<T>: T;
begin
  Result := T(Get(T));
end;

{ TRootSettings }

constructor TRootSettings.Create;
begin
  inherited Create(Self);
  FSubSettings := TSubSettings.Create;
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

initialization

RootSettings := TRootSettings.Create;

finalization

RootSettings.Free;

end.
