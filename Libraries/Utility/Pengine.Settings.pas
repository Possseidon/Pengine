unit Pengine.Settings;

interface

uses
  System.SysUtils,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.EventHandling;

type

  ESettings = class(Exception);

  TSettingsClass = class of TSettings;

  TRootSettings = class;

  TSettings = class
  public type

    TEventInfo = TSenderEventInfo<TSettings>;

    TEvent = TEvent<TEventInfo>;

  private
    FRoot: TRootSettings;
    FOnReload: TEvent;

  protected
    constructor Create(ARoot: TRootSettings); virtual;

    procedure DoReload; virtual;

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

    procedure Reload; 
    function OnReload: TEvent.TAccess;

  end;

  TRootSettings = class(TSettings)
  public type

    TSubSettings = TClassObjectMap<TSettingsClass, TSettings>;

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

procedure TSettings.DoReload;
begin
  // nothing by default
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

function TSettings.OnReload: TEvent.TAccess;
begin
  Result := FOnReload.Access;
end;

procedure TSettings.Reload;
begin
  DoReload;
  FOnReload.Execute(TEventInfo.Create(Self));
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
    Result.FOnReload.Disable;
    Result.SetDefaults;
    Result.FOnReload.Enable;
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
