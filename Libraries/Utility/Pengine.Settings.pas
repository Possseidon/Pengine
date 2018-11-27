unit Pengine.Settings;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.SyncObjs,
  System.Classes,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.EventHandling,
  Pengine.JSON,
  Pengine.Interfaces;

type

  ESettings = class(Exception);

  TRootSettings = class;

  TSettingsClass = class of TSettings;

  /// <summary>A json backed class for settings which can be required using a root settings object.</summary>
  /// <remarks>A global <c>TRootSettings</c> instance <c>RootSettingsG</c> is created upon startup for ease of use.<p/>
  /// /!\ Make sure to call <c>Modify</c>.</remarks>
  TSettings = class(TInterfaceBase, IJSerializable)
  public type

    TEventInfo = TSenderEventInfo<TSettings>;

    TEvent = TEvent<TEventInfo>;

    TDependents = TRefArray<TSettings>;

  private
    FRoot: TRootSettings;
    FModified: Boolean;
    FDependents: TDependents;
    FOnReload: TEvent;

  protected
    /// <summary>Hidden constructor, as instances should only be created using the <c>Get</c> functions.</summary>
    constructor Create(ARoot: TRootSettings); virtual;

    /// <summary>Sets the modified flag to true.</summary>
    procedure Modify;

    /// <summary>Override to provide reload functionality.</summary>
    procedure DoReload; virtual;

    class function GetNameForVersion(AVersion: Integer): string; virtual; abstract;

  public
    destructor Destroy; override;

    /// <returns>A unique name used to identify the settings in a json file.</summary>
    class function GetName(AVersion: Integer = -1): string;
    /// <returns>The user-friendly title.</returns>
    class function GetTitle: string; virtual; abstract;
    /// <returns>A user-friendly description.</returns>
    class function GetDescription: string; virtual;
    /// <returns>Wether there is nothing to save.</returns>
    class function SkipSave: Boolean; virtual;

    /// <summary>The relevant root settings object.</summary>
    property Root: TRootSettings read FRoot;

    /// <summary>Sets all properties of the object to their default values.</summary>
    procedure SetDefaults; virtual;
    /// <returns>True, if both settings objects are equal.</returns>
    function Equals(Obj: TObject): Boolean; override;
    /// <summary>Copies all settings.</summary>
    /// <remarks>The root is not changed.</remarks>
    procedure Assign(AFrom: TSettings); virtual;

    /// <summary>Wether any settings changed from the last save.</summary>
    property Modified: Boolean read FModified;

    /// <summary>Reloads any relevant files and reloads all dependents afterwards.</summary>
    procedure Reload; overload;
    /// <summary>Called, after the settings and all dependents are reloaded.</summary>
    function OnReload: TEvent.TAccess;

    /// <summary>Adds dependent settings, to reload after reloading this instance.</summary>
    procedure AddDependent(ADependent: TSettings);

    function GetJVersion: Integer; virtual;
    procedure DefineJStorage(ASerializer: TJSerializer); virtual; abstract;

  end;

  /// <summary>A class for a root object of all settings.</summary>
  /// <remarks>As some applications might need multiple root settings for some reason, it is good practice, to add
  /// a <c>RootSettings</c> property to a class, which gets initialized to the <c>RootSettingsG</c> object.</remarks>
  TRootSettings = class
  public type

    TSubSettings = TClassObjectMap<TSettingsClass, TSettings>;

  public
    class var

      LatestVersion: Integer;

  private
    FSubSettings: TSubSettings;
    FPath: string;
    FJObject: TJObject;
    FVersion: Integer;
    FLock: TCriticalSection;
    FLoadCanceled: Boolean;

    procedure SetPath(const Value: string);

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property LoadCanceled: Boolean read FLoadCanceled;
    
    property Version: Integer read FVersion;

    property Path: string read FPath write SetPath;

    procedure Load;
    procedure Save;

    /// <summary>Reloads any relevant files and reloads all dependents afterwards.</summary>
    procedure Reload(ASettingsClass: TSettingsClass); overload;

    /// <returns>A new or existing instance of the specified class in the relevant root object.</returns>
    function Get<T: TSettings>: T; overload;
    /// <returns>A new or existing instance of the specified class in the relevant root object.</returns>
    function Get(ASettingsClass: TSettingsClass): TSettings; overload;

    function IsLoaded(ASettingsClass: TSettingsClass): Boolean; overload;
    function GetIfLoaded<T: TSettings>(out ASettings: T): Boolean; overload;
    function GetIfLoaded(ASettingsClass: TSettingsClass; out ASettings: TSettings): Boolean; overload;
    procedure Preload(ASettingsClass: TSettingsClass);

  end;

var
  RootSettingsG: TRootSettings;

implementation

{ TSettings }

procedure TSettings.AddDependent(ADependent: TSettings);
begin
  FDependents.Add(ADependent);
end;

procedure TSettings.Assign(AFrom: TSettings);
begin
  if AFrom.ClassType <> Self.ClassType then
    raise ESettings.CreateFmt('Cannot assign settings "%s" to "%s".', [AFrom.GetTitle, GetTitle]);
end;

constructor TSettings.Create(ARoot: TRootSettings);
begin
  FRoot := ARoot;
  FDependents := TDependents.Create;
end;

destructor TSettings.Destroy;
begin
  FDependents.Free;
  inherited;
end;

procedure TSettings.DoReload;
begin
  // nothing by default
end;

function TSettings.Equals(Obj: TObject): Boolean;
begin
  Assert(ClassType = Obj.ClassType);
  Result := True;
end;

class function TSettings.GetDescription: string;
begin
  Result := '-';
end;

function TSettings.GetJVersion: Integer;
begin
  Result := 0;
end;

class function TSettings.GetName(AVersion: Integer): string;
begin
  if AVersion = -1 then
    AVersion := TRootSettings.LatestVersion;
  Result := GetNameForVersion(AVersion);
end;

procedure TSettings.Modify;
begin
  FModified := True;
end;

function TSettings.OnReload: TEvent.TAccess;
begin
  Result := FOnReload.Access;
end;

procedure TSettings.Reload;
var
  DependentSet: TRefSet<TSettings>;
  Dependent: TSettings;

  procedure Add(ASettings: TSettings);
  var
    Dependent: TSettings;
  begin
    for Dependent in ASettings.FDependents do
    begin
      DependentSet.TryAdd(Dependent);
      Add(Dependent);
    end;
  end;

begin
  DoReload;
  Add(Self);
  for Dependent in DependentSet do
    Dependent.Reload;
  FOnReload.Execute(TEventInfo.Create(Self));
end;

procedure TSettings.SetDefaults;
begin
  // nothing by default
end;

class function TSettings.SkipSave: Boolean;
begin
  Result := False;
end;

{ TRootSettings }

constructor TRootSettings.Create;
begin
  FSubSettings := TSubSettings.Create;
  FLock := TCriticalSection.Create;
end;

destructor TRootSettings.Destroy;
var
  SubSettings: TSettings;
begin
  FLock.Enter;
  FLoadCanceled := True;
  for SubSettings in FSubSettings.Values do
  begin
    TMonitor.Enter(SubSettings);
    TMonitor.Exit(SubSettings);
  end;
  FSubSettings.Free;
  FLock.Leave;
  FLock.Leave;
  FLock.Free;
  FJObject.Free;
  inherited;
end;

function TRootSettings.Get(ASettingsClass: TSettingsClass): TSettings;
var
  JSettings: TJObject;
begin
  FLock.Enter;
  if not FSubSettings.Get(ASettingsClass, Result) then
  begin
    Result := ASettingsClass.Create(Self);
    FSubSettings[ASettingsClass] := Result;
    TMonitor.Enter(Result);
    FLock.Leave;
    Result.SetDefaults;
    if not Result.SkipSave and FJObject.Get(Result.GetName(Version), JSettings) then
      TJSerializer.Unserialize(Result, JSettings);
    Result.DoReload;
    TMonitor.Exit(Result);
  end
  else
  begin
    FLock.Leave;
    TMonitor.Enter(Result);
    TMonitor.Exit(Result);
  end;
end;

function TRootSettings.Get<T>: T;
begin
  Result := T(Get(T));
end;

function TRootSettings.GetIfLoaded(ASettingsClass: TSettingsClass; out ASettings: TSettings): Boolean;
begin
  FLock.Enter;
  Result := FSubSettings.Get(ASettingsClass, ASettings);
  FLock.Leave;
end;

function TRootSettings.GetIfLoaded<T>(out ASettings: T): Boolean;
begin
  Result := GetIfLoaded(T, TSettings(ASettings));
end;

function TRootSettings.IsLoaded(ASettingsClass: TSettingsClass): Boolean;
begin
  FLock.Enter;
  Result := FSubSettings.KeyExists(ASettingsClass);
  FLock.Leave;
end;

procedure TRootSettings.Load;
var
  Parser: TJObject.TParser;
begin
  FLock.Enter;
  FSubSettings.Clear;
  if not TFile.Exists(Path) then
    FJObject := TJObject.Create
  else
  begin
    Parser := TJObject.TParser.Create(TFile.ReadAllText(Path), False);
    try
      if not Parser.Success then
      begin
        FJObject := TJObject.Create;
        FVersion := LatestVersion;
      end
      else
      begin
        FJObject := Parser.OwnParseResult;
        FVersion := FJObject['_VERSION'] or 0;
      end;

    finally
      Parser.Free;

    end;
  end;
  FLock.Leave;
end;

procedure TRootSettings.Preload(ASettingsClass: TSettingsClass);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Get(ASettingsClass);
    end
    ).Start;
end;

procedure TRootSettings.Reload(ASettingsClass: TSettingsClass);
var
  Settings: TSettings;
begin
  FLock.Enter;
  if FSubSettings.Get(ASettingsClass, Settings) then
    Settings.Reload
  else
    Get(ASettingsClass);
  FLock.Leave;
end;

procedure TRootSettings.Save;
var
  Setting: TSettings;
begin
  if not ForceDirectories(ExtractFilePath(Path)) then
    raise ESettings.CreateFmt('Could not create settings directory "%s".', [Path]);

  FLock.Enter;
  for Setting in FSubSettings.Values do
  begin
    if Setting.SkipSave then
      Continue;
    FJObject[Setting.GetName] := TJSerializer.Serialize(Setting);
  end;
  FLock.Leave;

  TFile.WriteAllText(Path, FJObject.Format);
end;

procedure TRootSettings.SetPath(const Value: string);
begin
  FPath := Value;
  Load;
end;

end.
