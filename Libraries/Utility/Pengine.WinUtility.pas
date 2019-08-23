unit Pengine.WinUtility;

interface

uses
  System.SysUtils,
  System.Win.Registry,

  Winapi.ShlObj,
  Winapi.Windows;

type

  TExtensionPresence = (
    epMissing,
    epExists,
    epDifferentProgram
    );

function ExpandEnvVars(AText: string): string;

function RegisterExtension(
  AExtension: string;
  AProgramName: string;
  ADescription: string;
  AOpenWith: string = '';
  AIconPath: string = '';
  AIconIndex: Integer = 0;
  AParamString: string = '"%1"';
  ANotifyWindows: Boolean = True): Boolean;

function UnregisterExtension(AExtension: String; ANotifyWindows: Boolean = True): Boolean;

function ExtensionRegistered(AExtension: String): TExtensionPresence;

function GetFullExePath: string;

implementation

function GetFullExePath: string;
var
  PathSize: Integer;
begin
  PathSize := 130;
  repeat
    SetLength(Result, PathSize * 2);
    PathSize := GetModuleFileName(0, @Result[1], Length(Result));
    // if PathSize is e.g. 42 (with space for null) and that is enough
    // then it returns 41 (length without null)
    // otherwise returns the given length (with space for null)
  until PathSize < Length(Result);
  SetLength(Result, PathSize);
end;

function ExpandEnvVars(AText: string): string;
var
  Len: Cardinal;
begin
  if AText.IsEmpty then
    Exit('');
  Len := ExpandEnvironmentStrings(PChar(AText), nil, 0);
  SetLength(Result, Len);
  ExpandEnvironmentStrings(PChar(AText), PChar(Result), Len);
  Result := TrimRight(Result);
end;

function RegisterExtension(
  AExtension: string;
  AProgramName: string;
  ADescription: string;
  AOpenWith: string;
  AIconPath: string;
  AIconIndex: Integer;
  AParamString: string;
  ANotifyWindows: Boolean): Boolean;
var
  Reg: TRegistry;
begin
  if AExtension.IsEmpty then
    Exit(False);

  if not AExtension.StartsWith('.') then
    AExtension := '.' + AExtension;

  if AOpenWith.IsEmpty then
    AOpenWith := GetFullExePath;

  if AIconPath.IsEmpty then
    AIconPath := AOpenWith;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Create the extension key
    if not Reg.OpenKey('Software\Classes\' + AExtension, True) then
      Exit(False);
    // Set its default value to the program name
    Reg.WriteString('', AProgramName);
    Reg.CloseKey;

    // Create the program key
    if not Reg.OpenKey('Software\Classes\' + AProgramName, True) then
      Exit(False);
    // Set its default value to the description
    Reg.WriteString('', ADescription);
    Reg.CloseKey;

    // Set up default icon
    if not Reg.OpenKey('Software\Classes\' + AProgramName + '\DefaultIcon', True) then
      Exit(False);
    Reg.WriteString('', AIconPath + ',' + AIconIndex.ToString);
    Reg.CloseKey;

    // Set up link to exe
    if not Reg.OpenKey('Software\Classes\' + AProgramName + '\Shell\Open\Command', True) then
      Exit(False);
    Reg.WriteExpandString('', Format('"%s" %s', [AOpenWith, AParamString]));
    Reg.CloseKey;

    if ANotifyWindows then
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

  finally
    Reg.Free;
  end;
  Result := True;
end;

function UnregisterExtension(AExtension: String; ANotifyWindows: Boolean): Boolean;
var
  Reg: TRegistry;
  ProgramName: string;
begin
  if AExtension.IsEmpty then
    Exit(False);

  if not AExtension.StartsWith('.') then
    AExtension := '.' + AExtension;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if not Reg.OpenKey('Software\Classes\' + AExtension, False) then
      Exit(False);
    ProgramName := Reg.ReadString('');
    Reg.CloseKey;

    if not Reg.DeleteKey('Software\Classes\' + AExtension) then
      Exit(False);

    if not ProgramName.IsEmpty then
      Reg.DeleteKey('Software\Classes\' + ProgramName);

    if ANotifyWindows then
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

  finally
    Reg.Free;
  end;
  Result := True;
end;

function ExtensionRegistered(AExtension: String): TExtensionPresence;
var
  Reg: TRegistry;
  ProgramName: string;
begin
  if AExtension.IsEmpty then
    Exit(epMissing);

  if not AExtension.StartsWith('.') then
    AExtension := '.' + AExtension;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if not Reg.OpenKey('Software\Classes\' + AExtension, False) then
      Exit(epMissing);
    ProgramName := Reg.ReadString('');
    Reg.CloseKey;

    if not Reg.OpenKey('Software\Classes\' + ProgramName + '\Shell\Open\Command', False) then
      Exit(epMissing);
    if Reg.ReadString('').StartsWith('"' + GetFullExePath + '"') then
      Exit(epExists);
    Result := epDifferentProgram;

  finally
    Reg.Free;
  end;
end;

end.
