unit Pengine.WinUtility;

interface

uses
  System.SysUtils,
  System.Win.Registry,

  Winapi.ShlObj,
  Winapi.Windows;

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

function ExtensionExists(AExtension: String): Boolean;

implementation

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
    AOpenWith :=  ParamStr(0);

  if AIconPath.IsEmpty then
    AIconPath := AOpenWith;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Create the extension key
    if not Reg.OpenKey(AExtension, True) then
      Exit(False);
    // Set its default value to the program name
    Reg.WriteString('', AProgramName);
    Reg.CloseKey;

    // Create the program key
    if not Reg.OpenKey(AProgramName, True) then
      Exit(False);
    // Set its default value to the description
    Reg.WriteString('', ADescription);
    Reg.CloseKey;

    // Set up default icon
    if not Reg.OpenKey(AProgramName + '\DefaultIcon', True) then
      Exit(False);
    Reg.WriteString('', AIconPath + ',' + AIconIndex.ToString);
    Reg.CloseKey;

    // Set up link to exe
    if not Reg.OpenKey(AProgramName + '\Shell\Open\Command', True) then
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
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if not Reg.OpenKey(AExtension, False) then
      Exit(False);
    ProgramName := Reg.ReadString('');
    Reg.CloseKey;

    if not Reg.DeleteKey(AExtension) then
      Exit(False);

    if not ProgramName.IsEmpty then
      Reg.DeleteKey(ProgramName);

    if ANotifyWindows then
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

  finally
    Reg.Free;
  end;
  Result := True;
end;

function ExtensionExists(AExtension: String): Boolean;
var
  Reg: TRegistry;
begin
  if AExtension.IsEmpty then
    Exit(False);

  if not AExtension.StartsWith('.') then
    AExtension := '.' + AExtension;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Result := Reg.KeyExists(AExtension);

  finally
    Reg.Free;
  end;
end;

end.
