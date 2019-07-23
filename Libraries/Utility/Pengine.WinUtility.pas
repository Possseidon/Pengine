unit Pengine.WinUtility;

interface

function ExpandEnvVars(AText: string): string;

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

end.
