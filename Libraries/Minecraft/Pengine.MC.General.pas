unit Pengine.MC.General;

interface

uses
  System.SysUtils;

const

  IdentChars = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '.', '+', '-'];

  NamespaceChars = IdentChars - ['+']; // wtf

  NamespacePathChars = NamespaceChars + [':', '/'];

  IdentCharsText = 'letters, numbers and "_ . + -"';

  NamespaceCharsText = 'letters, numbers and "_ . -"';

  UsernameMaxLength = 16;

function DblQuoted(AText: string): string;
function DblUnquoted(AText: string): string;

implementation

function DblQuoted(AText: string): string;
var
  C: Char;
begin
  Result := '"';
  for C in AText do
  begin
    if CharInSet(C, ['\', '"']) then
      Result := Result + '\';
    Result := Result + C;
  end;
  Result := Result + '"';
end;

function DblUnquoted(AText: string): string;
var
  I: Integer;
begin
  if not AText.StartsWith('"') or not AText.EndsWith('"') then
    raise EArgumentException.Create('The string is not quoted.');
  Result := '';
  I := 2;
  while I < AText.Length do
  begin
    if AText[I] = '\' then
    begin
      Inc(I);
      if I = AText.Length then
        raise EArgumentException.Create('The string is not terminated.');
    end;
    Result := Result + AText[I];
    Inc(I);
  end;
end;

end.
