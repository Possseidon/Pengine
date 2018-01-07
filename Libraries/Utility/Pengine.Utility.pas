unit Pengine.Utility;

interface

function GetBitCount(num: NativeUInt): Integer;

implementation

function GetBitCount(num: NativeUInt): Integer;
asm
  {$IFDEF CPUX64}
  POPCNT    rax, num
  {$ELSE}
  POPCNT    eax, num
  {$ENDIF}
end;

end.
