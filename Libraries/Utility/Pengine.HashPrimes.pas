unit Pengine.HashPrimes;

interface

uses
  System.Math,

  Pengine.Utility;

const

  HashPrimeOffset = 3;

  HashPrimes: array [0 .. 28] of Integer = (
    5, 13, 23, 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613, 393241, 786433,
    1572869, 3145739, 6291469, 12582917, 25165843, 50331653, 100663319, 201326611, 402653189, 805306457, 1610612741);

function ChooseHashPrime(ACount: Cardinal): Cardinal;

implementation

function ChooseHashPrime(ACount: Cardinal): Cardinal;
begin
  if ACount = 0 then
    Exit(HashPrimes[0]);
  Result := HashPrimes[EnsureRange(ILog2(ACount) - HashPrimeOffset, 0, High(HashPrimes))];
end;

end.
