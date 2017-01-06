unit MathUtils;

interface

function FMod(X, Min, Max: Single): Single; overload;
function FMod(X, Min, Max: Double): Double; overload;

implementation

uses
  Math;

function FMod(X, Min, Max: Single): Single;
var
  Range: Single;
begin
  Range := Max - Min;
  Result := X - Floor((X - Min) / Range) * Range;
end;

function FMod(X, Min, Max: Double): Double;
var
  Range: Double;
begin
  Range := Max - Min;
  Result := X - Floor((X - Min) / Range) * Range;
end;

end.

