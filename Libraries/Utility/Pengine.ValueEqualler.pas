unit Pengine.ValueEqualler;

interface

type

  TValueEqualler<T> = class abstract
    class function Equal(const AValue1, AValue2: T): Boolean; virtual; abstract;
  end;

implementation

end.

