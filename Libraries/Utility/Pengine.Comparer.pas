unit Pengine.Comparer;

interface

type

  /// <summary>A generic, abstract class assistant, to compare two values.</summary>
  TComparer<T> = class abstract
  public
    class function Compare(A, B: T): Boolean; virtual; abstract;
  end;

implementation

end.
