unit Pengine.CollectionInterfaces;

interface

uses
  System.Classes,
  System.Math,
  System.SysUtils,

  Pengine.Interfaces,
  Pengine.Sorting,
  Pengine.IntMaths;

type

  /// <summary>A generic Interface for an Iterator.</summary>
  IIterator<T> = interface
    ['{01FA5E8D-FB71-4D60-B113-429284B8B8F7}']
    function MoveNext: Boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  // TODO: XmlDoc
  TIterator<T> = class abstract(TInterfacedObject, IIterator<T>)
  public
    function MoveNext: Boolean; virtual; abstract;
    function GetCurrent: T; virtual; abstract;
    property Current: T read GetCurrent;
  end;

  /// <summary>A generic interface for an iteratable type.</summary>
  IIterable<T> = interface
    ['{86380564-F207-4B73-A40D-F10AD12B5B98}']
    function GetEnumerator: IIterator<T>;
    function Count: Integer;
    function CountOptimized: Boolean;
    function Empty: Boolean;
  end;

  /// <summary>A generic interface, for searchable objects.</summary>
  IFindable<T> = interface
    ['{9FE5998D-6A74-4BC9-A06B-129C4E72D313}']
    function Check(AItem: T): Boolean;
  end;

  TFindFuncStatic<T> = function(AItem: T): Boolean;
  TFindFuncRef<T> = reference to function(AItem: T): Boolean;
  TFindFunc<T> = function(AItem: T): Boolean of object;

  /// <summary>A generic interface for comparable objects.</summary>
  IComparable<T> = interface
    ['{577FBD7F-8894-4012-BAD0-42809985D928}']
    function Compare(ALeft, ARight: T): Boolean;
  end;

  TCompareFuncStatic<T> = function(ALeft, ARight: T): Boolean;
  TCompareFuncRef<T> = reference to function(ALeft, ARight: T): Boolean;
  TCompareFunc<T> = function(ALeft, ARight: T): Boolean of object;

  /// <summary>A generic base class for iteratable types. Implements <see cref="Pengine.Collections|IIterable`1"/>.</summary>
  /// <remarks>The Count function should almost definitly be overriden in the derived class.</remarks>
  TIterable<T> = class abstract(TInterfaceBase, IIterable<T>)
  public
    function GetEnumerator: IIterator<T>; virtual; abstract;

    /// <remarks>This function should almost definitely be overwritten.</remarks>
    function Count: Integer; virtual;
    // TODO: XmlDoc
    function CountOptimized: Boolean; virtual;
    function Empty: Boolean; inline;
  end;

implementation

{ TIterable<T> }

function TIterable<T>.Count: Integer;
var
  Item: T;
begin
  Result := 0;
  for Item in Self do
    Inc(Result);
end;

function TIterable<T>.CountOptimized: Boolean;
begin
  Result := False;
end;

function TIterable<T>.Empty: Boolean;
begin
  Result := Count = 0;
end;

end.
