unit IntegerMaths;

interface

uses
  Math, SysUtils, Types;

type

  PIntBounds1 = ^TIntBounds1;
  PIntBounds2 = ^TIntBounds2;
  PIntBounds3 = ^TIntBounds3;

  PIntVector2 = ^TIntVector2;
  PIntVector3 = ^TIntVector3;

  { TIntVector2 }
  /// <summary>A two Component Vector of Type Integer</summary>
  TIntVector2 = record
  public
    /// <summary>The X-Component of the IntVector</summary>
    X: Integer;
    /// <summary>The Y-Component of the IntVector</summary>
    Y: Integer;

    /// <summary>Creates a new TIntVector2 object with the specified Components</summary>
    constructor Create(X, Y: Integer); overload;
    /// <summary>Creates a new TIntVector2 object with both Components being the same, given Value</summary>
    constructor Create(V: Integer); overload;

    class operator Implicit(V: Integer): TIntVector2; inline;
    class operator Implicit(A: TIntVector2): TPoint;

    {$REGION 'All versions of rearrangement TIntVector2'}

    function XX: TIntVector2;
    function XY: TIntVector2;
    function YX: TIntVector2;
    function YY: TIntVector2;

    {$ENDREGION}

    class operator Add(const A, B: TIntVector2): TIntVector2;
    class operator Subtract(const A, B: TIntVector2): TIntVector2;
    class operator Multiply(const A, B: TIntVector2): TIntVector2;
    class operator IntDivide(const A, B: TIntVector2): TIntVector2;

    class operator Positive(const A: TIntVector2): TIntVector2; inline;
    class operator Negative(const A: TIntVector2): TIntVector2;

    class operator Equal(const A, B: TIntVector2): Boolean;
    class operator NotEqual(const A, B: TIntVector2): Boolean;
    class operator LessThan(const A, B: TIntVector2): Boolean;
    class operator LessThanOrEqual(const A, B: TIntVector2): Boolean;
    class operator GreaterThan(const A, B: TIntVector2): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntVector2): Boolean;

    /// <summary>Returns a string representative in the form: [X|Y]</summary>
    /// <remarks>Also implicitly convertible</remarks>
    function ToString: string; inline;
    class operator Implicit(A: TIntVector2): string; inline;

  end;

  { TIntVector3 }
  /// <summary>A three Component Vector of Type Integer</summary>
  TIntVector3 = record
  public
    /// <summary>The X-Component of the IntVector</summary>
    X: Integer;
    /// <summary>The Y-Component of the IntVector</summary>
    Y: Integer;
    /// <summary>The Z-Component of the IntVector</summary>
    Z: Integer;

    /// <summary>Creates a new TIntVector3 object with the specified Components</summary>
    constructor Create(X, Y, Z: Integer); overload;
    /// <summary>Creates a new TIntVector3 object with all Components being the same, given Value</summary>
    constructor Create(V: Integer); overload;

    class operator Implicit(V: Integer): TIntVector3; inline;

    {$REGION 'All versions of rearrangement TIntVector2'}

    function XX: TIntVector2;
    function XY: TIntVector2;
    function XZ: TIntVector2;
    function YX: TIntVector2;
    function YY: TIntVector2;
    function YZ: TIntVector2;
    function ZX: TIntVector2;
    function ZY: TIntVector2;
    function ZZ: TIntVector2;

    {$ENDREGION}
    {$REGION 'All versions of rearrangement TIntVector3'}

    function XXX: TIntVector3;
    function XXY: TIntVector3;
    function XXZ: TIntVector3;
    function XYX: TIntVector3;
    function XYY: TIntVector3;
    function XYZ: TIntVector3;
    function XZX: TIntVector3;
    function XZY: TIntVector3;
    function XZZ: TIntVector3;
    function YXX: TIntVector3;
    function YXY: TIntVector3;
    function YXZ: TIntVector3;
    function YYX: TIntVector3;
    function YYY: TIntVector3;
    function YYZ: TIntVector3;
    function YZX: TIntVector3;
    function YZY: TIntVector3;
    function YZZ: TIntVector3;
    function ZXX: TIntVector3;
    function ZXY: TIntVector3;
    function ZXZ: TIntVector3;
    function ZYX: TIntVector3;
    function ZYY: TIntVector3;
    function ZYZ: TIntVector3;
    function ZZX: TIntVector3;
    function ZZY: TIntVector3;
    function ZZZ: TIntVector3;

    {$ENDREGION}

    class operator Add(const A, B: TIntVector3): TIntVector3;
    class operator Subtract(const A, B: TIntVector3): TIntVector3;
    class operator Multiply(const A, B: TIntVector3): TIntVector3;
    class operator IntDivide(const A, B: TIntVector3): TIntVector3;

    class operator Positive(const A: TIntVector3): TIntVector3; inline;
    class operator Negative(const A: TIntVector3): TIntVector3;

    class operator Equal(const A, B: TIntVector3): Boolean;
    class operator NotEqual(const A, B: TIntVector3): Boolean;
    class operator LessThan(const A, B: TIntVector3): Boolean;
    class operator LessThanOrEqual(const A, B: TIntVector3): Boolean;
    class operator GreaterThan(const A, B: TIntVector3): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntVector3): Boolean;

    /// <summary>Returns a string representative in the form: [X|Y|Z]</summary>
    /// <remarks>Also implicitly convertible</remarks>
    function ToString: string; inline;
    class operator Implicit(A: TIntVector3): string; inline;

  end;

  { TIntBounds1 }
  /// <summary>
  /// Represents 1-Dimensional bounds using two Integers
  /// <para>Shorthand constructor using:</para>
  /// <para><see cref="IntegerMaths|Range1"/> / <see cref="IntegerMaths|Range1X"/></para>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized (C1 &lt;= C2)<para/>
  /// It is possible to iterate over all positions, which will include both ends
  /// </remarks>
  TIntBounds1 = record
  public type

    TCorners = array [0 .. 1] of Integer;

    { TIterator }

    TIterator = class
    private
      FCurrent: Integer;
      FEnd: Integer;
    public
      constructor Create(const ABounds: TIntBounds1);

      function MoveNext: Boolean;
      property Current: Integer read FCurrent;
    end;

  private
    function GetLength: Integer; inline;

  public
    /// <summary>The (usually) lower Value of the bounds</summary>
    C1: Integer;
    /// <summary>The (usually) higher Value of the bounds</summary>
    C2: Integer;

    /// <summary>Equivalent to C1</summary>
    property Low: Integer read C1 write C1;
    /// <summary>Equivalent to C2</summary>
    property High: Integer read C2 write C2;

    /// <summary>Creates a new TIntBounds object with the specified Values</summary>
    constructor Create(AC1, AC2: Integer); overload;
    /// <summary>Creates a new TIntBounds object with both bounds laying on the same, given Value</summary>
    constructor Create(A: Integer); overload;

    class operator Implicit(A: Integer): TIntBounds1; inline;

    /// <remarks>Will give a negative length for non-normalized bounds</remarks>
    property Length: Integer read GetLength;

    /// <summary>Returns the given Bounds clamped to be inside of the calling bounds</summary>
    function EnsureRange(ARange: TIntBounds1): TIntBounds1; overload;
    /// <summary>Returns the given Value being clamped in/on the bounds</summary>
    function EnsureRange(AValue: Integer): Integer; overload;

    /// <returns>Returns true, if C1 &lt; C2</returns>
    function Normalized: Boolean; inline;
    /// <summary>Swaps C1 and C2 if the bounds are not normalized</summary>
    function Normalize: TIntBounds1;

    /// <summary>Increments C1 by Amount and decrements C2 by Amount</summary>
    function Inset(AAmount: Integer): TIntBounds1;
    /// <summary>Decrements C1 by Amount and increments C2 by Amount</summary>
    function Outset(AAmount: Integer): TIntBounds1;

    class operator Add(const A, B: TIntBounds1): TIntBounds1;
    class operator Subtract(const A, B: TIntBounds1): TIntBounds1;
    class operator Multiply(const A, B: TIntBounds1): TIntBounds1;
    class operator IntDivide(const A, B: TIntBounds1): TIntBounds1;

    // inclusive on both sides
    class operator in(const A, B: TIntBounds1): Boolean;
    // inclusive on C1 and exclusive on C2
    class operator in(A: Integer; const B: TIntBounds1): Boolean;

    class operator Equal(const A, B: TIntBounds1): Boolean;
    class operator NotEqual(const A, B: TIntBounds1): Boolean;
    class operator GreaterThan(const A, B: TIntBounds1): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntBounds1): Boolean;
    class operator LessThan(const A, B: TIntBounds1): Boolean;
    class operator LessThanOrEqual(const A, B: TIntBounds1): Boolean;

    /// <summary>Returns a string representative in the form: &lt;C1~C2&gt;</summary>
    /// <remarks>Also implicitly convertible</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TIntBounds1): string; inline;

    function GetEnumerator: TIterator;

  end;

  { TIntBounds2 }
  /// <summary>
  /// Represents 2-Dimensional bounds using two TIntVector2
  /// <para>Shorthand constructor using:</para>
  /// <para><see cref="IntegerMaths|Range2"/> / <see cref="IntegerMaths|Range2X"/></para>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized (C1 &lt;= C2)<para/>
  /// It is possible to iterate over all positions, which will include both ends
  /// </remarks>
  TIntBounds2 = record
  public type

    TCorners = array [0 .. 3] of TIntVector2;

    { TIterator }

    TIterator = class
    private
      FCurrent: TIntVector2;
      FBounds: PIntBounds2;
    public
      constructor Create(const ABounds: TIntBounds2);

      function MoveNext: Boolean;
      property Current: TIntVector2 read FCurrent;
    end;

  private
    function GetLineX: TIntBounds1; inline;
    function GetLineY: TIntBounds1; inline;

    procedure SetLineX(const Value: TIntBounds1); inline;
    procedure SetLineY(const Value: TIntBounds1); inline;

    function GetSize: TIntVector2; inline;
    function GetArea: Integer; inline;
    function GetWidth: Integer; inline;
    function GetHeight: Integer; inline;

  public
    /// <summary>The (usually) lower Values of the bounds</summary>
    C1: TIntVector2;
    /// <summary>The (usually) higher Values of the bounds</summary>
    C2: TIntVector2;

    /// <summary>Equivalent to C1</summary>
    property Low: TIntVector2 read C1 write C1;
    /// <summary>Equivalent to C2</summary>
    property High: TIntVector2 read C2 write C2;

    /// <summary>Creates a new TIntBounds object with the specified Points</summary>
    constructor Create(AC1, AC2: TIntVector2); overload;
    /// <summary>Creates a new TIntBounds object with both bounds laying on the same, given Point</summary>
    constructor Create(A: TIntVector2); overload;

    class operator Implicit(A: Integer): TIntBounds2; inline;
    class operator Implicit(A: TIntVector2): TIntBounds2; inline;

    /// <remarks>Will give a negative values for non-normalized bounds</remarks>
    property Size: TIntVector2 read GetSize;
    /// <summary>Calculates the Area in between the Bounds</summary>
    property Area: Integer read GetArea;

    /// <summary>Resembles the X Components of the bounds as a TIntBounds1</summary>
    /// <remarks>WARNING! You cannot change the result directly, as it creates a copy of the values</remarks>
    property LineX: TIntBounds1 read GetLineX write SetLineX;
    /// <summary>Resembles the Y Components of the bounds as a TIntBounds1</summary>
    /// <remarks>WARNING! You cannot change the result directly, as it creates a copy of the values</remarks>
    property LineY: TIntBounds1 read GetLineY write SetLineY;

    /// <returns>Returns the horizontal length of the Bounds</returns>
    /// <remarks>Will give a negative length for non-normalized bounds</remarks>
    property Width: Integer read GetWidth;
    /// <returns>Returns the vertical length of the Bounds</returns>
    /// <remarks>Will give a negative length for non-normalized bounds</remarks>
    property Height: Integer read GetHeight;

    /// <summary>Returns the given Bounds clamped to be inside of the calling bounds</summary>
    function EnsureRange(ARange: TIntBounds2): TIntBounds2; overload;
    /// <summary>Returns the given Point being clamped in/on the bounds</summary>
    function EnsureRange(AValue: TIntVector2): TIntVector2; overload;

    /// <returns>Returns true, if C1 &lt; C2</returns>
    function Normalized: Boolean; inline;
    /// <summary>Swaps all unnormalized components of C1 and C2</summary>
    function Normalize: TIntBounds2;

    /// <summary>Increments C1 by Amount and decrements C2 by Amount</summary>
    function Inset(AAmount: TIntVector2): TIntBounds2;
    /// <summary>Decrements C1 by Amount and increments C2 by Amount</summary>
    function Outset(AAmount: TIntVector2): TIntBounds2;

    class operator Add(const A, B: TIntBounds2): TIntBounds2;
    class operator Subtract(const A, B: TIntBounds2): TIntBounds2;
    class operator Multiply(const A, B: TIntBounds2): TIntBounds2;
    class operator IntDivide(const A, B: TIntBounds2): TIntBounds2;

    // inclusive on both sides
    class operator in(const A, B: TIntBounds2): Boolean;
    // inclusive on C1 and exclusive on C2
    class operator in(A: TIntVector2; const B: TIntBounds2): Boolean;

    class operator Equal(const A, B: TIntBounds2): Boolean;
    class operator NotEqual(const A, B: TIntBounds2): Boolean;
    class operator GreaterThan(const A, B: TIntBounds2): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntBounds2): Boolean;
    class operator LessThan(const A, B: TIntBounds2): Boolean;
    class operator LessThanOrEqual(const A, B: TIntBounds2): Boolean;

    /// <summary>Returns a string representative in the form: &lt;C1~C2&gt;</summary>
    /// <remarks>Also implicitly convertible</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TIntBounds2): string; inline;

    function GetEnumerator: TIterator;

  end;

  { TIntBounds3 }
  /// <summary>
  /// Represents 3-Dimensional bounds using two TIntVector3
  /// <para>Shorthand constructor using:</para>
  /// <para><see cref="IntegerMaths|Range3"/> / <see cref="IntegerMaths|Range3X"/></para>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized (C1 &lt;= C2)<para/>
  /// It is possible to iterate over all positions, which will include both ends
  /// </remarks>
  TIntBounds3 = record
  public type

    TCorners = array [0 .. 7] of TIntVector3;

    { TIterator }

    TIterator = class
    private
      FCurrent: TIntVector3;
      FBounds: PIntBounds3;
    public
      constructor Create(const ABounds: TIntBounds3);

      function MoveNext: Boolean;
      property Current: TIntVector3 read FCurrent;
    end;

  private
    function GetLineX: TIntBounds1; inline;
    function GetLineY: TIntBounds1; inline;
    function GetLineZ: TIntBounds1; inline;

    procedure SetLineX(const Value: TIntBounds1); inline;
    procedure SetLineY(const Value: TIntBounds1); inline;
    procedure SetLineZ(const Value: TIntBounds1); inline;

    function GetPlaneXY: TIntBounds2; inline;
    function GetPlaneYZ: TIntBounds2; inline;
    function GetPlaneZX: TIntBounds2; inline;
    function GetPlaneYX: TIntBounds2; inline;
    function GetPlaneZY: TIntBounds2; inline;
    function GetPlaneXZ: TIntBounds2; inline;

    procedure SetPlaneXY(const Value: TIntBounds2); inline;
    procedure SetPlaneYZ(const Value: TIntBounds2); inline;
    procedure SetPlaneZX(const Value: TIntBounds2); inline;
    procedure SetPlaneYX(const Value: TIntBounds2); inline;
    procedure SetPlaneZY(const Value: TIntBounds2); inline;
    procedure SetPlaneXZ(const Value: TIntBounds2); inline;

    function GetSize: TIntVector3; inline;
    function GetVolume: Integer; inline;
    function GetWidth: Integer; inline;
    function GetHeight: Integer; inline;
    function GetDepth: Integer; inline;

  public
    /// <summary>The (usually) lower Values of the bounds</summary>
    C1: TIntVector3;
    /// <summary>The (usually) higher Values of the bounds</summary>
    C2: TIntVector3;

    /// <summary>Equivalent to C1</summary>
    property Low: TIntVector3 read C1 write C1;
    /// <summary>Equivalent to C2</summary>
    property High: TIntVector3 read C2 write C2;

    /// <summary>Creates a new TIntBounds object with the specified Points</summary>
    constructor Create(AC1, AC2: TIntVector3); overload;
    /// <summary>Creates a new TIntBounds object with both bounds laying on the same, given Point</summary>
    constructor Create(A: TIntVector3); overload;

    class operator Implicit(A: TIntVector3): TIntBounds3; inline;

    /// <remarks>Will give a negative values for non-normalized bounds</remarks>
    property Size: TIntVector3 read GetSize;
    /// <summary>Calculates the Volume inside of the bounds</summary>
    property Volume: Integer read GetVolume;

    /// <summary>Resembles the X Components of the bounds as a TIntBounds1</summary>
    /// <remarks>WARNING! You cannot change the result directly, as it creates a copy of the values</remarks>
    property LineX: TIntBounds1 read GetLineX write SetLineX;
    /// <summary>Resembles the Y Components of the bounds as a TIntBounds1</summary>
    /// <remarks>WARNING! You cannot change the result directly, as it creates a copy of the values</remarks>
    property LineY: TIntBounds1 read GetLineY write SetLineY;
    /// <summary>Resembles the Z Components of the bounds as a TIntBounds1</summary>
    /// <remarks>WARNING! You cannot change the result directly, as it creates a copy of the values</remarks>
    property LineZ: TIntBounds1 read GetLineZ write SetLineZ;

    property PlaneXY: TIntBounds2 read GetPlaneXY write SetPlaneXY;
    property PlaneYZ: TIntBounds2 read GetPlaneYZ write SetPlaneYZ;
    property PlaneZX: TIntBounds2 read GetPlaneZX write SetPlaneZX;
    property PlaneYX: TIntBounds2 read GetPlaneYX write SetPlaneYX;
    property PlaneZY: TIntBounds2 read GetPlaneZY write SetPlaneZY;
    property PlaneXZ: TIntBounds2 read GetPlaneXZ write SetPlaneXZ;

    /// <returns>Returns the horizontal length of the Bounds</returns>
    /// <remarks>Will give a negative length for non-normalized bounds</remarks>
    property Width: Integer read GetWidth;
    /// <returns>Returns the vertical length of the Bounds</returns>
    /// <remarks>Will give a negative length for non-normalized bounds</remarks>
    property Height: Integer read GetHeight;
    /// <returns>Returns the Z-directed length of the Bounds</returns>
    /// <remarks>Will give a negative length for non-normalized bounds</remarks>
    property Depth: Integer read GetDepth;

    /// <summary>Returns the given Bounds clamped to be inside of the calling bounds</summary>
    function EnsureRange(ARange: TIntBounds3): TIntBounds3; overload;
    /// <summary>Returns the given Point being clamped in/on the bounds</summary>
    function EnsureRange(AValue: TIntVector3): TIntVector3; overload;

    /// <returns>Returns true, if C1 &lt; C2</returns>
    function Normalized: Boolean; inline;
    /// <summary>Swaps all unnormalized components of C1 and C2</summary>
    function Normalize: TIntBounds3;

    /// <summary>Increments C1 by Amount and decrements C2 by Amount</summary>
    function Inset(AAmount: TIntVector3): TIntBounds3;
    /// <summary>Decrements C1 by Amount and increments C2 by Amount</summary>
    function Outset(AAmount: TIntVector3): TIntBounds3;

    class operator Add(const A, B: TIntBounds3): TIntBounds3;
    class operator Subtract(const A, B: TIntBounds3): TIntBounds3;
    class operator Multiply(const A, B: TIntBounds3): TIntBounds3;
    class operator IntDivide(const A, B: TIntBounds3): TIntBounds3;

    // inclusive on both sides
    class operator in(const A, B: TIntBounds3): Boolean;
    // inclusive on C1 and exclusive on C2
    class operator in(A: TIntVector3; const B: TIntBounds3): Boolean;

    class operator Equal(const A, B: TIntBounds3): Boolean;
    class operator NotEqual(const A, B: TIntBounds3): Boolean;
    class operator GreaterThan(const A, B: TIntBounds3): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntBounds3): Boolean;
    class operator LessThan(const A, B: TIntBounds3): Boolean;
    class operator LessThanOrEqual(const A, B: TIntBounds3): Boolean;

    /// <summary>Returns a string representative in the form: &lt;C1~C2&gt;</summary>
    /// <remarks>Also implicitly convertible</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TIntBounds3): string; inline;

    function GetEnumerator: TIterator;

  end;

// Shorthand Constructors

/// <summary>Shorthand constructor for TIntVector2(X, Y)</summary>
function IVec2(X, Y: Integer): TIntVector2; overload; inline;
/// <summary>Shorthand constructor for TIntVector2(V, V)</summary>
function IVec2(V: Integer): TIntVector2; overload; inline;
/// <summary>Shorthand constructor for TIntVector3(X, Y, Z)</summary>
function IVec3(X, Y, Z: Integer): TIntVector3; overload; inline;
/// <summary>Shorthand constructor for TIntVector3(V, V, V)</summary>
function IVec3(V: Integer): TIntVector3; overload; inline;

/// <summary>Shorthand constructor for TIntBounds1(A, B)</summary>
function Range1(A, B: Integer): TIntBounds1; overload; inline;
/// <summary>Shorthand constructor for TIntBounds1(0, A)</summary>
function Range1(A: Integer): TIntBounds1; overload; inline;

/// <summary>Shorthand constructor for TIntBounds2(A, B)</summary>
function Range2(A, B: TIntVector2): TIntBounds2; overload; inline;
/// <summary>Shorthand constructor for TIntBounds2(0, A)</summary>
function Range2(A: TIntVector2): TIntBounds2; overload; inline;

/// <summary>Shorthand constructor for TIntBounds3(A, B)</summary>
function Range3(A, B: TIntVector3): TIntBounds3; overload; inline;
/// <summary>Shorthand constructor for TIntBounds2(0, A)</summary>
function Range3(A: TIntVector3): TIntBounds3; overload; inline;

implementation

{ TIntVector2 }

constructor TIntVector2.Create(X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

constructor TIntVector2.Create(V: Integer);
begin
  X := V;
  Y := V;
end;

class operator TIntVector2.Implicit(V: Integer): TIntVector2;
begin
  Result.X := V;
  Result.Y := V;
end;

{$REGION 'All version of rearrangement TIntVector2'}

function TIntVector2.XX: TIntVector2;
begin
  Result.X := X;
  Result.Y := X;
end;

function TIntVector2.XY: TIntVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TIntVector2.YX: TIntVector2;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TIntVector2.YY: TIntVector2;
begin
  Result.X := Y;
  Result.Y := Y;
end;

{$ENDREGION}

class operator TIntVector2.Add(const A, B: TIntVector2): TIntVector2;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TIntVector2.Subtract(const A, B: TIntVector2): TIntVector2;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TIntVector2.Multiply(const A, B: TIntVector2): TIntVector2;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

class operator TIntVector2.IntDivide(const A, B: TIntVector2): TIntVector2;
begin
  Result.X := A.X div B.X;
  Result.Y := A.Y div B.Y;
end;

class operator TIntVector2.Positive(const A: TIntVector2): TIntVector2;
begin
  Result := A;
end;

class operator TIntVector2.Negative(const A: TIntVector2): TIntVector2;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TIntVector2.Equal(const A, B: TIntVector2): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TIntVector2.NotEqual(const A, B: TIntVector2): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y);
end;

class operator TIntVector2.LessThan(const A, B: TIntVector2): Boolean;
begin
  Result := (A.X < B.X) and (A.Y < B.Y);
end;

class operator TIntVector2.LessThanOrEqual(const A, B: TIntVector2): Boolean;
begin
  Result := (A.X <= B.X) and (A.Y <= B.Y);
end;

class operator TIntVector2.GreaterThan(const A, B: TIntVector2): Boolean;
begin
  Result := (A.X > B.X) and (A.Y > B.Y);
end;

class operator TIntVector2.GreaterThanOrEqual(const A, B: TIntVector2): Boolean;
begin
  Result := (A.X >= B.X) and (A.Y >= B.Y);
end;

function TIntVector2.ToString: string;
begin
  Result := Format('[%d|%d]', [X, Y]);
end;

class operator TIntVector2.Implicit(A: TIntVector2): string;
begin
  Result := A.ToString;
end;

class operator TIntVector2.Implicit(A: TIntVector2): TPoint;
begin
  Result.X := A.X;
  Result.Y := A.Y;
end;

{ TIntVector3 }

constructor TIntVector3.Create(X, Y, Z: Integer);
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

constructor TIntVector3.Create(V: Integer);
begin
  X := V;
  Y := V;
  Z := V;
end;

class operator TIntVector3.Implicit(V: Integer): TIntVector3;
begin
  Result.X := V;
  Result.Y := V;
  Result.Z := V;
end;

{$REGION 'All version of rearrangement TIntVector2'}

function TIntVector3.XX: TIntVector2;
begin
  Result.X := X;
  Result.Y := X;
end;

function TIntVector3.XY: TIntVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TIntVector3.XZ: TIntVector2;
begin
  Result.X := X;
  Result.Y := Z;
end;

function TIntVector3.YX: TIntVector2;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TIntVector3.YY: TIntVector2;
begin
  Result.X := Y;
  Result.Y := Y;
end;

function TIntVector3.YZ: TIntVector2;
begin
  Result.X := Y;
  Result.Y := Z;
end;

function TIntVector3.ZX: TIntVector2;
begin
  Result.X := Z;
  Result.Y := X;
end;

function TIntVector3.ZY: TIntVector2;
begin
  Result.X := Z;
  Result.Y := Y;
end;

function TIntVector3.ZZ: TIntVector2;
begin
  Result.X := Z;
  Result.Y := Z;
end;

{$ENDREGION}
{$REGION 'All version of rearrangement TIntVector3'}

function TIntVector3.XXX: TIntVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := X;
end;

function TIntVector3.XXY: TIntVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := Y;
end;

function TIntVector3.XXZ: TIntVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := Z;
end;

function TIntVector3.XYX: TIntVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := X;
end;

function TIntVector3.XYY: TIntVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Y;
end;

function TIntVector3.XYZ: TIntVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TIntVector3.XZX: TIntVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := X;
end;

function TIntVector3.XZY: TIntVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := Y;
end;

function TIntVector3.XZZ: TIntVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := Z;
end;

function TIntVector3.YXX: TIntVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := X;
end;

function TIntVector3.YXY: TIntVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := Y;
end;

function TIntVector3.YXZ: TIntVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := Z;
end;

function TIntVector3.YYX: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := X;
end;

function TIntVector3.YYY: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := Y;
end;

function TIntVector3.YYZ: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := Z;
end;

function TIntVector3.YZX: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := X;
end;

function TIntVector3.YZY: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := Y;
end;

function TIntVector3.YZZ: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := Z;
end;

function TIntVector3.ZXX: TIntVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := X;
end;

function TIntVector3.ZXY: TIntVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := Y;
end;

function TIntVector3.ZXZ: TIntVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := Z;
end;

function TIntVector3.ZYX: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := X;
end;

function TIntVector3.ZYY: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := Y;
end;

function TIntVector3.ZYZ: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := Z;
end;

function TIntVector3.ZZX: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := X;
end;

function TIntVector3.ZZY: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := Y;
end;

function TIntVector3.ZZZ: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := Z;
end;

{$ENDREGION}

class operator TIntVector3.Add(const A, B: TIntVector3): TIntVector3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TIntVector3.Subtract(const A, B: TIntVector3): TIntVector3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TIntVector3.Multiply(const A, B: TIntVector3): TIntVector3;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
end;

class operator TIntVector3.IntDivide(const A, B: TIntVector3): TIntVector3;
begin
  Result.X := A.X div B.X;
  Result.Y := A.Y div B.Y;
  Result.Z := A.Z div B.Z;
end;

class operator TIntVector3.Positive(const A: TIntVector3): TIntVector3;
begin
  Result := A;
end;

class operator TIntVector3.Negative(const A: TIntVector3): TIntVector3;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
end;

class operator TIntVector3.Equal(const A, B: TIntVector3): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
end;

class operator TIntVector3.NotEqual(const A, B: TIntVector3): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z);
end;

class operator TIntVector3.LessThan(const A, B: TIntVector3): Boolean;
begin
  Result := (A.X < B.X) and (A.Y < B.Y) and (A.Z < B.Z);
end;

class operator TIntVector3.LessThanOrEqual(const A, B: TIntVector3): Boolean;
begin
  Result := (A.X <= B.X) and (A.Y <= B.Y) and (A.Z <= B.Z);
end;

class operator TIntVector3.GreaterThan(const A, B: TIntVector3): Boolean;
begin
  Result := (A.X > B.X) and (A.Y > B.Y) and (A.Z > B.Z);
end;

class operator TIntVector3.GreaterThanOrEqual(const A, B: TIntVector3): Boolean;
begin
  Result := (A.X >= B.X) and (A.Y >= B.Y) and (A.Z >= B.Z);
end;

function TIntVector3.ToString: string;
begin
  Result := Format('[%d|%d|%d]', [X, Y, Z]);
end;

class operator TIntVector3.Implicit(A: TIntVector3): string;
begin
  Result := A.ToString;
end;

{ TIntBounds1 }

constructor TIntBounds1.Create(AC1, AC2: Integer);
begin
  C1 := AC1;
  C2 := AC2;
end;

constructor TIntBounds1.Create(A: Integer);
begin
  C1 := A;
  C2 := A;
end;

class operator TIntBounds1.Implicit(A: Integer): TIntBounds1;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

function TIntBounds1.GetLength: Integer;
begin
  Result := C2 - C1;
end;

function TIntBounds1.EnsureRange(ARange: TIntBounds1): TIntBounds1;
begin
  Result.C1 := Max(C1, ARange.C1);
  Result.C2 := Min(C2, ARange.C2);
end;

function TIntBounds1.EnsureRange(AValue: Integer): Integer;
begin
  Result := Math.EnsureRange(AValue, C1, C2);
end;

function TIntBounds1.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TIntBounds1.Normalize: TIntBounds1;
begin
  if not Normalized then
  begin
    Result.C1 := C2;
    Result.C2 := C1;
  end
  else
    Result := Self;
end;

function TIntBounds1.Inset(AAmount: Integer): TIntBounds1;
begin
  Result.C1 := C1 + AAmount;
  Result.C2 := C2 - AAmount;
end;

function TIntBounds1.Outset(AAmount: Integer): TIntBounds1;
begin
  Result.C1 := C1 - AAmount;
  Result.C2 := C2 + AAmount;
end;

class operator TIntBounds1.Add(const A, B: TIntBounds1): TIntBounds1;
begin
  Result.C1 := A.C1 + B.C1;
  Result.C2 := A.C2 + B.C2;
end;

class operator TIntBounds1.Subtract(const A, B: TIntBounds1): TIntBounds1;
begin
  Result.C1 := A.C1 - B.C1;
  Result.C2 := A.C2 - B.C2;
end;

class operator TIntBounds1.Multiply(const A, B: TIntBounds1): TIntBounds1;
begin
  Result.C1 := A.C1 * B.C1;
  Result.C2 := A.C2 * B.C2;
end;

class operator TIntBounds1.IntDivide(const A, B: TIntBounds1): TIntBounds1;
begin
  Result.C1 := A.C1 div B.C1;
  Result.C2 := A.C2 div B.C2;
end;

class operator TIntBounds1.in(const A, B: TIntBounds1): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TIntBounds1.in(A: Integer; const B: TIntBounds1): Boolean;
begin
  Result := (A >= B.C1) and (A < B.C2);
end;

class operator TIntBounds1.Equal(const A, B: TIntBounds1): Boolean;
begin
  Result := (A.C1 = B.C1) and (A.C2 = B.C2);
end;

class operator TIntBounds1.NotEqual(const A, B: TIntBounds1): Boolean;
begin
  Result := (A.C1 <> B.C1) or (A.C2 <> B.C2);
end;

class operator TIntBounds1.GreaterThan(const A, B: TIntBounds1): Boolean;
begin
  Result := A.C1 > B.C2;
end;

class operator TIntBounds1.GreaterThanOrEqual(const A, B: TIntBounds1): Boolean;
begin
  Result := A.C1 >= B.C2;
end;

class operator TIntBounds1.LessThan(const A, B: TIntBounds1): Boolean;
begin
  Result := A.C2 < B.C1;
end;
class operator TIntBounds1.LessThanOrEqual(const A, B: TIntBounds1): Boolean;
begin
  Result := A.C2 <= B.C1;
end;

function TIntBounds1.ToString: string;
begin
  Result := Format('<%d~%d>', [C1, C2]);
end;

class operator TIntBounds1.Implicit(ABounds: TIntBounds1): string;
begin
  Result := ABounds.ToString;
end;

function TIntBounds1.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

{ TIntBounds1.TIterator }

constructor TIntBounds1.TIterator.Create(const ABounds: TIntBounds1);
begin
  FCurrent := ABounds.C1 - 1;
  FEnd := ABounds.C2;
end;

function TIntBounds1.TIterator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < FEnd;
end;

{ TIntBounds2 }

function TIntBounds2.GetLineX: TIntBounds1;
begin
  Result := Range1(C1.X, C2.X);
end;

function TIntBounds2.GetLineY: TIntBounds1;
begin
  Result := Range1(C1.Y, C2.Y);
end;

procedure TIntBounds2.SetLineX(const Value: TIntBounds1);
begin
  C1.X := Value.C1;
  C2.X := Value.C2;
end;

procedure TIntBounds2.SetLineY(const Value: TIntBounds1);
begin
  C1.Y := Value.C1;
  C2.Y := Value.C2;
end;

constructor TIntBounds2.Create(AC1, AC2: TIntVector2);
begin
  C1 := AC1;
  C2 := AC2;
end;

constructor TIntBounds2.Create(A: TIntVector2);
begin
  C1 := A;
  C2 := A;
end;

class operator TIntBounds2.Implicit(A: TIntVector2): TIntBounds2;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

function TIntBounds2.GetSize: TIntVector2;
begin
  Result := C2 - C1;
end;

function TIntBounds2.GetArea: Integer;
begin
  Result := Width * Height;
end;

function TIntBounds2.GetWidth: Integer;
begin
  Result := LineX.Length;
end;

function TIntBounds2.GetHeight: Integer;
begin
  Result := LineY.Length;
end;

class operator TIntBounds2.Implicit(A: Integer): TIntBounds2;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

function TIntBounds2.EnsureRange(ARange: TIntBounds2): TIntBounds2;
begin
  Result.LineX := LineX.EnsureRange(ARange.LineX);
  Result.LineY := LineY.EnsureRange(ARange.LineY);
end;

function TIntBounds2.EnsureRange(AValue: TIntVector2): TIntVector2;
begin
  Result.X := LineX.EnsureRange(AValue.X);
  Result.Y := LineY.EnsureRange(AValue.Y);
end;

function TIntBounds2.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TIntBounds2.Normalize: TIntBounds2;
begin
  Result.LineX := LineX.Normalize;
  Result.LineY := LineY.Normalize;
end;

function TIntBounds2.Inset(AAmount: TIntVector2): TIntBounds2;
begin
  Result.C1 := C1 + AAmount;
  Result.C2 := C2 - AAmount;
end;

function TIntBounds2.Outset(AAmount: TIntVector2): TIntBounds2;
begin
  Result.C1 := C1 - AAmount;
  Result.C2 := C2 + AAmount;
end;

class operator TIntBounds2.Add(const A, B: TIntBounds2): TIntBounds2;
begin
  Result.C1 := A.C1 + B.C1;
  Result.C2 := A.C2 + B.C2;
end;

class operator TIntBounds2.Subtract(const A, B: TIntBounds2): TIntBounds2;
begin
  Result.C1 := A.C1 - B.C1;
  Result.C2 := A.C2 - B.C2;
end;

class operator TIntBounds2.Multiply(const A, B: TIntBounds2): TIntBounds2;
begin
  Result.C1 := A.C1 * B.C1;
  Result.C2 := A.C2 * B.C2;
end;

class operator TIntBounds2.IntDivide(const A, B: TIntBounds2): TIntBounds2;
begin
  Result.C1 := A.C1 div B.C1;
  Result.C2 := A.C2 div B.C2;
end;

class operator TIntBounds2.in(const A, B: TIntBounds2): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TIntBounds2.in(A: TIntVector2; const B: TIntBounds2): Boolean;
begin
  Result := (A >= B.C1) and (A < B.C2);
end;

class operator TIntBounds2.Equal(const A, B: TIntBounds2): Boolean;
begin
  Result := (A.C1 = B.C1) and (A.C2 = B.C2);
end;

class operator TIntBounds2.NotEqual(const A, B: TIntBounds2): Boolean;
begin
  Result := (A.C1 <> B.C1) or (A.C2 <> B.C2);
end;

class operator TIntBounds2.GreaterThan(const A, B: TIntBounds2): Boolean;
begin
  Result := A.C1 > B.C2;
end;

class operator TIntBounds2.GreaterThanOrEqual(const A, B: TIntBounds2): Boolean;
begin
  Result := A.C1 >= B.C2;
end;

class operator TIntBounds2.LessThan(const A, B: TIntBounds2): Boolean;
begin
  Result := A.C2 < B.C1;
end;

class operator TIntBounds2.LessThanOrEqual(const A, B: TIntBounds2): Boolean;
begin
  Result := A.C2 <= B.C1;
end;

function TIntBounds2.ToString: string;
begin
  Result := Format('<%s~%s>', [C1.ToString, C2.ToString]);
end;

class operator TIntBounds2.Implicit(ABounds: TIntBounds2): string;
begin
  Result := ABounds.ToString;
end;

function TIntBounds2.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

{ TIntBounds2.TIterator }

constructor TIntBounds2.TIterator.Create(const ABounds: TIntBounds2);
begin
  FCurrent.X := ABounds.C1.X - 1;
  FCurrent.Y := ABounds.C1.Y;
  FBounds := @ABounds;
end;

function TIntBounds2.TIterator.MoveNext: Boolean;
begin
  Inc(FCurrent.X);
  if FCurrent.X >= FBounds.C2.X then
  begin
    FCurrent.X := FBounds.C1.X;
    Inc(FCurrent.Y);
  end;
  Result := FCurrent.Y < FBounds.C2.Y;
end;

{ TIntBounds3 }

function TIntBounds3.GetLineX: TIntBounds1;
begin
  Result := Range1(C1.X, C2.X);
end;

function TIntBounds3.GetLineY: TIntBounds1;
begin
  Result := Range1(C1.Y, C2.Y);
end;

function TIntBounds3.GetLineZ: TIntBounds1;
begin
  Result := Range1(C1.Z, C2.Z);
end;

procedure TIntBounds3.SetLineX(const Value: TIntBounds1);
begin
  C1.X := Value.C1;
  C2.X := Value.C2;
end;

procedure TIntBounds3.SetLineY(const Value: TIntBounds1);
begin
  C1.Y := Value.C1;
  C2.Y := Value.C2;
end;

procedure TIntBounds3.SetLineZ(const Value: TIntBounds1);
begin
  C1.Z := Value.C1;
  C2.Z := Value.C2;
end;

function TIntBounds3.GetPlaneXY: TIntBounds2;
begin
  Result.LineX := LineX;
  Result.LineY := LineY;
end;

function TIntBounds3.GetPlaneYZ: TIntBounds2;
begin
  Result.LineX := LineY;
  Result.LineY := LineZ;
end;

function TIntBounds3.GetPlaneZX: TIntBounds2;
begin
  Result.LineX := LineZ;
  Result.LineY := LineX;
end;

function TIntBounds3.GetPlaneYX: TIntBounds2;
begin
  Result.LineX := LineY;
  Result.LineY := LineX;
end;

function TIntBounds3.GetPlaneZY: TIntBounds2;
begin
  Result.LineX := LineZ;
  Result.LineY := LineY;
end;

function TIntBounds3.GetPlaneXZ: TIntBounds2;
begin
  Result.LineX := LineX;
  Result.LineY := LineZ;
end;

procedure TIntBounds3.SetPlaneXY(const Value: TIntBounds2);
begin
  LineX := Value.LineX;
  LineY := Value.LineY;
end;

procedure TIntBounds3.SetPlaneYZ(const Value: TIntBounds2);
begin
  LineY := Value.LineX;
  LineZ := Value.LineY;
end;

procedure TIntBounds3.SetPlaneZX(const Value: TIntBounds2);
begin
  LineZ := Value.LineX;
  LineX := Value.LineY;
end;

procedure TIntBounds3.SetPlaneYX(const Value: TIntBounds2);
begin
  LineY := Value.LineX;
  LineX := Value.LineY;
end;

procedure TIntBounds3.SetPlaneZY(const Value: TIntBounds2);
begin
  LineZ := Value.LineX;
  LineY := Value.LineY;
end;

procedure TIntBounds3.SetPlaneXZ(const Value: TIntBounds2);
begin
  LineX := Value.LineX;
  LineZ := Value.LineY;
end;

constructor TIntBounds3.Create(AC1, AC2: TIntVector3);
begin
  C1 := AC1;
  C2 := AC2;
end;

constructor TIntBounds3.Create(A: TIntVector3);
begin
  C1 := A;
  C2 := A;
end;

class operator TIntBounds3.Implicit(A: TIntVector3): TIntBounds3;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

function TIntBounds3.GetSize: TIntVector3;
begin
  Result := C2 - C1;
end;

function TIntBounds3.GetVolume: Integer;
begin
  Result := Width * Height * Depth;
end;

function TIntBounds3.GetWidth: Integer;
begin
  Result := LineX.Length;
end;

function TIntBounds3.GetHeight: Integer;
begin
  Result := LineY.Length;
end;

function TIntBounds3.GetDepth: Integer;
begin
  Result := LineZ.Length;
end;

function TIntBounds3.EnsureRange(ARange: TIntBounds3): TIntBounds3;
begin
  Result.LineX := LineX.EnsureRange(ARange.LineX);
  Result.LineY := LineY.EnsureRange(ARange.LineY);
  Result.LineZ := LineZ.EnsureRange(ARange.LineZ);
end;

function TIntBounds3.EnsureRange(AValue: TIntVector3): TIntVector3;
begin
  Result.X := LineX.EnsureRange(AValue.X);
  Result.Y := LineY.EnsureRange(AValue.Y);
  Result.Z := LineZ.EnsureRange(AValue.Z);
end;

function TIntBounds3.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TIntBounds3.Normalize: TIntBounds3;
begin
  Result.LineX := LineX.Normalize;
  Result.LineY := LineY.Normalize;
  Result.LineZ := LineZ.Normalize;
end;

function TIntBounds3.Inset(AAmount: TIntVector3): TIntBounds3;
begin
  Result.C1 := C1 + AAmount;
  Result.C2 := C2 - AAmount;
end;

function TIntBounds3.Outset(AAmount: TIntVector3): TIntBounds3;
begin
  Result.C1 := C1 - AAmount;
  Result.C2 := C2 + AAmount;
end;

class operator TIntBounds3.Add(const A, B: TIntBounds3): TIntBounds3;
begin
  Result.C1 := A.C1 + B.C1;
  Result.C2 := A.C2 + B.C2;
end;

class operator TIntBounds3.Subtract(const A, B: TIntBounds3): TIntBounds3;
begin
  Result.C1 := A.C1 - B.C1;
  Result.C2 := A.C2 - B.C2;
end;

class operator TIntBounds3.Multiply(const A, B: TIntBounds3): TIntBounds3;
begin
  Result.C1 := A.C1 * B.C1;
  Result.C2 := A.C2 * B.C2;
end;

class operator TIntBounds3.IntDivide(const A, B: TIntBounds3): TIntBounds3;
begin
  Result.C1 := A.C1 div B.C1;
  Result.C2 := A.C2 div B.C2;
end;

class operator TIntBounds3.in(const A, B: TIntBounds3): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TIntBounds3.in(A: TIntVector3; const B: TIntBounds3): Boolean;
begin
  Result := (A >= B.C1) and (A < B.C2);
end;

class operator TIntBounds3.Equal(const A, B: TIntBounds3): Boolean;
begin
  Result := (A.C1 = B.C1) and (A.C2 = B.C2);
end;

class operator TIntBounds3.NotEqual(const A, B: TIntBounds3): Boolean;
begin
  Result := (A.C1 <> B.C1) or (A.C2 <> B.C2);
end;

class operator TIntBounds3.GreaterThan(const A, B: TIntBounds3): Boolean;
begin
  Result := A.C1 > B.C2;
end;

class operator TIntBounds3.GreaterThanOrEqual(const A, B: TIntBounds3): Boolean;
begin
  Result := A.C1 >= B.C2;
end;

class operator TIntBounds3.LessThan(const A, B: TIntBounds3): Boolean;
begin
  Result := A.C2 < B.C1;
end;
class operator TIntBounds3.LessThanOrEqual(const A, B: TIntBounds3): Boolean;
begin
  Result := A.C2 <= B.C1;
end;

function TIntBounds3.ToString: string;
begin
  Result := Format('<%s~%s>', [C1.ToString, C2.ToString]);
end;

class operator TIntBounds3.Implicit(ABounds: TIntBounds3): string;
begin
  Result := ABounds.ToString;
end;

function TIntBounds3.GetEnumerator: TIterator;
begin
  Result := TIterator.Create(Self);
end;

{ TIntBounds3.TIterator }

constructor TIntBounds3.TIterator.Create(const ABounds: TIntBounds3);
begin
  FCurrent.X := ABounds.C1.X - 1;
  FCurrent.Y := ABounds.C1.Y;
  FCurrent.Z := ABounds.C1.Z;
  FBounds := @ABounds;
end;

function TIntBounds3.TIterator.MoveNext: Boolean;
begin
  Inc(FCurrent.X);
  if FCurrent.X >= FBounds.C2.X then
  begin
    FCurrent.X := FBounds.C1.X;
    Inc(FCurrent.Y);
    if FCurrent.Y >= FBounds.C2.Y then
    begin
      FCurrent.Y := FBounds.C1.Y;
      Inc(FCurrent.Z);
    end;
  end;
  Result := FCurrent.Z < FBounds.C2.Z;
end;

{ Shorthand Constructors }

function IVec2(X, Y: Integer): TIntVector2;
begin
  Result.Create(X, Y);
end;

function IVec2(V: Integer): TIntVector2;
begin
  Result.Create(V);
end;

function IVec3(X, Y, Z: Integer): TIntVector3;
begin
  Result.Create(X, Y, Z);
end;

function IVec3(V: Integer): TIntVector3;
begin
  Result.Create(V);
end;

function Range1(A, B: Integer): TIntBounds1;
begin
  Result.Create(A, B);
end;

function Range1(A: Integer): TIntBounds1;
begin
  Result.Create(0, A);
end;

function Range2(A, B: TIntVector2): TIntBounds2;
begin
  Result.Create(A, B);
end;

function Range2(A: TIntVector2): TIntBounds2;
begin
  Result.Create(0, A);
end;

function Range3(A, B: TIntVector3): TIntBounds3;
begin
  Result.Create(A, B);
end;

function Range3(A: TIntVector3): TIntBounds3;
begin
  Result.Create(0, A);
end;

end.
