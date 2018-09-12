unit Pengine.IntMaths;

{$POINTERMATH ON}

interface

uses
  System.Math,
  System.SysUtils,
  System.Types;

type

  // TODO: Big todo! Make bounds [C1, C2] instead of [C1, C2) as it is not cabable of representing every int value

  /// <summary>A vertex-index for a triangle in range: <c>[0, 3)</c></summary>
  TTriangleIndex = 0 .. 2;
  /// <summary>A vertex-index for a render-quad in range: <c>[0, 6)</c></summary>
  TQuadIndex = 0 .. 5;

  /// <summary>Describes one of the three axes: <c>caX, caY, caZ</c> and also contains <c>caNone</c> to specify no axis.</summary>
  TCoordAxis = (caNone, caX, caY, caZ);

  /// <summary>A 1-Dimensional subrange for <see cref="Pengine.Vector|TCoordAxis"/>, which does not contain caNone.</summary>
  TCoordAxis1 = caX .. caX;
  /// <summary>A 2-Dimensional subrange for <see cref="Pengine.Vector|TCoordAxis"/>, which does not contain caNone.</summary>
  TCoordAxis2 = caX .. caY;
  /// <summary>A 3-Dimensional subrange for <see cref="Pengine.Vector|TCoordAxis"/>, which does not contain caNone.</summary>
  TCoordAxis3 = caX .. caZ;

  /// <summary>A 1-Dimensional subrange for <see cref="Pengine.Vector|TCoordAxis"/>, which also contains caNone.</summary>
  TCoordAxis1Nonable = caNone .. caX;
  /// <summary>A 2-Dimensional subrange for <see cref="Pengine.Vector|TCoordAxis"/>, which also contains caNone.</summary>
  TCoordAxis2Nonable = caNone .. caY;
  /// <summary>A 3-Dimensional subrange for <see cref="Pengine.Vector|TCoordAxis"/>, which also contains caNone.</summary>
  TCoordAxis3Nonable = caNone .. caZ;

  /// <summary>Set of 1-Dimensional axes. <p>See <see cref="Pengine.Vector|TCoordAxis"/> for more info.</p></summary>
  TCoordAxes1 = set of TCoordAxis1;
  /// <summary>Set of 2-Dimensional axes. <p>See <see cref="Pengine.Vector|TCoordAxis"/> for more info.</p></summary>
  TCoordAxes2 = set of TCoordAxis2;
  /// <summary>Set of 3-Dimensional axes. <p>See <see cref="Pengine.Vector|TCoordAxis"/> for more info.</p></summary>
  TCoordAxes3 = set of TCoordAxis3;

  /// <summary>Describes one of the six simple directions in the following order:
  /// <code>
  /// 0  -> bdNone<p/>
  /// -X -> bdLeft<p/>
  /// +X -> bdRight<p/>
  /// -Y -> bdDown<p/>
  /// +Y -> bdUp<p/>
  /// -Z -> bdBack<p/>
  /// +Z -> bdFront
  /// </code>
  /// <p>The following subranges exist:</p>
  /// <p>
  /// <see cref="Pengine.Vector|TBasicDir1"/>,
  /// <see cref="Pengine.Vector|TBasicDir2"/>,
  /// <see cref="Pengine.Vector|TBasicDir3"/>,
  /// <see cref="Pengine.Vector|TBasicDir1Nonable"/>,
  /// <see cref="Pengine.Vector|TBasicDir2Nonable"/>,
  /// <see cref="Pengine.Vector|TBasicDir3Nonable"/>
  /// </p>
  /// </summary>
  /// <remarks>The included <c>bdNone</c> direction is defined as the origin vector.</remarks>
  TBasicDir = (bdNone, bdLeft, bdRight, bdDown, bdUp, bdBack, bdFront);

  /// <summary>1-Dimensional, subrange for <see cref="Pengine.Vector|TBasicDir"/>, no <c>bdNone</c>.</summary>
  TBasicDir1 = bdLeft .. bdRight;
  /// <summary>2-Dimensional, subrange for <see cref="Pengine.Vector|TBasicDir"/>, no <c>bdNone</c>.</summary>
  TBasicDir2 = bdLeft .. bdUp;
  /// <summary>3-Dimensional, subrange for <see cref="Pengine.Vector|TBasicDir"/>, no <c>bdNone</c>.</summary>
  TBasicDir3 = bdLeft .. bdFront;

  /// <summary>1-Dimensional, subrange for <see cref="Pengine.Vector|TBasicDir"/>, with <c>bdNone</c>.</summary>
  TBasicDir1Nonable = bdNone .. bdRight;
  /// <summary>2-Dimensional, subrange for <see cref="Pengine.Vector|TBasicDir"/>, with <c>bdNone</c>.</summary>
  TBasicDir2Nonable = bdNone .. bdUp;
  /// <summary>3-Dimensional, subrange for <see cref="Pengine.Vector|TBasicDir"/>, with <c>bdNone</c>.</summary>
  TBasicDir3Nonable = bdNone .. bdFront;

  /// <summary>Set of 1-Dimensional directions. <p>See <see cref="Pengine.Vector|TBasicDir"/> for more info.</p></summary>
  TBasicDirs1 = set of TBasicDir1;
  /// <summary>Set of 2-Dimensional directions. <p>See <see cref="Pengine.Vector|TBasicDir"/> for more info.</p></summary>
  TBasicDirs2 = set of TBasicDir2;
  /// <summary>Set of 3-Dimensional directions. <p>See <see cref="Pengine.Vector|TBasicDir"/> for more info.</p></summary>
  TBasicDirs3 = set of TBasicDir3;

  PIntBounds1 = ^TIntBounds1;
  PIntBounds2 = ^TIntBounds2;
  PIntBounds3 = ^TIntBounds3;

  PIntVector2 = ^TIntVector2;
  PIntVector3 = ^TIntVector3;

  TIntBounds1Iterator = class;
  TIntBounds1ReverseIterator = class;
  TIntBounds2Iterator = class;
  TIntBounds2ReverseIterator = class;
  TIntBounds3Iterator = class;
  TIntBounds3ReverseIterator = class;

  /// <summary>A two component vector of type <see cref="System|Integer"/>.</summary>
  TIntVector2 = record
  private
    function GetComponent(AAxis: TCoordAxis2): Integer; inline;
    procedure SetComponent(AAxis: TCoordAxis2; const Value: Integer); inline;

    function GetXX: TIntVector2;
    function GetXY: TIntVector2;
    function GetYX: TIntVector2;
    function GetYY: TIntVector2;

    procedure SetXY(const Value: TIntVector2);
    procedure SetYX(const Value: TIntVector2);

  public
    /// <summary>The X-Component of the vector.</summary>
    X: Integer;
    /// <summary>The Y-Component of the vector.</summary>
    Y: Integer;

    /// <remarks>Allows simple array-like access of each component, thanks to it being a default property.</remarks>
    property Component[AAxis: TCoordAxis2]: Integer read GetComponent write SetComponent; default;

    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntVector2"/> with the specified components.</summary>
    constructor Create(X, Y: Integer); overload;
    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntVector2"/> with both components being the same, given value.</summary>
    constructor Create(V: Integer); overload;

    class operator Implicit(V: Integer): TIntVector2; inline;
    class operator Implicit(A: TIntVector2): TPoint;
    class operator Implicit(A: TPoint): TIntVector2;

    {$REGION 'All versions of rearrangement TIntVector2'}

    property XX: TIntVector2 read GetXX;
    property XY: TIntVector2 read GetXY write SetXY;
    property YX: TIntVector2 read GetYX write SetYX;
    property YY: TIntVector2 read GetYY;

    {$ENDREGION}

    class operator Add(const A, B: TIntVector2): TIntVector2;
    class operator Subtract(const A, B: TIntVector2): TIntVector2;
    class operator Multiply(const A, B: TIntVector2): TIntVector2;
    class operator IntDivide(const A, B: TIntVector2): TIntVector2;
    class operator Modulus(const A, B: TIntVector2): TIntVector2;

    class operator Positive(const A: TIntVector2): TIntVector2; inline;
    class operator Negative(const A: TIntVector2): TIntVector2;

    class operator Equal(const A, B: TIntVector2): Boolean;
    class operator NotEqual(const A, B: TIntVector2): Boolean;
    class operator LessThan(const A, B: TIntVector2): Boolean;
    class operator LessThanOrEqual(const A, B: TIntVector2): Boolean;
    class operator GreaterThan(const A, B: TIntVector2): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntVector2): Boolean;

    class operator In (const A, B: TIntVector2): Boolean; inline;

    /// <returns>A string representative in the form: <c>[X|Y]</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(const A: TIntVector2): string; inline;

    function GetEnumerator: TIntBounds2Iterator;

    /// <returns>The vector, with each negative component being positive.</returns>
    function Abs: TIntVector2;
    /// <returns>A vector with the smaller components of both vectors.</returns>
    function Min(const A: TIntVector2): TIntVector2;
    /// <returns>A vector with the greater components of both vectors.</returns>
    function Max(const A: TIntVector2): TIntVector2;

    /// <returns>A vector, which is Rotate by 90° counter-clockwise.</returns>
    function Cross: TIntVector2;

  end;

  /// <summary>A three component vector of type <see cref="System|Integer"/>.</summary>
  TIntVector3 = record
  private
    function GetComponent(AAxis: TCoordAxis3): Integer; inline;
    procedure SetComponent(AAxis: TCoordAxis3; const Value: Integer); inline;

    function GetXX: TIntVector2;
    function GetXY: TIntVector2;
    function GetXZ: TIntVector2;
    function GetYX: TIntVector2;
    function GetYY: TIntVector2;
    function GetYZ: TIntVector2;
    function GetZX: TIntVector2;
    function GetZY: TIntVector2;
    function GetZZ: TIntVector2;

    function GetXXX: TIntVector3;
    function GetXXY: TIntVector3;
    function GetXXZ: TIntVector3;
    function GetXYX: TIntVector3;
    function GetXYY: TIntVector3;
    function GetXYZ: TIntVector3;
    function GetXZX: TIntVector3;
    function GetXZY: TIntVector3;
    function GetXZZ: TIntVector3;
    function GetYXX: TIntVector3;
    function GetYXY: TIntVector3;
    function GetYXZ: TIntVector3;
    function GetYYX: TIntVector3;
    function GetYYY: TIntVector3;
    function GetYYZ: TIntVector3;
    function GetYZX: TIntVector3;
    function GetYZY: TIntVector3;
    function GetYZZ: TIntVector3;
    function GetZXX: TIntVector3;
    function GetZXY: TIntVector3;
    function GetZXZ: TIntVector3;
    function GetZYX: TIntVector3;
    function GetZYY: TIntVector3;
    function GetZYZ: TIntVector3;
    function GetZZX: TIntVector3;
    function GetZZY: TIntVector3;
    function GetZZZ: TIntVector3;

    procedure SetXY(const Value: TIntVector2);
    procedure SetXZ(const Value: TIntVector2);
    procedure SetYX(const Value: TIntVector2);
    procedure SetYZ(const Value: TIntVector2);
    procedure SetZX(const Value: TIntVector2);
    procedure SetZY(const Value: TIntVector2);

    procedure SetXYZ(const Value: TIntVector3);
    procedure SetXZY(const Value: TIntVector3);
    procedure SetYXZ(const Value: TIntVector3);
    procedure SetYZX(const Value: TIntVector3);
    procedure SetZXY(const Value: TIntVector3);
    procedure SetZYX(const Value: TIntVector3);

  public
    /// <summary>The X-Component of the vector.</summary>
    X: Integer;
    /// <summary>The Y-Component of the vector.</summary>
    Y: Integer;
    /// <summary>The Z-Component of the vector.</summary>
    Z: Integer;

    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntVector3"/> with the specified components.</summary>
    constructor Create(X, Y, Z: Integer); overload;
    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntVector3"/> with all components being the same, given value.</summary>
    constructor Create(V: Integer); overload;

    /// <remarks>Allows simple array-like access of each component, thanks to it being a default property.</remarks>
    property Component[AAxis: TCoordAxis3]: Integer read GetComponent write SetComponent; default;

    class operator Implicit(V: Integer): TIntVector3; inline;

    {$REGION 'All versions of rearrangement TIntVector2'}

    property XX: TIntVector2 read GetXX;
    property XY: TIntVector2 read GetXY write SetXY;
    property XZ: TIntVector2 read GetXZ write SetXZ;
    property YX: TIntVector2 read GetYX write SetYX;
    property YY: TIntVector2 read GetYY;
    property YZ: TIntVector2 read GetYZ write SetYZ;
    property ZX: TIntVector2 read GetZX write SetZX;
    property ZY: TIntVector2 read GetZY write SetZY;
    property ZZ: TIntVector2 read GetZZ;

    {$ENDREGION}
    {$REGION 'All versions of rearrangement TIntVector3'}

    property XXX: TIntVector3 read GetXXX;
    property XXY: TIntVector3 read GetXXY;
    property XXZ: TIntVector3 read GetXXZ;
    property XYX: TIntVector3 read GetXYX;
    property XYY: TIntVector3 read GetXYY;
    property XYZ: TIntVector3 read GetXYZ write SetXYZ;
    property XZX: TIntVector3 read GetXZX;
    property XZY: TIntVector3 read GetXZY write SetXZY;
    property XZZ: TIntVector3 read GetXZZ;
    property YXX: TIntVector3 read GetYXX;
    property YXY: TIntVector3 read GetYXY;
    property YXZ: TIntVector3 read GetYXZ write SetYXZ;
    property YYX: TIntVector3 read GetYYX;
    property YYY: TIntVector3 read GetYYY;
    property YYZ: TIntVector3 read GetYYZ;
    property YZX: TIntVector3 read GetYZX write SetYZX;
    property YZY: TIntVector3 read GetYZY;
    property YZZ: TIntVector3 read GetYZZ;
    property ZXX: TIntVector3 read GetZXX;
    property ZXY: TIntVector3 read GetZXY write SetZXY;
    property ZXZ: TIntVector3 read GetZXZ;
    property ZYX: TIntVector3 read GetZYX write SetZYX;
    property ZYY: TIntVector3 read GetZYY;
    property ZYZ: TIntVector3 read GetZYZ;
    property ZZX: TIntVector3 read GetZZX;
    property ZZY: TIntVector3 read GetZZY;
    property ZZZ: TIntVector3 read GetZZZ;

    {$ENDREGION}

    class operator Add(const A, B: TIntVector3): TIntVector3;
    class operator Subtract(const A, B: TIntVector3): TIntVector3;
    class operator Multiply(const A, B: TIntVector3): TIntVector3;
    class operator IntDivide(const A, B: TIntVector3): TIntVector3;
    class operator Modulus(const A, B: TIntVector3): TIntVector3;

    class operator Positive(const A: TIntVector3): TIntVector3; inline;
    class operator Negative(const A: TIntVector3): TIntVector3;

    class operator Equal(const A, B: TIntVector3): Boolean;
    class operator NotEqual(const A, B: TIntVector3): Boolean;
    class operator LessThan(const A, B: TIntVector3): Boolean;
    class operator LessThanOrEqual(const A, B: TIntVector3): Boolean;
    class operator GreaterThan(const A, B: TIntVector3): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntVector3): Boolean;

    class operator In (A: Integer; const B: TIntVector3): Boolean; inline;

    /// <returns>A string representative in the form: <c>[X|Y|Z]</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(const A: TIntVector3): string; inline;

    function GetEnumerator: TIntBounds3Iterator;

    /// <returns>The vector, with each negative component being positive.</returns>
    function Abs: TIntVector3;
    /// <returns>A vector with the smaller components of both vectors.</returns>
    function Min(const A: TIntVector3): TIntVector3;
    /// <returns>A vector with the greater components of both vectors.</returns>
    function Max(const A: TIntVector3): TIntVector3;

  end;

  /// <summary>
  /// Represents 1-Dimensional bounds <c>[C1, C2)</c> using two <see cref="System|Integer"/>.
  /// <p>Shorthand constructor using: <see cref="Pengine.IntMaths|IBounds1"/></p>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized: <c>C1 &lt;= C2</c><p/>
  /// Iteration with a <c>for-in</c> loop is possible.
  /// </remarks>
  TIntBounds1 = record
  public type

    TCornerIndex = 0 .. 1;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X<p/>
    /// [0]  (0)<p/>
    /// [1]  (1)<p/>
    /// </code></summary>
    TCorners = array [TCornerIndex] of Integer;

  public
    /// <summary>The (usually) lower value of the bounds.</summary>
    C1: Integer;
    /// <summary>The (usually) higher value of the bounds.</summary>
    C2: Integer;

    /// <summary>Alias for <see cref="Pengine.IntMaths|TIntBounds1.C1"/>.</summary>
    property Low: Integer read C1 write C1;
    /// <summary>Alias for <see cref="Pengine.IntMaths|TIntBounds1.C2"/>.</summary>
    property High: Integer read C2 write C2;

    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntBounds1"/> with the specified range.</summary>
    constructor Create(AC1, AC2: Integer); overload;
    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntBounds1"/> with both bounds laying on the same, given value.</summary>
    constructor Create(A: Integer); overload;

    class operator Implicit(A: Integer): TIntBounds1; inline;

    /// <returns>The difference between C1 and C2.</returns>
    /// <remarks>Will give a negative length for non-normalized bounds.</remarks>
    function Length: Integer; inline;

    /// <returns>The center point between C1 and C2 rounded down.</returns>
    function Center: Integer; inline;

    /// <returns>The given bounds clamped to be inside of the calling bounds.</returns>
    /// <remarks>This operation is performed inclusive, so that repeated clamping stays the same.</remarks>
    function Clamp(ARange: TIntBounds1): TIntBounds1; overload;
    /// <returns>The given value being clamped to the bounds <c>[C1, C2).</c></returns>
    function Clamp(AValue: Integer): Integer; overload;

    /// <returns>The given value in the interval: <c>[C1..C2)</c></returns>
    function RangedMod(AValue: Integer): Integer;

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TIntBounds1;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: Integer): TIntBounds1;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: Integer): TIntBounds1;

    class operator Add(const A, B: TIntBounds1): TIntBounds1;
    class operator Subtract(const A, B: TIntBounds1): TIntBounds1;
    class operator Multiply(const A, B: TIntBounds1): TIntBounds1;
    class operator IntDivide(const A, B: TIntBounds1): TIntBounds1;

    // inclusive
    class operator in (const A, B: TIntBounds1): Boolean;
    // exclusive
    class operator in (A: Integer; const B: TIntBounds1): Boolean;

    class operator Equal(const A, B: TIntBounds1): Boolean;
    class operator NotEqual(const A, B: TIntBounds1): Boolean;
    class operator GreaterThan(const A, B: TIntBounds1): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntBounds1): Boolean;
    class operator LessThan(const A, B: TIntBounds1): Boolean;
    class operator LessThanOrEqual(const A, B: TIntBounds1): Boolean;

    /// <returns>A string representative in the form: <c>&lt;C1~C2&gt;</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TIntBounds1): string; inline;

    function GetEnumerator: TIntBounds1Iterator;

  end;

  /// <summary>
  /// Represents 2-Dimensional bounds <c>[C1, C2)</c> using two <see cref="Pengine.IntMaths|TIntVector2"/>.
  /// <p>Shorthand constructor using: <see cref="Pengine.IntMaths|IBounds2"/></p>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized: <c>C1 &lt;= C2</c><p/>
  /// Iteration with a <c>for-in</c> loop is possible.
  /// </remarks>
  TIntBounds2 = record
  public type

    TCornerIndex = 0 .. 3;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X, Y<p/>
    /// [0]  (0, 0)<p/>
    /// [1]  (1, 0)<p/>
    /// [2]  (0, 1)<p/>
    /// [3]  (1, 1)<p/>
    /// </code></summary>
    TCorners = array [TCornerIndex] of TIntVector2;

    PReverseWrapper = ^TReverseWrapper;
    TReverseWrapper = record
    public
      function GetEnumerator: TIntBounds2ReverseIterator;

    end;

  private

    function GetLineX: TIntBounds1; inline;
    function GetLineY: TIntBounds1; inline;

    procedure SetLineX(const Value: TIntBounds1); inline;
    procedure SetLineY(const Value: TIntBounds1); inline;

  public
    /// <summary>The (usually) lower value of the bounds.</summary>
    C1: TIntVector2;
    /// <summary>The (usually) higher value of the bounds.</summary>
    C2: TIntVector2;

    /// <summary>Alias for <see cref="Pengine.IntMaths|TIntBounds2.C1"/>.</summary>
    property Low: TIntVector2 read C1 write C1;
    /// <summary>Alias for <see cref="Pengine.IntMaths|TIntBounds2.C2"/>.</summary>
    property High: TIntVector2 read C2 write C2;

    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntBounds2"/> with the specified range.</summary>
    constructor Create(AC1, AC2: TIntVector2); overload;
    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntBounds2"/> with both bounds laying on the same, given value.</summary>
    constructor Create(A: TIntVector2); overload;

    // class operator Implicit(A: Integer): TIntBounds2; inline;
    class operator Implicit(A: TIntVector2): TIntBounds2; inline;

    /// <returns>The difference between C1 and C2.</returns>
    /// <remarks>Will give negative values for non-normalized bounds.</remarks>
    function Size: TIntVector2; inline;
    /// <returns>The area of the bounds.</returns>
    function Area: Integer; inline;

    /// <summary>Resembles both X-Components of the bounds as a <see cref="Pengine.IntMaths|TIntBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineX: TIntBounds1 read GetLineX write SetLineX;
    /// <summary>Resembles both Y-Components of the bounds as a <see cref="Pengine.IntMaths|TIntBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineY: TIntBounds1 read GetLineY write SetLineY;

    /// <returns>The horizontal length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Width: Integer; inline;
    /// <returns>The vertical length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Height: Integer; inline;

    /// <returns>The center point between C1 and C2 rounded down.</returns>
    function Center: TIntVector2; inline;

    /// <returns>The given bounds clamped to be inside of the calling bounds.</returns>
    /// <remarks>This operation is performed inclusive, so that repeated clamping stays the same.</remarks>
    function Clamp(ARange: TIntBounds2): TIntBounds2; overload;
    /// <returns>The given value being clamped to the bounds <c>[C1, C2).</c></returns>
    function Clamp(AValue: TIntVector2): TIntVector2; overload;

    /// <returns>The given value in the interval: <c>[C1..C2)</c></returns>
    function RangedMod(AValue: TIntVector2): TIntVector2;

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TIntBounds2;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: TIntVector2): TIntBounds2;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: TIntVector2): TIntBounds2;

    class operator Add(const A, B: TIntBounds2): TIntBounds2;
    class operator Subtract(const A, B: TIntBounds2): TIntBounds2;
    class operator Multiply(const A, B: TIntBounds2): TIntBounds2;
    class operator IntDivide(const A, B: TIntBounds2): TIntBounds2;

    // inclusive
    class operator in (const A, B: TIntBounds2): Boolean;
    // exclusive
    class operator in (A: TIntVector2; const B: TIntBounds2): Boolean;

    class operator Equal(const A, B: TIntBounds2): Boolean;
    class operator NotEqual(const A, B: TIntBounds2): Boolean;
    class operator GreaterThan(const A, B: TIntBounds2): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntBounds2): Boolean;
    class operator LessThan(const A, B: TIntBounds2): Boolean;
    class operator LessThanOrEqual(const A, B: TIntBounds2): Boolean;

    /// <returns>A string representative in the form: <c>&lt;C1~C2&gt;</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TIntBounds2): string; inline;

    function GetEnumerator: TIntBounds2Iterator;
    function InReverse: TReverseWrapper;

  end;

  /// <summary>
  /// Represents 2-Dimensional bounds <c>[C1, C2)</c> using two <see cref="Pengine.IntMaths|TIntVector2"/>.
  /// <p>Shorthand constructor using: <see cref="Pengine.IntMaths|IBounds2"/></p>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized: <c>C1 &lt;= C2</c><p/>
  /// Iteration with a <c>for-in</c> loop is possible.
  /// </remarks>
  TIntBounds3 = record
  public type

    TCornerIndex = 0 .. 7;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X, Y, Z<p/>
    ///  [0]  (0, 0, 0)<p/>
    ///  [1]  (1, 0, 0)<p/>
    ///  [2]  (0, 1, 0)<p/>
    ///  [3]  (1, 1, 0)<p/>
    ///  [4]  (0, 0, 1)<p/>
    ///  [5]  (1, 0, 1)<p/>
    ///  [6]  (0, 1, 1)<p/>
    ///  [7]  (1, 1, 1)<p/>
    /// </code></summary>
    TCorners = array [TCornerIndex] of TIntVector2;

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

  public
    /// <summary>The (usually) lower values of the bounds.</summary>
    C1: TIntVector3;
    /// <summary>The (usually) higher values of the bounds.</summary>
    C2: TIntVector3;

    /// <summary>Alias for <see cref="Pengine.IntMaths|TIntBounds3.C1"/>.</summary>
    property Low: TIntVector3 read C1 write C1;
    /// <summary>Alias for <see cref="Pengine.IntMaths|TIntBounds3.C2"/>.</summary>
    property High: TIntVector3 read C2 write C2;

    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntBounds3"/> with the specified range.</summary>
    constructor Create(AC1, AC2: TIntVector3); overload;
    /// <summary>Creates a <see cref="Pengine.IntMaths|TIntBounds3"/> with both bounds laying on the same, given value.</summary>
    constructor Create(A: TIntVector3); overload;

    class operator Implicit(A: TIntVector3): TIntBounds3; inline;

    /// <returns>The difference between C1 and C2.</returns>
    /// <remarks>Will give negative values for non-normalized bounds.</remarks>
    function Size: TIntVector3; inline;
    /// <returns>The volume of the bounds.</returns>
    function Volume: Integer;

    /// <summary>Resembles both X-Components of the bounds as a <see cref="Pengine.Vector|TIntBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineX: TIntBounds1 read GetLineX write SetLineX;
    /// <summary>Resembles both Y-Components of the bounds as a <see cref="Pengine.Vector|TIntBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineY: TIntBounds1 read GetLineY write SetLineY;
    /// <summary>Resembles both Z-Components of the bounds as a <see cref="Pengine.Vector|TIntBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineZ: TIntBounds1 read GetLineZ write SetLineZ;

    /// <summary>Resembles an XY-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneXY: TIntBounds2 read GetPlaneXY write SetPlaneXY;
    /// <summary>Resembles an YZ-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneYZ: TIntBounds2 read GetPlaneYZ write SetPlaneYZ;
    /// <summary>Resembles an ZX-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneZX: TIntBounds2 read GetPlaneZX write SetPlaneZX;
    /// <summary>Resembles an YX-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneYX: TIntBounds2 read GetPlaneYX write SetPlaneYX;
    /// <summary>Resembles an ZY-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneZY: TIntBounds2 read GetPlaneZY write SetPlaneZY;
    /// <summary>Resembles an XZ-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneXZ: TIntBounds2 read GetPlaneXZ write SetPlaneXZ;

    /// <returns>The horizontal length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Width: Integer; inline;
    /// <returns>The vertical length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Height: Integer; inline;
    /// <returns>The depth of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Depth: Integer; inline;

    /// <returns>The center point between C1 and C2 rounded down.</returns>
    function Center: TIntVector3; inline;

    /// <returns>The given bounds clamped to be inside of the calling bounds.</returns>
    /// <remarks>This operation is performed inclusive, so that repeated clamping stays the same.</remarks>
    function Clamp(ARange: TIntBounds3): TIntBounds3; overload;
    /// <returns>The given value being clamped to the bounds <c>[C1, C2).</c></returns>
    function Clamp(AValue: TIntVector3): TIntVector3; overload;

    /// <returns>The given value in the interval: <c>[C1..C2)</c></returns>
    function RangedMod(AValue: TIntVector3): TIntVector3;

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TIntBounds3;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: TIntVector3): TIntBounds3;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: TIntVector3): TIntBounds3;

    class operator Add(const A, B: TIntBounds3): TIntBounds3;
    class operator Subtract(const A, B: TIntBounds3): TIntBounds3;
    class operator Multiply(const A, B: TIntBounds3): TIntBounds3;
    class operator IntDivide(const A, B: TIntBounds3): TIntBounds3;

    // inclusive
    class operator in (const A, B: TIntBounds3): Boolean;
    // exclusive
    class operator in (const A: TIntVector3; const B: TIntBounds3): Boolean;

    class operator Equal(const A, B: TIntBounds3): Boolean;
    class operator NotEqual(const A, B: TIntBounds3): Boolean;
    class operator GreaterThan(const A, B: TIntBounds3): Boolean;
    class operator GreaterThanOrEqual(const A, B: TIntBounds3): Boolean;
    class operator LessThan(const A, B: TIntBounds3): Boolean;
    class operator LessThanOrEqual(const A, B: TIntBounds3): Boolean;

    /// <returns>A string representative in the form: <c>&lt;C1~C2&gt;</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TIntBounds3): string; inline;

    function GetEnumerator: TIntBounds3Iterator;

  end;

  // TODO: XmlDoc
  TIntBounds1Iterator = class
  private
    FCurrent: Integer;
    FEnd: Integer;

  public
    constructor Create(const ABounds: TIntBounds1);

    function MoveNext: Boolean;
    property Current: Integer read FCurrent;

  end;

  // TODO: XmlDoc
  TIntBounds1ReverseIterator = class
  private
    FCurrent: Integer;
    FEnd: Integer;

  public
    constructor Create(const ABounds: TIntBounds1);

    function MoveNext: Boolean;
    property Current: Integer read FCurrent;

  end;

  // TODO: XmlDoc
  TIntBounds2Iterator = class
  private
    FCurrent: TIntVector2;
    FC1, FC2: TIntVector2;

  public
    constructor Create(const ABounds: TIntBounds2);

    function MoveNext: Boolean;
    property Current: TIntVector2 read FCurrent;

  end;

  // TODO: XmlDoc
  TIntBounds2ReverseIterator = class
  private
    FCurrent: TIntVector2;
    FC1, FC2: TIntVector2;

  public
    constructor Create(const ABounds: TIntBounds2);

    function MoveNext: Boolean;
    property Current: TIntVector2 read FCurrent;

  end;

  // TODO: XmlDoc
  TIntBounds3Iterator = class
  private
    FCurrent: TIntVector3;
    FC1, FC2: TIntVector3;

  public
    constructor Create(const ABounds: TIntBounds3);

    function MoveNext: Boolean;
    property Current: TIntVector3 read FCurrent;

  end;

  // TODO: XmlDoc
  TIntBounds3ReverseIterator = class
  private
    FCurrent: TIntVector3;
    FC1, FC2: TIntVector3;

  public
    constructor Create(const ABounds: TIntBounds3);

    function MoveNext: Boolean;
    property Current: TIntVector3 read FCurrent;

  end;

  TIntVector2Helper = record helper for TIntVector2
  public
    /// <returns>A <see cref="Pengine.IntMaths|TIntBounds2"/> for the interval: <c>[Self, Self + ASize)</c></returns>
    function Bounds(ASize: TIntVector2): TIntBounds2;
  end;

  TIntVector3Helper = record helper for TIntVector3
  public
    /// <returns>A <see cref="Pengine.IntMaths|TIntBounds3"/> for the interval: <c>[Self, Self + ASize)</c></returns>
    function Bounds(ASize: TIntVector3): TIntBounds3;
  end;

const

  Vec1Dir: array [TBasicDir1Nonable] of Integer = (
    0,
    -1,
    +1
    );

  Vec2Dir: array [TBasicDir2Nonable] of TIntVector2 = (
    (X:  0; Y:  0),
    (X: -1; Y:  0),
    (X: +1; Y:  0),
    (X:  0; Y: -1),
    (X:  0; Y: +1)
    );

  Vec3Dir: array [TBasicDir] of TIntVector3 = (
    (X: 0; Y: 0; Z: 0),
    (X: - 1; Y: 0; Z: 0),
    (X: + 1; Y: 0; Z: 0),
    (X: 0; Y: - 1; Z: 0),
    (X: 0; Y: + 1; Z: 0),
    (X: 0; Y: 0; Z: - 1),
    (X: 0; Y: 0; Z: + 1)
    );

  FlippedBasicDirs: array [TBasicDir] of TBasicDir = (
    bdNone,
    bdRight,
    bdLeft,
    bdUp,
    bdDown,
    bdFront,
    bdBack
    );

  AbsBasicDirs: array [TBasicDir] of TBasicDir = (
    bdNone,
    bdRight,
    bdRight,
    bdUp,
    bdUp,
    bdFront,
    bdBack
    );

  BasicDirAxis: array [TBasicDir] of TCoordAxis = (
    caNone,
    caX,
    caX,
    caY,
    caY,
    caZ,
    caZ
    );

  // -                      direction    around    ccw times
  BasicDirRotations: array [TBasicDir3, TBasicDir3, 1 .. 3] of TBasicDir = (
    ( // left
    (bdLeft, bdLeft, bdLeft), // left
    (bdLeft, bdLeft, bdLeft), // right
    (bdBack, bdRight, bdFront), // down
    (bdFront, bdRight, bdBack), // up
    (bdUp, bdRight, bdDown), // back
    (bdDown, bdRight, bdUp) // front
    ), ( // right
    (bdRight, bdRight, bdRight), // left
    (bdRight, bdRight, bdRight), // right
    (bdFront, bdLeft, bdBack), // down
    (bdBack, bdLeft, bdFront), // up
    (bdDown, bdLeft, bdUp), // back
    (bdUp, bdLeft, bdDown) // front
    ), ( // down
    (bdBack, bdUp, bdFront), // left
    (bdFront, bdUp, bdBack), // right
    (bdDown, bdDown, bdDown), // down
    (bdDown, bdDown, bdDown), // up
    (bdLeft, bdUp, bdRight), // back
    (bdRight, bdUp, bdLeft) // front
    ), ( // up
    (bdFront, bdDown, bdBack), // left
    (bdBack, bdDown, bdFront), // right
    (bdUp, bdUp, bdUp), // down
    (bdUp, bdUp, bdUp), // up
    (bdRight, bdDown, bdLeft), // back
    (bdLeft, bdDown, bdRight) // front
    ), ( // back
    (bdDown, bdFront, bdUp), // left
    (bdUp, bdFront, bdDown), // right
    (bdLeft, bdFront, bdRight), // down
    (bdRight, bdFront, bdLeft), // up
    (bdBack, bdBack, bdBack), // back
    (bdBack, bdBack, bdBack) // front
    ), ( // front
    (bdUp, bdBack, bdDown), // left
    (bdDown, bdBack, bdUp), // right
    (bdRight, bdBack, bdLeft), // down
    (bdLeft, bdBack, bdRight), // up
    (bdFront, bdFront, bdFront), // back
    (bdFront, bdFront, bdFront) // front
    ));

  AxisBasicDir: array [TCoordAxis] of TBasicDir = (
    bdNone,
    bdRight,
    bdUp,
    bdFront
    );

  QuadSideCount = High(TQuadIndex) + 1;

  QuadTexCoords: array [TQuadIndex] of TIntVector2 = (
    (X: 0; Y: 0),
    (X: 1; Y: 0),
    (X: 1; Y: 1),
    (X: 1; Y: 1),
    (X: 0; Y: 1),
    (X: 0; Y: 0)
    );

  QuadMiddleCoords: array [TQuadIndex] of TIntVector2 = (
    (X: -1; Y: -1),
    (X: +1; Y: -1),
    (X: +1; Y: +1),
    (X: +1; Y: +1),
    (X: -1; Y: +1),
    (X: -1; Y: -1)
    );

  TriangleTexCoords: array [TTriangleIndex] of TIntVector2 = (
    (X: 0; Y: 0),
    (X: 1; Y: 0),
    (X: 0; Y: 1)
    );

  BasicDirectionNames: array [TBasicDir] of string = (
    'none',
    'left',
    'right',
    'down',
    'up',
    'backwards',
    'forward'
    );

  BasicPrepositionNames: array [TBasicDir] of string = (
    'none',
    'left',
    'right',
    'bottom',
    'top',
    'back',
    'front'
    );

  CoordAxisNames: array [TCoordAxis] of string = (
    'None',
    'X',
    'Y',
    'Z'
    );

  CoordAxisNamesLow: array [TCoordAxis] of string = (
    'none',
    'x',
    'y',
    'z'
    );

  { Shorthand Constructors }

/// <returns>A <see cref="Pengine.IntMaths|TIntVector2"/> with the given values for X and Y.</returns>
function IVec2(X, Y: Integer): TIntVector2; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntVector2"/> with the given value for X and Y.</returns>
function IVec2(V: Integer): TIntVector2; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntVector3"/> with the given values for X, Y and Z.</returns>
function IVec3(X, Y, Z: Integer): TIntVector3; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntVector3"/> with the given value for X, Y and Z.</returns>
function IVec3(V: Integer): TIntVector3; overload; inline;

/// <returns>A <see cref="Pengine.IntMaths|TIntBounds1"/> for the interval: <c>[A, B)</c></returns>
function IBounds1(A, B: Integer): TIntBounds1; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntBounds1"/> for the interval: <c>[A, B]</c></returns>
function IBounds1I(A, B: Integer): TIntBounds1; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntBounds1"/> for the interval: <c>[0, A)</c></returns>
function IBounds1(A: Integer): TIntBounds1; overload; inline;

/// <returns>A <see cref="Pengine.IntMaths|TIntBounds2"/> for the interval: <c>[A, B)</c></returns>
function IBounds2(A, B: TIntVector2): TIntBounds2; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntBounds2"/> for the interval: <c>[A, B]</c></returns>
function IBounds2I(A, B: TIntVector2): TIntBounds2; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntBounds2"/> for the interval: <c>[0, A)</c></returns>
function IBounds2(A: TIntVector2): TIntBounds2; overload; inline;

/// <returns>A <see cref="Pengine.IntMaths|TIntBounds3"/> for the interval: <c>[A, B)</c></returns>
function IBounds3(A, B: TIntVector3): TIntBounds3; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntBounds3"/> for the interval: <c>[A, B]</c></returns>
function IBounds3I(A, B: TIntVector3): TIntBounds3; overload; inline;
/// <returns>A <see cref="Pengine.IntMaths|TIntBounds3"/> for the interval: <c>[0, A)</c></returns>
function IBounds3(A: TIntVector3): TIntBounds3; overload; inline;

implementation

{ TIntVector2 }

function TIntVector2.GetComponent(AAxis: TCoordAxis2): Integer;
begin
  Result := (PInteger(@Self) + Ord(AAxis) - Ord(caX))^;
end;

procedure TIntVector2.SetComponent(AAxis: TCoordAxis2; const Value: Integer);
begin
  (PInteger(@Self) + Ord(AAxis) - Ord(caX))^ := Value;
end;

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

class operator TIntVector2.Implicit(A: TIntVector2): TPoint;
begin
  Result.X := A.X;
  Result.Y := A.Y;
end;

class operator TIntVector2.Implicit(A: TPoint): TIntVector2;
begin
  Result.X := A.X;
  Result.Y := A.Y;
end;

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

class operator TIntVector2.In(const A, B: TIntVector2): Boolean;
begin
  Result := A in IBounds2(B);
end;

function TIntVector2.ToString: string;
begin
  Result := Format('[%d|%d]', [X, Y]);
end;

class operator TIntVector2.Implicit(const A: TIntVector2): string;
begin
  Result := A.ToString;
end;

function TIntVector2.GetEnumerator: TIntBounds2Iterator;
begin
  Result := TIntBounds2Iterator.Create(IBounds2(Self));
end;

function TIntVector2.Abs: TIntVector2;
begin
  Result.X := System.Abs(X);
  Result.Y := System.Abs(Y);
end;

function TIntVector2.Min(const A: TIntVector2): TIntVector2;
begin
  Result.X := System.Math.Min(X, A.X);
  Result.Y := System.Math.Min(Y, A.Y);
end;

class operator TIntVector2.Modulus(const A, B: TIntVector2): TIntVector2;
begin
  Result.X := A.X mod B.X;
  Result.Y := A.Y mod B.Y;
end;

function TIntVector2.Max(const A: TIntVector2): TIntVector2;
begin
  Result.X := System.Math.Max(X, A.X);
  Result.Y := System.Math.Max(Y, A.Y);
end;

function TIntVector2.Cross: TIntVector2;
begin
  Result.X := -Y;
  Result.Y := X;
end;

{$REGION 'All version of rearrangement TIntVector2'}

function TIntVector2.GetXX: TIntVector2;
begin
  Result.X := X;
  Result.Y := X;
end;

function TIntVector2.GetXY: TIntVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TIntVector2.GetYX: TIntVector2;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TIntVector2.GetYY: TIntVector2;
begin
  Result.X := Y;
  Result.Y := Y;
end;

procedure TIntVector2.SetXY(const Value: TIntVector2);
begin
  X := Value.X;
  Y := Value.Y;
end;

procedure TIntVector2.SetYX(const Value: TIntVector2);
begin
  Y := Value.X;
  X := Value.Y;
end;

{$ENDREGION}

{ TIntVector3 }

function TIntVector3.GetComponent(AAxis: TCoordAxis3): Integer;
begin
  Result := (PInteger(@Self) + Ord(AAxis) - Ord(caX))^;
end;

procedure TIntVector3.SetComponent(AAxis: TCoordAxis3; const Value: Integer);
begin
  (PInteger(@Self) + Ord(AAxis) - Ord(caX))^ := Value;
end;

function TIntVector3.GetEnumerator: TIntBounds3Iterator;
begin
  Result := TIntBounds3Iterator.Create(IBounds3(Self));
end;

{$REGION 'All version of rearrangement TIntVector2'}

function TIntVector3.GetXX: TIntVector2;
begin
  Result.X := X;
  Result.Y := X;
end;

function TIntVector3.GetXY: TIntVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TIntVector3.GetXZ: TIntVector2;
begin
  Result.X := X;
  Result.Y := Z;
end;

function TIntVector3.GetYX: TIntVector2;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TIntVector3.GetYY: TIntVector2;
begin
  Result.X := Y;
  Result.Y := Y;
end;

function TIntVector3.GetYZ: TIntVector2;
begin
  Result.X := Y;
  Result.Y := Z;
end;

function TIntVector3.GetZX: TIntVector2;
begin
  Result.X := Z;
  Result.Y := X;
end;

function TIntVector3.GetZY: TIntVector2;
begin
  Result.X := Z;
  Result.Y := Y;
end;

function TIntVector3.GetZZ: TIntVector2;
begin
  Result.X := Z;
  Result.Y := Z;
end;

procedure TIntVector3.SetXY(const Value: TIntVector2);
begin
  X := Value.X;
  Y := Value.Y;
end;

procedure TIntVector3.SetXZ(const Value: TIntVector2);
begin
  X := Value.X;
  Z := Value.Y;
end;

procedure TIntVector3.SetYX(const Value: TIntVector2);
begin
  Y := Value.X;
  X := Value.Y;
end;

procedure TIntVector3.SetYZ(const Value: TIntVector2);
begin
  Y := Value.X;
  Z := Value.Y;
end;

procedure TIntVector3.SetZX(const Value: TIntVector2);
begin
  Z := Value.X;
  X := Value.Y;
end;

procedure TIntVector3.SetZY(const Value: TIntVector2);
begin
  Z := Value.X;
  Y := Value.Y;
end;

{$ENDREGION}
{$REGION 'All version of rearrangement TIntVector3'}

function TIntVector3.GetXXX: TIntVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := X;
end;

function TIntVector3.GetXXY: TIntVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := Y;
end;

function TIntVector3.GetXXZ: TIntVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := Z;
end;

function TIntVector3.GetXYX: TIntVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := X;
end;

function TIntVector3.GetXYY: TIntVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Y;
end;

function TIntVector3.GetXYZ: TIntVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TIntVector3.GetXZX: TIntVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := X;
end;

function TIntVector3.GetXZY: TIntVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := Y;
end;

function TIntVector3.GetXZZ: TIntVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := Z;
end;

function TIntVector3.GetYXX: TIntVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := X;
end;

function TIntVector3.GetYXY: TIntVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := Y;
end;

function TIntVector3.GetYXZ: TIntVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := Z;
end;

function TIntVector3.GetYYX: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := X;
end;

function TIntVector3.GetYYY: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := Y;
end;

function TIntVector3.GetYYZ: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := Z;
end;

function TIntVector3.GetYZX: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := X;
end;

function TIntVector3.GetYZY: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := Y;
end;

function TIntVector3.GetYZZ: TIntVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := Z;
end;

function TIntVector3.GetZXX: TIntVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := X;
end;

function TIntVector3.GetZXY: TIntVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := Y;
end;

function TIntVector3.GetZXZ: TIntVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := Z;
end;

function TIntVector3.GetZYX: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := X;
end;

function TIntVector3.GetZYY: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := Y;
end;

function TIntVector3.GetZYZ: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := Z;
end;

function TIntVector3.GetZZX: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := X;
end;

function TIntVector3.GetZZY: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := Y;
end;

function TIntVector3.GetZZZ: TIntVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := Z;
end;

procedure TIntVector3.SetXYZ(const Value: TIntVector3);
begin
  X := Value.X;
  Y := Value.Y;
  Z := Value.Z;
end;

procedure TIntVector3.SetXZY(const Value: TIntVector3);
begin
  X := Value.X;
  Z := Value.Y;
  Y := Value.Z;
end;

procedure TIntVector3.SetYXZ(const Value: TIntVector3);
begin
  Y := Value.X;
  X := Value.Y;
  Z := Value.Z;
end;

procedure TIntVector3.SetYZX(const Value: TIntVector3);
begin
  Y := Value.X;
  Z := Value.Y;
  X := Value.Z;
end;

procedure TIntVector3.SetZXY(const Value: TIntVector3);
begin
  Z := Value.X;
  X := Value.Y;
  Y := Value.Z;
end;

procedure TIntVector3.SetZYX(const Value: TIntVector3);
begin
  Z := Value.X;
  Y := Value.Y;
  X := Value.Z;
end;

{$ENDREGION}

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

class operator TIntVector3.In(A: Integer; const B: TIntVector3): Boolean;
begin
  Result := A in IBounds3(B);
end;

function TIntVector3.ToString: string;
begin
  Result := Format('[%d|%d|%d]', [X, Y, Z]);
end;

class operator TIntVector3.Implicit(const A: TIntVector3): string;
begin
  Result := A.ToString;
end;

function TIntVector3.Abs: TIntVector3;
begin
  Result.X := System.Abs(X);
  Result.Y := System.Abs(Y);
  Result.Z := System.Abs(Z);
end;

function TIntVector3.Min(const A: TIntVector3): TIntVector3;
begin
  Result.X := System.Math.Min(X, A.X);
  Result.Y := System.Math.Min(Y, A.Y);
  Result.Z := System.Math.Min(Z, A.Z);
end;

class operator TIntVector3.Modulus(const A, B: TIntVector3): TIntVector3;
begin
  Result.X := A.X mod B.X;
  Result.Y := A.Y mod B.Y;
  Result.Z := A.Z mod B.Z;
end;

function TIntVector3.Max(const A: TIntVector3): TIntVector3;
begin
  Result.X := System.Math.Max(X, A.X);
  Result.Y := System.Math.Max(Y, A.Y);
  Result.Z := System.Math.Max(Z, A.Z);
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
  Result.Create(A);
end;

function TIntBounds1.Length: Integer;
begin
  Result := C2 - C1;
end;

function TIntBounds1.Clamp(ARange: TIntBounds1): TIntBounds1;
begin
  Result.C1 := Max(C1, ARange.C1);
  Result.C2 := Min(C2, ARange.C2);
end;

function TIntBounds1.Center: Integer;
begin
  Result := (C1 + C2 - 1) div 2;
end;

function TIntBounds1.Clamp(AValue: Integer): Integer;
begin
  Result := EnsureRange(AValue, C1, C2 - 1);
end;

function TIntBounds1.RangedMod(AValue: Integer): Integer;

  function IntMod(const ANumerator, ADenominator: Integer): Integer;
  begin
    Result := ANumerator - Floor(ANumerator / ADenominator) * ADenominator;
  end;

begin
  Result := IntMod(AValue - C1, Length) + C1;
end;

function TIntBounds1.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TIntBounds1.Normalize: TIntBounds1;
begin
  if Normalized then
    Exit(Self);
  Result.C1 := C2;
  Result.C2 := C1;
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

class operator TIntBounds1.In(const A, B: TIntBounds1): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TIntBounds1.In(A: Integer; const B: TIntBounds1): Boolean;
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
  Result := Format('[%d, %d]', [C1, C2 - 1]);
end;

class operator TIntBounds1.Implicit(ABounds: TIntBounds1): string;
begin
  Result := ABounds.ToString;
end;

function TIntBounds1.GetEnumerator: TIntBounds1Iterator;
begin
  Result := TIntBounds1Iterator.Create(Self);
end;

{ TIntBounds2 }

function TIntBounds2.GetLineX: TIntBounds1;
begin
  Result.Create(C1.X, C2.X);
end;

function TIntBounds2.GetLineY: TIntBounds1;
begin
  Result.Create(C1.Y, C2.Y);
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
                                                            {
class operator TIntBounds2.Implicit(A: Integer): TIntBounds2;
begin
  Result.Create(A);
end;
                                                             }
class operator TIntBounds2.Implicit(A: TIntVector2): TIntBounds2;
begin
  Result.Create(A);
end;

function TIntBounds2.Size: TIntVector2;
begin
  Result := C2 - C1;
end;

function TIntBounds2.Area: Integer;
begin
  Result := Width * Height;
end;

function TIntBounds2.Width: Integer;
begin
  Result := LineX.Length;
end;

function TIntBounds2.Height: Integer;
begin
  Result := LineY.Length;
end;

function TIntBounds2.Clamp(ARange: TIntBounds2): TIntBounds2;
begin
  Result.LineX := LineX.Clamp(ARange.LineX);
  Result.LineY := LineY.Clamp(ARange.LineY);
end;

function TIntBounds2.Center: TIntVector2;
begin
  Result := (C1 + C2 - 1) div 2;
end;

function TIntBounds2.Clamp(AValue: TIntVector2): TIntVector2;
begin
  Result.X := LineX.Clamp(AValue.X);
  Result.Y := LineY.Clamp(AValue.Y);
end;

function TIntBounds2.RangedMod(AValue: TIntVector2): TIntVector2;
begin
  Result.X := LineX.RangedMod(AValue.X);
  Result.Y := LineY.RangedMod(AValue.Y);
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

function TIntBounds2.InReverse: TReverseWrapper;
begin
  Result := PReverseWrapper(@Self)^;
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

class operator TIntBounds2.In(const A, B: TIntBounds2): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TIntBounds2.In(A: TIntVector2; const B: TIntBounds2): Boolean;
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
  Result := Format('[%s, %s]', [C1.ToString, (C2 - 1).ToString]);
end;

class operator TIntBounds2.Implicit(ABounds: TIntBounds2): string;
begin
  Result := ABounds.ToString;
end;

function TIntBounds2.GetEnumerator: TIntBounds2Iterator;
begin
  Result := TIntBounds2Iterator.Create(Self);
end;

{ TIntBounds3 }

function TIntBounds3.GetLineX: TIntBounds1;
begin
  Result.Create(C1.X, C2.X);
end;

function TIntBounds3.GetLineY: TIntBounds1;
begin
  Result.Create(C1.Y, C2.Y);
end;

function TIntBounds3.GetLineZ: TIntBounds1;
begin
  Result.Create(C1.Z, C2.Z);
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
  Result.Create(A);
end;

function TIntBounds3.Size: TIntVector3;
begin
  Result.Create(Width, Height, Depth);
end;

function TIntBounds3.Volume: Integer;
begin
  Result := Width * Height * Depth;
end;

function TIntBounds3.Width: Integer;
begin
  Result := LineX.Length;
end;

function TIntBounds3.Height: Integer;
begin
  Result := LineY.Length;
end;

function TIntBounds3.Depth: Integer;
begin
  Result := LineZ.Length;
end;

function TIntBounds3.Clamp(ARange: TIntBounds3): TIntBounds3;
begin
  Result.LineX := LineX.Clamp(ARange.LineX);
  Result.LineY := LineY.Clamp(ARange.LineY);
  Result.LineZ := LineZ.Clamp(ARange.LineZ);
end;

function TIntBounds3.Center: TIntVector3;
begin
  Result := (C1 + C2 - 1) div 2;
end;

function TIntBounds3.Clamp(AValue: TIntVector3): TIntVector3;
begin
  Result.X := LineX.Clamp(AValue.X);
  Result.Y := LineY.Clamp(AValue.Y);
  Result.Z := LineZ.Clamp(AValue.Z);
end;

function TIntBounds3.RangedMod(AValue: TIntVector3): TIntVector3;
begin
  Result.X := LineX.RangedMod(AValue.X);
  Result.Y := LineX.RangedMod(AValue.Y);
  Result.Z := LineX.RangedMod(AValue.Z);
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

class operator TIntBounds3.In(const A, B: TIntBounds3): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TIntBounds3.In(const A: TIntVector3; const B: TIntBounds3): Boolean;
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
  Result := Format('[%s, %s]', [C1.ToString, (C2 - 1).ToString]);
end;

class operator TIntBounds3.Implicit(ABounds: TIntBounds3): string;
begin
  Result := ABounds.ToString;
end;

function TIntBounds3.GetEnumerator: TIntBounds3Iterator;
begin
  Result := TIntBounds3Iterator.Create(Self);
end;

{ TIntBounds1Iterator }

constructor TIntBounds1Iterator.Create(const ABounds: TIntBounds1);
begin
  FCurrent := ABounds.C1 - 1;
  FEnd := ABounds.C2;
end;

function TIntBounds1Iterator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < FEnd;
end;

{ TIntBounds2Iterator }

constructor TIntBounds2Iterator.Create(const ABounds: TIntBounds2);
begin
  FCurrent.X := ABounds.C1.X - 1;
  FCurrent.Y := ABounds.C1.Y;
  FC1 := ABounds.C1;
  FC2 := ABounds.C2;
end;

function TIntBounds2Iterator.MoveNext: Boolean;
begin
  Inc(FCurrent.X);
  if FCurrent.X >= FC2.X then
  begin
    FCurrent.X := FC1.X;
    Inc(FCurrent.Y);
  end;
  Result := FCurrent.Y < FC2.Y;
end;

{ TIntBounds3Iterator }

constructor TIntBounds3Iterator.Create(const ABounds: TIntBounds3);
begin
  FCurrent.X := ABounds.C1.X - 1;
  FCurrent.Y := ABounds.C1.Y;
  FCurrent.Z := ABounds.C1.Z;
  FC1 := ABounds.C1;
  FC2 := ABounds.C2;
end;

function TIntBounds3Iterator.MoveNext: Boolean;
begin
  Inc(FCurrent.X);
  if FCurrent.X >= FC2.X then
  begin
    FCurrent.X := FC1.X;
    Inc(FCurrent.Y);
    if FCurrent.Y >= FC2.Y then
    begin
      FCurrent.Y := FC1.Y;
      Inc(FCurrent.Z);
    end;
  end;
  Result := FCurrent.Z < FC2.Z;
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

function IBounds1(A, B: Integer): TIntBounds1;
begin
  Result.Create(A, B);
end;

function IBounds1I(A, B: Integer): TIntBounds1;
begin
  Result.Create(A, B + 1);
end;

function IBounds1(A: Integer): TIntBounds1;
begin
  Result.Create(0, A);
end;

function IBounds2(A, B: TIntVector2): TIntBounds2;
begin
  Result.Create(A, B);
end;

function IBounds2I(A, B: TIntVector2): TIntBounds2;
begin
  Result.Create(A, B + 1);
end;

function IBounds2(A: TIntVector2): TIntBounds2;
begin
  Result.Create(0, A);
end;

function IBounds3(A, B: TIntVector3): TIntBounds3;
begin
  Result.Create(A, B);
end;

function IBounds3I(A, B: TIntVector3): TIntBounds3;
begin
  Result.Create(A, B + 1);
end;

function IBounds3(A: TIntVector3): TIntBounds3;
begin
  Result.Create(0, A);
end;

{ TIntVector2Helper }

function TIntVector2Helper.Bounds(ASize: TIntVector2): TIntBounds2;
begin
  Result.Create(Self, Self + ASize);
end;

{ TIntVector3Helper }

function TIntVector3Helper.Bounds(ASize: TIntVector3): TIntBounds3;
begin
  Result.Create(Self, Self + ASize);
end;

{ TIntBounds2.TReverseWrapper }

function TIntBounds2.TReverseWrapper.GetEnumerator: TIntBounds2ReverseIterator;
begin
  Result := TIntBounds2ReverseIterator.Create(PIntBounds2(@Self)^);
end;

end.
