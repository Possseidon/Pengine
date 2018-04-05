unit Pengine.Vector;

{$POINTERMATH ON}
{$EXCESSPRECISION OFF}

interface

{ TODO 3 -oPossseidon -cFunction :
  Line Intsec with other Line as normal for Plane }

{ TODO 3 -oPossseidon -cFunction :
  Height function for TPlane3 and TLine2, that can return negative results. See/use what is in PointSide in TLine2 }

{ TODO 4 -oPossseidon -cInline :
  Inline all functions, which call exactly one other function }

{ TODO 3 -oPossseidon -cInline :
  Add various inline to functions with different compiler switches. }

{ TODO 5 -oPossseidon -cXmlDoc :
  TLocation3 XmlDoc }

{ TODO 3 -oPossseidon -cClass :
  Code TAxisSystem. }

{ TODO 5 -oPossseidon -cXmlDoc :
  TBasicAxisDir XmlDoc }

{ TODO 3 -oPossseidon -cClass :
  Code TBlock2Raycaster }

{ TODO 4 -oPossseidon -cConst :
  Add const to correct arguments }

uses
  System.Math,
  System.SysUtils,

  Pengine.EventHandling,
  Pengine.IntMaths,
  Pengine.Matrix,
  Pengine.CollectionInterfaces,
  Pengine.Utility;

type

  /// <summary>Only one direction per axis is allowed in a TBasicAxisSystem.</summary>
  EAxisSystemError = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>A two component vector of type <see cref="System|Single"/>.</summary>
  TVector2 = record
  private
    function GetComponent(AAxis: TCoordAxis2): Single;
    procedure SetComponent(AAxis: TCoordAxis2; const Value: Single);

    function GetXX: TVector2;
    function GetXY: TVector2;
    function GetYX: TVector2;
    function GetYY: TVector2;

    procedure SetXY(const Value: TVector2);
    procedure SetYX(const Value: TVector2);

  public
    /// <summary>The X-Component of the vector.</summary>
    X: Single;
    /// <summary>The Y-Component of the vector.</summary>
    Y: Single;

    /// <summary>Texture coordinate alias for X.</summary>
    property S: Single read X write X;
    /// <summary>Texture coordinate alias for Y.</summary>
    property T: Single read Y write Y;
    /// <summary>Texture coordinate alias for X.</summary>
    property U: Single read X write X;
    /// <summary>Texture coordinate alias for Y.</summary>
    property V: Single read Y write Y;

    /// <remarks>Allows simple array-like access of each component, thanks to it being a default property.</remarks>
    property Component[AAxis: TCoordAxis2]: Single read GetComponent write SetComponent; default;

    /// <summary>Creates a <see cref="Pengine.Vector|TVector2"/> with the specified components.</summary>
    constructor Create(X, Y: Single); overload;
    /// <summary>Creates a <see cref="Pengine.Vector|TVector2"/> with both components being the same, given value.</summary>
    constructor Create(V: Single); overload;

    /// <returns>A <see cref="Pengine.Vector|TVector2"/>, with a specified <see cref="Pengine.Vector|TVector2.Slope"/>.</returns>
    class function FromSlope(ASlope: Single): TVector2; static;

    /// <returns>A normlaized <see cref="Pengine.Vector|TVector2"/>, with a specified angle in radians.</returns>
    class function FromAngleRad(AAngle: Single): TVector2; static;
    /// <returns>A normlaized <see cref="Pengine.Vector|TVector2"/>, with a specified angle in degrees.</returns>
    class function FromAngle(AAngle: Single): TVector2; static;

    /// <returns>A <see cref="Pengine.Vector|TVector2"/>, with each component being a random value in the interval: <c>[0, 1)</c></returns>
    class function Random: TVector2; static;
    /// <returns>A <see cref="Pengine.Vector|TVector2"/>, with each component being a random value in the interval: <c>[-1, 1)</c></returns>
    class function RandomBox: TVector2; static;
    /// <returns>A normalized <see cref="Pengine.Vector|TVector2"/>, pointing into a random direction.</returns>
    /// <remarks>The probability is homogeneous.</remarks>
    class function RandomNormal: TVector2; static;

    class operator Implicit(V: Single): TVector2;
    class operator Implicit(const AVector: TIntVector2): TVector2;

    {$REGION 'All versions of rearrangement TVector2'}

    property XX: TVector2 read GetXX;
    property XY: TVector2 read GetXY write SetXY;
    property YX: TVector2 read GetYX write SetYX;
    property YY: TVector2 read GetYY;

    {$ENDREGION}

    class operator Add(const A, B: TVector2): TVector2;
    class operator Subtract(const A, B: TVector2): TVector2;
    class operator Multiply(const A, B: TVector2): TVector2;
    class operator Divide(const A, B: TVector2): TVector2;

    class operator Multiply(const A: TMatrix2; const B: TVector2): TVector2;
    class operator Multiply(const A: TVector2; const B: TMatrix2): TVector2;

    class operator Positive(const A: TVector2): TVector2; inline;
    class operator Negative(const A: TVector2): TVector2;

    class operator Equal(const A, B: TVector2): Boolean;
    class operator NotEqual(const A, B: TVector2): Boolean;
    class operator LessThan(const A, B: TVector2): Boolean;
    class operator LessThanOrEqual(const A, B: TVector2): Boolean;
    class operator GreaterThan(const A, B: TVector2): Boolean;
    class operator GreaterThanOrEqual(const A, B: TVector2): Boolean;

    /// <returns>A string representative in the form: <c>[X|Y]</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(const A: TVector2): string; inline;

    /// <returns>The length of the vector.</returns>
    function Length: Single; inline;
    /// <returns>The normalized vector.</returns>
    function Normalize: TVector2; inline;

    /// <returns>The distance to the specified vector.</returns>
    function DistanceTo(const A: TVector2): Single;
    /// <returns>A vector, pointing from the calling to the specified vector</returns>
    function VectorTo(const A: TVector2): TVector2; inline;

    /// <returns>A vector, which is Rotate by 90° counter-clockwise.</returns>
    function Cross: TVector2;
    /// <returns>The dot-product with another vector.</returns>
    function Dot(const A: TVector2): Single;
    /// <returns>The dot-product with the vector itself.</returns>
    /// <remarks>See <see cref="Pengine.Vector|TVector2.Dot"/>. Equal to: <c>A.Dot(A)</c></remarks>
    function SqrDot: Single;

    /// <returns>The cosine of the angle between the calling and given vector.</returns>
    function CosAngleTo(const A: TVector2): Single;
    /// <returns>The angle between the calling and given vector in radians.</returns>
    /// <remarks>Use <c>A.CosAngleTo(B)</c> instead of <c>Cos(A.AngleRadTo(B))</c> as it is faster.
    /// <p>See: <see cref="Pengine.Vector|TVector2.CosAngleTo"/></p></remarks>
    function AngleRadTo(const A: TVector2): Single;
    /// <returns>The angle between the calling and the given vector in degrees.</returns>
    function AngleTo(const A: TVector2): Single; inline;
    /// <returns>The by the given radian angle counter-clockwise Rotate vector.</returns>
    function RotateRad(AAngle: Single): TVector2;
    /// <returns>The by the given degree angle counter-clockwise Rotate vector.</returns>
    function Rotate(AAngle: Single): TVector2;

    /// <returns>The vector, with each negative component being positive.</returns>
    function Abs: TVector2;
    /// <returns>The vector, with each component being rounded down.</returns>
    function Floor: TIntVector2;
    /// <returns>The vector, with each component being rounded up.</returns>
    function Ceil: TIntVector2;
    /// <returns>A vector with the smaller components of both vectors.</returns>
    function Min(const A: TVector2): TVector2;
    /// <returns>A vector with the greater components of both vectors.</returns>
    function Max(const A: TVector2): TVector2;

    /// <returns>A set of all basic directions, that the vector is pointing to.</returns>
    /// <remarks>Therefore bdLeft and bdRight etc. are mutually exclusive, as each axis can have only one direction.</remarks>
    function Dirs: TBasicDirs2;

    /// <returns>The slope of the line:<code>
    /// X | Y | slope <p/>
    /// --|---|-------<p/>
    /// 1 | 0 |     0 <p/>
    /// 1 | 1 |     1 <p/>
    /// 0 | 1 |  +inf
    /// </code></returns>
    /// <remarks>Make sure to check, if X &lt; 0, which means that the line is flipped.</remarks>
    function Slope: Single;

  end;

  /// <summary>A three component vector of type <see cref="System|Single"/>.</summary>
  TVector3 = record
  private
    function GetComponent(AAxis: TCoordAxis3): Single;
    procedure SetComponent(AAxis: TCoordAxis3; const Value: Single);

    function GetXX: TVector2;
    function GetXY: TVector2;
    function GetXZ: TVector2;
    function GetYX: TVector2;
    function GetYY: TVector2;
    function GetYZ: TVector2;
    function GetZX: TVector2;
    function GetZY: TVector2;
    function GetZZ: TVector2;

    function GetXXX: TVector3;
    function GetXXY: TVector3;
    function GetXXZ: TVector3;
    function GetXYX: TVector3;
    function GetXYY: TVector3;
    function GetXYZ: TVector3;
    function GetXZX: TVector3;
    function GetXZY: TVector3;
    function GetXZZ: TVector3;
    function GetYXX: TVector3;
    function GetYXY: TVector3;
    function GetYXZ: TVector3;
    function GetYYX: TVector3;
    function GetYYY: TVector3;
    function GetYYZ: TVector3;
    function GetYZX: TVector3;
    function GetYZY: TVector3;
    function GetYZZ: TVector3;
    function GetZXX: TVector3;
    function GetZXY: TVector3;
    function GetZXZ: TVector3;
    function GetZYX: TVector3;
    function GetZYY: TVector3;
    function GetZYZ: TVector3;
    function GetZZX: TVector3;
    function GetZZY: TVector3;
    function GetZZZ: TVector3;

    procedure SetXY(const Value: TVector2);
    procedure SetXZ(const Value: TVector2);
    procedure SetYX(const Value: TVector2);
    procedure SetYZ(const Value: TVector2);
    procedure SetZX(const Value: TVector2);
    procedure SetZY(const Value: TVector2);

    procedure SetXYZ(const Value: TVector3);
    procedure SetXZY(const Value: TVector3);
    procedure SetYXZ(const Value: TVector3);
    procedure SetYZX(const Value: TVector3);
    procedure SetZXY(const Value: TVector3);
    procedure SetZYX(const Value: TVector3);

  public
    /// <summary>The X-Component of the vector.</summary>
    X: Single;
    /// <summary>The Y-Component of the vector.</summary>
    Y: Single;
    /// <summary>The Z-Component of the vector.</summary>
    Z: Single;

    /// <summary>Texture coordinate alias for X.</summary>
    property S: Single read X write X;
    /// <summary>Texture coordinate alias for Y.</summary>
    property T: Single read Y write Y;
    /// <summary>Texture coordinate alias for Z.</summary>
    property U: Single read Z write Z;

    /// <remarks>Allows simple array-like access of each component, thanks to it being a default property.</remarks>
    property Component[AAxis: TCoordAxis3]: Single read GetComponent write SetComponent; default;

    /// <summary>Creates a <see cref="Pengine.Vector|TVector3"/> with the specified components.</summary>
    constructor Create(X, Y, Z: Single); overload;
    /// <summary>Creates a <see cref="Pengine.Vector|TVector3"/> with all components being the same, given value.</summary>
    constructor Create(V: Single); overload;

    /// <returns>A <see cref="Pengine.Vector|TVector3"/>, with each component being a random value in the interval: <c>[0, 1)</c></returns>
    class function Random: TVector3; static;
    /// <returns>A <see cref="Pengine.Vector|TVector3"/>, with each component being a random value in the interval: <c>[-1, 1)</c></returns>
    class function RandomBox: TVector3; static;
    /// <returns>A normalized <see cref="Pengine.Vector|TVector3"/>, pointing into a random direction.</returns>
    /// <remarks>The probability is homogeneous.</remarks>
    class function RandomNormal: TVector3; static;

    class operator Implicit(V: Single): TVector3;
    class operator Implicit(const A: TIntVector3): TVector3;

    {$REGION 'All versions of rearrangement TIntVector2'}

    property XX: TVector2 read GetXX;
    property XY: TVector2 read GetXY write SetXY;
    property XZ: TVector2 read GetXZ write SetXZ;
    property YX: TVector2 read GetYX write SetYX;
    property YY: TVector2 read GetYY;
    property YZ: TVector2 read GetYZ write SetYZ;
    property ZX: TVector2 read GetZX write SetZX;
    property ZY: TVector2 read GetZY write SetZY;
    property ZZ: TVector2 read GetZZ;

    {$ENDREGION}
    {$REGION 'All versions of rearrangement TIntVector3'}

    property XXX: TVector3 read GetXXX;
    property XXY: TVector3 read GetXXY;
    property XXZ: TVector3 read GetXXZ;
    property XYX: TVector3 read GetXYX;
    property XYY: TVector3 read GetXYY;
    property XYZ: TVector3 read GetXYZ write SetXYZ;
    property XZX: TVector3 read GetXZX;
    property XZY: TVector3 read GetXZY write SetXZY;
    property XZZ: TVector3 read GetXZZ;
    property YXX: TVector3 read GetYXX;
    property YXY: TVector3 read GetYXY;
    property YXZ: TVector3 read GetYXZ write SetYXZ;
    property YYX: TVector3 read GetYYX;
    property YYY: TVector3 read GetYYY;
    property YYZ: TVector3 read GetYYZ;
    property YZX: TVector3 read GetYZX write SetYZX;
    property YZY: TVector3 read GetYZY;
    property YZZ: TVector3 read GetYZZ;
    property ZXX: TVector3 read GetZXX;
    property ZXY: TVector3 read GetZXY write SetZXY;
    property ZXZ: TVector3 read GetZXZ;
    property ZYX: TVector3 read GetZYX write SetZYX;
    property ZYY: TVector3 read GetZYY;
    property ZYZ: TVector3 read GetZYZ;
    property ZZX: TVector3 read GetZZX;
    property ZZY: TVector3 read GetZZY;
    property ZZZ: TVector3 read GetZZZ;

    {$ENDREGION}

    class operator Add(const A, B: TVector3): TVector3;
    class operator Subtract(const A, B: TVector3): TVector3;
    class operator Multiply(const A, B: TVector3): TVector3;
    class operator Divide(const A, B: TVector3): TVector3;

    class operator Multiply(const A: TMatrix3; const B: TVector3): TVector3;
    class operator Multiply(const A: TVector3; const B: TMatrix3): TVector3;

    class operator Multiply(const A: TMatrix4; const B: TVector3): TVector3;
    class operator Multiply(const A: TVector3; const B: TMatrix4): TVector3;

    class operator Positive(const A: TVector3): TVector3; inline;
    class operator Negative(const A: TVector3): TVector3;

    class operator Equal(const A, B: TVector3): Boolean;
    class operator NotEqual(const A, B: TVector3): Boolean;
    class operator LessThan(const A, B: TVector3): Boolean;
    class operator LessThanOrEqual(const A, B: TVector3): Boolean;
    class operator GreaterThan(const A, B: TVector3): Boolean;
    class operator GreaterThanOrEqual(const A, B: TVector3): Boolean;

    /// <returns>A string representative in the form: <c>[X|Y|Z]</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(const A: TVector3): string; inline;

    /// <returns>The length of the vector.</returns>
    function Length: Single; inline;
    /// <returns>The normalized vector.</returns>
    function Normalize: TVector3; inline;

    /// <returns>The distance to the specified vector.</returns>
    function DistanceTo(const A: TVector3): Single;
    /// <returns>A vector, pointing from the calling to the specified vector</returns>
    function VectorTo(const A: TVector3): TVector3; inline;

    /// <returns>The cross-product with another vector.</returns>
    /// <remarks>Left-Hand-Rule: <c>middle x pointing = thumb</c></remarks>
    function Cross(const A: TVector3): TVector3;
    /// <returns>The dot-product with another vector.</returns>
    function Dot(const A: TVector3): Single;
    /// <returns>The dot-product with the vector itself.</returns>
    /// <remarks>See <see cref="Pengine.Vector|TVector3.Dot"/>. Equal to: <c>A.Dot(A)</c></remarks>
    function SqrDot: Single;

    /// <returns>The cosine of the angle between the calling and given vector.</returns>
    function CosAngleTo(const A: TVector3): Single;
    /// <returns>The angle between the calling and given vector in radians.</returns>
    /// <remarks>Use <c>A.CosAngleTo(B)</c> instead of <c>Cos(A.AngleRadTo(B))</c> as it is faster.
    /// <p>See: <see cref="Pengine.Vector|TVector3.CosAngleTo"/></p></remarks>
    function AngleRadTo(const A: TVector3): Single;
    /// <returns>The angle between the calling and the given vector in degrees.</returns>
    function AngleTo(const A: TVector3): Single; inline;
    /// <returns>The vector Rotate around the specified vector using the given angle in radians.</returns>
    /// <remarks>
    /// If the specified vector points towards you, the rotation is counter-clockwise.<p/>
    /// /!\ The given vector must be normalized.
    /// </remarks>
    function RotateRad(const AAxis: TVector3; AAngle: Single): TVector3;
    /// <returns>The vector Rotate around the specified vector using the given angle in degrees.</returns>
    /// <remarks>
    /// If the specified vector points towards you, the rotation is counter-clockwise.<p/>
    /// /!\ The given vector must be normalized.
    /// </remarks>
    function Rotate(const AAxis: TVector3; AAngle: Single): TVector3;
    /// <returns>A reflected vector. The given vector is used as a normal for the reflection-plane.</returns>
    function Reflect(const A: TVector3): TVector3;

    /// <returns>The vector, with each negative component being positive.</returns>
    function Abs: TVector3;
    /// <returns>The vector, with each component being rounded down.</returns>
    function Floor: TIntVector3;
    /// <returns>The vector, with each component being rounded up.</returns>
    function Ceil: TIntVector3;
    /// <returns>A vector with the smaller components of both vectors.</returns>
    function Min(const A: TVector3): TVector3;
    /// <returns>A vector with the greater components of both vectors.</returns>
    function Max(const A: TVector3): TVector3;

    /// <returns>A set of all basic directions, that the vector is pointing to.</returns>
    /// <remarks>Therefore bdLeft and bdRight etc. are mutually exclusive, as each axis can have only one direction.</remarks>
    function Dirs: TBasicDirs3;

    /// <returns>The component, which has the biggest absolute value.</returns>
    /// <remarks>If there are multiple choices, the function prefers X over Y and Y over Z.</remarks>
    function LongestAxis: TCoordAxis3;

  end;

  /// <summary>A four component vector of type <see cref="System|Single"/>.</summary>
  /// <remarks>Not really necessary, use <see cref="Pengine.Vector|TVector3"/> in most cases.</remarks>
  TVector4 = record
  public
    /// <summary>The X-Component of the vector</summary>
    X: Single;
    /// <summary>The Y-Component of the vector</summary>
    Y: Single;
    /// <summary>The Z-Component of the vector</summary>
    Z: Single;
    /// <summary>The W-Component of the vector</summary>
    W: Single;

    class operator Implicit(AVector: TVector4): TVector3;
  end;

  /// <summary>An alias for <see cref="Pengine.Vector|TVector2"/>.</summary>
  TTexCoord2 = TVector2;
  /// <summary>An alias for <see cref="Pengine.Vector|TVector3"/>.</summary>
  TTexCoord3 = TVector3;

  /// <summary>
  /// Represents 1-Dimensional bounds using two <see cref="System|Single"/>.
  /// <p>Shorthand constructor using: <see cref="Pengine.Vector|Bounds1"/></p>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized: <c>C1 &lt;= C2</c><p/>
  /// The <c>in</c>-operator is inclusive: <c>C1 &lt;= A &lt;= C2</c>
  /// </remarks>
  TBounds1 = record
  public const

    CornerCount = 2;

  public type

    TCornerIndex = 0 .. CornerCount - 1;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X<p/>
    /// [0]   (0)<p/>
    /// [1]   (1)<p/>
    /// </code></summary>
    TCorners = array [TCornerIndex] of Single;

  private
    function GetPoint(APos: Single): Single;
    function GetPointSym(APos: Single): Single;
    function GetPointLeft(APos: Single): Single;

    function GetInvPoint(APos: Single): Single;
    function GetInvPointSym(APos: Single): Single;
    function GetInvPointLeft(APos: Single): Single;

    function GetCorner(AIndex: TCornerIndex): Single;
    procedure SetCorner(AIndex: TCornerIndex; const Value: Single);

  public
    /// <summary>The (usually) lower value of the bounds.</summary>
    C1: Single;
    /// <summary>The (usually) higher value of the bounds.</summary>
    C2: Single;

    /// <summary>Alias for <see cref="Pengine.Vector|TBounds1.C1"/>.</summary>
    property Low: Single read C1 write C1;
    /// <summary>Alias for <see cref="Pengine.Vector|TBounds1.C2"/>.</summary>
    property High: Single read C2 write C2;

    /// <returns>An array of both corners using <see cref="Pengine.Vector|TBounds1.TCorners"/>.</returns>
    function GetCorners: TCorners;
    /// <summary>Gives direct access to each corner.<p>See <see cref="Pengine.Vector|TBounds1.TCorners"/>.</p></summary>
    // LOL! corner index 0/1 and point 0.0/1.0 is the same!
    property Point[AIndex: TCornerIndex]: Single read GetCorner write SetCorner; default;

    /// <summary>Creates a <see cref="Pengine.Vector|TBounds1"/> with the specified range.</summary>
    constructor Create(AC1, AC2: Single); overload;
    /// <summary>Creates a <see cref="Pengine.Vector|TBounds1"/> with both bounds laying on the same, given value.</summary>
    constructor Create(A: Single); overload;

    class operator Implicit(A: Single): TBounds1;
    class operator Implicit(A: TIntBounds1): TBounds1;

    /// <returns>The difference between C1 and C2.</returns>
    /// <remarks>Will give a negative length for non-normalized bounds.</remarks>
    function Length: Single; inline;

    /// <returns>The center point between C1 and C2.</returns>
    function Center: Single;

    /// <summary>Converts from <c>[0, 1]</c> to <c>[C1, C2]</c>.</summary>
    /// <remarks>Default property.<p/>
    /// For inverse operation see <see cref="VectorGeometry|TBounds1.InvPoint"/>.</remarks>
    property Point[APos: Single]: Single read GetPoint; default;
    /// <summary>Converts from <c>[-1, +1]</c> to <c>[C1, C2]</c></summary>
    /// <remarks>For inverse operation see <see cref="VectorGeometry|TBounds1.InvPointSym"/>.</remarks>
    property PointSym[APos: Single]: Single read GetPointSym;
    /// <summary>Converts from <c>[-1, 0]</c> to <c>[C1, C2]</c></summary>
    /// <remarks>For inverse operation see <see cref="VectorGeometry|TBounds1.InvPointLeft"/>.</remarks>
    property PointLeft[APos: Single]: Single read GetPointLeft;

    /// <summary>Converts from <c>[C1, C2]</c> to <c>[0, 1]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPoint[APos: Single]: Single read GetInvPoint;
    /// <summary>Converts from <c>[C1, C2]</c> to <c>[-1, +1]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPointSym[APos: Single]: Single read GetInvPointSym;
    /// <summary>Converts from <c>[C1, C2]</c> to <c>[-1, 0]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPointLeft[APos: Single]: Single read GetInvPointLeft;

    /// <returns>Converts from <c>[C1, C2]</c> to specified bounds <c>[C1, C2]</c></returns>
    function Convert(APoint: Single; const ABounds: TBounds1): Single;

    /// <returns>The given bounds clamped to be inside of the calling bounds.</returns>
    function Clamp(ARange: TBounds1): TBounds1; overload;
    /// <returns>The given value being clamped to the bounds <c>[C1, C2].</c></returns>
    function Clamp(AValue: Single): Single; overload;

    /// <returns>The given value in the interval: <c>[C1..C2)</c></returns>
    function RangedModL(AValue: Single): Single;
    /// <returns>The given value in the interval: <c>(C1..C2]</c></returns>
    function RangedModR(AValue: Single): Single;

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TBounds1;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: Single): TBounds1;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: Single): TBounds1;

    /// <returns>THe bounds with all values rounded down.</returns>
    function Floor: TIntBounds1;
    /// <returns>THe bounds with all values rounded up.</returns>
    function Ceil: TIntBounds1;

    class operator Add(const A, B: TBounds1): TBounds1;
    class operator Subtract(const A, B: TBounds1): TBounds1;
    class operator Multiply(const A, B: TBounds1): TBounds1;
    class operator Divide(const A, B: TBounds1): TBounds1;

    // The in operator is inclusive, comparing with <= and >=
    class operator in (const A, B: TBounds1): Boolean;
    class operator in (A: Single; const B: TBounds1): Boolean;

    class operator Equal(const A, B: TBounds1): Boolean;
    class operator NotEqual(const A, B: TBounds1): Boolean;
    class operator GreaterThan(const A, B: TBounds1): Boolean;
    class operator GreaterThanOrEqual(const A, B: TBounds1): Boolean;
    class operator LessThan(const A, B: TBounds1): Boolean;
    class operator LessThanOrEqual(const A, B: TBounds1): Boolean;

    /// <returns>A string representative in the form: <c>&lt;C1~C2&gt;</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(const ABounds: TBounds1): string; inline;

  end;

  /// <summary>
  /// Represents 2-Dimensional bounds using two <see cref="Pengine.Vector|TVector2"/>.
  /// <p>Shorthand constructor using: <see cref="Pengine.Vector|Bounds2"/></p>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized: <c>C1 &lt;= C2</c><p/>
  /// The <c>in</c>-operator is inclusive: <c>C1 &lt;= A &lt;= C2</c>
  /// </remarks>
  TBounds2 = record
  public const

    CornerCount = 4;

  public type

    TCornerIndex = 0 .. CornerCount - 1;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X, Y<p/>
    /// [0]   (0, 0)<p/>
    /// [1]   (1, 0)<p/>
    /// [2]   (0, 1)<p/>
    /// [3]   (1, 1)<p/>
    /// </code></summary>
    TCorners = array [TCornerIndex] of TVector2;

  private
    function GetPoint(Value: TVector2): TVector2;
    function GetPointSym(Value: TVector2): TVector2;
    function GetPointLeft(APos: TVector2): TVector2;

    function GetInvPoint(Value: TVector2): TVector2;
    function GetInvPointSym(Value: TVector2): TVector2;
    function GetInvPointLeft(APos: TVector2): TVector2;

    function GetLineX: TBounds1; inline;
    function GetLineY: TBounds1; inline;

    procedure SetLineX(const Value: TBounds1); inline;
    procedure SetLineY(const Value: TBounds1); inline;

    function GetCorner(AIndex: TCornerIndex): TVector2;
    procedure SetCorner(AIndex: TCornerIndex; const Value: TVector2);

  public
    /// <summary>The (usually) lower values of the bounds.</summary>
    C1: TVector2;
    /// <summary>The (usually) higher values of the bounds.</summary>
    C2: TVector2;

    /// <summary>Alias for <see cref="Pengine.Vector|TBounds2.C1"/>.</summary>
    property Low: TVector2 read C1 write C1;
    /// <summary>Alias for <see cref="Pengine.Vector|TBounds2.C2"/>.</summary>
    property High: TVector2 read C2 write C2;

    /// <returns>An array of all corners using <see cref="Pengine.Vector|TBounds2.TCorners"/>.</returns>
    function GetCorners: TCorners;
    /// <summary>Gives direct access to each corner.<p>See <see cref="Pengine.Vector|TBounds2.TCorners"/>.</p></summary>
    property Point[AIndex: TCornerIndex]: TVector2 read GetCorner write SetCorner; default;

    /// <summary>Creates a <see cref="Pengine.Vector|TBounds2"/> with the specified range.</summary>
    constructor Create(AC1, AC2: TVector2); overload;
    /// <summary>Creates a <see cref="Pengine.Vector|TBounds2"/> with both bounds laying on the same, given value.</summary>
    constructor Create(A: TVector2); overload;

    class operator Implicit(A: Single): TBounds2; inline;
    class operator Implicit(A: TVector2): TBounds2; inline;
    class operator Implicit(A: TIntBounds2): TBounds2; inline;

    /// <returns>The difference between C1 and C2.</returns>
    /// <remarks>Will give negative values for non-normalized bounds.</remarks>
    function Size: TVector2; inline;
    /// <returns>The area of the bounds.</returns>
    function Area: Single; inline;

    /// <summary>Resembles both X-Components of the bounds as a <see cref="Pengine.Vector|TBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineX: TBounds1 read GetLineX write SetLineX;
    /// <summary>Resembles both Y-Components of the bounds as a <see cref="Pengine.Vector|TBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineY: TBounds1 read GetLineY write SetLineY;

    /// <returns>The horizontal length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Width: Single; inline;
    /// <returns>The vertical length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Height: Single; inline;

    /// <returns>The center point between C1 and C2.</returns>
    function Center: TVector2;

    /// <summary>Converts from <c>[0, 1]</c> to <c>[C1, C2]</c>.</summary>
    /// <remarks>Default property.<p/>
    /// For inverse operation see <see cref="VectorGeometry|TBounds2.InvPoint"/>.</remarks>
    property Point[APos: TVector2]: TVector2 read GetPoint; default;
    /// <summary>Converts from <c>[-1, +1]</c> to <c>[C1, C2]</c></summary>
    /// <remarks>For inverse operation see <see cref="VectorGeometry|TBounds2.InvPointSym"/>.</remarks>
    property PointSym[APos: TVector2]: TVector2 read GetPointSym;
    /// <summary>Converts from <c>[-1, 0]</c> to <c>[C1, C2]</c></summary>
    /// <remarks>For inverse operation see <see cref="VectorGeometry|TBounds2.InvPointLeft"/>.</remarks>
    property PointLeft[APos: TVector2]: TVector2 read GetPointLeft;

    /// <summary>Converts from <c>[C1, C2]</c> to <c>[0, 1]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPoint[APos: TVector2]: TVector2 read GetInvPoint;
    /// <summary>Converts from <c>[C1, C2]</c> to <c>[-1, +1]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPointSym[APos: TVector2]: TVector2 read GetInvPointSym;
    /// <summary>Converts from <c>[C1, C2]</c> to <c>[-1, 0]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPointLeft[APos: TVector2]: TVector2 read GetInvPointLeft;

    /// <returns>Converts from <c>[C1, C2]</c> to specified bounds <c>[C1, C2].</c></returns>
    function Convert(APoint: TVector2; ABounds: TBounds2): TVector2;

    /// <returns>The given bounds clamped to be inside of the calling bounds.</returns>
    function Clamp(ARange: TBounds2): TBounds2; overload;
    /// <returns>The given value being clamped to the bounds <c>[C1, C2].</c></returns>
    function Clamp(AValue: TVector2): TVector2; overload;

    /// <returns>The given vector in the interval: <c>[C1..C2)</c></returns>
    function RangedModL(AValue: TVector2): TVector2;
    /// <returns>The given vector in the interval: <c>(C1..C2]</c></returns>
    function RangedModR(AValue: TVector2): TVector2;

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TBounds2;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: TVector2): TBounds2;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: TVector2): TBounds2;

    /// <returns>THe bounds with all values rounded down.</returns>
    function Floor: TIntBounds2;
    /// <returns>THe bounds with all values rounded up.</returns>
    function Ceil: TIntBounds2;

    class operator Add(const A, B: TBounds2): TBounds2;
    class operator Subtract(const A, B: TBounds2): TBounds2;
    class operator Multiply(const A, B: TBounds2): TBounds2;
    class operator Divide(const A, B: TBounds2): TBounds2;

    // The in operator is inclusive, comparing with <= and >=
    class operator in (const A, B: TBounds2): Boolean;
    class operator in (A: TVector2; const B: TBounds2): Boolean;

    class operator Equal(const A, B: TBounds2): Boolean;
    class operator NotEqual(const A, B: TBounds2): Boolean;
    class operator GreaterThan(const A, B: TBounds2): Boolean;
    class operator GreaterThanOrEqual(const A, B: TBounds2): Boolean;
    class operator LessThan(const A, B: TBounds2): Boolean;
    class operator LessThanOrEqual(const A, B: TBounds2): Boolean;

    /// <returns>A string representative in the form: <c>&lt;C1~C2&gt;</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TBounds2): string; inline;

  end;

  /// <summary>
  /// Represents 3-Dimensional bounds using two <see cref="Pengine.Vector|TVector3"/>.
  /// <p>Shorthand constructor using: <see cref="Pengine.Vector|Bounds3"/></p>
  /// </summary>
  /// <remarks>
  /// For better performance, most functions assume, that the bounds are normalized: <c>C1 &lt;= C2</c><para/>
  /// The <c>in</c>-operator is inclusive: <c>C1 &lt;= A &lt;= C2</c>
  /// </remarks>
  TBounds3 = record
  public const

    CornerCount = 8;

  public type

    TCornerIndex = 0 .. CornerCount - 1;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X, Y, Z<p/>
    /// [0]   (0, 0, 0)<p/>
    /// [1]   (1, 0, 0)<p/>
    /// [2]   (0, 1, 0)<p/>
    /// [3]   (1, 1, 0)<p/>
    /// [4]   (0, 0, 1)<p/>
    /// [5]   (1, 0, 1)<p/>
    /// [6]   (0, 1, 1)<p/>
    /// [7]   (1, 1, 1)
    /// </code></summary>
    TCorners = array [TCornerIndex] of TVector3;

  private
    function GetPoint(Value: TVector3): TVector3;
    function GetPointSym(Value: TVector3): TVector3;
    function GetPointLeft(APos: TVector3): TVector3;

    function GetInvPoint(Value: TVector3): TVector3;
    function GetInvPointSym(Value: TVector3): TVector3;
    function GetInvPointLeft(APos: TVector3): TVector3;

    function GetLineX: TBounds1; inline;
    function GetLineY: TBounds1; inline;
    function GetLineZ: TBounds1; inline;

    procedure SetLineX(const Value: TBounds1); inline;
    procedure SetLineY(const Value: TBounds1); inline;
    procedure SetLineZ(const Value: TBounds1); inline;

    function GetPlaneXY: TBounds2; inline;
    function GetPlaneYZ: TBounds2; inline;
    function GetPlaneZX: TBounds2; inline;
    function GetPlaneYX: TBounds2; inline;
    function GetPlaneZY: TBounds2; inline;
    function GetPlaneXZ: TBounds2; inline;

    procedure SetPlaneXY(const Value: TBounds2); inline;
    procedure SetPlaneYZ(const Value: TBounds2); inline;
    procedure SetPlaneZX(const Value: TBounds2); inline;
    procedure SetPlaneYX(const Value: TBounds2); inline;
    procedure SetPlaneZY(const Value: TBounds2); inline;
    procedure SetPlaneXZ(const Value: TBounds2); inline;

    function GetCorner(AIndex: TCornerIndex): TVector3;
    procedure SetCorner(AIndex: TCornerIndex; const Value: TVector3);

  public
    /// <summary>The (usually) lower values of the bounds.</summary>
    C1: TVector3;
    /// <summary>The (usually) higher values of the bounds.</summary>
    C2: TVector3;

    /// <summary>Alias for <see cref="Pengine.Vector|TBounds3.C1"/>.</summary>
    property Low: TVector3 read C1 write C1;
    /// <summary>Alias for <see cref="Pengine.Vector|TBounds3.C2"/>.</summary>
    property High: TVector3 read C2 write C2;

    /// <returns>An array of all corners using <see cref="Pengine.Vector|TBounds3.TCorners"/>.</returns>
    function GetCorners: TCorners;
    /// <summary>Gives direct access to each corner.<p>See <see cref="Pengine.Vector|TBounds3.TCorners"/>.</p></summary>
    property Point[AIndex: TCornerIndex]: TVector3 read GetCorner write SetCorner; default;

    /// <summary>Creates a <see cref="Pengine.Vector|TBounds3"/> with the specified range.</summary>
    constructor Create(AC1, AC2: TVector3); overload;
    /// <summary>Creates a <see cref="Pengine.Vector|TBounds3"/> with both bounds laying on the same, given value.</summary>
    constructor Create(A: TVector3); overload;

    class operator Implicit(A: Single): TBounds3; inline;
    class operator Implicit(A: TVector3): TBounds3; inline;
    class operator Implicit(A: TIntBounds1): TBounds3; inline;

    /// <returns>The difference between C1 and C2.</returns>
    /// <remarks>Will give negative values for non-normalized bounds.</remarks>
    function Size: TVector3; inline;
    /// <returns>The volume of the bounds.</returns>
    function Volume: Single; inline;

    /// <summary>Resembles both X-Components of the bounds as a <see cref="Pengine.Vector|TBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineX: TBounds1 read GetLineX write SetLineX;
    /// <summary>Resembles both Y-Components of the bounds as a <see cref="Pengine.Vector|TBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineY: TBounds1 read GetLineY write SetLineY;
    /// <summary>Resembles both Z-Components of the bounds as a <see cref="Pengine.Vector|TBounds1"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property LineZ: TBounds1 read GetLineZ write SetLineZ;

    /// <summary>Resembles an XY-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneXY: TBounds2 read GetPlaneXY write SetPlaneXY;
    /// <summary>Resembles an YZ-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneYZ: TBounds2 read GetPlaneYZ write SetPlaneYZ;
    /// <summary>Resembles an ZX-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneZX: TBounds2 read GetPlaneZX write SetPlaneZX;
    /// <summary>Resembles an YX-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneYX: TBounds2 read GetPlaneYX write SetPlaneYX;
    /// <summary>Resembles an ZY-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneZY: TBounds2 read GetPlaneZY write SetPlaneZY;
    /// <summary>Resembles an XZ-Plane of the bounds as a <see cref="Pengine.Vector|TBounds2"/>.</summary>
    /// <remarks>/!\ You cannot change the result directly, as it creates a copy of the values.</remarks>
    property PlaneXZ: TBounds2 read GetPlaneXZ write SetPlaneXZ;

    /// <returns>The horizontal length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Width: Single; inline;
    /// <returns>The vertical length of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Height: Single; inline;
    /// <returns>The depth of the bounds.</returns>
    /// <remarks>Gives a negative length for non-normalized bounds.</remarks>
    function Depth: Single; inline;

    /// <returns>The center point between C1 and C2.</remarks>
    function Center: TVector3;

    /// <summary>Converts from <c>[0, 1]</c> to <c>[C1, C2]</c>.</summary>
    /// <remarks>Default property.<p/>
    /// For inverse operation see <see cref="VectorGeometry|TBounds3.InvPoint"/>.</remarks>
    property Point[APos: TVector3]: TVector3 read GetPoint; default;
    /// <summary>Converts from <c>[-1, +1]</c> to <c>[C1, C2]</c></summary>
    /// <remarks>For inverse operation see <see cref="VectorGeometry|TBounds3.InvPointSym"/>.</remarks>
    property PointSym[APos: TVector3]: TVector3 read GetPointSym;
    /// <summary>Converts from <c>[-1, 0]</c> to <c>[C1, C2]</c></summary>
    /// <remarks>For inverse operation see <see cref="VectorGeometry|TBounds3.InvPointLeft"/>.</remarks>
    property PointLeft[APos: TVector3]: TVector3 read GetPointLeft;

    /// <summary>Converts from <c>[C1, C2]</c> to <c>[0, 1]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPoint[APos: TVector3]: TVector3 read GetInvPoint;
    /// <summary>Converts from <c>[C1, C2]</c> to <c>[-1, +1]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPointSym[APos: TVector3]: TVector3 read GetInvPointSym;
    /// <summary>Converts from <c>[C1, C2]</c> to <c>[-1, 0]</c></summary>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if length is zero.</exception>
    property InvPointLeft[APos: TVector3]: TVector3 read GetInvPointLeft;

    /// <returns>Converts from <c>[C1, C2]</c> to specified bounds <c>[C1, C2].</c></returns>
    function Convert(APoint: TVector3; ABounds: TBounds3): TVector3;

    /// <returns>The given bounds clamped to be inside of the calling bounds.</returns>
    function Clamp(ARange: TBounds3): TBounds3; overload;
    /// <returns>The given value being clamped to the bounds <c>[C1, C2].</c></returns>
    function Clamp(AValue: TVector3): TVector3; overload;

    /// <returns>The given vector in the interval: <c>[C1..C2)</c></returns>
    function RangedModL(AValue: TVector3): TVector3;
    /// <returns>The given vector in the interval: <c>(C1..C2]</c></returns>
    function RangedModR(AValue: TVector3): TVector3;

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TBounds3;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: TVector3): TBounds3;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: TVector3): TBounds3;

    /// <returns>THe bounds with all values rounded down.</returns>
    function Floor: TIntBounds3;
    /// <returns>THe bounds with all values rounded up.</returns>
    function Ceil: TIntBounds3;

    class operator Add(const A, B: TBounds3): TBounds3;
    class operator Subtract(const A, B: TBounds3): TBounds3;
    class operator Multiply(const A, B: TBounds3): TBounds3;
    class operator Divide(const A, B: TBounds3): TBounds3;

    // The in operator is inclusive, comparing with <= and >=
    class operator in (const A, B: TBounds3): Boolean;
    class operator in (A: TVector3; const B: TBounds3): Boolean;

    class operator Equal(const A, B: TBounds3): Boolean;
    class operator NotEqual(const A, B: TBounds3): Boolean;
    class operator GreaterThan(const A, B: TBounds3): Boolean;
    class operator GreaterThanOrEqual(const A, B: TBounds3): Boolean;
    class operator LessThan(const A, B: TBounds3): Boolean;
    class operator LessThanOrEqual(const A, B: TBounds3): Boolean;

    /// <returns>A string representative in the form: <c>&lt;C1~C2&gt;</c></returns>
    /// <remarks>Direct implicit conversion to string is possible.</remarks>
    function ToString: string; inline;
    class operator Implicit(ABounds: TBounds3): string; inline;

  end;

  /// <summary>
  /// Represents the direction of a 3-Dimensional vector using turn and pitch angles.
  /// <p>The default direction points along the positive Z-Axis. (pointed towards camera)</p>
  /// <code>
  /// vector | turn | pitch <p/>
  /// ---------+------+-------<p/>
  /// [1|0|0] |   90 |     0 <p/>
  /// [0|1|0] |    0 |    90 <p/>
  /// [0|0|1] |    0 |     0
  /// </code>
  /// </summary>
  /// <remarks>This type cannot represent rolling, since a vector does not have such an orientation.</remarks>
  TVectorDir = record
  private
    function GetTurnAngle: Single;
    function GetPitchAngle: Single;

    procedure SetTurnAngle(Value: Single);
    procedure SetPitchAngle(Value: Single);

  public
    /// <summary>The Turn-Component of the direction in radians.</summary>
    /// <remarks>Positive values rotate from pointed towards the camera to the right.</remarks>
    TurnAngleRad: Single;
    /// <summary>The Pitch-Component of the direction in radians.</summary>
    /// <remarks>Positive values rotate from pointed towards the camera upwards.</remarks>
    PitchAngleRad: Single;

    /// <summary>The Turn-Component of the direction in degrees.</summary>
    /// <remarks>Positive values rotate from pointed towards the camera to the right.</remarks>
    property TurnAngle: Single read GetTurnAngle write SetTurnAngle;
    /// <summary>The Pitch-Component of the direction in degrees.</summary>
    /// <remarks>Positive values rotate from pointed towards the camera upwards.</remarks>
    property PitchAngle: Single read GetPitchAngle write SetPitchAngle;

    /// <returns>A <see cref="Pengine.Vector|TVectorDir"/> from the specified Turn and Pitch angles in radians.</returns>
    class function CreateRad(ATurnAngleRad, APitchAngleRad: Single): TVectorDir; static;

    /// <summary>Creates a <see cref="Pengine.Vector|TVectorDir"/> from the specified Turn and Pitch angles in degrees.</summary>
    constructor Create(ATurnAngle, APitchAngle: Single); overload;
    /// <summary>Creates a <see cref="Pengine.Vector|TVectorDir"/>, that points into the same direction, as the given vector.</summary>
    constructor Create(AVector: TVector3); overload;
    /// <returns>A normalized vector, pointing into the direction of the <see cref="Pengine.Vector|TVectorDir"/>.</returns>
    function Vector: TVector3;

    class operator Equal(const A, B: TVectorDir): Boolean;
    class operator NotEqual(const A, B: TVectorDir): Boolean;

    class operator Implicit(const AVector: TVector3): TVectorDir; inline;
    class operator Implicit(const ADirection: TVectorDir): TVector3; inline;

  end;

  /// <summary>Describes a side of a <see cref="Pengine.Vector|TLine2"/>.</summary>
  TLineSide = (lsLeft, lsOn, lsRight);

  /// <summary>Represents a 2-Dimensional line, defined by a support vector S and a direction vector D.</summary>
  TLine2 = record
  private
    function GetPoint(Value: Single): TVector2;

    function GetHead: TVector2;
    function GetTail: TVector2;

    procedure SetHead(Value: TVector2);
    procedure SetTail(Value: TVector2);

  public type

    /// <summary>Contains the multiplication-factors of both lines to get to the intersection point.</summary>
    TIntsecFactors = record
      /// <summary>The multiplication-factor with the calling line to get to the intersection point.</summary>
      Factor: Single;
      /// <summary>The multiplication-factor with the argument line to get to the intersection point</summary>
      FactorOther: Single;
    end;

  public
    /// <summary>The support vector S of the line.</summary>
    S: TVector2;
    /// <summary>The direction vector D of the line.</summary>
    D: TVector2;

    /// <summary>Equal to S + D.<p>Setting it will leave the tail at its current position.</p></summary>
    property Head: TVector2 read GetHead write SetHead;
    /// <summary>Equal to S.<p>/!\ Setting it will leave the head at its current position.</p></summary>
    property Tail: TVector2 read GetTail write SetTail;

    /// <summary>Gets a point on the line, where <c>[0, 1]</c> turns into <c>[S, S + D]</c></summary>
    property Point[Factor: Single]: TVector2 read GetPoint; default;

    /// <summary>Creates a line with the given support and direction vector.</summary>
    constructor Create(const S, D: TVector2);

    /// <returns>The multiplication-factor, to reach the point on the line, which is closest to the given point.</returns>
    function OrthoProj(const A: TVector2): Single;

    /// <returns>True, if the lines cross each other at exactly one point.</returns>
    function Intsec(const A: TLine2): Boolean; overload; inline;
    /// <param name="A">The line, to check the intersection with.</param>
    /// <param name="AFactors">Contains the intersection result, on success.</param>
    /// <returns>True, if the lines cross each other at exactly one point.</returns>
    function Intsec(const A: TLine2; out AFactors: TIntsecFactors): Boolean; overload;

    /// <returns>The shortest distance between the line and the given point.</returns>
    /// <remarks>Will be positive on the left and negative on the right</remarks>
    function Height(const A: TVector2): Single;

    /// <returns>The side of the line, on which the given point is, while looking in the direction of D.</summary>
    function Side(A: TVector2): TLineSide;

    class operator Equal(const A, B: TLine2): Boolean;
    class operator NotEqual(const A, B: TLine2): Boolean;

  end;

  /// <summary>Represents a 3-Dimensional line, defined by a support vector S and a direction vector D.</summary>
  TLine3 = record
  private
    function GetPoint(Value: Single): TVector3;

    function GetHead: TVector3;
    function GetTail: TVector3;

    procedure SetHead(Value: TVector3);
    procedure SetTail(Value: TVector3);

  public
    /// <summary>The support vector S of the line.</summary>
    S: TVector3;
    /// <summary>The direction vector D of the line.</summary>
    D: TVector3;

    /// <summary>Equal to S + D.<p>Setting it will leave the tail at its current position.</p></summary>
    property Head: TVector3 read GetHead write SetHead;
    /// <summary>Equal to S.<p>/!\ Setting it will leave the head at its current position.</p></summary>
    property Tail: TVector3 read GetTail write SetTail;

    /// <summary>Gets a point on the line, where <c>[0, 1]</c> turns into <c>[S, S + D]</c>.</summary>
    /// <remarks>Default property.</remarks>
    property Point[Distance: Single]: TVector3 read GetPoint; default;

    /// <summary>Creates a line with the given support vector and direction vector.</summary>
    constructor Create(const S, D: TVector3);

    /// <returns>The multiplication-factor, to reach the point on the line, which is closest to the given point.</returns>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if D has a length of 0.</exception>
    function OrthoProj(const A: TVector3): Single; overload;

    /// <returns>True, if there is exactly one closest connection between both lines.</returns>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if D has a length of 0.</exception>
    function OrthoProj(const A: TLine3): Boolean; overload;
    /// <param name="[out] AFactor">Contains the multiplication-factor, to reach the closest point on the calling line.</param>
    /// <returns>True, if there is exactly one closest connection between both lines.</returns>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if D has a length of 0.</exception>
    function OrthoProj(const A: TLine3; out AFactor: Single): Boolean; overload;
    /// <param name="[out] APoint">Contains the closest point on the calling calling line.</param>
    /// <returns>True, if there is exactly one closest connection between both lines.</returns>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if D has a length of 0.</exception>
    function OrthoProj(const A: TLine3; out APoint: TVector3): Boolean; overload;
    /// <param name="[out] AFactor">Contains the multiplication-factor, to reach the closest point on the calling line.</param>
    /// <param name="[out] APoint">Contains the closest point on the calling line.</param>
    /// <returns>True, if there is exactly one closest connection between both lines.</returns>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if D has a length of 0.</exception>
    function OrthoProj(const A: TLine3; out AFactor: Single; out APoint: TVector3): Boolean; overload;

    /// <returns>The shortest distance between the line and the given point.</returns>
    /// <remarks>Will always be positive.</remarks>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if D has a length of 0.</exception>
    function Height(const A: TVector3): Single;

    /// <returns>The mirrored point.</returns>
    /// <remarks>The calling line defines the mirror plane.</remarks>
    function Mirror(APoint: TVector3): TVector3;

    class operator Equal(const A, B: TLine3): Boolean;
    class operator NotEqual(const A, B: TLine3): Boolean;

  end;

  /// <summary>Represents a 3-Dimensional plane, described by one support vector S and two direction vectors X and Y.</summary>
  TPlane3 = record
  private
    function GetPoint(Value: TVector2): TVector3;

  public type

    /// <summary>Contains the multiplication-factors, to reach the intersection points on the plane and the line.</summary>
    TLineIntsecFactors = record
      /// <summary>The multiplication-factors, to reach the intersection point on the plane.</summary>
      PlaneFactors: TVector2;
      /// <summary>The multiplication-factor, to reach the intersection point on the line.</summary>
      LineFactor: Single;
    end;

  public
    /// <summary>The support vector S of the line.</summary>
    S: TVector3;
    /// <summary>The direction vector DX of the line.</summary>
    DX: TVector3;
    /// <summary>The direction vector DY of the line.</summary>
    DY: TVector3;

    /// <returns>The perpendicular of the plane with an arbitrary length.</returns>
    /// <remarks>The length is actually the area of the paralellogram, hence the cross-product.</remarks>
    function Perpendicular: TVector3; inline;
    /// <returns>The area of the paralellogram.</returns>
    function Area: Single; inline;
    /// <returns>The normal of the plane.</returns>
    function Normal: TVector3; inline;

    /// <summary>Gets a point on the plane, where <c>[0, 1]</c> turns into <c>[S, S + X + Y]</c></summary>
    /// <remarks>Default property.</remarks>
    property Point[APos: TVector2]: TVector3 read GetPoint; default;

    /// <summary>Creates a plane with the given support vector and direction vectors.</summary>
    constructor Create(const S, X, Y: TVector3);

    /// <returns>The multiplication-factors, to reach the point on the plane, which is closest to the given point.</returns>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if either X or Y is 0.</exception>
    function OrthoProj(const A: TVector3): TVector2; overload;

    /// <summary>Calculates the shortest distance between the line and a point</summary>
    /// <remarks>Will be positive on the left and negative on the right</remarks>
    function Height(const A: TVector3): Single;

    // --- line intersection ---
    /// <returns>True, if there is exactly one intersection with the line.</returns>
    function Intsec(const A: TLine3): Boolean; overload; inline;
    /// <param name="[out] AFactors">Contains the multiplication-factors, to reach the intersection point.</param>
    /// <returns>True, if there is exactly one intersection with the line.</returns>
    function Intsec(const A: TLine3; out AFactors: TLineIntsecFactors): Boolean; overload;
    /// <param name="[out] APoint">Contains the intersection point.</param>
    /// <returns>True, if there is exactly one intersection with the line.</returns>
    /// <remarks>For quicker calculation, the line is used to get the point. Use <see cref="Pengine.Vector|TPlane3.IntsecP"/>
    /// to use the plane instead.</remarks>
    function Intsec(const A: TLine3; out APoint: TVector3): Boolean; overload;
    /// <param name="[out] APoint">Contains the intersection point.</param>
    /// <returns>True, if there is exactly one intersection with the line.</returns>
    /// <remarks>The plane is used to get the point in this function.</remarks>
    function IntsecP(const A: TLine3; out APoint: TVector3): Boolean; overload;
    /// <param name="[out] AFactors">Contains the multiplication-factors, to reach the intersection point.</param>
    /// <param name="[out] APoint">Contains the intersection point.</param>
    /// <returns>True, if there is exactly one intersection with the line.</returns>
    /// <remarks>For quicker calculation, the line is used to get the point. Use <see cref="Pengine.Vector|TPlane3.IntsecP"/>
    /// to use the plane instead.</remarks>
    function Intsec(const A: TLine3; out AFactors: TLineIntsecFactors; out APoint: TVector3): Boolean; overload;
    /// <param name="[out] AFactors">Contains the multiplication-factors, to reach the intersection point.</param>
    /// <param name="[out] APoint">Contains the intersection point.</param>
    /// <returns>True, if there is exactly one intersection with the line.</returns>
    /// <remarks>The plane is used to get the point in this function.</remarks>
    function IntsecP(const A: TLine3; out AFactors: TLineIntsecFactors; out APoint: TVector3): Boolean; overload;

    // --- plane intersection ---
    /// <returns>True, if the planes intersect at exactly one line.</returns>
    function Intsec(const A: TPlane3): Boolean; overload;
    /// <param name="[out] ALine">Contains an aribtrary line, which runs along the intersection of the planes.</param>
    /// <returns>True, if the planes intersect at exactly one line.</returns>
    function Intsec(const A: TPlane3; out ALine: TLine3): Boolean; overload;

    /// <returns>The cosine of the angle between the plane and the given vector in radians.</returns>
    function CosAngleTo(const A: TVector3): Single; overload;
    /// <returns>The angle between the plane and the given vector in radians.</returns>
    function AngleRadTo(const A: TVector3): Single; overload; inline;
    /// <returns>The angle between the plane and the given vector in degrees.</returns>
    function AngleTo(const A: TVector3): Single; overload; inline;

    /// <returns>The cosine of the angle to another plane in radians.</returns>
    function CosAngleTo(const A: TPlane3): Single; overload;
    /// <returns>The angle to another plane in radians.</returns>
    function AngleRadTo(const A: TPlane3): Single; overload; inline;
    /// <returns>The angle to another plane in degrees.</returns>
    function AngleTo(const A: TPlane3): Single; overload; inline;

    class operator Equal(const A, B: TPlane3): Boolean;
    class operator NotEqual(const A, B: TPlane3): Boolean;

  end;

  /// <summary>Record helper for <see cref="Pengine.Vector|TVector2"/></summary>
  TVector2Helper = record helper for TVector2
    /// <returns>Creates a <see cref="Pengine.Vector|TLine2"/> between two vectors.</returns>
    function LineTo(const A: TVector2): TLine2;
    function Bounds(const ASize: TVector2): TBounds2;
  end;

  /// <summary>Record helper for <see cref="Pengine.Vector|TVector3"/></summary>
  TVector3Helper = record helper for TVector3
    /// <returns>Creates a <see cref="Pengine.Vector|TLine3"/> between two vectors.</returns>
    function LineTo(const A: TVector3): TLine3;
    function Bounds(const ASize: TVector3): TBounds3;
  end;

  /// <summary>Represents a 2-Dimensional plane, described by one support vector S and two direction vectors X and Y.</summary>
  TAxisSystem2 = record
  private
    function GetPoint(APos: TVector2): TVector2;
    function GetInvPoint(APos: TVector2): TVector2;

  public
    /// <summary>The support vector S of the line.</summary>
    S: TVector2;
    /// <summary>The direction vector DX of the line.</summary>
    DX: TVector2;
    /// <summary>The direction vector DY of the line.</summary>
    DY: TVector2;

    /// <summary>Creates a plane with the given support vector and direction vectors.</summary>
    constructor Create(const S, X, Y: TVector2);

    /// <returns>The area of the paralellogram.</returns>
    function Area: Single; inline;

    /// <summary>Gets a point on the plane, where <c>[0, 1]</c> turns into <c>[S, S + X + Y]</c></summary>
    /// <remarks>Default property.</remarks>
    property Point[APos: TVector2]: TVector2 read GetPoint; default;
    /// <summary>Gets where a point lies on the plane, where <c>[S, S + X + Y]</c> turns into <c>[0, 1]</c></summary>
    property InvPoint[APos: TVector2]: TVector2 read GetInvPoint;

    class operator in(const A: TVector2; const B: TAxisSystem2): Boolean;

    class operator Equal(const A, B: TAxisSystem2): Boolean;
    class operator NotEqual(const A, B: TAxisSystem2): Boolean;

  end;

  TAxisSystem3 = record
  private
    function GetPoint(APos: TVector3): TVector3;
    function GetInvPoint(APos: TVector3): TVector3;

  public
    S: TVector3;
    DX: TVector3;
    DY: TVector3;
    DZ: TVector3;

    /// <summary>Creates an axis system with the given support vector and direction vectors.</summary>
    constructor Create(const S, X, Y, Z: TVector3);

    /// <summary>Gets a point in axis system, where <c>[0, 1]</c> turns into <c>[S, S + X + Y + Z]</c></summary>
    /// <remarks>Default property.</remarks>
    property Point[APos: TVector3]: TVector3 read GetPoint; default;
    /// <summary>Gets where a point lies in the axis system, where <c>[S, S + X + Y]</c> turns into <c>[0, 1]</c></summary>
    property InvPoint[APos: TVector3]: TVector3 read GetInvPoint;

    class operator in(const A: TVector3; const B: TAxisSystem3): Boolean;

    class operator Equal(const A, B: TAxisSystem3): Boolean;
    class operator NotEqual(const A, B: TAxisSystem3): Boolean;

  end;

  TLocationChange = (
    lcParent,
    lcPos,
    lcOffset,
    lcScale,
    lcTurn,
    lcPitch,
    lcRoll,
    lcFreeTranslation,
    lcFreeScale,
    lcFreeRotation,
    lcFreeMirror
    );

  TLocation2 = class
  public type

    TChange = lcParent .. lcTurn;

    TChanges = set of TChange;

    TChangeEventInfo = class(TEventInfo, IEventSender<TLocation2>)
    private
      FSender: TLocation2;
      FChanges: TChanges;

    public
      constructor Create(ASender: TLocation2; AChanges: TChanges);

      function Sender: TLocation2;

      property Changes: TChanges read FChanges;

    end;

    TChangeEvent = TEvent<TChangeEventInfo>;

  private
    FParent: TLocation2;
    FPos: TVector2;
    FOffset: TVector2;
    FScale: TVector2;
    FRotation: Single;
    FAxisSystem: TOpt<TAxisSystem2>;
    FUpdateCounter: Integer;
    FChanges: TChanges;
    FOnChanged: TChangeEvent;

    procedure SetParent(const Value: TLocation2);

    procedure SetPos(const Value: TVector2);
    procedure SetPosX(const Value: Single);
    procedure SetPosY(const Value: Single);

    procedure SetOffset(const Value: TVector2);
    procedure SetOffsetX(const Value: Single);
    procedure SetOffsetY(const Value: Single);

    procedure SetRotation(Value: Single);

    procedure SetScale(const Value: TVector2);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);

    function GetAxisSystem: TAxisSystem2;
    function GetInvPoint(APoint: TVector2): TVector2;
    function GetPoint(APoint: TVector2): TVector2;

    function GetOnChanged: TChangeEvent.TAccess;

    procedure Changed(AChange: TChange);

    procedure ParentChanged(AInfo: TChangeEventInfo);
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(ALocation: TLocation2);
    function Copy: TLocation2;

    procedure Reset;

    property Parent: TLocation2 read FParent write SetParent;

    property Pos: TVector2 read FPos write SetPos;
    property PosX: Single read FPos.X write SetPosX;
    property PosY: Single read FPos.Y write SetPosY;

    property Offset: TVector2 read FOffset write SetOffset;
    property OffsetX: Single read FOffset.X write SetOffsetX;
    property OffsetY: Single read FOffset.Y write SetOffsetY;

    property Rotation: Single read FRotation write SetRotation;

    property Scale: TVector2 read FScale write SetScale;
    property ScaleX: Single read FScale.X write SetScaleX;
    property ScaleY: Single read FScale.Y write SetScaleY;

    property AxisSystem: TAxisSystem2 read GetAxisSystem;
    property Point[APoint: TVector2]: TVector2 read GetPoint; default;
    property InvPoint[APoint: TVector2]: TVector2 read GetInvPoint;

    procedure BeginUpdate;
    procedure EndUpdate;

    property OnChanged: TChangeEvent.TAccess read GetOnChanged;

  end;

  // TODO: Remove the constructor parameter and calculate invmatrix with the current inverted calculation
  /// <summary>A class for an oriented position in 3D space.</summary>
  TLocation3 = class
  public type

    TChange = lcParent .. lcFreeMirror;

    TChanges = set of TChange;

    TChangeEventInfo = class(TEventInfo, IEventSender<TLocation3>)
    private
      FSender: TLocation3;
      FChanges: TChanges;

    public
      constructor Create(ASender: TLocation3; AChanges: TChanges);

      function Sender: TLocation3;

      property Changes: TChanges read FChanges;

    end;

    TChangeEvent = TEvent<TChangeEventInfo>;

  public const

    FreeChange = [lcFreeTranslation .. lcFreeMirror];

  private
    FPos: TVector3;
    FOffset: TVector3;
    FScale: TVector3;
    FRotation: TVector3;

    FMatrix: TMatrix4;
    FInvMatrix: TMatrix4;

    FRotMatrix: TMatrix3;
    FInvRotMatrix: TMatrix3;

    FMatrixChanged: Boolean;
    FInvMatrixChanged: Boolean;
    FRotMatrixChanged: Boolean;
    FInvRotMatrixChanged: Boolean;

    FFreeChanged: Boolean;
    FInverted: Boolean;

    FChanged: Boolean;

    FParent: TLocation3;

    FOnChanged: TChangeEvent;

    procedure SetLook(AValue: TVector3);

    function GetMatrix: TMatrix4;
    function GetInvMatrix: TMatrix4;

    function GetRotMatrix: TMatrix3;
    function GetInvRotMatrix: TMatrix3;

    function GetRealPosition: TVector3;

    function GetRight: TVector3;
    function GetLook: TVector3;
    function GetUp: TVector3;

    procedure BuildMatrix;

    procedure SetPos(AValue: TVector3);
    procedure SetPosX(AValue: Single);
    procedure SetPosY(AValue: Single);
    procedure SetPosZ(AValue: Single);

    procedure SetOffset(AValue: TVector3);
    procedure SetOffsetX(AValue: Single);
    procedure SetOffsetY(AValue: Single);
    procedure SetOffsetZ(AValue: Single);

    procedure SetScale(AValue: TVector3);
    procedure SetScaleX(AValue: Single);
    procedure SetScaleY(AValue: Single);
    procedure SetScaleZ(AValue: Single);

    procedure SetPitch(AValue: Single);
    procedure SetRoll(AValue: Single);
    procedure SetTurn(AValue: Single);

    procedure ParentChanged(AInfo: TChangeEventInfo);
    procedure SetParent(const Value: TLocation3);

    procedure Changed(AChange: TChange); overload;
    procedure Changed(AChanges: TChanges); overload;

    function GetAxisSystem: TAxisSystem3;
    function GetPoint(APos: TVector3): TVector3;
    function GetInvPoint(APos: TVector3): TVector3;

    function GetOnChanged: TChangeEvent.TAccess;

  public
    /// <summary>Creates a <see cref="Pengine.Vector|TLocation3"/>.</summary>
    /// <param name="AInverted">Output matrix is inverse, mainly used for cameras.</param>
    constructor Create(AInverted: Boolean = False);

    /// <summary>The 4x4 output transformation-matrix.</summary>
    property Matrix: TMatrix4 read GetMatrix;
    /// <summary>The inverse of <see cref="Pengine.Vector|TLocation3.Matrix"/>.</summary>
    /// <remarks>If only the inverse matrix is used, flip both matrices with the constructor parameter.</remarks>
    property InvMatrix: TMatrix4 read GetInvMatrix;

    property RotMatrix: TMatrix3 read GetRotMatrix;
    property InvRotMatrix: TMatrix3 read GetInvRotMatrix;

    property Parent: TLocation3 read FParent write SetParent;

    property Pos: TVector3 read FPos write SetPos;
    property PosX: Single read FPos.X write SetPosX;
    property PosY: Single read FPos.Y write SetPosY;
    property PosZ: Single read FPos.Z write SetPosZ;

    property Offset: TVector3 read FOffset write SetOffset;
    property OffsetX: Single read FOffset.X write SetOffsetX;
    property OffsetY: Single read FOffset.Y write SetOffsetY;
    property OffsetZ: Single read FOffset.Z write SetOffsetZ;

    property Scale: TVector3 read FScale write SetScale;
    property ScaleX: Single read FScale.X write SetScaleX;
    property ScaleY: Single read FScale.Y write SetScaleY;
    property ScaleZ: Single read FScale.Z write SetScaleZ;

    procedure Slide(ADistance: Single; AHorizontal: Boolean = False);
    procedure Lift(ADistance: Single; AYOnly: Boolean = False);
    procedure Move(ADistance: Single; AHorizontal: Boolean = False);

    property TurnAngle: Single read FRotation.Y write SetTurn;
    property PitchAngle: Single read FRotation.X write SetPitch;
    property RollAngle: Single read FRotation.Z write SetRoll;

    property Right: TVector3 read GetRight;
    property Up: TVector3 read GetUp;
    property Look: TVector3 read GetLook write SetLook;

    procedure LookAt(APoint: TVector3);

    property RealPosition: TVector3 read GetRealPosition;

    // All of the following functions will trigger the rebuild once the Matrix is requested
    procedure Reset;
    procedure ResetTranslation;
    procedure ResetOffset;
    procedure ResetScale;
    procedure ResetRotation;

    procedure Turn(const ATurn: Single);
    procedure Pitch(const APitch: Single);
    procedure Roll(const ARoll: Single);

    procedure Rotate(const ARotation: TVector3);
    procedure Translate(const AVector: TVector3);
    procedure MoveOffset(const AVector: TVector3);
    procedure ScaleBy(const AScale: TVector3);

    procedure Approach(ALocation: TLocation3; ADelta: Single);
    procedure Assign(ALocation: TLocation3);
    procedure Swap(ALocation: TLocation3);

    // Those will directly change the current Matrix and thus not trigger the rebuild
    // FTurn/FPitch/FRoll/FPos/FOffset won't be correct anymore
    procedure FreeRotate(AVector: TVector3; const AAngle: Single);
    procedure FreeTurn(ATurn: Single);
    procedure FreePitch(APitch: Single);
    procedure FreeRoll(ARoll: Single);
    procedure FreeTranslate(const AVector: TVector3);
    procedure FreeScale(const AScale: TVector3);

    procedure FreeMirror(ANormal: TLine3);

    procedure FromMatrix(AMatrix: TMatrix4);
    procedure FromRotMatrix(AMatrix: TMatrix3);

    property AxisSystem: TAxisSystem3 read GetAxisSystem;
    property Point[APos: TVector3]: TVector3 read GetPoint; default;
    property InvPoint[APos: TVector3]: TVector3 read GetInvPoint;

    property OnChanged: TChangeEvent.TAccess read GetOnChanged;

  end;

  /// <summary>Represents the orientation of an axis system, that only allows <see cref="Pengine.Vector|TBasicDir"/>
  /// <p>The default directions are:</p><code>
  /// X -> bdRight<p/>
  /// Y -> bdUp<p/>
  /// Z -> bdFront
  /// </code></summary>
  TBasicAxisSystem = class
  private
    FX, FY, FZ: TBasicDir3;
    FChanged: Boolean;

  public
    /// <summary>Create a <see cref="Pengine.Vector|TBlockRotation"> object with default directions</summary>
    constructor Create; overload;
    constructor Create(AX, AY, AZ: TBasicDir3); overload;

    property X: TBasicDir3 read FX;
    property Y: TBasicDir3 read FY;
    property Z: TBasicDir3 read FZ;

    function Convert(ADirection: TBasicDir3): TBasicDir3;
    function ConvertBack(ADirection: TBasicDir3): TBasicDir3;

    function Matrix: TMatrix3;
    procedure FromMatrix(AMatrix: TMatrix3);

    procedure Rotate(AAxis: TBasicDir3; ASteps: Integer = 1);
    procedure Mirror(ANormal: TBasicDir3);
    { TODO -oPossseidon -cFunction : Mirror with mutiple two and three directions for diagonal mirroring }
    procedure Invert;

    procedure Reset;

    function Equal(ABlockRotation: TBasicAxisSystem): Boolean;

    procedure Assign(ABlockRotation: TBasicAxisSystem);

    function Changed: Boolean;
    procedure NotifyChanges;

  end;

  // TODO: XmlDoc
  TBlock2Raycaster = class

  end;

  // TODO: XmlDoc
  TBlock3Raycaster = class
  private
    FLocation: TLocation3;
    FSize: TIntVector3;

    FLine: TLine3;
    FDirections: TBasicDirs3;
    FMin, FMax: Single;

    FCurrent: TIntVector3;
    FLastDirection: TBasicDir3;
    FPosition: TVector3;

  public
    constructor Create(ALocation: TLocation3; ASize: TIntVector3);

    procedure AddMin(AMin: Single);
    procedure AddMax(AMax: Single);

    procedure Start(ALine: TLine3);
    function Next: Boolean;

    property Current: TIntVector3 read FCurrent;
    property LastDirection: TBasicDir3 read FLastDirection;
    property Position: TVector3 read FPosition;

  end;

  /// <summary>A six-sided polyhedron, defined by six face normals, pointing to the outside.</summary>
  THexahedron = record
  public const

    CheckOrder: array [0 .. 5] of TBasicDir3 = (
      bdFront, // check for behind the camera first, as half of the objects will be there
      bdLeft,
      bdRight,
      bdBack,
      bdDown,
      bdUp
      );

  private
    FFaceNormals: array [TBasicDir3] of TLine3;
    function GetFaceNormal(ADir: TBasicDir3): TLine3; inline;
    procedure SetFaceNormal(ADir: TBasicDir3; const Value: TLine3); inline;

  public
    /// <returns>False, if there is at least one face, where all points are on the outside.</retruns>
    function AnyVisible(APoints: IIterable<TVector3>): Boolean;
    function SphereVisible(const ACenter: TVector3; ARadius: Single): Boolean;

    class operator in(APoint: TVector3; const AHexahedron: THexahedron): Boolean;

    property FaceNormals[ADir: TBasicDir3]: TLine3 read GetFaceNormal write SetFaceNormal; default;

  end;

  PBounds1 = ^TBounds1;
  PBounds2 = ^TBounds2;
  PBounds3 = ^TBounds3;

  PVector3 = ^TVector3;
  PVector2 = ^TVector2;
  PVectorDir = ^TVectorDir;
  PLine2 = ^TLine2;
  PLine3 = ^TLine3;
  PPlane2 = ^TAxisSystem2;
  PPlane3 = ^TPlane3;

const

  InfVec2: TVector2 = (X: Infinity; Y: Infinity);
  NaNVec2: TVector2 = (X: NaN; Y: NaN);

  InfVec3: TVector3 = (X: Infinity; Y: Infinity; Z: Infinity);
  NaNVec3: TVector3 = (X: NaN; Y: NaN; Z: NaN);

  InfBounds1: TBounds1 = (C1: -Infinity; C2: +Infinity);
  InfBounds2: TBounds2 = (C1: (X: -Infinity; Y: -Infinity); C2: (X: Infinity; Y: Infinity));
  InfBounds3: TBounds3 = (C1: (X: -Infinity; Y: -Infinity; Z: -Infinity); C2: (X: Infinity; Y: Infinity; Z: Infinity));

  CubePlanes: array [TBasicDir3] of TPlane3 = (
    (S: (X: 0; Y: 0; Z: 0); DX: (X: 0; Y: 0; Z: 1); DY: (X: 0; Y: 1; Z: 0)),
    (S: (X: 1; Y: 0; Z: 1); DX: (X: 0; Y: 0; Z: - 1); DY: (X: 0; Y: 1; Z: 0)),
    (S: (X: 0; Y: 0; Z: 0); DX: (X: 1; Y: 0; Z: 0); DY: (X: 0; Y: 0; Z: 1)),
    (S: (X: 0; Y: 1; Z: 1); DX: (X: 1; Y: 0; Z: 0); DY: (X: 0; Y: 0; Z: - 1)),
    (S: (X: 1; Y: 0; Z: 0); DX: (X: - 1; Y: 0; Z: 0); DY: (X: 0; Y: 1; Z: 0)),
    (S: (X: 0; Y: 0; Z: 1); DX: (X: 1; Y: 0; Z: 0); DY: (X: 0; Y: 1; Z: 0))
    );

function FlipDir(ADir: TBasicDir): TBasicDir; inline;
function AbsDir(ADir: TBasicDir): TBasicDir; inline;
function RotateDir(ADir: TBasicDir; AAxis: TBasicDir3; ASteps: Integer = 1): TBasicDir;

// Shorthand Constructors

/// <returns>A <see cref="Pengine.Vector|TVector2"/> with the given values for X and Y.</returns>
function Vec2(X, Y: Single): TVector2; overload; inline;
/// <returns>A <see cref="Pengine.Vector|TVector2"/> with the given value for X and Y.</returns>
function Vec2(V: Single): TVector2; overload; inline;

/// <returns>A <see cref="Pengine.Vector|TVector3"/> with the given values for X, Y and Z.</returns>
function Vec3(X, Y, Z: Single): TVector3; overload; inline;
/// <returns>A <see cref="Pengine.Vector|TVector3"/> with the given value for X, Y and Z.</returns>
function Vec3(V: Single): TVector3; overload; inline;

/// <returns>A <see cref="Pengine.Vector|TBounds1"/> for the interval: <c>[A, B]</c></returns>
function Bounds1(A, B: Single): TBounds1; overload; inline;
/// <returns>A <see cref="Pengine.Vector|TBounds1"/> for the interval: <c>[A, A]</c></returns>
function Bounds1(A: Single): TBounds1; overload; inline;

/// <returns>A <see cref="Pengine.Vector|TBounds2"/> for the interval: <c>[A, B]</c></returns>
function Bounds2(A, B: TVector2): TBounds2; overload; inline;
/// <returns>A <see cref="Pengine.Vector|TBounds2"/> for the interval: <c>[A, A]</c></returns>
function Bounds2(A: TVector2): TBounds2; overload; inline;

/// <returns>A <see cref="Pengine.Vector|TBounds3"/> for the interval: <c>[A, B]</c></returns>
function Bounds3(A, B: TVector3): TBounds3; overload; inline;
/// <returns>A <see cref="Pengine.Vector|TBounds3"/> for the interval: <c>[A, A]</c></returns>
function Bounds3(A: TVector3): TBounds3; overload; inline;

/// <returns>A <see cref="Pengine.Vector|TLine2"/> with the given values for S and D.</returns>
function Line2(S, D: TVector2): TLine2; inline;
/// <returns>A <see cref="Pengine.Vector|TLine3"/> with the given values for S and D.</returns>
function Line3(S, D: TVector3): TLine3; inline;

/// <returns>A <see cref="Pengine.Vector|TPlane2"/> with the given values for S, X and Y.</returns>
function Plane2(S, X, Y: TVector2): TAxisSystem2; inline;
/// <returns>A <see cref="Pengine.Vector|TPlane3"/> with the given values for S, X and Y.</returns>
function Plane3(S, X, Y: TVector3): TPlane3; inline;

implementation

const
  RotationLimit: TBounds1 = (C1: - 180; C2: + 180);

{ EAxisError }

constructor EAxisSystemError.Create;
begin
  inherited Create('Only one direction per axis is allowed in a TBasicAxisSystem.');
end;

{ TVector2 }

function TVector2.GetComponent(AAxis: TCoordAxis2): Single;
begin
  Result := (PSingle(@Self) + Ord(AAxis) - Ord(caX))^;
end;

procedure TVector2.SetComponent(AAxis: TCoordAxis2; const Value: Single);
begin
  (PSingle(@Self) + Ord(AAxis) - Ord(caX))^ := Value;
end;

function TVector2.GetXX: TVector2;
begin
  Result.X := X;
  Result.Y := X;
end;

function TVector2.GetXY: TVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TVector2.GetYX: TVector2;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TVector2.GetYY: TVector2;
begin
  Result.X := Y;
  Result.Y := Y;
end;

procedure TVector2.SetXY(const Value: TVector2);
begin
  X := Value.X;
  Y := Value.Y;
end;

procedure TVector2.SetYX(const Value: TVector2);
begin
  Y := Value.X;
  X := Value.Y;
end;

constructor TVector2.Create(X, Y: Single);
begin
  Self.X := X;
  Self.Y := Y;
end;

constructor TVector2.Create(V: Single);
begin
  X := V;
  Y := V;
end;

class function TVector2.FromSlope(ASlope: Single): TVector2;
begin
  Result.Create(1, ASlope);
end;

class function TVector2.FromAngleRad(AAngle: Single): TVector2;
begin
  SinCos(AAngle, Result.Y, Result.X);
end;

class function TVector2.FromAngle(AAngle: Single): TVector2;
begin
  Result := FromAngleRad(DegToRad(AAngle));
end;

class function TVector2.Random: TVector2;
begin
  Result.X := System.Random;
  Result.Y := System.Random;
end;

class function TVector2.RandomBox: TVector2;
begin
  Result := Random * 2 - 1;
end;

class function TVector2.RandomNormal: TVector2;
var
  A: Single;
begin
  A := System.Random * 2 * Pi;
  Result := Vec2(Sin(A), Cos(A));
end;

class operator TVector2.Implicit(V: Single): TVector2;
begin
  Result.X := V;
  Result.Y := V;
end;

class operator TVector2.Implicit(const AVector: TIntVector2): TVector2;
begin
  Result.X := AVector.X;
  Result.Y := AVector.Y;
end;

class operator TVector2.Add(const A, B: TVector2): TVector2;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TVector2.Subtract(const A, B: TVector2): TVector2;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TVector2.Multiply(const A, B: TVector2): TVector2;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

class operator TVector2.Divide(const A, B: TVector2): TVector2;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

class operator TVector2.Multiply(const A: TMatrix2; const B: TVector2): TVector2;
begin
  Result.X := A[0, 0] * B.X + A[1, 0] * B.Y;
  Result.Y := A[0, 1] * B.X + A[1, 1] * B.Y;
end;

class operator TVector2.Multiply(const A: TVector2; const B: TMatrix2): TVector2;
begin
  Result.X := B[0, 0] * A.X + B[0, 1] * A.Y;
  Result.Y := B[1, 0] * A.X + B[1, 1] * A.Y;
end;

class operator TVector2.Positive(const A: TVector2): TVector2;
begin
  Result := A;
end;

class operator TVector2.Negative(const A: TVector2): TVector2;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TVector2.Equal(const A, B: TVector2): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TVector2.NotEqual(const A, B: TVector2): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y);
end;

class operator TVector2.LessThan(const A, B: TVector2): Boolean;
begin
  Result := (A.X < B.X) and (A.Y < B.Y);
end;

class operator TVector2.LessThanOrEqual(const A, B: TVector2): Boolean;
begin
  Result := (A.X <= B.X) and (A.Y <= B.Y);
end;

class operator TVector2.GreaterThan(const A, B: TVector2): Boolean;
begin
  Result := (A.X > B.X) and (A.Y > B.Y);
end;

class operator TVector2.GreaterThanOrEqual(const A, B: TVector2): Boolean;
begin
  Result := (A.X >= B.X) and (A.Y >= B.Y);
end;

function TVector2.ToString: string;
begin
  Result := Format('[%f|%f]', [X, Y]);
end;

class operator TVector2.Implicit(const A: TVector2): string;
begin
  Result := A.ToString;
end;

function TVector2.Length: Single;
begin
  Result := Sqrt(SqrDot);
end;

function TVector2.Normalize: TVector2;
begin
  Result := Self / Length;
end;

function TVector2.DistanceTo(const A: TVector2): Single;
begin
  Result := VectorTo(A).Length;
end;

function TVector2.VectorTo(const A: TVector2): TVector2;
begin
  Result := A - Self;
end;

function TVector2.Cross: TVector2;
begin
  Result.X := -Y;
  Result.Y := +X;
end;

function TVector2.Dot(const A: TVector2): Single;
begin
  Result := X * A.X + Y * A.Y;
end;

function TVector2.SqrDot: Single;
begin
  Result := Sqr(X) + Sqr(Y);
end;

function TVector2.CosAngleTo(const A: TVector2): Single;
begin
  Result := EnsureRange(Dot(A) / (Length * A.Length), -1, 1);
end;

function TVector2.AngleRadTo(const A: TVector2): Single;
begin
  Result := ArcCos(CosAngleTo(A));
end;

function TVector2.AngleTo(const A: TVector2): Single;
begin
  Result := RadToDeg(AngleRadTo(A));
end;

function TVector2.RotateRad(AAngle: Single): TVector2;
begin
  Result := Sin(AAngle) * Cross + Cos(AAngle) * Self;
end;

function TVector2.Rotate(AAngle: Single): TVector2;
begin
  Result := RotateRad(DegToRad(AAngle));
end;

function TVector2.Abs: TVector2;
begin
  Result.X := System.Abs(X);
  Result.Y := System.Abs(Y);
end;

function TVector2.Floor: TIntVector2;
begin
  Result.X := System.Math.Floor(X);
  Result.Y := System.Math.Floor(Y);
end;

function TVector2.Ceil: TIntVector2;
begin
  Result.X := System.Math.Ceil(X);
  Result.Y := System.Math.Ceil(Y);
end;

function TVector2.Min(const A: TVector2): TVector2;
begin
  Result.X := System.Math.Min(X, A.X);
  Result.Y := System.Math.Min(X, A.Y);
end;

function TVector2.Max(const A: TVector2): TVector2;
begin
  Result.X := System.Math.Max(X, A.X);
  Result.Y := System.Math.Max(X, A.Y);
end;

function TVector2.Dirs: TBasicDirs2;
begin
  Result := [];
  if X < 0 then
    Include(Result, bdLeft);
  if X > 0 then
    Include(Result, bdRight);
  if Y < 0 then
    Include(Result, bdDown);
  if Y > 0 then
    Include(Result, bdUp);
end;

function TVector2.Slope: Single;
begin
  if X = 0 then
    Exit(Infinity * Sign(Y));
  Exit(Y / X);
end;

{ TVector3 }

function TVector3.GetComponent(AAxis: TCoordAxis3): Single;
begin
  Result := (PSingle(@Self) + Ord(AAxis) - Ord(caX))^;
end;

procedure TVector3.SetComponent(AAxis: TCoordAxis3; const Value: Single);
begin
  (PSingle(@Self) + Ord(AAxis) - Ord(caX))^ := Value;
end;

{$REGION 'All versions of rearrangement TIntVector2'}

function TVector3.GetXX: TVector2;
begin
  Result.X := X;
  Result.Y := X;
end;

function TVector3.GetXY: TVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TVector3.GetXZ: TVector2;
begin
  Result.X := X;
  Result.Y := Z;
end;

function TVector3.GetYX: TVector2;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TVector3.GetYY: TVector2;
begin
  Result.X := Y;
  Result.Y := Y;
end;

function TVector3.GetYZ: TVector2;
begin
  Result.X := Y;
  Result.Y := Z;
end;

function TVector3.GetZX: TVector2;
begin
  Result.X := Z;
  Result.Y := X;
end;

function TVector3.GetZY: TVector2;
begin
  Result.X := Z;
  Result.Y := Y;
end;

function TVector3.GetZZ: TVector2;
begin
  Result.X := Z;
  Result.Y := Z;
end;

procedure TVector3.SetXY(const Value: TVector2);
begin
  X := Value.X;
  Y := Value.Y;
end;

procedure TVector3.SetXZ(const Value: TVector2);
begin
  X := Value.X;
  Z := Value.Y;
end;

procedure TVector3.SetYX(const Value: TVector2);
begin
  Y := Value.X;
  X := Value.Y;
end;

procedure TVector3.SetYZ(const Value: TVector2);
begin
  Y := Value.X;
  Z := Value.Y;
end;

procedure TVector3.SetZX(const Value: TVector2);
begin
  Z := Value.X;
  X := Value.Y;
end;

procedure TVector3.SetZY(const Value: TVector2);
begin
  Z := Value.X;
  Y := Value.Y;
end;

{$ENDREGION}
{$REGION 'All versions of rearrangement TIntVector3'}

function TVector3.GetXXX: TVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := X;
end;

function TVector3.GetXXY: TVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := Y;
end;

function TVector3.GetXXZ: TVector3;
begin
  Result.X := X;
  Result.Y := X;
  Result.Z := Z;
end;

function TVector3.GetXYX: TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := X;
end;

function TVector3.GetXYY: TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Y;
end;

function TVector3.GetXYZ: TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TVector3.GetXZX: TVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := X;
end;

function TVector3.GetXZY: TVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := Y;
end;

function TVector3.GetXZZ: TVector3;
begin
  Result.X := X;
  Result.Y := Z;
  Result.Z := Z;
end;

function TVector3.GetYXX: TVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := X;
end;

function TVector3.GetYXY: TVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := Y;
end;

function TVector3.GetYXZ: TVector3;
begin
  Result.X := Y;
  Result.Y := X;
  Result.Z := Z;
end;

function TVector3.GetYYX: TVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := X;
end;

function TVector3.GetYYY: TVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := Y;
end;

function TVector3.GetYYZ: TVector3;
begin
  Result.X := Y;
  Result.Y := Y;
  Result.Z := Z;
end;

function TVector3.GetYZX: TVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := X;
end;

function TVector3.GetYZY: TVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := Y;
end;

function TVector3.GetYZZ: TVector3;
begin
  Result.X := Y;
  Result.Y := Z;
  Result.Z := Z;
end;

function TVector3.GetZXX: TVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := X;
end;

function TVector3.GetZXY: TVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := Y;
end;

function TVector3.GetZXZ: TVector3;
begin
  Result.X := Z;
  Result.Y := X;
  Result.Z := Z;
end;

function TVector3.GetZYX: TVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := X;
end;

function TVector3.GetZYY: TVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := Y;
end;

function TVector3.GetZYZ: TVector3;
begin
  Result.X := Z;
  Result.Y := Y;
  Result.Z := Z;
end;

function TVector3.GetZZX: TVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := X;
end;

function TVector3.GetZZY: TVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := Y;
end;

function TVector3.GetZZZ: TVector3;
begin
  Result.X := Z;
  Result.Y := Z;
  Result.Z := Z;
end;

procedure TVector3.SetXYZ(const Value: TVector3);
begin
  X := Value.X;
  Y := Value.Y;
  Z := Value.Z;
end;

procedure TVector3.SetXZY(const Value: TVector3);
begin
  X := Value.X;
  Z := Value.Y;
  Y := Value.Z;
end;

procedure TVector3.SetYXZ(const Value: TVector3);
begin
  Y := Value.X;
  X := Value.Y;
  Z := Value.Z;
end;

procedure TVector3.SetYZX(const Value: TVector3);
begin
  Y := Value.X;
  Z := Value.Y;
  X := Value.Z;
end;

procedure TVector3.SetZXY(const Value: TVector3);
begin
  Z := Value.X;
  X := Value.Y;
  Y := Value.Z;
end;

procedure TVector3.SetZYX(const Value: TVector3);
begin
  Z := Value.X;
  Y := Value.Y;
  X := Value.Z;
end;

{$ENDREGION}

constructor TVector3.Create(X, Y, Z: Single);
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

constructor TVector3.Create(V: Single);
begin
  X := V;
  Y := V;
  Z := V;
end;

class function TVector3.Random: TVector3;
begin
  Result.X := System.Random;
  Result.Y := System.Random;
  Result.Z := System.Random;
end;

class function TVector3.RandomBox: TVector3;
begin
  Result := Random * 2 - 1;
end;

class function TVector3.RandomNormal: TVector3;
var
  O, U: Single;
begin
  O := System.Random * 2 * Pi;
  U := System.Random * 2 - 1;
  Result.X := Sqrt(1 - Sqr(U)) * Sin(O);
  Result.Y := Sqrt(1 - Sqr(U)) * Cos(O);
  Result.Z := U;
end;

class operator TVector3.Implicit(V: Single): TVector3;
begin
  Result.X := V;
  Result.Y := V;
  Result.Z := V;
end;

class operator TVector3.Implicit(const A: TIntVector3): TVector3;
begin
  Result.X := A.X;
  Result.Y := A.Y;
  Result.Z := A.Z;
end;

class operator TVector3.Add(const A, B: TVector3): TVector3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TVector3.Subtract(const A, B: TVector3): TVector3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TVector3.Multiply(const A, B: TVector3): TVector3;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
end;

class operator TVector3.Divide(const A, B: TVector3): TVector3;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
  Result.Z := A.Z / B.Z;
end;

class operator TVector3.Multiply(const A: TMatrix3; const B: TVector3): TVector3;
begin
  Result.X := A[0, 0] * B.X + A[1, 0] * B.Y + A[2, 0] * B.Z;
  Result.Y := A[0, 1] * B.X + A[1, 1] * B.Y + A[2, 1] * B.Z;
  Result.Z := A[0, 2] * B.X + A[1, 2] * B.Y + A[2, 2] * B.Z;
end;

class operator TVector3.Multiply(const A: TVector3; const B: TMatrix3): TVector3;
begin
  Result.X := A.X * B[0, 0] + A.Y * B[0, 1] + A.Z * B[0, 2];
  Result.Y := A.X * B[1, 0] + A.Y * B[1, 1] + A.Z * B[1, 2];
  Result.Z := A.X * B[2, 0] + A.Y * B[2, 1] + A.Z * B[2, 2];
end;

class operator TVector3.Multiply(const A: TMatrix4; const B: TVector3): TVector3;
begin
  Result.X := A[0, 0] * B.X + A[1, 0] * B.Y + A[2, 0] * B.Z + A[3, 0];
  Result.Y := A[0, 1] * B.X + A[1, 1] * B.Y + A[2, 1] * B.Z + A[3, 1];
  Result.Z := A[0, 2] * B.X + A[1, 2] * B.Y + A[2, 2] * B.Z + A[3, 2];
  Result := Result / (A[0, 3] * B.X + A[1, 3] * B.Y + A[2, 3] * B.Z + A[3, 3]);
end;

class operator TVector3.Multiply(const A: TVector3; const B: TMatrix4): TVector3;
begin
  Result.X := A.X * B[0, 0] + A.Y * B[0, 1] + A.Z * B[0, 2] + B[0, 3];
  Result.Y := A.X * B[1, 0] + A.Y * B[1, 1] + A.Z * B[1, 2] + B[1, 3];
  Result.Y := A.X * B[2, 0] + A.Y * B[2, 1] + A.Z * B[2, 2] + B[2, 3];
  Result := Result / (A.X * B[3, 0] + A.Y * B[3, 1] + A.Z * B[3, 2] + B[3, 3]);
end;

class operator TVector3.Positive(const A: TVector3): TVector3;
begin
  Result := A;
end;

class operator TVector3.Negative(const A: TVector3): TVector3;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
end;

class operator TVector3.Equal(const A, B: TVector3): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
end;

class operator TVector3.NotEqual(const A, B: TVector3): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z);
end;

class operator TVector3.LessThan(const A, B: TVector3): Boolean;
begin
  Result := (A.X < B.X) and (A.Y < B.Y) and (A.Z < B.Z);
end;

class operator TVector3.LessThanOrEqual(const A, B: TVector3): Boolean;
begin
  Result := (A.X <= B.X) and (A.Y <= B.Y) and (A.Z <= B.Z);
end;

function TVector3.LongestAxis: TCoordAxis3;
begin
  if System.Abs(X) >= System.Abs(Y) then
  begin
    if System.Abs(X) >= System.Abs(Z) then
      Exit(caX);
  end
  else if System.Abs(Y) >= System.Abs(Z) then
    Exit(caY);
  Result := caZ;
end;

class operator TVector3.GreaterThan(const A, B: TVector3): Boolean;
begin
  Result := (A.X > B.X) and (A.Y > B.Y) and (A.Z > B.Z);
end;

class operator TVector3.GreaterThanOrEqual(const A, B: TVector3): Boolean;
begin
  Result := (A.X >= B.X) and (A.Y >= B.Y) and (A.Z >= B.Z);
end;

function TVector3.ToString: string;
begin
  Result := Format('[%f|%f|%f]', [X, Y, Z]);
end;

class operator TVector3.Implicit(const A: TVector3): string;
begin
  Result := A.ToString;
end;

function TVector3.Length: Single;
begin
  Result := Sqrt(SqrDot);
end;

function TVector3.Normalize: TVector3;
begin
  Result := Self / Length;
end;

function TVector3.DistanceTo(const A: TVector3): Single;
begin
  Result := VectorTo(A).Length;
end;

function TVector3.VectorTo(const A: TVector3): TVector3;
begin
  Result := A - Self;
end;

function TVector3.Cross(const A: TVector3): TVector3;
begin
  Result.X := Y * A.Z - Z * A.Y;
  Result.Y := Z * A.X - X * A.Z;
  Result.Z := X * A.Y - Y * A.X;
end;

function TVector3.Dot(const A: TVector3): Single;
begin
  Result := X * A.X + Y * A.Y + Z * A.Z;
end;

function TVector3.SqrDot: Single;
begin
  Result := Sqr(X) + Sqr(Y) + Sqr(Z);
end;

function TVector3.CosAngleTo(const A: TVector3): Single;
begin
  Result := EnsureRange(Dot(A) / (Length * A.Length), -1, 1);
end;

function TVector3.AngleRadTo(const A: TVector3): Single;
begin
  Result := ArcCos(CosAngleTo(A));
end;

function TVector3.AngleTo(const A: TVector3): Single;
begin
  Result := RadToDeg(AngleRadTo(A));
end;

function TVector3.RotateRad(const AAxis: TVector3; AAngle: Single): TVector3;
var
  UX, UZ: TVector3;
  VX, VY, UXSqr, ASqr: Single;
begin
  UZ := Self.Cross(AAxis);
  UX := AAxis.Cross(UZ);

  UXSqr := UX.SqrDot;
  ASqr := AAxis.SqrDot;

  if (UXSqr = 0) or (ASqr = 0) then
    Exit(AAxis);

  VX := UX.Dot(Self) / UXSqr;
  VY := AAxis.Dot(Self) / ASqr;

  Result := Cos(AAngle) * VX * UX + VY * AAxis + Sin(AAngle) * VX * UZ;
end;

function TVector3.Rotate(const AAxis: TVector3; AAngle: Single): TVector3;
begin
  Result := RotateRad(AAxis, DegToRad(AAngle));
end;

function TVector3.Reflect(const A: TVector3): TVector3;
begin
  Result := Self - 2 * Dot(A) * A;
end;

function TVector3.Abs: TVector3;
begin
  Result.X := System.Abs(X);
  Result.Y := System.Abs(Y);
  Result.Z := System.Abs(Z);
end;

function TVector3.Floor: TIntVector3;
begin
  Result.X := System.Math.Floor(X);
  Result.Y := System.Math.Floor(Y);
  Result.Z := System.Math.Floor(Z);
end;

function TVector3.Ceil: TIntVector3;
begin
  Result.X := System.Math.Ceil(X);
  Result.Y := System.Math.Ceil(Y);
  Result.Z := System.Math.Ceil(Z);
end;

function TVector3.Min(const A: TVector3): TVector3;
begin
  Result.X := System.Math.Min(X, A.X);
  Result.Y := System.Math.Min(X, A.Y);
  Result.Z := System.Math.Min(X, A.Z);
end;

function TVector3.Max(const A: TVector3): TVector3;
begin
  Result.X := System.Math.Max(X, A.X);
  Result.Y := System.Math.Max(X, A.Y);
  Result.Z := System.Math.Max(X, A.Z);
end;

function TVector3.Dirs: TBasicDirs3;
begin
  Result := [];
  if X < 0 then
    Include(Result, bdLeft);
  if X > 0 then
    Include(Result, bdRight);
  if Y < 0 then
    Include(Result, bdDown);
  if Y > 0 then
    Include(Result, bdUp);
  if Z < 0 then
    Include(Result, bdBack);
  if Z > 0 then
    Include(Result, bdFront);
end;

{ TVector4 }

class operator TVector4.Implicit(AVector: TVector4): TVector3;
begin
  Result.Create(AVector.X, AVector.Y, AVector.Z);
  Result := Result / AVector.W;
end;

{ TBounds1 }

function TBounds1.GetPoint(APos: Single): Single;
begin
  Result := C1 + Length * APos;
end;

function TBounds1.GetPointSym(APos: Single): Single;
begin
  Result := C1 + Length * (APos / 2 + 0.5);
end;

function TBounds1.GetPointLeft(APos: Single): Single;
begin
  Result := C1 + Length * (APos + 1);
end;

function TBounds1.GetInvPoint(APos: Single): Single;
begin
  Result := (APos - C1) / Length;
end;

function TBounds1.GetInvPointSym(APos: Single): Single;
begin
  Result := (APos - C1) / Length * 2 - 1;
end;

function TBounds1.GetInvPointLeft(APos: Single): Single;
begin
  Result := (APos - C1) / Length - 1;
end;

function TBounds1.GetCorner(AIndex: TCornerIndex): Single;
begin
  if AIndex = 0 then
    Result := C1
  else
    Result := C2;
end;

procedure TBounds1.SetCorner(AIndex: TCornerIndex; const Value: Single);
begin
  if AIndex = 0 then
    C1 := Value
  else
    C2 := Value;
end;

function TBounds1.GetCorners: TCorners;
begin
  Result[0] := C1;
  Result[0] := C2;
end;

constructor TBounds1.Create(AC1, AC2: Single);
begin
  C1 := AC1;
  C2 := AC2;
end;

constructor TBounds1.Create(A: Single);
begin
  C1 := A;
  C2 := A;
end;

class operator TBounds1.Implicit(A: Single): TBounds1;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

class operator TBounds1.Implicit(A: TIntBounds1): TBounds1;
begin
  Result.C1 := A.C1;
  Result.C2 := A.C2;
end;

function TBounds1.Length: Single;
begin
  Result := C2 - C1;
end;

function TBounds1.Ceil: TIntBounds1;
begin
  Result.C1 := System.Math.Ceil(C1);
  Result.C2 := System.Math.Ceil(C2);
end;

function TBounds1.Center: Single;
begin
  Result := (C1 + C2) / 2;
end;

function TBounds1.Convert(APoint: Single; const ABounds: TBounds1): Single;
begin
  Result := ABounds[InvPoint[APoint]];
end;

function TBounds1.Clamp(ARange: TBounds1): TBounds1;
begin
  Result.C1 := Max(C1, ARange.C1);
  Result.C2 := Min(C2, ARange.C2);
end;

function TBounds1.Clamp(AValue: Single): Single;
begin
  Result := EnsureRange(AValue, C1, C2);
end;

function TBounds1.RangedModL(AValue: Single): Single;

// Copy of System.Math.FMod, but that uses Trunc, which results in a symmetric behavior
  function FMod(const ANumerator, ADenominator: Single): Single;
  begin
    Result := ANumerator - System.Math.Floor(ANumerator / ADenominator) * ADenominator;
  end;

begin
  Result := FMod(AValue - C1, Length) + C1;
end;

function TBounds1.RangedModR(AValue: Single): Single;

// Copy of System.Math.FMod, but that uses Trunc, which results in a symmetric behavior
  function FMod(const ANumerator, ADenominator: Single): Single;
  begin
    Result := ANumerator - (System.Math.Ceil(ANumerator / ADenominator) - 1) * ADenominator;
  end;

begin
  Result := FMod(AValue - C1, Length) + C1;
end;

function TBounds1.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TBounds1.Normalize: TBounds1;
begin
  if Normalized then
    Exit(Self);
  Result.C1 := C2;
  Result.C2 := C1;
end;

function TBounds1.Inset(AAmount: Single): TBounds1;
begin
  Result.C1 := C1 + AAmount;
  Result.C2 := C2 - AAmount;
end;

function TBounds1.Outset(AAmount: Single): TBounds1;
begin
  Result.C1 := C1 - AAmount;
  Result.C2 := C2 + AAmount;
end;

class operator TBounds1.Add(const A, B: TBounds1): TBounds1;
begin
  Result.C1 := A.C1 + B.C1;
  Result.C2 := A.C2 + B.C2;
end;

class operator TBounds1.Subtract(const A, B: TBounds1): TBounds1;
begin
  Result.C1 := A.C1 - B.C1;
  Result.C2 := A.C2 - B.C2;
end;

class operator TBounds1.Multiply(const A, B: TBounds1): TBounds1;
begin
  Result.C1 := A.C1 * B.C1;
  Result.C2 := A.C2 * B.C2;
end;

class operator TBounds1.Divide(const A, B: TBounds1): TBounds1;
begin
  Result.C1 := A.C1 / B.C1;
  Result.C2 := A.C2 / B.C2;
end;

class operator TBounds1.in(const A, B: TBounds1): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TBounds1.in(A: Single; const B: TBounds1): Boolean;
begin
  Result := (A >= B.C1) and (A <= B.C2);
end;

class operator TBounds1.Equal(const A, B: TBounds1): Boolean;
begin
  Result := (A.C1 = B.C1) and (A.C2 = B.C2);
end;

function TBounds1.Floor: TIntBounds1;
begin
  Result.C1 := System.Math.Floor(C1);
  Result.C2 := System.Math.Floor(C2);
end;

class operator TBounds1.NotEqual(const A, B: TBounds1): Boolean;
begin
  Result := (A.C1 <> B.C1) or (A.C2 <> B.C2);
end;

class operator TBounds1.GreaterThan(const A, B: TBounds1): Boolean;
begin
  Result := A.C1 > B.C2;
end;

class operator TBounds1.GreaterThanOrEqual(const A, B: TBounds1): Boolean;
begin
  Result := A.C1 >= B.C2;
end;

class operator TBounds1.LessThan(const A, B: TBounds1): Boolean;
begin
  Result := A.C2 < B.C1;
end;

class operator TBounds1.LessThanOrEqual(const A, B: TBounds1): Boolean;
begin
  Result := A.C2 <= B.C1;
end;

function TBounds1.ToString: string;
begin
  Result := Format('<%f~%f>', [C1, C2]);
end;

class operator TBounds1.Implicit(const ABounds: TBounds1): string;
begin
  Result := ABounds.ToString;
end;

{ TBounds2 }

function TBounds2.GetPoint(Value: TVector2): TVector2;
begin
  Result := C1 + Size * Value;
end;

function TBounds2.GetPointSym(Value: TVector2): TVector2;
begin
  Result := C1 + Size / 2 * (Value + 1);
end;

function TBounds2.GetPointLeft(APos: TVector2): TVector2;
begin
  Result := C1 + Size * (APos + 1);
end;

function TBounds2.GetInvPoint(Value: TVector2): TVector2;
begin
  Result := (Value - C1) / Size;
end;

function TBounds2.GetInvPointSym(Value: TVector2): TVector2;
begin
  Result := (Value - C1) * 2 / Size - 1;
end;

function TBounds2.GetInvPointLeft(APos: TVector2): TVector2;
begin
  Result := (APos - C1) / Size - 1;
end;

function TBounds2.GetLineX: TBounds1;
begin
  Result := Bounds1(C1.X, C2.X);
end;

function TBounds2.GetLineY: TBounds1;
begin
  Result := Bounds1(C1.Y, C2.Y);
end;

procedure TBounds2.SetLineX(const Value: TBounds1);
begin
  C1.X := Value.C1;
  C2.X := Value.C2;
end;

procedure TBounds2.SetLineY(const Value: TBounds1);
begin
  C1.Y := Value.C1;
  C2.Y := Value.C2;
end;

function TBounds2.GetCorner(AIndex: TCornerIndex): TVector2;
begin
  Result.X := IfThen(AIndex mod 2 = 0, C1.X, C2.X);
  Result.Y := IfThen(AIndex div 2 mod 2 = 0, C1.Y, C2.Y);
end;

procedure TBounds2.SetCorner(AIndex: TCornerIndex; const Value: TVector2);
begin
  if AIndex mod 2 = 0 then
    C1.X := Value.X
  else
    C2.X := Value.X;
  if AIndex div 2 mod 2 = 0 then
    C1.Y := Value.Y
  else
    C2.Y := Value.Y;
end;

function TBounds2.GetCorners: TCorners;
begin
  Result[0].Create(C1.X, C1.Y);
  Result[1].Create(C2.X, C1.Y);
  Result[2].Create(C1.X, C2.Y);
  Result[3].Create(C2.X, C2.Y);
end;

constructor TBounds2.Create(AC1, AC2: TVector2);
begin
  C1 := AC1;
  C2 := AC2;
end;

constructor TBounds2.Create(A: TVector2);
begin
  C1 := A;
  C2 := A;
end;

class operator TBounds2.Implicit(A: Single): TBounds2;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

class operator TBounds2.Implicit(A: TVector2): TBounds2;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

class operator TBounds2.Implicit(A: TIntBounds2): TBounds2;
begin
  Result.C1 := A.C1;
  Result.C2 := A.C2;
end;

function TBounds2.Size: TVector2;
begin
  Result := C2 - C1;
end;

function TBounds2.Area: Single;
begin
  Result := Width * Height;
end;

function TBounds2.Width: Single;
begin
  Result := LineX.Length;
end;

function TBounds2.Height: Single;
begin
  Result := LineY.Length;
end;

function TBounds2.Ceil: TIntBounds2;
begin
  Result.C1 := C1.Ceil;
  Result.C2 := C2.Ceil;
end;

function TBounds2.Center: TVector2;
begin
  Result := (C1 + C2) / 2;
end;

function TBounds2.Convert(APoint: TVector2; ABounds: TBounds2): TVector2;
begin
  Result := ABounds.Point[InvPoint[APoint]];
end;

function TBounds2.Clamp(ARange: TBounds2): TBounds2;
begin
  Result.LineX := LineX.Clamp(ARange.LineX);
  Result.LineY := LineY.Clamp(ARange.LineY);
end;

function TBounds2.Clamp(AValue: TVector2): TVector2;
begin
  Result.X := LineX.Clamp(AValue.X);
  Result.Y := LineY.Clamp(AValue.Y);
end;

function TBounds2.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TBounds2.Normalize: TBounds2;
begin
  Result.LineX := LineX.Normalize;
  Result.LineY := LineY.Normalize;
end;

function TBounds2.Inset(AAmount: TVector2): TBounds2;
begin
  Result.C1 := C1 + AAmount;
  Result.C2 := C2 - AAmount;
end;

function TBounds2.Outset(AAmount: TVector2): TBounds2;
begin
  Result.C1 := C1 - AAmount;
  Result.C2 := C2 + AAmount;
end;

function TBounds2.RangedModL(AValue: TVector2): TVector2;
begin
  AValue.X := LineX.RangedModL(AValue.X);
  AValue.Y := LineY.RangedModL(AValue.Y);
end;

function TBounds2.RangedModR(AValue: TVector2): TVector2;
begin
  AValue.X := LineX.RangedModR(AValue.X);
  AValue.Y := LineY.RangedModR(AValue.Y);
end;

class operator TBounds2.Add(const A, B: TBounds2): TBounds2;
begin
  Result.C1 := A.C1 + B.C1;
  Result.C2 := A.C2 + B.C2;
end;

class operator TBounds2.Subtract(const A, B: TBounds2): TBounds2;
begin
  Result.C1 := A.C1 - B.C1;
  Result.C2 := A.C2 - B.C2;
end;

class operator TBounds2.Multiply(const A, B: TBounds2): TBounds2;
begin
  Result.C1 := A.C1 * B.C1;
  Result.C2 := A.C2 * B.C2;
end;

class operator TBounds2.Divide(const A, B: TBounds2): TBounds2;
begin
  Result.C1 := A.C1 / B.C1;
  Result.C2 := A.C2 / B.C2;
end;

class operator TBounds2.in(const A, B: TBounds2): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TBounds2.in(A: TVector2; const B: TBounds2): Boolean;
begin
  Result := (A >= B.C1) and (A <= B.C2);
end;

class operator TBounds2.Equal(const A, B: TBounds2): Boolean;
begin
  Result := (A.C1 = B.C1) and (A.C2 = B.C2);
end;

function TBounds2.Floor: TIntBounds2;
begin
  Result.C1 := C1.Floor;
  Result.C2 := C2.Floor;
end;

class operator TBounds2.NotEqual(const A, B: TBounds2): Boolean;
begin
  Result := (A.C1 <> B.C1) or (A.C2 <> B.C2);
end;

class operator TBounds2.GreaterThan(const A, B: TBounds2): Boolean;
begin
  Result := A.C1 > B.C2;
end;

class operator TBounds2.GreaterThanOrEqual(const A, B: TBounds2): Boolean;
begin
  Result := A.C1 >= B.C2;
end;

class operator TBounds2.LessThan(const A, B: TBounds2): Boolean;
begin
  Result := A.C2 < B.C1;
end;

class operator TBounds2.LessThanOrEqual(const A, B: TBounds2): Boolean;
begin
  Result := A.C2 <= B.C1;
end;

function TBounds2.ToString: string;
begin
  Result := Format('<%s~%s>', [C1.ToString, C2.ToString]);
end;

class operator TBounds2.Implicit(ABounds: TBounds2): string;
begin
  Result := ABounds.ToString;
end;

{ TBounds3 }

function TBounds3.GetPoint(Value: TVector3): TVector3;
begin
  Result := C1 + Size * Value;
end;

function TBounds3.GetPointSym(Value: TVector3): TVector3;
begin
  Result := C1 + Size / 2 * (Value + 1);
end;

function TBounds3.GetPointLeft(APos: TVector3): TVector3;
begin
  Result := C1 + Size * (APos + 1);
end;

function TBounds3.GetInvPoint(Value: TVector3): TVector3;
begin
  Result := (Value - C1) / Size;
end;

function TBounds3.GetInvPointSym(Value: TVector3): TVector3;
begin
  Result := (Value - C1) * 2 / Size - 1;
end;

function TBounds3.GetInvPointLeft(APos: TVector3): TVector3;
begin
  Result := (APos - C1) / Size - 1;
end;

function TBounds3.GetLineX: TBounds1;
begin
  Result := Bounds1(C1.X, C2.X);
end;

function TBounds3.GetLineY: TBounds1;
begin
  Result := Bounds1(C1.Y, C2.Y);
end;

function TBounds3.GetLineZ: TBounds1;
begin
  Result := Bounds1(C1.Z, C2.Z);
end;

procedure TBounds3.SetLineX(const Value: TBounds1);
begin
  C1.X := Value.C1;
  C2.X := Value.C2;
end;

procedure TBounds3.SetLineY(const Value: TBounds1);
begin
  C1.Y := Value.C1;
  C2.Y := Value.C2;
end;

procedure TBounds3.SetLineZ(const Value: TBounds1);
begin
  C1.Z := Value.C1;
  C2.Z := Value.C2;
end;

function TBounds3.GetPlaneXY: TBounds2;
begin
  Result.LineX := LineX;
  Result.LineY := LineY;
end;

function TBounds3.GetPlaneYZ: TBounds2;
begin
  Result.LineX := LineY;
  Result.LineY := LineZ;
end;

function TBounds3.GetPlaneZX: TBounds2;
begin
  Result.LineX := LineZ;
  Result.LineY := LineX;
end;

function TBounds3.GetPlaneYX: TBounds2;
begin
  Result.LineX := LineY;
  Result.LineY := LineX;
end;

function TBounds3.GetPlaneZY: TBounds2;
begin
  Result.LineX := LineZ;
  Result.LineY := LineY;
end;

function TBounds3.GetPlaneXZ: TBounds2;
begin
  Result.LineX := LineX;
  Result.LineY := LineZ;
end;

procedure TBounds3.SetPlaneXY(const Value: TBounds2);
begin
  LineX := Value.LineX;
  LineY := Value.LineY;
end;

procedure TBounds3.SetPlaneYZ(const Value: TBounds2);
begin
  LineY := Value.LineX;
  LineZ := Value.LineY;
end;

procedure TBounds3.SetPlaneZX(const Value: TBounds2);
begin
  LineZ := Value.LineX;
  LineX := Value.LineY;
end;

procedure TBounds3.SetPlaneYX(const Value: TBounds2);
begin
  LineY := Value.LineX;
  LineX := Value.LineY;
end;

procedure TBounds3.SetPlaneZY(const Value: TBounds2);
begin
  LineZ := Value.LineX;
  LineY := Value.LineY;
end;

procedure TBounds3.SetPlaneXZ(const Value: TBounds2);
begin
  LineX := Value.LineX;
  LineZ := Value.LineY;
end;

function TBounds3.GetCorner(AIndex: TCornerIndex): TVector3;
begin
  Result.X := IfThen(AIndex mod 2 = 0, C1.X, C2.X);
  Result.Y := IfThen(AIndex div 2 mod 2 = 0, C1.Y, C2.Y);
  Result.Z := IfThen(AIndex div 4 mod 2 = 0, C1.Z, C2.Z);
end;

procedure TBounds3.SetCorner(AIndex: TCornerIndex; const Value: TVector3);
begin
  if AIndex mod 2 = 0 then
    C1.X := Value.X
  else
    C2.X := Value.X;
  if AIndex div 2 mod 2 = 0 then
    C1.Y := Value.Y
  else
    C2.Y := Value.Y;
  if AIndex div 4 mod 2 = 0 then
    C1.Z := Value.Z
  else
    C2.Z := Value.Z;
end;

function TBounds3.GetCorners: TCorners;
begin
  Result[0].Create(C1.X, C1.Y, C1.Z);
  Result[1].Create(C2.X, C1.Y, C1.Z);
  Result[2].Create(C1.X, C2.Y, C1.Z);
  Result[3].Create(C2.X, C2.Y, C1.Z);
  Result[4].Create(C1.X, C1.Y, C2.Z);
  Result[5].Create(C2.X, C1.Y, C2.Z);
  Result[6].Create(C1.X, C2.Y, C2.Z);
  Result[7].Create(C2.X, C2.Y, C2.Z);
end;

constructor TBounds3.Create(AC1, AC2: TVector3);
begin
  C1 := AC1;
  C2 := AC2;
end;

constructor TBounds3.Create(A: TVector3);
begin
  C1 := A;
  C2 := A;
end;

class operator TBounds3.Implicit(A: Single): TBounds3;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

class operator TBounds3.Implicit(A: TVector3): TBounds3;
begin
  Result.C1 := A;
  Result.C2 := A;
end;

class operator TBounds3.Implicit(A: TIntBounds1): TBounds3;
begin
  Result.C1 := A.C1;
  Result.C2 := A.C2;
end;

function TBounds3.Size: TVector3;
begin
  Result := C2 - C1;
end;

function TBounds3.Volume: Single;
begin
  Result := Width * Height * Depth;
end;

function TBounds3.Width: Single;
begin
  Result := LineX.Length;
end;

function TBounds3.Height: Single;
begin
  Result := LineY.Length;
end;

function TBounds3.Depth: Single;
begin
  Result := LineZ.Length;
end;

function TBounds3.Ceil: TIntBounds3;
begin
  Result.C1 := C1.Ceil;
  Result.C2 := C2.Ceil;
end;

function TBounds3.Center: TVector3;
begin
  Result := (C1 + C2) / 2;
end;

function TBounds3.Convert(APoint: TVector3; ABounds: TBounds3): TVector3;
begin
  Result := ABounds.Point[InvPoint[APoint]];
end;

function TBounds3.Clamp(ARange: TBounds3): TBounds3;
begin
  Result.LineX := LineX.Clamp(ARange.LineX);
  Result.LineY := LineY.Clamp(ARange.LineY);
  Result.LineZ := LineZ.Clamp(ARange.LineZ);
end;

function TBounds3.Clamp(AValue: TVector3): TVector3;
begin
  Result.X := LineX.Clamp(AValue.X);
  Result.Y := LineY.Clamp(AValue.Y);
  Result.Z := LineZ.Clamp(AValue.Z);
end;

function TBounds3.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TBounds3.Normalize: TBounds3;
begin
  Result.LineX := LineX.Normalize;
  Result.LineY := LineY.Normalize;
  Result.LineZ := LineZ.Normalize;
end;

function TBounds3.Inset(AAmount: TVector3): TBounds3;
begin
  Result.C1 := C1 + AAmount;
  Result.C2 := C2 - AAmount;
end;

function TBounds3.Outset(AAmount: TVector3): TBounds3;
begin
  Result.C1 := C1 - AAmount;
  Result.C2 := C2 + AAmount;
end;

function TBounds3.RangedModL(AValue: TVector3): TVector3;
begin
  AValue.X := LineX.RangedModL(AValue.X);
  AValue.Y := LineY.RangedModL(AValue.Y);
  AValue.Z := LineZ.RangedModL(AValue.Z);
end;

function TBounds3.RangedModR(AValue: TVector3): TVector3;
begin
  AValue.X := LineX.RangedModR(AValue.X);
  AValue.Y := LineY.RangedModR(AValue.Y);
  AValue.Z := LineZ.RangedModR(AValue.Z);
end;

class operator TBounds3.Add(const A, B: TBounds3): TBounds3;
begin
  Result.C1 := A.C1 + B.C1;
  Result.C2 := A.C2 + B.C2;
end;

class operator TBounds3.Subtract(const A, B: TBounds3): TBounds3;
begin
  Result.C1 := A.C1 - B.C1;
  Result.C2 := A.C2 - B.C2;
end;

class operator TBounds3.Multiply(const A, B: TBounds3): TBounds3;
begin
  Result.C1 := A.C1 * B.C1;
  Result.C2 := A.C2 * B.C2;
end;

class operator TBounds3.Divide(const A, B: TBounds3): TBounds3;
begin
  Result.C1 := A.C1 / B.C1;
  Result.C2 := A.C2 / B.C2;
end;

class operator TBounds3.in(const A, B: TBounds3): Boolean;
begin
  Result := (A.C1 >= B.C1) and (A.C2 <= B.C2);
end;

class operator TBounds3.in(A: TVector3; const B: TBounds3): Boolean;
begin
  Result := (A >= B.C1) and (A <= B.C2);
end;

class operator TBounds3.Equal(const A, B: TBounds3): Boolean;
begin
  Result := (A.C1 = B.C1) and (A.C2 = B.C2);
end;

function TBounds3.Floor: TIntBounds3;
begin
  Result.C1 := C1.Floor;
  Result.C2 := C2.Floor;
end;

class operator TBounds3.NotEqual(const A, B: TBounds3): Boolean;
begin
  Result := (A.C1 <> B.C1) or (A.C2 <> B.C2);
end;

class operator TBounds3.GreaterThan(const A, B: TBounds3): Boolean;
begin
  Result := A.C1 > B.C2;
end;

class operator TBounds3.GreaterThanOrEqual(const A, B: TBounds3): Boolean;
begin
  Result := A.C1 >= B.C2;
end;

class operator TBounds3.LessThan(const A, B: TBounds3): Boolean;
begin
  Result := A.C2 < B.C1;
end;

class operator TBounds3.LessThanOrEqual(const A, B: TBounds3): Boolean;
begin
  Result := A.C2 <= B.C1;
end;

function TBounds3.ToString: string;
begin
  Result := Format('<%s~%s>', [C1.ToString, C2.ToString]);
end;

class operator TBounds3.Implicit(ABounds: TBounds3): string;
begin
  Result := ABounds.ToString;
end;

{ TVectorDir }

function TVectorDir.GetTurnAngle: Single;
begin
  Result := RadToDeg(TurnAngleRad);
end;

function TVectorDir.GetPitchAngle: Single;
begin
  Result := RadToDeg(PitchAngleRad);
end;

procedure TVectorDir.SetTurnAngle(Value: Single);
begin
  TurnAngleRad := DegToRad(Value);
end;

procedure TVectorDir.SetPitchAngle(Value: Single);
begin
  PitchAngleRad := DegToRad(Value);
end;

class function TVectorDir.CreateRad(ATurnAngleRad, APitchAngleRad: Single): TVectorDir;
begin
  Result.TurnAngleRad := ATurnAngleRad;
  Result.PitchAngleRad := APitchAngleRad;
end;

constructor TVectorDir.Create(ATurnAngle, APitchAngle: Single);
begin
  TurnAngle := ATurnAngle;
  PitchAngle := APitchAngle;
end;

constructor TVectorDir.Create(AVector: TVector3);
begin
  if AVector = 0 then
  begin
    PitchAngleRad := NaN;
    TurnAngleRad := NaN;
    Exit;
  end;
  AVector := AVector.Normalize;
  PitchAngleRad := ArcSin(AVector.Y);
  TurnAngleRad := Sqrt(1 - Sqr(AVector.Y));
  if TurnAngleRad <> 0 then
  begin
    TurnAngleRad := EnsureRange(AVector.X / TurnAngleRad, -1, +1);
    TurnAngleRad := ArcSin(TurnAngleRad);
    if AVector.Z < 0 then
      TurnAngleRad := Pi - TurnAngleRad;
  end;
end;

function TVectorDir.Vector: TVector3;
begin
  Result := TVector3.Create(
    Sin(TurnAngleRad) * Cos(PitchAngleRad),
    Sin(PitchAngleRad),
    Cos(TurnAngleRad) * Cos(PitchAngleRad));
end;

class operator TVectorDir.Equal(const A, B: TVectorDir): Boolean;
begin
  Result := (A.TurnAngleRad = B.TurnAngleRad) and (A.PitchAngleRad = B.PitchAngleRad);
end;

class operator TVectorDir.NotEqual(const A, B: TVectorDir): Boolean;
begin
  Result := (A.TurnAngleRad <> B.TurnAngleRad) or (A.PitchAngleRad <> B.PitchAngleRad);
end;

class operator TVectorDir.Implicit(const AVector: TVector3): TVectorDir;
begin
  Result.Create(AVector);
end;

class operator TVectorDir.Implicit(const ADirection: TVectorDir): TVector3;
begin
  Result := ADirection.Vector;
end;

{ TLine2 }

function TLine2.GetPoint(Value: Single): TVector2;
begin
  Result := S + Value * D;
end;

class operator TLine2.Equal(const A, B: TLine2): Boolean;
begin
  Result := (A.S = B.S) and (A.D = B.D);
end;

function TLine2.GetHead: TVector2;
begin
  Result := S + D;
end;

function TLine2.GetTail: TVector2;
begin
  Result := S;
end;

procedure TLine2.SetHead(Value: TVector2);
begin
  D := Value - S;
end;

procedure TLine2.SetTail(Value: TVector2);
begin
  D := D - S + Value;
  S := Value;
end;

constructor TLine2.Create(const S, D: TVector2);
begin
  Self.S := S;
  Self.D := D;
end;

function TLine2.OrthoProj(const A: TVector2): Single;
begin
  Result := D.Dot(A - S) / D.SqrDot;
end;

function TLine2.Intsec(const A: TLine2): Boolean;
var
  Data: TIntsecFactors;
begin
  Result := Intsec(A, Data);
end;

function TLine2.Intsec(const A: TLine2; out AFactors: TIntsecFactors): Boolean;
var
  R: array [0 .. 1] of Single;
  M: TMatrix;
begin
  M.Size := IVec2(3, 2);
  M[0, 0] := D.X;
  M[1, 0] := -A.D.X;
  M[2, 0] := A.S.X - S.X;
  M[0, 1] := D.Y;
  M[1, 1] := -A.D.Y;
  M[2, 1] := A.S.Y - S.Y;

  Result := M.Solve(R);
  if Result then
  begin
    AFactors.Factor := R[0];
    AFactors.FactorOther := R[1];
  end;
end;

class operator TLine2.NotEqual(const A, B: TLine2): Boolean;
begin
  Result := (A.S <> B.S) or (A.D <> B.D);
end;

function TLine2.Height(const A: TVector2): Single;
begin
  if D = 0 then
    Exit(S.DistanceTo(A));
  Result := TLine2.Create(S, D.Cross.Normalize).OrthoProj(A);
end;

function TLine2.Side(A: TVector2): TLineSide;
var
  H: Single;
begin
  H := Height(A);
  if H > 0 then
    Result := lsLeft
  else if H <> 0 then // Checking for 0 is faster
    Result := lsRight
  else
    Result := lsOn;
end;

{ TLine3 }

function TLine3.GetPoint(Value: Single): TVector3;
begin
  Result := S + Value * D;
end;

class operator TLine3.Equal(const A, B: TLine3): Boolean;
begin
  Result := (A.S = B.S) and (A.D = B.D);
end;

function TLine3.GetHead: TVector3;
begin
  Result := S + D;
end;

function TLine3.GetTail: TVector3;
begin
  Result := S;
end;

procedure TLine3.SetHead(Value: TVector3);
begin
  D := Value - S;
end;

procedure TLine3.SetTail(Value: TVector3);
begin
  D := D - S + Value;
  S := Value;
end;

constructor TLine3.Create(const S, D: TVector3);
begin
  Self.S := S;
  Self.D := D;
end;

function TLine3.OrthoProj(const A: TVector3): Single;
begin
  Result := D.Dot(A - S) / D.SqrDot;
end;

function TLine3.OrthoProj(const A: TLine3): Boolean;
var
  Data: Single;
begin
  Result := OrthoProj(A, Data);
end;

function TLine3.OrthoProj(const A: TLine3; out AFactor: Single): Boolean;
var
  Plane: TPlane3;
  Factors: TPlane3.TLineIntsecFactors;
begin
  Plane.S := S;
  Plane.DX := D;
  Plane.DY := D.Cross(A.D);
  Result := Plane.Intsec(A, Factors);
  if Result then
    AFactor := Factors.LineFactor;
end;

function TLine3.OrthoProj(const A: TLine3; out APoint: TVector3): Boolean;
var
  Factor: Single;
begin
  Result := OrthoProj(A, Factor);
  if Result then
    APoint := Self[Factor];
end;

function TLine3.OrthoProj(const A: TLine3; out AFactor: Single; out APoint: TVector3): Boolean;
begin
  Result := OrthoProj(A, AFactor);
  if Result then
    APoint := Self[AFactor];
end;

function TLine3.Height(const A: TVector3): Single;

{$IFDEF CPUX86}

var
  B: TVector3;
  C: Single;
begin
  B := A.VectorTo(S);
  C := D.SqrDot;
  Result := Sqrt(Abs(B.SqrDot * C - Sqr(B.Dot(D)) / C));

{$ELSE}

begin
  Result := D.Cross(A.VectorTo(S)).Length / D.Length;

  {$ENDIF}

end;

function TLine3.Mirror(APoint: TVector3): TVector3;
begin
  Result := APoint - D * OrthoProj(APoint) * 2;
end;

class operator TLine3.NotEqual(const A, B: TLine3): Boolean;
begin
  Result := (A.S <> B.S) or (A.D <> B.D);
end;

{ TPlane2 }

function TAxisSystem2.GetPoint(APos: TVector2): TVector2;
begin
  Result := S + APos.X * DX + APos.Y * DY;
end;

class operator TAxisSystem2.in(const A: TVector2; const B: TAxisSystem2): Boolean;
begin
  Result := B.InvPoint[A] in Bounds2(0, 1);
end;

function TAxisSystem2.GetInvPoint(APos: TVector2): TVector2;
begin
  Result.X := (DY.Y * (APos.X - S.X) - DY.X * (APos.Y - S.Y)) / (DX.X * DY.Y - DX.Y * DY.X);
  if Abs(DY.X) > Abs(DY.Y) then
    Result.Y := (APos.X - S.X - Result.X * DX.X) / DY.X
  else
    Result.Y := (APos.Y - S.Y - Result.X * DX.Y) / DY.Y;
end;

constructor TAxisSystem2.Create(const S, X, Y: TVector2);
begin
  Self.S := S;
  Self.DX := X;
  Self.DY := Y;
end;

function TAxisSystem2.Area: Single;
begin
  Result := Vec3(DX.X, DX.Y, 0).Cross(Vec3(DY.X, DY.Y, 0)).Z;
end;

class operator TAxisSystem2.Equal(const A, B: TAxisSystem2): Boolean;
begin
  Result := (A.S = B.S) and (A.DX = B.DX) and (A.DY = B.DY);
end;

class operator TAxisSystem2.NotEqual(const A, B: TAxisSystem2): Boolean;
begin
  Result := (A.S <> B.S) or (A.DX <> B.DX) or (A.DY <> B.DY);
end;

{ TPlane3 }

function TPlane3.GetPoint(Value: TVector2): TVector3;
begin
  Result := S + Value.X * DX + Value.Y * DY;
end;

function TPlane3.Perpendicular: TVector3;
begin
  Result := DX.Cross(DY);
end;

function TPlane3.Area: Single;
begin
  Result := Normal.Length;
end;

function TPlane3.Normal: TVector3;
begin
  Result := Perpendicular.Normalize;
end;

class operator TPlane3.NotEqual(const A, B: TPlane3): Boolean;
begin
  Result := (A.S <> B.S) or (A.DX <> B.DX) or (A.DY <> B.DY);
end;

constructor TPlane3.Create(const S, X, Y: TVector3);
begin
  Self.S := S;
  Self.DX := X;
  Self.DY := Y;
end;

class operator TPlane3.Equal(const A, B: TPlane3): Boolean;
begin
  Result := (A.S = B.S) and (A.DX = B.DX) and (A.DY = B.DY);
end;

function TPlane3.OrthoProj(const A: TVector3): TVector2;
begin
  // Yes, this is probably the fastest I can get :)
  Result.X := (DY.SqrDot * DX.Dot(A - S) - DY.Dot(DX) * DY.Dot(A - S)) / (DX.SqrDot * DY.SqrDot - Sqr(DX.Dot(DY)));
  Result.Y := (DX.SqrDot * DY.Dot(A - S) - DX.Dot(DY) * DX.Dot(A - S)) / (DY.SqrDot * DX.SqrDot - Sqr(DY.Dot(DX)));
end;

function TPlane3.Height(const A: TVector3): Single;
begin
  Result := TLine3.Create(S, Normal).OrthoProj(A);
end;

function TPlane3.Intsec(const A: TLine3): Boolean;
var
  Data: TLineIntsecFactors;
begin
  Result := Intsec(A, Data);
end;

function TPlane3.Intsec(const A: TLine3; out AFactors: TLineIntsecFactors): Boolean;
var
  R: array [0 .. 2] of Single;
  M: TMatrix;
begin
  M.Size := IVec2(4, 3);
  M[0, 0] := DX.X;
  M[1, 0] := DY.X;
  M[2, 0] := -A.D.X;
  M[3, 0] := A.S.X - S.X;
  M[0, 1] := DX.Y;
  M[1, 1] := DY.Y;
  M[2, 1] := -A.D.Y;
  M[3, 1] := A.S.Y - S.Y;
  M[0, 2] := DX.Z;
  M[1, 2] := DY.Z;
  M[2, 2] := -A.D.Z;
  M[3, 2] := A.S.Z - S.Z;

  Result := M.Solve(R);
  if Result then
  begin
    AFactors.PlaneFactors.X := R[0];
    AFactors.PlaneFactors.Y := R[1];
    AFactors.LineFactor := R[2];
  end;
end;

function TPlane3.Intsec(const A: TLine3; out APoint: TVector3): Boolean;
var
  Factors: TLineIntsecFactors;
begin
  Result := Intsec(A, Factors);
  if Result then
    APoint := A[Factors.LineFactor];
end;

function TPlane3.Intsec(const A: TLine3; out AFactors: TLineIntsecFactors; out APoint: TVector3): Boolean;
begin
  Result := Intsec(A, AFactors);
  if Result then
    APoint := A[AFactors.LineFactor];
end;

function TPlane3.Intsec(const A: TPlane3): Boolean;
var
  Line: TLine3;
begin
  Result := Intsec(A, Line);
end;

function TPlane3.Intsec(const A: TPlane3; out ALine: TLine3): Boolean;
var
  N: TVector3;
  IntsecLine: TLine3;
  Data: TLineIntsecFactors;
begin
  N := Perpendicular;
  ALine.D := N.Cross(A.Perpendicular);
  IntsecLine.D := ALine.D.Cross(N);
  IntsecLine.S := S;
  Result := A.Intsec(IntsecLine, Data);
  if Result then
    ALine.S := IntsecLine[Data.LineFactor];
end;

function TPlane3.IntsecP(const A: TLine3; out AFactors: TLineIntsecFactors; out APoint: TVector3): Boolean;
begin
  Result := Intsec(A, AFactors);
  if Result then
    APoint := Self[AFactors.PlaneFactors];
end;

function TPlane3.IntsecP(const A: TLine3; out APoint: TVector3): Boolean;
var
  Factors: TLineIntsecFactors;
begin
  Result := Intsec(A, Factors);
  if Result then
    APoint := Self[Factors.PlaneFactors];
end;

function TPlane3.CosAngleTo(const A: TVector3): Single;
begin
  Result := Perpendicular.CosAngleTo(A);
end;

function TPlane3.AngleRadTo(const A: TVector3): Single;
begin
  Result := EnsureRange(Pi / 2 - Perpendicular.AngleRadTo(A), 0, Pi / 2);
end;

function TPlane3.AngleTo(const A: TVector3): Single;
begin
  Result := EnsureRange(90 - Perpendicular.AngleTo(A), 0, 90);
end;

function TPlane3.CosAngleTo(const A: TPlane3): Single;
begin
  Result := Perpendicular.CosAngleTo(A.Perpendicular);
end;

function TPlane3.AngleRadTo(const A: TPlane3): Single;
begin
  Result := Perpendicular.AngleRadTo(A.Perpendicular);
end;

function TPlane3.AngleTo(const A: TPlane3): Single;
begin
  Result := Perpendicular.AngleTo(A.Perpendicular);
end;

{ TVector2Helper }

function TVector2Helper.Bounds(const ASize: TVector2): TBounds2;
begin
  Result.Create(Self, Self + ASize);
end;

function TVector2Helper.LineTo(const A: TVector2): TLine2;
begin
  Result.Create(Self, VectorTo(A));
end;

{ TVector3Helper }

function TVector3Helper.Bounds(const ASize: TVector3): TBounds3;
begin
  Result.Create(Self, Self + ASize);
end;

function TVector3Helper.LineTo(const A: TVector3): TLine3;
begin
  Result.Create(Self, VectorTo(A));
end;

{ TLocation3.TChangeEventInfo }

constructor TLocation3.TChangeEventInfo.Create(ASender: TLocation3; AChanges: TChanges);
begin
  FSender := ASender;
  FChanges := AChanges;
end;

function TLocation3.TChangeEventInfo.Sender: TLocation3;
begin
  Result := FSender;
end;

{ TLocation3 }

procedure TLocation3.SetLook(AValue: TVector3);
var
  D: TVectorDir;
begin
  // Z points in other Direction
  AValue.Z := -AValue.Z;
  D := AValue;
  TurnAngle := D.TurnAngle;
  PitchAngle := D.PitchAngle;
end;

procedure TLocation3.Changed(AChanges: TChanges);
begin
  FChanged := True;
  if AChanges - FreeChange = AChanges then // free changes matrix > no notify
    FMatrixChanged := True;
  FOnChanged.Execute(TChangeEventInfo.Create(Self, AChanges));
end;

function TLocation3.GetMatrix: TMatrix4;
begin
  if FMatrixChanged then
    BuildMatrix;
  FInvMatrixChanged := True;
  FRotMatrixChanged := True;
  Result := FMatrix;
end;

function TLocation3.GetOnChanged: TChangeEvent.TAccess;
begin
  Result := FOnChanged.Access;
end;

function TLocation3.GetPoint(APos: TVector3): TVector3;
begin
  Result := Matrix * APos;
end;

function TLocation3.GetAxisSystem: TAxisSystem3;
begin
  Result.Create(
    Vec3(Matrix[3, 0], Matrix[3, 1], Matrix[3, 2]),
    Vec3(Matrix[0, 0], Matrix[0, 1], Matrix[0, 2]),
    Vec3(Matrix[1, 0], Matrix[1, 1], Matrix[1, 2]),
    Vec3(Matrix[2, 0], Matrix[2, 1], Matrix[2, 2])
    );
end;

function TLocation3.GetInvMatrix: TMatrix4;
begin
  if FMatrixChanged or FInvMatrixChanged then
    FInvMatrix := Matrix.Inverse;
  Result := FInvMatrix;
  FInvMatrixChanged := False;
end;

function TLocation3.GetInvPoint(APos: TVector3): TVector3;
begin
  Result := InvMatrix * APos;
end;

function TLocation3.GetRotMatrix: TMatrix3;
begin
  if FMatrixChanged or FRotMatrixChanged then
    FRotMatrix := Matrix.Minor[3];
  Result := FRotMatrix;
  FRotMatrixChanged := False;
  FInvRotMatrixChanged := True;
end;

function TLocation3.GetInvRotMatrix: TMatrix3;
begin
  if FMatrixChanged or FRotMatrixChanged or FInvRotMatrixChanged then
    FInvRotMatrix := RotMatrix.Inverse;
  Result := FInvRotMatrix;
  FInvRotMatrixChanged := False;
end;

function TLocation3.GetRealPosition: TVector3;
begin
  Result := TVector3.Create(
    Matrix[3, 0],
    Matrix[3, 1],
    Matrix[3, 2]
    );
  Result := Result * RotMatrix;
  if FInverted then
    Result := -Result;
end;

function TLocation3.GetRight: TVector3;
begin
  if FMatrixChanged then
    BuildMatrix;
  if FInverted then
  begin
    Result := TVector3.Create(
      FMatrix[0, 0],
      FMatrix[1, 0],
      FMatrix[2, 0]
      );
  end
  else
  begin
    Result := TVector3.Create(
      FMatrix[0, 0],
      FMatrix[0, 1],
      FMatrix[0, 2]
      );
  end;
end;

function TLocation3.GetLook: TVector3;
begin
  if FMatrixChanged then
    BuildMatrix;
  if FInverted then
  begin
    Result := TVector3.Create(
      -FMatrix[0, 2],
      -FMatrix[1, 2],
      -FMatrix[2, 2]
      );
  end
  else
  begin
    Result := TVector3.Create(
      -FMatrix[2, 0],
      -FMatrix[2, 1],
      -FMatrix[2, 2]
      );
  end;
end;

function TLocation3.GetUp: TVector3;
begin
  if FMatrixChanged then
    BuildMatrix;
  if FInverted then
  begin
    Result := TVector3.Create(
      FMatrix[0, 1],
      FMatrix[1, 1],
      FMatrix[2, 1]
      );
  end
  else
  begin
    Result := TVector3.Create(
      FMatrix[1, 0],
      FMatrix[1, 1],
      FMatrix[1, 2]
      );
  end;
end;

procedure TLocation3.BuildMatrix;
begin
  FOnChanged.Disable;
  if Parent <> nil then
    FMatrix := Parent.Matrix
  else
    FMatrix.LoadIdentity;
  if FInverted then
  begin
    FreeTranslate( -FOffset);
    FreeScale(1 / FScale);
    FreeRoll( -RollAngle);
    FreePitch( -PitchAngle);
    FreeTurn( -TurnAngle);
    FreeTranslate( -FPos);
  end
  else
  begin
    FreeTranslate(FPos);
    FreeTurn(TurnAngle);
    FreePitch(PitchAngle);
    FreeRoll(RollAngle);
    FreeScale(FScale);
    FreeTranslate(FOffset);
  end;
  FMatrixChanged := False;
  FFreeChanged := False;
  FChanged := True;
  FOnChanged.Enable;
end;

procedure TLocation3.SetPos(AValue: TVector3);
begin
  if FPos = AValue then
    Exit;
  FPos := AValue;
  Changed(lcPos);
end;

procedure TLocation3.SetPosX(AValue: Single);
begin
  if FPos.X = AValue then
    Exit;
  FPos.X := AValue;
  Changed(lcPos);
end;

procedure TLocation3.SetPosY(AValue: Single);
begin
  if FPos.Y = AValue then
    Exit;
  FPos.Y := AValue;
  Changed(lcPos);
end;

procedure TLocation3.SetPosZ(AValue: Single);
begin
  if FPos.Z = AValue then
    Exit;
  FPos.Z := AValue;
  Changed(lcPos);
end;

procedure TLocation3.SetOffset(AValue: TVector3);
begin
  if FOffset = AValue then
    Exit;
  FOffset := AValue;
  Changed(lcOffset);
end;

procedure TLocation3.SetOffsetX(AValue: Single);
begin
  if FOffset.X = AValue then
    Exit;
  FOffset.X := AValue;
  Changed(lcOffset);
end;

procedure TLocation3.SetOffsetY(AValue: Single);
begin
  if FOffset.Y = AValue then
    Exit;
  FOffset.Y := AValue;
  Changed(lcOffset);
end;

procedure TLocation3.SetOffsetZ(AValue: Single);
begin
  if FOffset.Z = AValue then
    Exit;
  FOffset.Z := AValue;
  Changed(lcOffset);
end;

procedure TLocation3.SetScale(AValue: TVector3);
begin
  if FScale = AValue then
    Exit;
  FScale := AValue;
  Changed(lcScale);
end;

procedure TLocation3.SetScaleX(AValue: Single);
begin
  if FScale.X = AValue then
    Exit;
  FScale.X := AValue;
  Changed(lcScale);
end;

procedure TLocation3.SetScaleY(AValue: Single);
begin
  if FScale.Y = AValue then
    Exit;
  FScale.Y := AValue;
  Changed(lcScale);
end;

procedure TLocation3.SetScaleZ(AValue: Single);
begin
  if FScale.Z = AValue then
    Exit;
  FScale.Z := AValue;
  Changed(lcScale);
end;

procedure TLocation3.SetPitch(AValue: Single);
begin
  AValue := RotationLimit.RangedModL(AValue);
  if FRotation.X = AValue then
    Exit;
  FRotation.X := AValue;
  Changed(lcPitch);
end;

procedure TLocation3.SetRoll(AValue: Single);
begin
  AValue := RotationLimit.RangedModL(AValue);
  if FRotation.Z = AValue then
    Exit;
  FRotation.Z := AValue;
  Changed(lcRoll);
end;

procedure TLocation3.SetTurn(AValue: Single);
begin
  AValue := RotationLimit.RangedModL(AValue);
  if FRotation.Y = AValue then
    Exit;
  FRotation.Y := AValue;
  Changed(lcTurn);
end;

procedure TLocation3.ParentChanged(AInfo: TChangeEventInfo);
begin
  Changed(lcParent);
end;

procedure TLocation3.SetParent(const Value: TLocation3);
begin
  if FParent = Value then
    Exit;
  if FParent <> nil then
    FParent.OnChanged.Del(ParentChanged);
  FParent := Value;
  if FParent <> nil then
    FParent.OnChanged.Add(ParentChanged);
  Changed(lcParent);
end;

procedure TLocation3.Changed(AChange: TChange);
begin
  FChanged := True;
  if not(AChange in FreeChange) then // free changes matrix > no notify
    FMatrixChanged := True;
  FOnChanged.Execute(TChangeEventInfo.Create(Self, [AChange]));
end;

constructor TLocation3.Create(AInverted: Boolean);
begin
  FInverted := AInverted;
  FPos := 0;
  FOffset := 0;
  FScale := 1;
  FRotation := 0;
  FChanged := True;
end;

procedure TLocation3.Slide(ADistance: Single; AHorizontal: Boolean);
begin
  if AHorizontal then
    Pos := Pos + Vec3(Right.X * ADistance, 0, Right.Z * ADistance)
  else
    Pos := Pos + Right * ADistance;
end;

procedure TLocation3.Lift(ADistance: Single; AYOnly: Boolean);
begin
  if AYOnly then
    Pos := Pos + Vec3(0, Up.Y * ADistance, 0)
  else
    Pos := Pos + Up * ADistance;
end;

procedure TLocation3.Move(ADistance: Single; AHorizontal: Boolean);
begin
  if AHorizontal then
    Pos := Pos + Vec3(Look.X * ADistance, 0, Look.Z * ADistance)
  else
    Pos := Pos + Look * ADistance;
end;

procedure TLocation3.LookAt(APoint: TVector3);
begin
  Look := RealPosition.VectorTo(APoint);
end;

procedure TLocation3.Reset;
begin
  Pos := 0;
  Offset := 0;
  Scale := 1;
  FRotation := 0;
  FMatrix.LoadIdentity;
  FChanged := True;
  FOnChanged.Execute(TChangeEventInfo.Create(Self, [lcPos .. lcRoll]));
end;

procedure TLocation3.ResetTranslation;
begin
  if Pos = 0 then
    Exit;
  Pos := 0;
  Changed(lcPos);
end;

procedure TLocation3.ResetOffset;
begin
  if Offset = 0 then
    Exit;
  Offset := 0;
  Changed(lcOffset);
end;

procedure TLocation3.ResetScale;
begin
  if Scale = 0 then
    Exit;
  Scale := 0;
  Changed(lcScale);
end;

procedure TLocation3.ResetRotation;
begin
  if FRotation = 0 then
    Exit;
  FRotation := 0;
  Changed([lcTurn, lcPitch, lcRoll]);
end;

procedure TLocation3.Turn(const ATurn: Single);
begin
  TurnAngle := TurnAngle + ATurn;
end;

procedure TLocation3.Pitch(const APitch: Single);
begin
  PitchAngle := PitchAngle + APitch;
end;

procedure TLocation3.Roll(const ARoll: Single);
begin
  RollAngle := RollAngle + ARoll;
end;

procedure TLocation3.Rotate(const ARotation: TVector3);
var
  Changes: TChanges;
begin
  Changes := [];
  if ARotation.X <> 0 then
    Include(Changes, lcPitch);
  if ARotation.Y <> 0 then
    Include(Changes, lcTurn);
  if ARotation.Z <> 0 then
    Include(Changes, lcRoll);
  if Changes <> [] then
  begin
    FRotation := FRotation + ARotation;
    FFreeChanged := True;
    Changed(Changes);
  end;
end;

procedure TLocation3.Translate(const AVector: TVector3);
begin
  FPos := FPos + AVector;
  Changed(lcPos);
end;

procedure TLocation3.MoveOffset(const AVector: TVector3);
begin
  FOffset := FOffset + AVector;
  Changed(lcOffset);
end;

procedure TLocation3.ScaleBy(const AScale: TVector3);
begin
  Scale := Scale * AScale;
end;

procedure TLocation3.Approach(ALocation: TLocation3; ADelta: Single);

  function ApproachRotation(AValue, AFinal: Single): Single;
  begin
    if AValue - AFinal > 180 then
      AFinal := AFinal + 360
    else if AValue - AFinal < -180 then
      AFinal := AFinal - 360;

    Result := (1 - ADelta) * AValue + ADelta * AFinal
  end;

begin
  ADelta := EnsureRange(ADelta, 0, 1);

  Pos := (1 - ADelta) * Pos + ADelta * ALocation.Pos;
  Offset := (1 - ADelta) * Offset + ADelta * ALocation.Offset;
  Scale := (1 - ADelta) * Scale + ADelta * ALocation.Scale;

  TurnAngle := ApproachRotation(TurnAngle, ALocation.TurnAngle);
  PitchAngle := ApproachRotation(PitchAngle, ALocation.PitchAngle);
  RollAngle := ApproachRotation(RollAngle, ALocation.RollAngle);

end;

procedure TLocation3.Assign(ALocation: TLocation3);
begin
  Pos := ALocation.Pos;
  Offset := ALocation.Offset;
  Scale := ALocation.Scale;
  TurnAngle := ALocation.TurnAngle;
  PitchAngle := ALocation.PitchAngle;
  RollAngle := ALocation.RollAngle;
end;

procedure TLocation3.Swap(ALocation: TLocation3);
var
  Tmp: TLocation3;
begin
  Tmp := TLocation3.Create;
  Tmp.Assign(ALocation);
  ALocation.Assign(Self);
  Self.Assign(Tmp);
  Tmp.Free;
end;

procedure TLocation3.FreeRotate(AVector: TVector3; const AAngle: Single);
var
  S, C, CInv: Single;
  M: TMatrix4;
begin
  AVector := AVector.Normalize;

  S := Sin(AAngle / 180 * Pi);
  C := Cos(AAngle / 180 * Pi);
  CInv := 1 - C;

  M.Clear;
  // right
  M[0, 0] := AVector.X * AVector.X * CInv + C;
  M[0, 1] := AVector.Y * AVector.X * CInv + AVector.Z * S;
  M[0, 2] := AVector.Z * AVector.X * CInv - AVector.Y * S;
  // up
  M[1, 0] := AVector.X * AVector.Y * CInv - AVector.Z * S;
  M[1, 1] := AVector.Y * AVector.Y * CInv + C;
  M[1, 2] := AVector.Z * AVector.Y * CInv + AVector.X * S;
  // look
  M[2, 0] := AVector.X * AVector.Z * CInv + AVector.Y * S;
  M[2, 1] := AVector.Y * AVector.Z * CInv - AVector.X * S;
  M[2, 2] := AVector.Z * AVector.Z * CInv + C;

  M[3, 3] := 1;

  FMatrix := FMatrix * M;

  Changed(lcFreeRotation);
end;

procedure TLocation3.FreeTurn(ATurn: Single);
var
  M: TMatrix4;
begin
  ATurn := ATurn * Pi / 180;
  M.Clear;
  M[1, 1] := 1;
  M[3, 3] := 1;
  M[0, 0] := Cos(ATurn);
  M[2, 0] := -Sin(ATurn);
  M[0, 2] := Sin(ATurn);
  M[2, 2] := Cos(ATurn);
  FMatrix := FMatrix * M;
  FFreeChanged := True;
  Changed(lcFreeRotation);
end;

procedure TLocation3.FreePitch(APitch: Single);
var
  M: TMatrix4;
begin
  APitch := APitch * Pi / 180;
  M.Clear;
  M[0, 0] := 1;
  M[3, 3] := 1;
  M[1, 1] := Cos(APitch);
  M[2, 1] := -Sin(APitch);
  M[1, 2] := Sin(APitch);
  M[2, 2] := Cos(APitch);
  FMatrix := FMatrix * M;
  FFreeChanged := True;
  Changed(lcFreeRotation);
end;

procedure TLocation3.FreeRoll(ARoll: Single);
var
  M: TMatrix4;
begin
  ARoll := ARoll * Pi / 180;
  M.Clear;
  M[2, 2] := 1;
  M[3, 3] := 1;
  M[0, 0] := Cos(ARoll);
  M[1, 0] := Sin(ARoll);
  M[0, 1] := -Sin(ARoll);
  M[1, 1] := Cos(ARoll);
  FMatrix := FMatrix * M;
  FFreeChanged := True;
  Changed(lcFreeRotation);
end;

procedure TLocation3.FreeTranslate(const AVector: TVector3);
var
  M: TMatrix4;
begin
  M.LoadIdentity;
  M[3, 0] := AVector.X;
  M[3, 1] := AVector.Y;
  M[3, 2] := AVector.Z;
  FMatrix := FMatrix * M;
  FFreeChanged := True;
  Changed(lcFreeTranslation);
end;

procedure TLocation3.FreeScale(const AScale: TVector3);
var
  M: TMatrix4;
begin
  M.Clear;
  M[0, 0] := AScale.X;
  M[1, 1] := AScale.Y;
  M[2, 2] := AScale.Z;
  M[3, 3] := 1;
  FMatrix := FMatrix * M;
  FFreeChanged := True;
  Changed(lcFreeScale);
end;

procedure TLocation3.FreeMirror(ANormal: TLine3);
var
  R, U, L, P: TVector3;
begin
  R := Right.Reflect(ANormal.D);
  U := Up.Reflect(ANormal.D);
  L := Look.Reflect(ANormal.D);
  P := ANormal.Mirror(RealPosition);

  // Right
  FMatrix[0, 0] := R.X;
  FMatrix[1, 0] := R.Y;
  FMatrix[2, 0] := R.Z;
  // Up
  FMatrix[0, 1] := U.X;
  FMatrix[1, 1] := U.Y;
  FMatrix[2, 1] := U.Z;
  // Down
  FMatrix[0, 2] := L.X;
  FMatrix[1, 2] := L.Y;
  FMatrix[2, 2] := L.Z;
  // Position
  FMatrix[3, 0] := P.X;
  FMatrix[3, 1] := P.Y;
  FMatrix[3, 2] := P.Z;

  FFreeChanged := True;
  Changed(lcFreeMirror);
end;

procedure TLocation3.FromMatrix(AMatrix: TMatrix4);
begin
  FMatrix := AMatrix;
  FFreeChanged := True;
  Changed([lcFreeRotation, lcFreeScale, lcFreeTranslation]);
end;

procedure TLocation3.FromRotMatrix(AMatrix: TMatrix3);
begin
  FMatrix.Minor[3] := AMatrix;
  FFreeChanged := True;
  Changed([lcFreeRotation, lcFreeScale]);
end;

{ TBasicAxisSystem }

constructor TBasicAxisSystem.Create;
begin
  Reset;
end;

constructor TBasicAxisSystem.Create(AX, AY, AZ: TBasicDir3);
var
  UsedDirections: TBasicDirs3;
begin
  UsedDirections := [];
  Include(UsedDirections, AbsDir(AX));
  Include(UsedDirections, AbsDir(AY));
  Include(UsedDirections, AbsDir(AZ));
  if UsedDirections = [bdRight, bdUp, bdFront] then
  begin
    FX := AX;
    FY := AY;
    FZ := AZ;
  end
  else
    raise EAxisSystemError.Create;
end;

function TBasicAxisSystem.Convert(ADirection: TBasicDir3): TBasicDir3;
begin
  case ADirection of
    bdRight:
      Exit(FX);
    bdLeft:
      Exit(FlipDir(FX));
    bdUp:
      Exit(FY);
    bdDown:
      Exit(FlipDir(FY));
    bdFront:
      Exit(FZ);
  else // sdBack
    Exit(FlipDir(FZ));
  end;
end;

function TBasicAxisSystem.ConvertBack(ADirection: TBasicDir3): TBasicDir3;
var
  M: TBasicAxisSystem;
begin
  M := TBasicAxisSystem.Create;
  M.FromMatrix(Matrix.Transpose);
  Result := M.Convert(ADirection);
  M.Free;
end;

function TBasicAxisSystem.Matrix: TMatrix3;

  function IfOrThen(A, B: Boolean): Single; inline;
  begin
    if A then
      Exit(1);
    if B then
      Exit( -1);
    Result := 0;
  end;

begin
  Result[0, 0] := IfOrThen(FX = bdRight, FX = bdLeft);
  Result[0, 1] := IfOrThen(FX = bdUp, FX = bdDown);
  Result[0, 2] := IfOrThen(FX = bdFront, FX = bdBack);
  Result[1, 0] := IfOrThen(FY = bdRight, FY = bdLeft);
  Result[1, 1] := IfOrThen(FY = bdUp, FY = bdDown);
  Result[1, 2] := IfOrThen(FY = bdFront, FY = bdBack);
  Result[2, 0] := IfOrThen(FZ = bdRight, FZ = bdLeft);
  Result[2, 1] := IfOrThen(FZ = bdUp, FZ = bdDown);
  Result[2, 2] := IfOrThen(FZ = bdFront, FZ = bdBack);
end;

procedure TBasicAxisSystem.FromMatrix(AMatrix: TMatrix3);

  procedure DoIt(Index: Integer; out ADir: TBasicDir3);
  var
    I: Integer;
  begin
    for I := 0 to 2 do
      if AMatrix[Index, I] = 1 then
      begin
        ADir := TBasicDir(1 + 2 * I);
        Break;
      end
      else if AMatrix[Index, I] = -1 then
      begin
        ADir := TBasicDir(2 + 2 * I);
        Break;
      end;
  end;

begin
  DoIt(0, FX);
  DoIt(1, FY);
  DoIt(2, FZ);
end;

procedure TBasicAxisSystem.Rotate(AAxis: TBasicDir3; ASteps: Integer);
begin
  FX := RotateDir(FX, AAxis, ASteps);
  FY := RotateDir(FY, AAxis, ASteps);
  FZ := RotateDir(FZ, AAxis, ASteps);
  FChanged := True;
end;

procedure TBasicAxisSystem.Mirror(ANormal: TBasicDir3);
var
  N: TBasicDir3;
begin
  N := AbsDir(ANormal);
  if N = AbsDir(FX) then
    FX := FlipDir(FX);
  if N = AbsDir(FY) then
    FY := FlipDir(FY);
  if N = AbsDir(FZ) then
    FZ := FlipDir(FZ);
end;

procedure TBasicAxisSystem.Invert;
begin
  FromMatrix(Matrix.Transpose);
end;

procedure TBasicAxisSystem.Reset;
begin
  FX := bdRight;
  FY := bdUp;
  FZ := bdFront;
  FChanged := True;
end;

function TBasicAxisSystem.Equal(ABlockRotation: TBasicAxisSystem): Boolean;
begin
  Result := (ABlockRotation <> nil) and
    (ABlockRotation.FX = FX) and
    (ABlockRotation.FY = FY) and
    (ABlockRotation.FZ = FZ);
end;

procedure TBasicAxisSystem.Assign(ABlockRotation: TBasicAxisSystem);
begin
  FX := ABlockRotation.FX;
  FY := ABlockRotation.FY;
  FZ := ABlockRotation.FZ;
  FChanged := True;
end;

function TBasicAxisSystem.Changed: Boolean;
begin
  Result := FChanged;
end;

procedure TBasicAxisSystem.NotifyChanges;
begin
  FChanged := False;
end;

{ TBlockRaycaster3 }

constructor TBlock3Raycaster.Create(ALocation: TLocation3; ASize: TIntVector3);
begin
  FLocation := ALocation;
  FSize := ASize;
  FMin := -Infinity;
  FMax := +Infinity;
end;

procedure TBlock3Raycaster.AddMin(AMin: Single);
begin
  FMin := Max(FMin, AMin);
end;

procedure TBlock3Raycaster.AddMax(AMax: Single);
begin
  FMax := Min(FMax, AMax);
end;

procedure TBlock3Raycaster.Start(ALine: TLine3);
var
  InvDirections: TBasicDirs3;
  D: TBasicDir;
  NormalizedLine: TLine3;
  Data: TPlane3.TLineIntsecFactors;
  FoundEnd: Boolean;
begin
  FLine.S := FLocation.InvMatrix * ALine.S;
  FLine.D := FLocation.InvRotMatrix * ALine.D;

  FDirections := FLine.D.Dirs;
  InvDirections := ( -FLine.D).Dirs;

  NormalizedLine.S := FLine.S / FSize;
  NormalizedLine.D := FLine.D / FSize;

  FoundEnd := False;
  for D in FDirections do // Find Furthest
    if CubePlanes[D].Intsec(NormalizedLine, Data) then
    begin
      FoundEnd := True;
      FMax := Min(Data.LineFactor, FMax);
      Break;
    end;

  if not FoundEnd then
  begin
    FMin := FMax; // make "Next" return False
  end;

  for D in InvDirections do // Find Closest
    if CubePlanes[D].Intsec(NormalizedLine, Data) and
      (Data.LineFactor > FMin) then
    begin
      FMin := Data.LineFactor;
      Break;
    end;

  FCurrent := FLine[FMin].Floor;
  FCurrent.X := EnsureRange(FCurrent.X, 0, FSize.X - 1);
  FCurrent.Y := EnsureRange(FCurrent.Y, 0, FSize.Y - 1);
  FCurrent.Z := EnsureRange(FCurrent.Z, 0, FSize.Z - 1);
end;

function TBlock3Raycaster.Next: Boolean;
var
  P: TPlane3;
  Dir: TBasicDir3;
  Data: TPlane3.TLineIntsecFactors;
  ClosestDistance: Single;
begin
  if FMin >= FMax then
    Exit(False);

  // Get new Closest FMin for CubePlanes
  ClosestDistance := Infinity;
  for Dir in FDirections do
  begin
    P := CubePlanes[Dir];
    P.S := P.S + FCurrent;
    if P.Intsec(FLine, Data) and (Data.LineFactor < ClosestDistance) then
    begin
      ClosestDistance := Data.LineFactor;
      FLastDirection := Dir;
      FPosition := FLine[Data.LineFactor];
    end;
  end;

  FMin := ClosestDistance;

  // Set Current to next block
  FCurrent := FCurrent + Vec3Dir[FLastDirection];

  Result := True;
end;

{ TGHexahedron }

function THexahedron.GetFaceNormal(ADir: TBasicDir3): TLine3;
begin
  Result := FFaceNormals[ADir];
end;

procedure THexahedron.SetFaceNormal(ADir: TBasicDir3; const Value: TLine3);
begin
  FFaceNormals[ADir] := Line3(Value.S, Value.D.Normalize);
end;

function THexahedron.AnyVisible(APoints: IIterable<TVector3>): Boolean;
var
  Direction: TBasicDir3;
  Point: TVector3;
  SingleFaceAllOutside: Boolean;
begin
  // return true if there is at least one face, where all points are not visible
  for Direction in CheckOrder do
  begin
    // are all points on the outside of the given plane
    SingleFaceAllOutside := True;
    for Point in APoints do
    begin
      // Dot-Product > 0 means outside
      if Self[Direction].D.Dot(Self[Direction].S.VectorTo(Point)) < 0 then
      begin
        SingleFaceAllOutside := False;
        Break;
      end;
    end;
    // If for one face, all points where on the outside, instantly return false
    if SingleFaceAllOutside then
      Exit(False);
  end;
  Result := True;
end;

function THexahedron.SphereVisible(const ACenter: TVector3; ARadius: Single): Boolean;
var
  Direction: TBasicDir3;
begin
  for Direction in CheckOrder do
    if Self[Direction].D.Dot(Self[Direction].S.VectorTo(ACenter - Self[Direction].D * ARadius)) >= 0 then
      Exit(False);
  Result := True;
end;

class operator THexahedron.in(APoint: TVector3; const AHexahedron: THexahedron): Boolean;
var
  Dir: TBasicDir3;
begin
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    if AHexahedron.FaceNormals[Dir].D.Dot(AHexahedron.FaceNormals[Dir].S.VectorTo(APoint)) > 0 then
      Exit(False);
  Result := True;
end;

{ TBasicDir helper functions }

function FlipDir(ADir: TBasicDir): TBasicDir;
begin
  Result := FlippedBasicDirs[ADir];
end;

function AbsDir(ADir: TBasicDir): TBasicDir;
begin
  Result := AbsBasicDirs[ADir];
end;

function RotateDir(ADir: TBasicDir; AAxis: TBasicDir3; ASteps: Integer): TBasicDir;
begin
  ASteps := IBounds1(4).RangedMod(ASteps);
  if ASteps = 0 then
    Exit(ADir);
  Result := BasicDirRotations[ADir, AAxis, ASteps];
end;

{ Shorthand Constructors }

function Vec2(X, Y: Single): TVector2;
begin
  Result.Create(X, Y);
end;

function Vec2(V: Single): TVector2;
begin
  Result.Create(V);
end;

function Vec3(X, Y, Z: Single): TVector3;
begin
  Result.Create(X, Y, Z);
end;

function Vec3(V: Single): TVector3;
begin
  Result.Create(V);
end;

function Bounds1(A, B: Single): TBounds1;
begin
  Result.Create(A, B);
end;

function Bounds1(A: Single): TBounds1;
begin
  Result.Create(A);
end;

function Bounds2(A, B: TVector2): TBounds2;
begin
  Result.Create(A, B);
end;

function Bounds2(A: TVector2): TBounds2;
begin
  Result.Create(A);
end;

function Bounds3(A, B: TVector3): TBounds3;
begin
  Result.Create(A, B);
end;

function Bounds3(A: TVector3): TBounds3;
begin
  Result.Create(A);
end;

function Line2(S, D: TVector2): TLine2;
begin
  Result.Create(S, D);
end;

function Line3(S, D: TVector3): TLine3;
begin
  Result.Create(S, D);
end;

function Plane2(S, X, Y: TVector2): TAxisSystem2;
begin
  Result.Create(S, X, Y);
end;

function Plane3(S, X, Y: TVector3): TPlane3;
begin
  Result.Create(S, X, Y);
end;

{ TAxisSystem }

function TAxisSystem3.GetPoint(APos: TVector3): TVector3;
begin
  Result := S + APos.X * DX + APos.Y * DY + APos.Z * DZ;
end;

function TAxisSystem3.GetInvPoint(APos: TVector3): TVector3;
var
  M: TMatrix4;
begin
  M[0, 0] := DX.X;
  M[0, 1] := DX.Y;
  M[0, 2] := DX.Z;
  M[0, 3] := 0;
  M[1, 0] := DY.X;
  M[1, 1] := DY.Y;
  M[1, 2] := DY.Z;
  M[1, 3] := 0;
  M[2, 0] := DZ.X;
  M[2, 1] := DZ.Y;
  M[2, 2] := DZ.Z;
  M[2, 3] := 0;
  M[3, 0] := S.X;
  M[3, 1] := S.Y;
  M[3, 2] := S.Z;
  M[3, 3] := 1;
  M := M.Inverse;
  Result := M * APos;
end;

constructor TAxisSystem3.Create(const S, X, Y, Z: TVector3);
begin
  Self.S := S;
  Self.DX := X;
  Self.DY := Y;
  Self.DZ := Z;
end;

class operator TAxisSystem3.in(const A: TVector3; const B: TAxisSystem3): Boolean;
begin
  Result := B.InvPoint[A] in Bounds3(0, 1);
end;

class operator TAxisSystem3.Equal(const A, B: TAxisSystem3): Boolean;
begin
  Result := (A.S = B.S) and (A.DX = B.DX) and (A.DY = B.DY) and (A.DZ = B.DZ);
end;

class operator TAxisSystem3.NotEqual(const A, B: TAxisSystem3): Boolean;
begin
  Result := (A.S <> B.S) or (A.DX <> B.DX) or (A.DY = B.DY) or (A.DZ = B.DZ);
end;

{ TLocation2.TChangeEventInfo }

constructor TLocation2.TChangeEventInfo.Create(ASender: TLocation2; AChanges: TChanges);
begin
  FSender := ASender;
  FChanges := AChanges;
end;

function TLocation2.TChangeEventInfo.Sender: TLocation2;
begin
  Result := FSender;
end;

{ TLocation2 }

procedure TLocation2.SetParent(const Value: TLocation2);
begin
  if Parent = Value then
    Exit;
  if Parent <> nil then
    Parent.OnChanged.Del(ParentChanged);
  FParent := Value;
  if Parent <> nil then
    Parent.OnChanged.Add(ParentChanged);
  Changed(lcParent);
end;

procedure TLocation2.SetPos(const Value: TVector2);
begin
  if Pos = Value then
    Exit;
  FPos := Value;
  Changed(lcPos);
end;

procedure TLocation2.SetPosX(const Value: Single);
begin
  if PosX = Value then
    Exit;
  FPos.X := Value;
  Changed(lcPos);
end;

procedure TLocation2.SetPosY(const Value: Single);
begin
  if PosY = Value then
    Exit;
  FPos.Y := Value;
  Changed(lcPos);
end;

procedure TLocation2.SetOffset(const Value: TVector2);
begin
  if Offset = Value then
    Exit;
  FOffset := Value;
  Changed(lcOffset);
end;

procedure TLocation2.SetOffsetX(const Value: Single);
begin
  if OffsetX = Value then
    Exit;
  FOffset.X := Value;
  Changed(lcOffset);
end;

procedure TLocation2.SetOffsetY(const Value: Single);
begin
  if OffsetY = Value then
    Exit;
  FOffset.Y := Value;
  Changed(lcOffset);
end;

procedure TLocation2.SetRotation(Value: Single);
begin
  Value := Bounds1(0, 360).RangedModL(Value);
  if Rotation = Value then
    Exit;
  FRotation := Value;
  Changed(lcTurn);
end;

procedure TLocation2.SetScale(const Value: TVector2);
begin
  if Scale = Value then
    Exit;
  FScale := Value;
  Changed(lcScale);
end;

procedure TLocation2.SetScaleX(const Value: Single);
begin
  if ScaleX = Value then
    Exit;
  FScale.X := Value;
  Changed(lcScale);
end;

procedure TLocation2.SetScaleY(const Value: Single);
begin
  if ScaleY = Value then
    Exit;
  FScale.Y := Value;
  Changed(lcScale);
end;

function TLocation2.GetAxisSystem: TAxisSystem2;
var
  P: TLocation2;
begin
  if not FAxisSystem.HasValue then
  begin
    Result.Create(
      Pos,
      TVector2.FromAngle(Rotation) * Scale.X,
      TVector2.FromAngle(Rotation + 90) * Scale.Y
      );
    if Parent <> nil then
      Result.S := Parent[Result.S];
    P := Parent;
    while P <> nil do
    begin
      Result.DX := Result.DX.Rotate(P.Rotation) * P.ScaleX;
      Result.DY := Result.DY.Rotate(P.Rotation) * P.ScaleY;
      P := P.Parent;
    end;
    Result.S := Result.S + Result.DX * Offset.X + Result.DY * Offset.Y;
    FAxisSystem.Value := Result;
  end
  else
    Result := FAxisSystem.Value;
end;

function TLocation2.GetInvPoint(APoint: TVector2): TVector2;
begin
  Result := AxisSystem.InvPoint[APoint];
end;

function TLocation2.GetPoint(APoint: TVector2): TVector2;
begin
  Result := AxisSystem[APoint];
end;

procedure TLocation2.ParentChanged(AInfo: TChangeEventInfo);
begin
  Changed(lcParent);
end;

procedure TLocation2.Reset;
begin
  BeginUpdate;
  Pos := 0;
  Offset := 0;
  Rotation := 0;
  Scale := 1;
  EndUpdate;
end;

function TLocation2.GetOnChanged: TChangeEvent.TAccess;
begin
  Result := FOnChanged.Access;
end;

procedure TLocation2.Changed(AChange: TChange);
begin
  FAxisSystem.Clear;
  if FUpdateCounter = 0 then
    FOnChanged.Execute(TChangeEventInfo.Create(Self, [AChange]))
  else
    Include(FChanges, AChange);
end;

procedure TLocation2.Assign(ALocation: TLocation2);
begin
  BeginUpdate;
  Pos := ALocation.Pos;
  Offset := ALocation.Offset;
  Scale := ALocation.Scale;
  Rotation := ALocation.Rotation;
  EndUpdate;
end;

function TLocation2.Copy: TLocation2;
begin
  Result := TLocation2.Create;
  Result.Assign(Self);
end;

procedure TLocation2.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

constructor TLocation2.Create;
begin
  FAxisSystem := TOpt<TAxisSystem2>.Create;
  FScale := 1;
end;

destructor TLocation2.Destroy;
begin
  if Parent <> nil then
    Parent.OnChanged.Del(ParentChanged);
  FAxisSystem.Free;
  inherited;
end;

procedure TLocation2.EndUpdate;
begin
  Dec(FUpdateCounter);
  if (FUpdateCounter = 0) and (FChanges <> []) then
  begin
    FOnChanged.Execute(TChangeEventInfo.Create(Self, FChanges));
    FChanges := [];
  end;
end;

end.
