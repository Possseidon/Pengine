unit Pengine.Vector;

interface   

{ TODO 5 -oPossseidon -cUnitTest :
  Unit-Tests for TBounds1.RangeModL and RangeModR }

{ TODO 4 -oPossseidon -cFunction :
  Add GetCorners to all Bounds (Int) }

{ TODO 3 -oPossseidon -cXmlDoc :
  Add XML-Doc <returns> to ortho projection and intersection
  => true means exactly one solution (which is in Data now) and false means none or infinite }

{ TODO 3 -oPossseidon -cFunction :
  Line Intsec with other Line as normal for Plane }

{ TODO 3 -oPossseidon -cFunction :
  Height function for TPlane3 and TLine2, that can return negative results. See/use what is in PointSide in TLine2}

{ TODO 4 -oPossseidon -cInline :
  Inline all functions, which call exactly one other function }

{ TODO 3 -oPossseidon -cInline :
  Add various inline to functions with different compiler switches. }

{ TODO 5 -oPossseidon -cRefactoring :
  Shorthand constructors for Plane and Line.
  Rename FRange to Bounds.
  Renanme all constructors to CreateX, to mark, that only the
  shorthanded ones should be used. }

{ TODO 5 -oPossseidon -cXmlDoc :
  TLocation XmlDoc }

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
  Pengine.Matrix;

type

  /// <summary>Only one direction per axis is allowed in a TBasicAxisSystem.</summary>
  EAxisSystemError = class(Exception)
  public
    constructor Create;
  end;

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
  ///  0 -> bdNone<p/>
  /// -X -> bdLeft<p/>
  /// +X -> bdRight<p/>
  /// -Y -> bdDown<p/>
  /// +Y -> bdUp<p/>
  /// -Z -> bdBack
  /// +Z -> bdFront<p/>
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

  /// <summary>A vertex-index for a triangle in range: <c>[0, 3)</c></summary>
  TTriangleIndex = 0 .. 2;
  /// <summary>A vertex-index for a render-quad in range: <c>[0, 6)</c></summary>
  TQuadIndex = 0 .. 5;

  /// <summary>A two component vector of type <see cref="System|Single"/>.</summary>
  TVector2 = record
  private
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

    /// <summary>Creates a <see cref="Pengine.Vector|TVector2"/> with the specified components.</summary>
    constructor Create(X, Y: Single); overload;
    /// <summary>Creates a <see cref="Pengine.Vector|TVector2"/> with both components being the same, given value.</summary>
    constructor Create(V: Single); overload;

    /// <returns>A <see cref="Pengine.Vector|TVector2"/>, with each component being a random value in the interval: <c>[0, 1)</c></returns>
    class function Random: TVector2; static;
    /// <returns>A <see cref="Pengine.Vector|TVector2"/>, with each component being a random value in the interval: <c>[-1, 1)</c></returns>
    class function RandomBox: TVector2; static;
    /// <returns>A normalized <see cref="Pengine.Vector|TVector2"/>, pointing into a random direction.</returns>
    /// <remarks>The probability is homogeneous.</remarks>
    class function RandomNormal: TVector2; static;

    class operator Implicit(V: Single): TVector2; inline;
    class operator Implicit(const AVector: TIntVector2): TVector2; inline;

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

    /// <returns>A vector, which is rotated by 90° counter-clockwise.</returns>
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
    /// <returns>The by the given radian angle counter-clockwise rotated vector.</returns>
    function RotatedRad(AAngle: Single): TVector2;
    /// <returns>The by the given degree angle counter-clockwise rotated vector.</returns>
    function Rotated(AAngle: Single): TVector2;

    /// <returns>The vector, with each negative component being positive.</returns>
    function Abs: TVector2;
    /// <returns>The vector, with each component being rounded down.</returns>
    function Floor: TIntVector2;
    /// <returns>The vector, with each component being rounded up.</returns>
    function Ceil: TIntVector2;

    /// <returns>A set of all basic directions, that the vector is pointing to.</returns>
    /// <remarks>Therefore bdLeft and bdRight etc. are mutually exclusive, as each axis can have only one direction.</remarks>
    function Dirs: TBasicDirs2;

  end;

  /// <summary>A three component vector of type <see cref="System|Single"/>.</summary>
  TVector3 = record
  private
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

    class operator Implicit(V: Single): TVector3; inline;
    class operator Implicit(const A: TIntVector3): TVector3; inline;

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
    /// <returns>The vector rotated around the specified vector using the given angle in radians.</returns>
    /// <remarks>
    /// If the specified vector points towards you, the rotation is counter-clockwise.<p/>
    /// /!\ The given vector must be normalized.
    /// </remarks>
    function RotateRad(const AAxis: TVector3; AAngle: Single): TVector3;
    /// <returns>The vector rotated around the specified vector using the given angle in degrees.</returns>
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

    /// <returns>A set of all basic directions, that the vector is pointing to.</returns>
    /// <remarks>Therefore bdLeft and bdRight etc. are mutually exclusive, as each axis can have only one direction.</remarks>
    function Dirs: TBasicDirs3;

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
  public type

    TCornerIndex = TIntBounds1.TCornerIndex;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X<p/>
    ///  [0]  (0)<p/>
    ///  [1]  (1)<p/>
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

    class operator Add(const A, B: TBounds1): TBounds1;
    class operator Subtract(const A, B: TBounds1): TBounds1;
    class operator Multiply(const A, B: TBounds1): TBounds1;
    class operator Divide(const A, B: TBounds1): TBounds1;

    // The in operator is inclusive, comparing with <= and >=
    class operator in(const A, B: TBounds1): Boolean;
    class operator in(A: Single; const B: TBounds1): Boolean;

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
  public type

    TCornerIndex = 0 .. 3;

    /// <summary>A simple array-type, that can represent the four corners of the bounds.
    /// <p>They are in the following order:</p><code>
    /// Index  X, Y<p/>
    ///  [0]  (0, 0)<p/>
    ///  [1]  (1, 0)<p/>
    ///  [2]  (0, 1)<p/>
    ///  [3]  (1, 1)<p/>
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
    class operator Implicit(A: TIntBounds1): TBounds2; inline;

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

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TBounds2;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: TVector2): TBounds2;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: TVector2): TBounds2;

    class operator Add(const A, B: TBounds2): TBounds2;
    class operator Subtract(const A, B: TBounds2): TBounds2;
    class operator Multiply(const A, B: TBounds2): TBounds2;
    class operator Divide(const A, B: TBounds2): TBounds2;

    // The in operator is inclusive, comparing with <= and >=
    class operator in(const A, B: TBounds2): Boolean;
    class operator in(A: TVector2; const B: TBounds2): Boolean;

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
    ///  [7]  (1, 1, 1)
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

    /// <returns>True, if C1 &lt;= C2</returns>
    function Normalized: Boolean; inline;
    /// <returns>The normalized version of the bounds.</summary>
    function Normalize: TBounds3;

    /// <returns>The bounds with C1 being increased and C2 being decreased by the specified amount.</summary>
    function Inset(AAmount: TVector3): TBounds3;
    /// <returns>The bounds with C1 being decreased and C2 being increased by the specified amount.</summary>
    function Outset(AAmount: TVector3): TBounds3;

    class operator Add(const A, B: TBounds3): TBounds3;
    class operator Subtract(const A, B: TBounds3): TBounds3;
    class operator Multiply(const A, B: TBounds3): TBounds3;
    class operator Divide(const A, B: TBounds3): TBounds3;

    // The in operator is inclusive, comparing with <= and >=
    class operator in(const A, B: TBounds3): Boolean;
    class operator in(A: TVector3; const B: TBounds3): Boolean;

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
  ///  vector | turn | pitch <p/>
  ///---------+------+-------<p/>
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

    /// <returns>The slope of the line:<code>
    ///  X | Y | slope <p/>
    /// ---|---|-------<p/>
    ///  1 | 0 |     0 <p/>
    ///  1 | 1 |     1 <p/>
    ///  0 | 1 |  +inf
    /// </code></returns>
    /// <remarks>Make sure to check, if D.X &lt; 0, which means that the line is flipped.</remarks>
    function Slope: Single;

    /// <returns>The side of the line, on which the given point is, while looking in the direction of D.</summary>
    function Side(A: TVector2): TLineSide;

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

  end;

  {TODO 1 -oPossseidon -cRecord : Code TPlane2 record}
  /// <summary>Represents a 2-Dimensional plane, described by one support vector S and two direction vectors D1 and D2.</summary>
  TPlane2 = record
    /// <summary>The support vector S of the line.</summary>
    S: TVector2;
    /// <summary>The first direction vector D1 of the line.</summary>
    D1: TVector2;
    /// <summary>The second direction vector D2 of the line.</summary>
    D2: TVector2;

  end;

  /// <summary>Represents a 3-Dimensional plane, described by one support vector S and two direction vectors D1 and D2.</summary>
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
    /// <summary>The first direction vector D1 of the line.</summary>
    D1: TVector3;
    /// <summary>The second direction vector D2 of the line.</summary>
    D2: TVector3;

    /// <returns>The perpendicular of the plane with an arbitrary length.</returns>
    /// <remarks>The length is actually the area of the paralellogram, hence the cross-product.</remarks>
    function Perpendicular: TVector3; inline;
    /// <returns>The area of the paralellogram.</returns>
    function Area: Single; inline;
    /// <returns>The normal of the plane.</returns>
    function Normal: TVector3; inline;

    /// <summary>Gets a point on the plane, where <c>[0, 1]</c> turns into <c>[S, S + D1 + D2]</c></summary>
    /// <remarks>Default property.</remarks>
    property Point[APos: TVector2]: TVector3 read GetPoint; default;

    /// <summary>Creates a line with the given support vector and direction vectors.</summary>
    constructor Create(const S, D1, D2: TVector3);

    /// <returns>The multiplication-factors, to reach the point on the plane, which is closest to the given point.</returns>
    /// <exception><see cref="System.SysUtils|EZeroDivide"/> if either D1 or D2 is 0.</exception>
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
    function Intsec(const A: TLine3; out APoint: TVector2): Boolean; overload;
    /// <param name="[out] AFactors">Contains the multiplication-factors, to reach the intersection point.</param>
    /// <param name="[out] APoint">Contains the intersection point.</param>
    /// <returns>True, if there is exactly one intersection with the line.</returns>
    function Intsec(const A: TLine3; out AFactors: TLineIntsecFactors; out APoint: TVector2): Boolean; overload;

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

  end;

  /// <summary>Record helper for <see cref="Pengine.Vector|TVector2"/></summary>
  TVector2Helper = record helper for TVector2
    /// <returns>Creates a <see cref="Pengine.Vector|TLine2"/> between two vectors.</returns>
    function LineTo(A: TVector2): TLine2;
  end;

  /// <summary>...</summary>
  TLocation = class
  public type

    TChangeType = (
      ctPosition,
      ctOffset,
      ctScale,
      ctTurn,
      ctPitch,
      ctRoll,
      ctFreeTranslation,
      ctFreeScale,
      ctFreeRotation,
      ctFreeMirror,
      ctParent
      );

    TChanges = set of TChangeType;

    TChangeEventInfo = class(TEventInfo, IEventSender<TLocation>)
    private
      FSender: TLocation;
      FChanges: TChanges;
    public
      constructor Create(ASender: TLocation; AChanges: TChanges);

      function Sender: TLocation;
      property Changes: TChanges read FChanges;
    end;

    TOnChangeEvent = TObservableEvent<TChangeEventInfo>;

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

    FParent: TLocation;

    procedure SetLook(AValue: TVector3);
    procedure TriggerChanges(AChanges: TChanges);

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
    procedure SetParent(const Value: TLocation);

  public
    constructor Create(AInverted: Boolean = False);

    property Matrix: TMatrix4 read GetMatrix;
    property InvMatrix: TMatrix4 read GetInvMatrix;

    property RotMatrix: TMatrix3 read GetRotMatrix;
    property InvRotMatrix: TMatrix3 read GetInvRotMatrix;

    property Parent: TLocation read FParent write SetParent;

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

    procedure Approach(ALocation: TLocation; ADelta: Single);
    procedure Assign(ALocation: TLocation);
    procedure Swap(ALocation: TLocation);

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

  public // Events 
    OnChanged: TOnChangeEvent;

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
    {TODO -oPossseidon -cFunction : Mirror with mutiple two and three directions for diagonal mirroring}
    procedure Invert;

    procedure Reset;

    function Equal(ABlockRotation: TBasicAxisSystem): Boolean;

    procedure Assign(ABlockRotation: TBasicAxisSystem);

    function Changed: Boolean;
    procedure NotifyChanges;

  end;

  TBlock2Raycaster = class

  end;

  TBlock3Raycaster = class
  private
    FLocation: TLocation;
    FSize: TIntVector3;

    FLine: TLine3;
    FDirections: TBasicDirs3;
    FMin, FMax: Single;

    FCurrent: TIntVector3;
    FLastDirection: TBasicDir3;
    FPosition: TVector3;
    
  public
    constructor Create(ALocation: TLocation; ASize: TIntVector3);

    procedure AddMin(AMin: Single);
    procedure AddMax(AMax: Single);

    procedure Start(ALine: TLine3);
    function Next: Boolean;

    property Current: TIntVector3 read FCurrent;
    property LastDirection: TBasicDir3 read FLastDirection;
    property Position: TVector3 read FPosition;

  end;

  /// <summary>A six-sided polyhedron, defined by six face normals.</summary>
  THexahedron = record
  public
    FaceNormals: array [TBasicDir3] of TLine3;

    // wtf does this... xD
    function Visible(APoints: TArray<TVector3>): Boolean;

    class operator in(APoint: TVector3; const AHexahedron: THexahedron): Boolean;
    
  end;

  PBounds1 = ^TBounds1;
  PBounds2 = ^TBounds2;
  PBounds3 = ^TBounds3;

  PVector3 = ^TVector3;
  PVector2 = ^TVector2;
  PVectorDir = ^TVectorDir;
  PLine2 = ^TLine2;
  PLine3 = ^TLine3;
  PPlane2 = ^TPlane2;
  PPlane3 = ^TPlane3;

const

  InfVec2: TVector2 = (X: Infinity; Y: Infinity);
  NaNVec2: TVector2 = (X: NaN; Y: NaN);

  InfVec3: TVector3 = (X: Infinity; Y: Infinity; Z: Infinity);
  NaNVec3: TVector3 = (X: NaN; Y: NaN; Z: NaN);

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
    (X:  0; Y:  0; Z:  0),
    (X: -1; Y:  0; Z:  0),
    (X: +1; Y:  0; Z:  0),
    (X:  0; Y: -1; Z:  0),
    (X:  0; Y: +1; Z:  0),
    (X:  0; Y:  0; Z: -1),
    (X:  0; Y:  0; Z: +1)
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

  //                        direction    around      ccw times
  BasicDirRotations: array [TBasicDir3, TBasicDir3, 1 .. 3] of TBasicDir = (
    ( // left
      (bdLeft, bdLeft, bdLeft), // left
      (bdLeft, bdLeft, bdLeft), // right
      (bdBack, bdRight, bdFront), // down
      (bdFront, bdRight, bdBack), // up
      (bdUp, bdRight, bdDown), // back
      (bdDown, bdRight, bdUp)  // front
    ),
    ( // right
      (bdRight, bdRight, bdRight), // left
      (bdRight, bdRight, bdRight), // right
      (bdFront, bdLeft, bdBack), // down
      (bdBack, bdLeft, bdFront), // up
      (bdDown, bdLeft, bdUp), // back
      (bdUp, bdLeft, bdDown)  // front
    ),
    ( // down
      (bdBack, bdUp, bdFront), // left
      (bdFront, bdUp, bdBack), // right
      (bdDown, bdDown, bdDown), // down
      (bdDown, bdDown, bdDown), // up
      (bdLeft, bdUp, bdRight), // back
      (bdRight, bdUp, bdLeft)  // front
    ),
    ( // up
      (bdFront, bdDown, bdBack), // left
      (bdBack, bdDown, bdFront), // right
      (bdUp, bdUp, bdUp), // down
      (bdUp, bdUp, bdUp), // up
      (bdRight, bdDown, bdLeft), // back
      (bdLeft, bdDown, bdRight)  // front
    ),
    ( // back
      (bdDown, bdFront, bdUp), // left
      (bdUp, bdFront, bdDown), // right
      (bdLeft, bdFront, bdRight), // down
      (bdRight, bdFront, bdLeft), // up
      (bdBack, bdBack, bdBack), // back
      (bdBack, bdBack, bdBack)  // front
    ),
    ( // front
      (bdUp, bdBack, bdDown), // left
      (bdDown, bdBack, bdUp), // right
      (bdRight, bdBack, bdLeft), // down
      (bdLeft, bdBack, bdRight), // up
      (bdFront, bdFront, bdFront), // back
      (bdFront, bdFront, bdFront)  // front
    )
  );

  AxisBasicDir: array [TCoordAxis] of TBasicDir = (
    bdNone,
    bdRight,
    bdUp,
    bdFront
  );

  QuadSideCount = High(TQuadIndex) + 1;

  QuadTexCoords: array [TQuadIndex] of TTexCoord2 = (
    (X: 0; Y: 0),
    (X: 1; Y: 0),
    (X: 1; Y: 1),
    (X: 1; Y: 1),
    (X: 0; Y: 1),
    (X: 0; Y: 0)
    );

  QuadMiddleCoords: array [TQuadIndex] of TTexCoord2 = (
    (X: -1; Y: -1),
    (X: +1; Y: -1),
    (X: +1; Y: +1),
    (X: +1; Y: +1),
    (X: -1; Y: +1),
    (X: -1; Y: -1)
    );

  TriangleTexCoords: array [TTriangleIndex] of TTexCoord2 = (
    (X: 0; Y: 0),
    (X: 1; Y: 0),
    (X: 0; Y: 1)
    );

  BasicDirectionNames: array [TBasicDir] of AnsiString = (
    'none',
    'left',
    'right',
    'down',
    'up',
    'backwards',
    'forward'
    );

  BasicPrepositionNames: array [TBasicDir] of AnsiString = (
    'none',
    'left',
    'right',
    'bottom',
    'top',
    'back',
    'front'
    );

  CubePlanes: array [TBasicDir3] of TPlane3 = (
    (S: (X: 0; Y: 0; Z: 0); D1: (X: 0; Y: 0; Z: 1); D2: (X: 0; Y: 1; Z: 0)),
    (S: (X: 1; Y: 0; Z: 1); D1: (X: 0; Y: 0; Z: -1); D2: (X: 0; Y: 1; Z: 0)),
    (S: (X: 0; Y: 0; Z: 0); D1: (X: 1; Y: 0; Z: 0); D2: (X: 0; Y: 0; Z: 1)),
    (S: (X: 0; Y: 1; Z: 1); D1: (X: 1; Y: 0; Z: 0); D2: (X: 0; Y: 0; Z: -1)),
    (S: (X: 1; Y: 0; Z: 0); D1: (X: -1; Y: 0; Z: 0); D2: (X: 0; Y: 1; Z: 0)),
    (S: (X: 0; Y: 0; Z: 1); D1: (X: 1; Y: 0; Z: 0); D2: (X: 0; Y: 1; Z: 0))
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

implementation

const
  RotationLimit: TBounds1 = (C1: -180; C2: +180);
           
{ EAxisError }

constructor EAxisSystemError.Create;
begin
  inherited Create('Only one direction per axis is allowed in a TBasicAxisSystem.');
end;

{ TVector2 }

{$REGION 'All versions of rearrangement TVector2'}

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

{$ENDREGION}

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

function TVector2.RotatedRad(AAngle: Single): TVector2;
begin
  Result := Sin(AAngle) * Cross + Cos(AAngle) * Self;
end;

function TVector2.Rotated(AAngle: Single): TVector2;
begin
  Result := RotatedRad(DegToRad(AAngle));
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

{ TVector3 }

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
end;

function TVector3.Floor: TIntVector3;
begin
  Result.X := System.Math.Floor(X);
  Result.Y := System.Math.Floor(Y);
end;

function TVector3.Ceil: TIntVector3;
begin
  Result.X := System.Math.Ceil(X);
  Result.Y := System.Math.Ceil(Y);
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
    Result := ANumerator - Floor(ANumerator / ADenominator) * ADenominator;
  end;

begin
  Result := FMod(AValue - C1, Length) + C1;
end;

function TBounds1.RangedModR(AValue: Single): Single;

  // Copy of System.Math.FMod, but that uses Trunc, which results in a symmetric behavior
  function FMod(const ANumerator, ADenominator: Single): Single;
  begin
    Result := ANumerator - (Ceil(ANumerator / ADenominator) - 1) * ADenominator;
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

class operator TBounds2.Implicit(A: TIntBounds1): TBounds2;
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
end;

function TBounds3.Clamp(AValue: TVector3): TVector3;
begin
  Result.X := LineX.Clamp(AValue.X);
  Result.Y := LineY.Clamp(AValue.Y);
end;

function TBounds3.Normalized: Boolean;
begin
  Result := C1 <= C2;
end;

function TBounds3.Normalize: TBounds3;
begin
  Result.LineX := LineX.Normalize;
  Result.LineY := LineY.Normalize;
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
begin
  M3x2[0, 0] := D.X;
  M3x2[1, 0] := -A.D.X;
  M3x2[2, 0] := A.S.X - S.X;
  M3x2[0, 1] := D.Y;
  M3x2[1, 1] := -A.D.Y;
  M3x2[2, 1] := A.S.Y - S.Y;

  Result := M3x2.Solve(R);
  if Result then
  begin
    AFactors.Factor := R[0];
    AFactors.FactorOther := R[1];
  end;
end;

function TLine2.Height(const A: TVector2): Single;
begin
  if D = 0 then
    Exit(S.DistanceTo(A));
  Result := TLine2.Create(S, D.Cross.Normalize).OrthoProj(A);
end;

function TLine2.Slope: Single;
begin
  if D.X = 0 then
    Exit(Infinity);
  Exit(D.Y / D.X);
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
  Plane.D1 := D;
  Plane.D2 := D.Cross(A.D);
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
begin
  Result := Self[OrthoProj(A)].DistanceTo(A);
end;

function TLine3.Mirror(APoint: TVector3): TVector3;
begin
  Result := APoint - D * OrthoProj(APoint) * 2;
end;

{ TPlane2 }

{ TPlane3 }

function TPlane3.GetPoint(Value: TVector2): TVector3;
begin
  Result := S + Value.X * D1 + Value.Y * D2;
end;

function TPlane3.Perpendicular: TVector3;
begin
  Result := D1.Cross(D2);
end;

function TPlane3.Area: Single;
begin
  Result := Normal.Length;
end;

function TPlane3.Normal: TVector3;
begin
  Result := Perpendicular.Normalize;
end;

function TPlane3.CosAngleTo(const A: TPlane3): Single;
begin
  Result := Perpendicular.CosAngleTo(A.Perpendicular);
end;

function TPlane3.CosAngleTo(const A: TVector3): Single;
begin
  Result := Perpendicular.CosAngleTo(A);
end;

constructor TPlane3.Create(const S, D1, D2: TVector3);
begin
  Self.S := S;
  Self.D1 := D1;
  Self.D2 := D2;
end;

function TPlane3.OrthoProj(const A: TVector3): TVector2;
begin
  // Yes, this is probably the fastest I can get :)
  Result.X := (D2.SqrDot * D1.Dot(A - S) - D2.Dot(D1) * D2.Dot(A - S)) / (D1.SqrDot * D2.SqrDot - Sqr(D1.Dot(D2)));
  Result.Y := (D1.SqrDot * D2.Dot(A - S) - D1.Dot(D2) * D1.Dot(A - S)) / (D2.SqrDot * D1.SqrDot - Sqr(D2.Dot(D1)));
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
begin
  M4x3[0, 0] := D1.X;
  M4x3[1, 0] := D2.X;
  M4x3[2, 0] := -A.D.X;
  M4x3[3, 0] := A.S.X - S.X;
  M4x3[0, 1] := D1.Y;
  M4x3[1, 1] := D2.Y;
  M4x3[2, 1] := -A.D.Y;
  M4x3[3, 1] := A.S.Y - S.Y;
  M4x3[0, 2] := D1.Z;
  M4x3[1, 2] := D2.Z;
  M4x3[2, 2] := -A.D.Z;
  M4x3[3, 2] := A.S.Z - S.Z;

  Result := M4x3.Solve(R);
  if Result then
  begin
    AFactors.PlaneFactors.X := R[0];
    AFactors.PlaneFactors.Y := R[1];
    AFactors.LineFactor := R[2];
  end;
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
  ALine.S := IntsecLine[Data.LineFactor];
end;

function TPlane3.Intsec(const A: TLine3; out APoint: TVector2): Boolean;
begin

end;

function TPlane3.Intsec(const A: TLine3; out AFactors: TLineIntsecFactors; out APoint: TVector2): Boolean;
begin
  
end;

function TPlane3.AngleRadTo(const A: TVector3): Single;
begin
  Result := EnsureRange(Pi / 2 - Perpendicular.AngleRadTo(A), 0, Pi / 2);
end;

function TPlane3.AngleTo(const A: TVector3): Single;
begin
  Result := EnsureRange(90 - Perpendicular.AngleTo(A), 0, 90);
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

function TVector2Helper.LineTo(A: TVector2): TLine2;
begin
  Result.Create(Self, VectorTo(A));
end;

{ TLocation.TChangeEventInfo }

constructor TLocation.TChangeEventInfo.Create(ASender: TLocation; AChanges: TChanges);
begin
  FSender := ASender;
  FChanges := AChanges;
end;

function TLocation.TChangeEventInfo.Sender: TLocation;
begin
  Result := FSender;
end;

{ TLocation }

procedure TLocation.TriggerChanges(AChanges: TChanges);
begin
  FChanged := True;
  if AChanges - [ctFreeScale, ctFreeTranslation, ctFreeRotation] = AChanges then // free changes matrix > no notify
    FMatrixChanged := True;
  OnChanged.Execute(TChangeEventInfo.Create(Self, AChanges));
end;

procedure TLocation.SetLook(AValue: TVector3);
var
  D: TVectorDir;
begin
  // Z points in other Direction
  AValue.Z := -AValue.Z;
  D := AValue;
  TurnAngle := D.TurnAngle;
  PitchAngle := D.PitchAngle;
end;

function TLocation.GetInvMatrix: TMatrix4;
begin
  if FMatrixChanged or FInvMatrixChanged then
    FInvMatrix := Matrix.Inverse;
  Result := FInvMatrix;
  FInvMatrixChanged := False;
end;

function TLocation.GetInvRotMatrix: TMatrix3;
begin
  if FMatrixChanged or FRotMatrixChanged or FInvRotMatrixChanged then
    FInvRotMatrix := RotMatrix.Inverse;
  Result := FInvRotMatrix;
  FInvRotMatrixChanged := False;
end;

procedure TLocation.SetScale(AValue: TVector3);
begin
  if FScale = AValue then
    Exit;
  FScale := AValue;
  TriggerChanges([ctScale]);
end;

procedure TLocation.SetScaleX(AValue: Single);
begin
  if FScale.X = AValue then
    Exit;
  FScale.X := AValue;
  TriggerChanges([ctScale]);
end;

procedure TLocation.SetScaleY(AValue: Single);
begin
  if FScale.Y = AValue then
    Exit;
  FScale.Y := AValue;
  TriggerChanges([ctScale]);
end;

procedure TLocation.SetScaleZ(AValue: Single);
begin
  if FScale.Z = AValue then
    Exit;
  FScale.Z := AValue;
  TriggerChanges([ctScale]);
end;

function TLocation.GetLook: TVector3;
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

function TLocation.GetMatrix: TMatrix4;
begin
  if FMatrixChanged then
    BuildMatrix;
  FInvMatrixChanged := True;
  FRotMatrixChanged := True;
  Result := FMatrix;
end;

function TLocation.GetRealPosition: TVector3;
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

function TLocation.GetRight: TVector3;
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

function TLocation.GetRotMatrix: TMatrix3;
begin
  if FMatrixChanged or FRotMatrixChanged then
    FRotMatrix := Matrix.Minor[3, 3];
  Result := FRotMatrix;
  FRotMatrixChanged := False;
  FInvRotMatrixChanged := True;
end;

function TLocation.GetUp: TVector3;
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

procedure TLocation.BuildMatrix;
begin
  OnChanged.Disable;
  if Parent <> nil then
    FMatrix := Parent.Matrix
  else
    FMatrix.LoadIdentity;
  if FInverted then
  begin
    FreeTranslate(-FOffset);
    FreeScale(1 / FScale);
    FreeRoll(-RollAngle);
    FreePitch(-PitchAngle);
    FreeTurn(-TurnAngle);
    FreeTranslate(-FPos);
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
  OnChanged.Enable;
end;

procedure TLocation.SetOffset(AValue: TVector3);
begin
  if FOffset = AValue then
    Exit;
  FOffset := AValue;
  TriggerChanges([ctOffset]);
end;

procedure TLocation.SetOffsetX(AValue: Single);
begin
  if FOffset.X = AValue then
    Exit;
  FOffset.X := AValue;
  TriggerChanges([ctOffset]);
end;

procedure TLocation.SetOffsetY(AValue: Single);
begin
  if FOffset.Y = AValue then
    Exit;
  FOffset.Y := AValue;
  TriggerChanges([ctOffset]);
end;

procedure TLocation.SetOffsetZ(AValue: Single);
begin
  if FOffset.Z = AValue then
    Exit;
  FOffset.Z := AValue;
  TriggerChanges([ctOffset]);
end;

procedure TLocation.SetPitch(AValue: Single);
begin
  AValue := RotationLimit.RangedModL(AValue);
  if FRotation.X = AValue then
    Exit;
  FRotation.X := AValue;
  TriggerChanges([ctPitch]);
end;

procedure TLocation.SetPos(AValue: TVector3);
begin
  if FPos = AValue then
    Exit;
  FPos := AValue;
  TriggerChanges([ctPosition]);
end;

procedure TLocation.SetPosX(AValue: Single);
begin
  if FPos.X = AValue then
    Exit;
  FPos.X := AValue;
  TriggerChanges([ctPosition]);
end;

procedure TLocation.SetPosY(AValue: Single);
begin
  if FPos.Y = AValue then
    Exit;
  FPos.Y := AValue;
  TriggerChanges([ctPosition]);
end;

procedure TLocation.SetPosZ(AValue: Single);
begin
  if FPos.Z = AValue then
    Exit;
  FPos.Z := AValue;
  TriggerChanges([ctPosition]);
end;

procedure TLocation.SetRoll(AValue: Single);
begin
  AValue := RotationLimit.RangedModL(AValue);
  if FRotation.Z = AValue then
    Exit;
  FRotation.Z := AValue;
  TriggerChanges([ctRoll]);
end;

procedure TLocation.SetTurn(AValue: Single);
begin
  AValue := RotationLimit.RangedModL(AValue);
  if FRotation.Y = AValue then
    Exit;
  FRotation.Y := AValue;
  TriggerChanges([ctTurn]);
end;

procedure TLocation.ParentChanged(AInfo: TChangeEventInfo);
begin
  TriggerChanges([ctParent]);
end;

procedure TLocation.SetParent(const Value: TLocation);
begin
  if FParent = Value then
    Exit;
  if FParent <> nil then
    FParent.OnChanged.Del(ParentChanged);
  FParent := Value;
  if FParent <> nil then
    FParent.OnChanged.Add(ParentChanged);
  TriggerChanges([ctParent]);
end;

constructor TLocation.Create(AInverted: Boolean);
begin
  Reset;
  FInverted := AInverted;
end;

procedure TLocation.Slide(ADistance: Single; AHorizontal: Boolean);
begin
  if AHorizontal then
    Pos := Pos + Vec3(Right.X * ADistance, 0, Right.Z * ADistance)
  else
    Pos := Pos + Right * ADistance;
end;

procedure TLocation.Lift(ADistance: Single; AYOnly: Boolean);
begin
  if AYOnly then
    Pos := Pos + Vec3(0, Up.Y * ADistance, 0)
  else
    Pos := Pos + Up * ADistance;
end;

procedure TLocation.Move(ADistance: Single; AHorizontal: Boolean);
begin
  if AHorizontal then
    Pos := Pos + Vec3(Look.X * ADistance, 0, Look.Z * ADistance)
  else
    Pos := Pos + Look * ADistance;
end;

procedure TLocation.LookAt(APoint: TVector3);
begin
  Look := RealPosition.VectorTo(APoint);
end;

procedure TLocation.Reset;
begin
  Pos := 0;
  Offset := 0;
  Scale := 1;
  FRotation := 0;
  FMatrix.LoadIdentity;
  FChanged := True;
  OnChanged.Execute(TChangeEventInfo.Create(Self, [
    ctPosition,
    ctOffset,
    ctScale,
    ctTurn,
    ctPitch,
    ctRoll]));
end;

procedure TLocation.ResetRotation;
begin
  if FRotation = 0 then
    Exit;
  FRotation := 0;
  TriggerChanges([ctTurn, ctPitch, ctRoll]);
end;

procedure TLocation.ResetTranslation;
begin
  if Pos = 0 then
    Exit;
  Pos := 0;
  TriggerChanges([ctPosition]);
end;

procedure TLocation.ResetOffset;
begin
  if Offset = 0 then
    Exit;
  Offset := 0;
  TriggerChanges([ctOffset]);
end;

procedure TLocation.ResetScale;
begin
  if Scale = 0 then
    Exit;
  Scale := 0;
  TriggerChanges([ctScale]);
end;

procedure TLocation.Turn(const ATurn: Single);
begin
  TurnAngle := TurnAngle + ATurn;
end;

procedure TLocation.Pitch(const APitch: Single);
begin
  PitchAngle := PitchAngle + APitch;
end;

procedure TLocation.Roll(const ARoll: Single);
begin
  RollAngle := RollAngle + ARoll;
end;

procedure TLocation.FreeTurn(ATurn: Single);
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
  TriggerChanges([ctFreeRotation]);
end;

procedure TLocation.FreePitch(APitch: Single);
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
  TriggerChanges([ctFreeRotation]);
end;

procedure TLocation.FreeRoll(ARoll: Single);
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
  TriggerChanges([ctFreeRotation]);
end;

procedure TLocation.FreeTranslate(const AVector: TVector3);
var
  M: TMatrix4;
begin
  M.LoadIdentity;
  M[3, 0] := AVector.X;
  M[3, 1] := AVector.Y;
  M[3, 2] := AVector.Z;
  FMatrix := FMatrix * M;
  FFreeChanged := True;
  TriggerChanges([ctFreeTranslation]);
end;

procedure TLocation.FreeScale(const AScale: TVector3);
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
  TriggerChanges([ctFreeScale]);
end;

procedure TLocation.FreeMirror(ANormal: TLine3);
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
  TriggerChanges([ctFreeMirror]);
end;

procedure TLocation.FromMatrix(AMatrix: TMatrix4);
begin
  FMatrix := AMatrix;
  FFreeChanged := True;
  TriggerChanges([ctFreeRotation, ctFreeScale, ctFreeTranslation]);
end;

procedure TLocation.FromRotMatrix(AMatrix: TMatrix3);
begin
  FMatrix.Minor[3, 3] := AMatrix;
  FFreeChanged := True;
  TriggerChanges([ctFreeRotation, ctFreeScale]);
end;

procedure TLocation.Rotate(const ARotation: TVector3);
var
  Changes: TChanges;
begin
  Changes := [];
  if ARotation.X <> 0 then
    Include(Changes, ctPitch);
  if ARotation.Y <> 0 then
    Include(Changes, ctTurn);
  if ARotation.Z <> 0 then
    Include(Changes, ctRoll);
  if Changes <> [] then
  begin
    FRotation := FRotation + ARotation;
    FFreeChanged := True;
    TriggerChanges(Changes);
  end;
end;

procedure TLocation.Translate(const AVector: TVector3);
begin
  FPos := FPos + AVector;
  TriggerChanges([ctPosition]);
end;

procedure TLocation.MoveOffset(const AVector: TVector3);
begin
  FOffset := FOffset + AVector;
  TriggerChanges([ctOffset]);
end;

procedure TLocation.ScaleBy(const AScale: TVector3);
begin
  Scale := Scale * AScale;
end;

procedure TLocation.Approach(ALocation: TLocation; ADelta: Single);

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

procedure TLocation.Assign(ALocation: TLocation);
begin
  Pos := ALocation.Pos;
  Offset := ALocation.Offset;
  Scale := ALocation.Scale;
  TurnAngle := ALocation.TurnAngle;
  PitchAngle := ALocation.PitchAngle;
  RollAngle := ALocation.RollAngle;
end;

procedure TLocation.Swap(ALocation: TLocation);
var
  Tmp: TLocation;
begin
  Tmp := TLocation.Create;
  Tmp.Assign(ALocation);
  ALocation.Assign(Self);
  Self.Assign(Tmp);
  Tmp.Free;
end;

procedure TLocation.FreeRotate(AVector: TVector3; const AAngle: Single);
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

  TriggerChanges([ctFreeRotation]);
end;

{ TBlockRotation }

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
      Exit(-1);
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

{ TBlockRaycaster }

constructor TBlock3Raycaster.Create(ALocation: TLocation; ASize: TIntVector3);
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
  InvDirections := (-FLine.D).Dirs;

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

function THexahedron.Visible(APoints: TArray<TVector3>): Boolean;
var
  Face: TLine3;
  Point: TVector3;
  AllPointsOutside: Boolean;
begin
  for Face in FaceNormals do
  begin
    AllPointsOutside := True;
    for Point in APoints do
    begin
      if Face.D.Dot(Face.S.VectorTo(Point)) > 0 then
      begin
        AllPointsOutside := False;
        Break;
      end;
    end;
    if AllPointsOutside then
      Exit(False);
  end;
  Result := True;
end;

class operator THexahedron.in(APoint: TVector3; const AHexahedron: THexahedron): Boolean;
var
  Dir: TBasicDir3;
begin
  for Dir := Low(TBasicDir3) to High(TBasicDir3) do
    if AHexahedron.FaceNormals[Dir].D.Dot(AHexahedron.FaceNormals[Dir].S.VectorTo(APoint)) < 0 then
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

end.
