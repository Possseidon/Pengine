unit Pengine.Box2D;

{$POINTERMATH ON}

interface

uses
  System.Math,
  System.SysUtils,

  Box2D.Dynamics,
  Box2D.Common,
  Box2D.Collision,
  Box2D.Rope,
  Box2DTypes,

  Pengine.ICollections,
  Pengine.Vector,
  Pengine.EventHandling;

type

  TDefBase = class(TObject);

  TShapeClass = class of TShape;

  /// <summary>A base class for all shapes.</summary>
  TShape = class abstract
  private
    FWrapper: b2ShapeWrapper;

    function GetRadius: Single; inline;
    procedure SetRadius(const Value: Single); inline;

  protected
    procedure SetWrapper(AWrapper: b2ShapeHandle); inline;

  public
    constructor Create; virtual;

    property Wrapper: b2ShapeWrapper read FWrapper;

    /// <summary>The radius in <c>m</c>.</summary>
    /// <remarks>Also used as polygon skin thickness.</remarks>
    property Radius: Single read GetRadius write SetRadius;

  end;

  TShape<W: record > = class abstract(TShape)
  private
    function GetWrapper: W; inline;

  public
    property Wrapper: W read GetWrapper;

  end;

  /// <summary>A completly round circle.</summary>
  /// <remarks>This is an actual circle and not a polygon.</remarks>
  TCircle = class(TShape<b2CircleShapeWrapper>)
  private
    function GetPosition: TVector2;
    procedure SetPosition(const Value: TVector2);

  public
    constructor Create; overload; override;
    constructor Create(ARadius: Single); reintroduce; overload;
    constructor Create(ARadius: Single; APosition: TVector2); reintroduce; overload;
    destructor Destroy; override;

    property Position: TVector2 read GetPosition write SetPosition;

  end;

  /// <summary>An up to 8 sided polygon.</summary>
  TPolygon = class(TShape<b2PolygonShapeWrapper>)
  public type

    TIterator<T> = class(TInterfacedObject, IIterator<T>)
    private
      FPolygon: TPolygon;
      FCurrent: Integer;

    protected
      function GetCurrent: T; virtual; abstract;

    public
      constructor Create(APolygon: TPolygon);

      function MoveNext: Boolean;

    end;

    TPointIterator = class(TIterator<TVector2>)
    protected
      function GetCurrent: TVector2; override;

    end;

    IPoints = interface(IReadonlyList<TVector2>)
    end;

    TPoints = class(TInterfacedObject, IPoints, IIterable<TVector2>)
    private
      FPolygon: TPolygon;

      // IReadonlyCollection<TVector2>
      function GetCount: Integer;

      // IReadonlyList<TVector2>
      function GetItem(AIndex: Integer): TVector2;
      function GetMaxIndex: Integer;
      function GetFirst: TVector2;
      function GetLast: TVector2;

    public
      constructor Create(APolygon: TPolygon);

      // IIterable<TVector2>
      function GetEnumerator: IIterator<TVector2>;
      function Iterate: IIterate<TVector2>;

      // IReadonlyCollection<TVector2>
      property Count: Integer read GetCount;
      function Empty: Boolean;
      function Contains(AItem: TVector2): Boolean;

      // IReadonlyList<TVector2>
      property Items[AIndex: Integer]: TVector2 read GetItem; default;
      property MaxIndex: Integer read GetMaxIndex;
      property First: TVector2 read GetFirst;
      property Last: TVector2 read GetLast;

      function IndexOf(AItem: TVector2): Integer;

      function Reverse: IIterate<TVector2>;

    end;

  private
    function GetPoints: IPoints; inline;

    function GetSkinThickness: Single; inline;
    procedure SetSkinThickness(const Value: Single); inline;

  public
    constructor Create; override;
    destructor Destroy; override;

    property Points: IPoints read GetPoints;

    /// <summary>The skin thickness, used to prevent tunneling.</summary>
    property SkinThickness: Single read GetSkinThickness write SetSkinThickness;

    procedure SetPoints(APoints: IList<TVector2>); overload;
    procedure SetPoints(APoints: TArray<TVector2>); overload;
    procedure SetPoints(APoints: PVector2; ACount: Integer); overload;

    procedure SetBox(ASize: TVector2); overload;
    procedure SetBox(ASize: TVector2; ACenter: TVector2; AAngleRad: Single); overload;

    function ValidateConvexity: Boolean;

  end;

  /// <summary>A single line with two sided colision.</summary>
  TEdge = class(TShape<b2EdgeShapeWrapper>)
  private
    function GetLine: TLine2; inline;
    procedure SetLine(const Value: TLine2); inline;
    function GetVertex1: TVector2; inline;
    procedure SetVertex1(const Value: TVector2); inline;
    function GetVertex2: TVector2; inline;
    procedure SetVertex2(const Value: TVector2); inline;

    function GetPrev: TVector2; inline;
    procedure SetPrev(const Value: TVector2); inline;

    function GetNext: TVector2; inline;
    procedure SetNext(const Value: TVector2); inline;

  public
    constructor Create; override;
    destructor Destroy; override;

    property Line: TLine2 read GetLine write SetLine;
    property Vertex1: TVector2 read GetVertex1 write SetVertex1;
    property Vertex2: TVector2 read GetVertex2 write SetVertex2;

    function HasPrev: Boolean;
    property Prev: TVector2 read GetPrev write SetPrev;

    function HasNext: Boolean;
    property Next: TVector2 read GetNext write SetNext;

  end;

  /// <summary>A chain of lines with an automatic loop or optional ghost vertexes.</summary>
  TChain = class(TShape<b2ChainShapeWrapper>)
  public type

    TIterator<T> = class(TInterfacedObject, IIterator<T>)
    private
      FChain: TChain;
      FCurrent: Integer;

    protected
      function GetCurrent: T; virtual; abstract;

    public
      constructor Create(AChain: TChain);

      function MoveNext: Boolean;

    end;

    TPointIterator = class(TIterator<TVector2>)
    protected
      function GetCurrent: TVector2; override;

    end;

    IPoints = interface(IReadonlyList<TVector2>)
    end;

    TPoints = class(TInterfacedObject, IPoints, IIterable<TVector2>)
    private
      FChain: TChain;

      // IReadonlyCollection<TVector2>
      function GetCount: Integer;

      // IReadonlyList<TVector2>
      function GetItem(AIndex: Integer): TVector2;
      function GetMaxIndex: Integer;
      function GetFirst: TVector2;
      function GetLast: TVector2;

    public
      constructor Create(AChain: TChain);

      // IIterable<TVector2>
      function GetEnumerator: IIterator<TVector2>;
      function Iterate: IIterate<TVector2>;

      // IReadonlyCollection<TVector2>
      property Count: Integer read GetCount;
      function Empty: Boolean;
      function Contains(AItem: TVector2): Boolean;

      // IReadonlyList<TVector2>
      property Items[AIndex: Integer]: TVector2 read GetItem; default;
      property MaxIndex: Integer read GetMaxIndex;
      property First: TVector2 read GetFirst;
      property Last: TVector2 read GetLast;

      function IndexOf(AItem: TVector2): Integer;

      function Reverse: IIterate<TVector2>;

    end;

  private
    function GetPoints: IPoints;

    function GetNext: TVector2;
    function GetPrev: TVector2;

    procedure SetNext(const Value: TVector2);
    procedure SetPrev(const Value: TVector2);

  public
    constructor Create; override;
    destructor Destroy; override;

    property Points: IPoints read GetPoints;

    function HasPrev: Boolean;
    property Prev: TVector2 read GetPrev write SetPrev;

    function HasNext: Boolean;
    property Next: TVector2 read GetNext write SetNext;

    procedure SetChain(APoints: IList<TVector2>); overload;
    procedure SetChain(APoints: TArray<TVector2>); overload;
    procedure SetChain(APoints: PVector2; ACount: Integer); overload;

    procedure SetLoop(APoints: IList<TVector2>); overload;
    procedure SetLoop(APoints: TArray<TVector2>); overload;
    procedure SetLoop(APoints: PVector2; ACount: Integer); overload;

  end;

  TBody = class;

  /// <summary>A fixture combines a shape with material properties and more.</summary>
  TFixture = class
  public type

    TDef = class(TDefBase)
    private
      FData: b2FixtureDef;

      function GetData: Pb2FixtureDef;

    public
      constructor Create(AShape: TShape);

      /// <summary>The density in <c>kg*m^2</c></summary>
      property Density: Single read FData.Density write FData.Density;
      /// <summary>The friction coefficient usually in range <c>[0, 1]</c>.</summary>
      property Friction: Single read FData.Friction write FData.Friction;
      /// <summary>The restitution (or bounce factor in layman's terms) usually in range <c>[0, 1]</c>.</summary>
      /// <remarks>Uses the greater of two touching fixtures for calculation.</remarks>
      property Restitution: Single read FData.Restitution write FData.Restitution;
      /// <summary>A sensor collectes contact info, but does not generate a collision response.</summary>
      property IsSensor: Boolean read FData.IsSensor write FData.IsSensor;
      // TODO: property Filter;

      property Data: Pb2FixtureDef read GetData;

    end;

  private
    FWrapper: b2Fixture;

    function GetBody: TBody;

    function GetDenstity: Single;
    procedure SetDensity(const Value: Single);
    function GetFriction: Single;
    procedure SetFriction(const Value: Single);
    function GetRestitution: Single;
    procedure SetRestitution(const Value: Single);

  public
    constructor Create(ABody: TBody; ADef: TFixture.TDef);
    destructor Destroy; override;

    property Wrapper: b2Fixture read FWrapper;

    property Body: TBody read GetBody;

    /// <summary>The density in <c>kg*m^2</c></summary>
    property Density: Single read GetDenstity write SetDensity;
    /// <summary>The friction coefficient usually in range <c>[0, 1]</c>.</summary>
    property Friction: Single read GetFriction write SetFriction;
    /// <summary>The restitution (or bounce factor in layman's terms) usually in range <c>[0, 1]</c>.</summary>
    /// <remarks>Uses the greater of two touching fixtures for calculation.</remarks>
    property Restitution: Single read GetRestitution write SetRestitution;
    // TODO: IsSensor
    // TODO: property Filter;

    function GetNext: TFixture;

  end;

  TWorld = class;

  TJoint = class;

  /// <summary>A body is made up of fixtures.</summary>
  TBody = class
  public type

    TType = (
      btStatic,
      btKinematic,
      btDynamic
      );

    TDef = class(TDefBase)
    private
      FData: b2BodyDef;

      function GetData: Pb2BodyDef;
      function GetType: TType;
      procedure SetType(const Value: TType);
      function GetPosition: TVector2;
      procedure SetPosition(const Value: TVector2);
      function GetRotation: Single;
      procedure SetRotation(const Value: Single);
      function GetLinearVelocity: TVector2;
      procedure SetLinearVelocity(const Value: TVector2);

    public
      constructor Create(AType: TType; APosition: TVector2);

      property Data: Pb2BodyDef read GetData;

      property &Type: TType read GetType write SetType;

      /// <summary>The position in <c>m</c>.</summary>
      property Position: TVector2 read GetPosition write SetPosition;
      /// <summary>The rotation in radians.</summary>
      property RotationRad: Single read FData.angle write FData.angle;
      /// <summary>The rotation in degrees.</summary>
      property Rotation: Single read GetRotation write SetRotation;
      /// <summary>The linear velocity in <c>m/s</c></summary>
      property LinearVelocity: TVector2 read GetLinearVelocity write SetLinearVelocity;
      /// <summary>The angular velocity in <c>rad/s</c></summary>
      property AngularVelocity: Single read FData.AngularVelocity write FData.AngularVelocity;
      /// <summary>The linear damping.</summary>
      /// <remarks>Values greater than 1 become sensitive to the timestep.</remarks>
      property LinearDamping: Single read FData.LinearDamping write FData.LinearDamping;
      /// <summary>The angular damping.</summary>
      /// <remarks>Values greater than 1 become sensitive to the timestep.</remarks>
      property AngularDamping: Single read FData.AngularDamping write FData.AngularDamping;
      /// <summary>Wether this body is allowed to sleep.</summary>
      property AllowSleep: Boolean read FData.AllowSleep write FData.AllowSleep;
      /// <summary>Wether this body is initially awake.</summary>
      property Awake: Boolean read FData.Awake write FData.Awake;
      /// <summary>Prevents all rotation.</summary>
      property FixedRotation: Boolean read FData.FixedRotation write FData.FixedRotation;
      /// <summary>Prevents tunneling, but uses more CPU.</summary>
      property Bullet: Boolean read FData.Bullet write FData.Bullet;
      /// <summary>Wether this body is initially active.</summary>
      property Active: Boolean read FData.Active write FData.Active;
      /// <summary>A scaling factor to apply on the worlds gravity.</summary>
      property GravityScale: Single read FData.GravityScale write FData.GravityScale;

    end;

    TFixtureIterator = class(TInterfacedObject, IIterator<TFixture>)
    private
      FCurrent: TFixture;
      FNext: TFixture;

      function GetCurrent: TFixture;

    public
      constructor Create(ABody: TBody);

      function MoveNext: Boolean;

    end;

    IFixtures = interface(IIterable<TFixture>)
      function GetFirst: TFixture;

      property First: TFixture read GetFirst;

      function Add(AFixture: TFixture.TDef): TFixture; overload;
      function Add(AShape: TShape): TFixture; overload;

    end;

    TFixtures = class(TInterfacedObject, IFixtures, IIterable<TFixture>)
    private
      FBody: TBody;

      // IFixtures
      function GetFirst: TFixture;

    public
      constructor Create(ABody: TBody);

      // IIterable<TFixture>
      function GetEnumerator: IIterator<TFixture>;
      function Iterate: IIterate<TFixture>;

      // IFixtures
      property First: TFixture read GetFirst;

      function Add(AFixtureDef: TFixture.TDef): TFixture; overload;
      function Add(AShape: TShape): TFixture; overload;

    end;

    TJointEdge = record
    private
      FData: Pb2JointEdge;

      function GetJoint: TJoint;
      function GetOther: TBody;

    public
      constructor Create(AJointEdge: Pb2JointEdge);

      property Other: TBody read GetOther;
      property Joint: TJoint read GetJoint;

      function Next: TJointEdge;
      function HasNext: Boolean;

      function Prev: TJointEdge;
      function HasPrev: Boolean;

    end;

    TJointEdgeIterator = class(TInterfacedObject, IIterator<TJointEdge>)
    private
      FCurrent: TJointEdge;
      FNext: TJointEdge;

      function GetCurrent: TJointEdge;

    public
      constructor Create(ABody: TBody);

      function MoveNext: Boolean;

    end;

    IJointEdges = interface(IIterable<TJointEdge>)
      function GetFirst: TJointEdge;

      property First: TJointEdge read GetFirst;

    end;

    TJointEdges = class(TInterfacedObject, IJointEdges, IIterable<TJointEdge>)
    private
      FBody: TBody;

      // IJoints
      function GetFirst: TJointEdge;

    public
      constructor Create(ABody: TBody);

      // IIterable<TJoint>
      function GetEnumerator: IIterator<TJointEdge>;
      function Iterate: IIterate<TJointEdge>;

      // IJoints
      property First: TJointEdge read GetFirst;

    end;

    TLocations = IList<TLocation2>;

    TEventInfo = TEventInfo<TBody>;

    TEvent = TEvent<TEventInfo>;

  private
    FWrapper: b2BodyWrapper;
    FUserData: TObject;
    FWorld: TWorld;
    FLocations: TLocations;
    FOnLocationChanged: TEvent;

    function GetType: TType;
    procedure SetType(const Value: TType);

    function GetFixtures: IFixtures;
    function GetJointEdges: IJointEdges;

    function GetPosition: TVector2;
    procedure SetPosition(const Value: TVector2);
    function GetRotationRad: Single;
    procedure SetRotationRad(const Value: Single);
    function GetRotation: Single;
    procedure SetRotation(const Value: Single);

    function GetWorldCenter: TVector2;
    function GetLocalCenter: TVector2;

    function GetLinearVelocity: TVector2;
    procedure SetLinearVelocity(const Value: TVector2);
    function GetLinearVelocityAtWorldPoint(APoint: TVector2): TVector2;
    function GetLinearVelocityAtLocalPoint(APoint: TVector2): TVector2;
    function GetAngularVelocity: Single;
    procedure SetAngularVelocity(const Value: Single);

    function GetLinearDamping: Single;
    procedure SetLinearDamping(const Value: Single);
    function GetAngularDamping: Single;
    procedure SetAngularDamping(const Value: Single);

    function GetAwake: Boolean;
    procedure SetAwake(const Value: Boolean);
    function GetSleepingAllowed: Boolean;
    procedure SetSleepingAllowed(const Value: Boolean);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);

    function GetFixedRotation: Boolean;
    procedure SetFixedRotation(const Value: Boolean);

    function GetBullet: Boolean;
    procedure SetBullet(const Value: Boolean);

    function GetGravityScale: Single;
    procedure SetGravityScale(const Value: Single);

    function GetMass: Single;
    function GetInertia: Single;

    function GetWorldPoint(ALocalPoint: TVector2): TVector2;
    function GetWorldVector(ALocalVector: TVector2): TVector2;
    function GetLocalPoint(AWorldPoint: TVector2): TVector2;
    function GetLocalVector(AWorldVector: TVector2): TVector2;

  public
    constructor Create(AWorld: TWorld; ADef: TDef);
    destructor Destroy; override;

    property Wrapper: b2BodyWrapper read FWrapper;
    property World: TWorld read FWorld;
    property &Type: TType read GetType write SetType;
    property UserData: TObject read FUserData write FUserData;

    function GetNext: TBody;

    property Fixtures: IFixtures read GetFixtures;
    property JointEdges: IJointEdges read GetJointEdges;

    /// <summary>The position in <c>m</c>.</summary>
    property Position: TVector2 read GetPosition write SetPosition;
    /// <summary>The rotation in radians.</summary>
    property RotationRad: Single read GetRotationRad write SetRotationRad;
    /// <summary>The rotation in degrees.</summary>
    property Rotation: Single read GetRotation write SetRotation;

    /// <summary>The world position of the center of mass.</summary>
    property WorldCenter: TVector2 read GetWorldCenter;
    /// <summary>The local position of the center of mass.</summary>
    property LocalCenter: TVector2 read GetLocalCenter;

    /// <summary>The linear velocity in <c>m/s</c>.</summary>
    property LinearVelocity: TVector2 read GetLinearVelocity write SetLinearVelocity;
    property LinearVelocityAtWorldPoint[APoint: TVector2]: TVector2 read GetLinearVelocityAtWorldPoint;
    property LinearVelocityAtLocalPoint[APoint: TVector2]: TVector2 read GetLinearVelocityAtLocalPoint;
    /// <summary>The angular velocity in <c>rad/s</c></summary>
    property AngularVelocity: Single read GetAngularVelocity write SetAngularVelocity;

    /// <summary>The linear damping.</summary>
    /// <remarks>Values greater than 1 become sensitive to the timestep.</remarks>
    property LinearDamping: Single read GetLinearDamping write SetLinearDamping;
    /// <summary>The angular damping.</summary>
    /// <remarks>Values greater than 1 become sensitive to the timestep.</remarks>
    property AngularDamping: Single read GetAngularDamping write SetAngularDamping;

    /// <summary>Sleeing bodies use a lot less CPU.</summary>
    property Awake: Boolean read GetAwake write SetAwake;
    /// <summary>Wether this body is allowed to sleep.</summary>
    property SleepingAllowed: Boolean read GetSleepingAllowed write SetSleepingAllowed;
    /// <summary>Inactive bodies are not simulated in any way and don't have collision.</summary>
    property Active: Boolean read GetActive write SetActive;

    /// <summary>Prevents all rotation.</summary>
    property FixedRotation: Boolean read GetFixedRotation write SetFixedRotation;

    /// <summary>Prevents tunneling, but uses more CPU.</summary>
    property Bullet: Boolean read GetBullet write SetBullet;

    /// <summary>A scaling factor to apply on the worlds gravity.</summary>
    property GravityScale: Single read GetGravityScale write SetGravityScale;

    /// <summary>The mass in <c>kg</c>.</summary>
    property Mass: Single read GetMass;
    /// <summary>the inertia in <c>kgm^2</c>.</summary>
    property Inertia: Single read GetInertia;

    /// <summary>Converts a local point to a world point.</summary>
    property WorldPoint[ALocalPoint: TVector2]: TVector2 read GetWorldPoint;
    /// <summary>Converts a world vector to a local vector.</summary>
    property WorldVector[ALocalVector: TVector2]: TVector2 read GetWorldVector;
    /// <summary>Converts a local point to a world point.</summary>
    property LocalPoint[AWorldPoint: TVector2]: TVector2 read GetLocalPoint;
    /// <summary>Converts a local vector to a world vector.</summary>
    property LocalVector[AWorldVector: TVector2]: TVector2 read GetLocalVector;

    /// <summary>Apllies a force in Newton at the point in world coordinates.</summary>
    procedure ApplyForce(AForce, APoint: TVector2; Awake: Boolean = True);
    /// <summary>Applies a force in Newton at the center of mass of the body.</summary>
    procedure ApplyForceToCenter(AForce: TVector2; Awake: Boolean = True);
    /// <summary>Applies a torque along the z-axis in <c>Nm</c>.</summary>
    procedure ApplyTorque(ATorque: Single; Awake: Boolean = True);
    /// <summary>Applies a linear impulse in <c>Ns</c>.</summary>
    procedure ApplyLinearImpulse(AImpulse, APoint: TVector2; Awake: Boolean = True);
    /// <summary>Applies an angular impulse in <c>kgm^2/s</c>.</summary>
    procedure ApplyAngularImpulse(AImpulse: Single; Awake: Boolean = True);

    /// <summary>Add a location, that automatically gets its transform updated by the Step function.</summary>
    procedure AddLocation(ALocation: TLocation2);
    /// <summary>Removes a location from being automatically updated by the Step function.</summary>
    procedure RemoveLocation(ALocation: TLocation2);

    function OnLocationChanged: TEvent.TAccess;

    procedure UpdateLocations;

    // TODO: MassData
    // TODO: ContactList

  end;

  /// <summary>A base class, which can add various constraints to bodies.</summary>
  TJoint = class abstract
  public type

    TType = (
      jtUnknown,
      jtRevolute,
      jtPrismatic,
      jtDistance,
      jtPulley,
      jtMouse,
      jtGear,
      jtWheel,
      jtWeld,
      jtFriction,
      jtRope,
      jtMotor
      );

    TDef = class(TDefBase)
    private
      function GetBodyA: TBody;
      procedure SetBodyA(const Value: TBody);
      function GetBodyB: TBody;
      procedure SetBodyB(const Value: TBody);

      function GetData: Pb2JointDef; virtual; abstract;
      function GetCollideConnected: Boolean;
      procedure SetCollideConnected(const Value: Boolean);

    public
      constructor Create; virtual;

      function GetType: TType;

      property BodyA: TBody read GetBodyA write SetBodyA;
      property BodyB: TBody read GetBodyB write SetBodyB;
      property CollideConnected: Boolean read GetCollideConnected write SetCollideConnected;

      property Data: Pb2JointDef read GetData;

    end;

    TDef<T: record > = class(TDef)
    public type

      PT = ^T;

    private
      function GetData: Pb2JointDef; override;
      function GetDataTyped: PT;

      function CreateData: T; virtual; abstract;

    protected
      FData: T;

    public
      constructor Create; override;

      property Data: PT read GetDataTyped;

    end;

    TIterator = class(TInterfacedObject, IIterator<TJoint>)
    private
      FCurrent: TJoint;
      FNext: TJoint;

      function GetCurrent: TJoint;

    public
      constructor Create(AFirst: TJoint);

      function MoveNext: Boolean;

    end;

  private
    FWorld: TWorld;
    FWrapper: b2JointWrapper;

    function GetBodyA: TBody;
    function GetBodyB: TBody;

    function GetAnchorA: TVector2;
    function GetAnchorB: TVector2;

    function GetCollideConnected: Boolean;
    function GetActive: Boolean;

  public
    constructor Create(AWorld: TWorld; ADef: TDef); virtual;
    destructor Destroy; override;

    function GetType: TType;

    property Wrapper: b2JointWrapper read FWrapper;

    property World: TWorld read FWorld;

    property BodyA: TBody read GetBodyA;
    property BodyB: TBody read GetBodyB;

    property AnchorA: TVector2 read GetAnchorA;
    property AnchorB: TVector2 read GetAnchorB;

    function ReactionForce(AInvDeltaTime: Single): TVector2;
    function ReactionTorque(AInvDeltaTime: Single): Single;

    property CollideConnected: Boolean read GetCollideConnected;

    property Active: Boolean read GetActive;

    procedure ShiftOrigin(ANewOrigin: TVector2);

    function GetNext: TJoint;

  end;

  TJointClass = class of TJoint;

  TJoint<T: record > = class abstract(TJoint)
  private
    function GetWrapper: T;

  public
    property Wrapper: T read GetWrapper;

  end;

  TDistanceJoint = class(TJoint<b2DistanceJointWrapper>)
  public type

    TDef = class(TJoint.TDef<b2DistanceJointDef>)
    private
      function GetAnchorA: TVector2;
      function GetAnchorB: TVector2;
      procedure SetAnchorA(const Value: TVector2);
      procedure SetAnchorB(const Value: TVector2);

    protected
      function CreateData: b2DistanceJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AAnchorA, AAnchorB: TVector2); reintroduce; overload;

      property AnchorA: TVector2 read GetAnchorA write SetAnchorA;
      property AnchorB: TVector2 read GetAnchorB write SetAnchorB;
      property Length: Single read FData.Length write FData.Length;
      property Frequency: Single read FData.frequencyHz write FData.frequencyHz;
      property DampingRatio: Single read FData.DampingRatio write FData.DampingRatio;

    end;

  private
    function GetLength: Single;
    procedure SetLength(const Value: Single);
    function GetFrequency: Single;
    procedure SetFrequency(const Value: Single);
    function GetDampingRatio: Single;
    procedure SetDampingRatio(const Value: Single);
    function GetLocalAnchorA: TVector2;
    function GetLocalAnchorB: TVector2;

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LocalAnchorA: TVector2 read GetLocalAnchorA;
    property LocalAnchorB: TVector2 read GetLocalAnchorB;
    property Length: Single read GetLength write SetLength;
    property Frequency: Single read GetFrequency write SetFrequency;
    property DampingRatio: Single read GetDampingRatio write SetDampingRatio;

  end;

  TRevoluteJoint = class(TJoint<b2RevoluteJointWrapper>)
  public type

    TDef = class(TJoint.TDef<b2RevoluteJointDef>)
    private
      function GetAnchorA: TVector2;
      procedure SetAnchorA(const Value: TVector2);
      function GetAnchorB: TVector2;
      procedure SetAnchorB(const Value: TVector2);
      function GetLimitsRad: TBounds1;
      procedure SetLimitsRad(const Value: TBounds1);
      function GetLimits: TBounds1;
      procedure SetLimits(const Value: TBounds1);
      function GetLowerAngle: Single;
      function GetUpperAngle: Single;
      procedure SetLowerAngle(const Value: Single);
      procedure SetUpperAngle(const Value: Single);

    protected
      function CreateData: b2RevoluteJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AAnchor: TVector2); reintroduce; overload;

      property AnchorA: TVector2 read GetAnchorA write SetAnchorA;
      property AnchorB: TVector2 read GetAnchorB write SetAnchorB;
      property ReferenceAngle: Single read FData.ReferenceAngle write FData.ReferenceAngle;
      property EnableLimit: Boolean read FData.EnableLimit write FData.EnableLimit;
      property LimitsRad: TBounds1 read GetLimitsRad write SetLimitsRad;
      property Limits: TBounds1 read GetLimits write SetLimits;
      property LowerAngleRad: Single read FData.LowerAngle write FData.LowerAngle;
      property LowerAngle: Single read GetLowerAngle write SetLowerAngle;
      property UpperAngleRad: Single read FData.UpperAngle write FData.UpperAngle;
      property UpperAngle: Single read GetUpperAngle write SetUpperAngle;
      property EnableMotor: Boolean read FData.EnableMotor write FData.EnableMotor;
      property MotorSpeed: Single read FData.MotorSpeed write FData.MotorSpeed;
      property MaxMotorTorque: Single read FData.MaxMotorTorque write FData.MaxMotorTorque;

    end;

  private
    function GetLocalAnchorA: TVector2;
    function GetLocalAnchorB: TVector2;
    function GetReferenceAngle: Single;
    function GetEnableLimit: Boolean;
    procedure SetEnableLimit(const Value: Boolean);
    function GetLimits: TBounds1;
    procedure SetLimits(const Value: TBounds1);
    function GetLimitsRad: TBounds1;
    procedure SetLimitsRad(const Value: TBounds1);
    function GetLowerLimitRad: Single;
    procedure SetLowerLimitRad(const Value: Single);
    function GetLowerLimit: Single;
    procedure SetLowerLimit(const Value: Single);
    function GetUpperLimitRad: Single;
    procedure SetUpperLimitRad(const Value: Single);
    function GetUpperLimit: Single;
    procedure SetUpperLimit(const Value: Single);
    function GetEnableMotor: Boolean;
    procedure SetEnableMotor(const Value: Boolean);
    function GetMotorSpeed: Single;
    procedure SetMotorSpeed(const Value: Single);
    function GetMaxMotorTorque: Single;
    procedure SetMaxMotorTorque(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LocalAnchorA: TVector2 read GetLocalAnchorA;
    property LocalAnchorB: TVector2 read GetLocalAnchorB;
    property ReferenceAngle: Single read GetReferenceAngle;
    property EnableLimit: Boolean read GetEnableLimit write SetEnableLimit;
    property LimitsRad: TBounds1 read GetLimitsRad write SetLimitsRad;
    property Limits: TBounds1 read GetLimits write SetLimits;
    property LowerLimitRad: Single read GetLowerLimitRad write SetLowerLimitRad;
    property LowerLimit: Single read GetLowerLimit write SetLowerLimit;
    property UpperLimitRad: Single read GetUpperLimitRad write SetUpperLimitRad;
    property UpperLimit: Single read GetUpperLimit write SetUpperLimit;
    property EnableMotor: Boolean read GetEnableMotor write SetEnableMotor;
    property MotorSpeed: Single read GetMotorSpeed write SetMotorSpeed;
    property MaxMotorTorque: Single read GetMaxMotorTorque write SetMaxMotorTorque;

  end;

  TPrismaticJoint = class(TJoint<b2PrismaticJointWrapper>)
  public type

    TDef = class(TDef<b2PrismaticJointDef>)
    private
      function GetAxis: TVector2;
      procedure SetAxis(const Value: TVector2);
      function GetLimits: TBounds1;
      procedure SetLimits(const Value: TBounds1);

    protected
      function CreateData: b2PrismaticJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AAnchor, AAxis: TVector2); reintroduce; overload;

      property Axis: TVector2 read GetAxis write SetAxis;
      property ReferenceAngle: Single read FData.ReferenceAngle write FData.ReferenceAngle;
      property EnableLimit: Boolean read FData.EnableLimit write FData.EnableLimit;
      property Limits: TBounds1 read GetLimits write SetLimits;
      property LowerTranslation: Single read FData.LowerTranslation write FData.LowerTranslation;
      property UpperTranslation: Single read FData.UpperTranslation write FData.UpperTranslation;
      property EnableMotor: Boolean read FData.EnableMotor write FData.EnableMotor;
      property MotorSpeed: Single read FData.MotorSpeed write FData.MotorSpeed;
      property MaxMotorForce: Single read FData.MaxMotorForce write FData.MaxMotorForce;

    end;

  private
    function GetLocalAnchorA: TVector2;
    function GetLocalAnchorB: TVector2;
    function GetAxis: TVector2;
    function GetReferenceAngle: Single;
    function GetEnableLimit: Boolean;
    procedure SetEnableLimit(const Value: Boolean);
    function GetLimits: TBounds1;
    procedure SetLimits(const Value: TBounds1);
    function GetLowerLimit: Single;
    procedure SetLowerLimit(const Value: Single);
    function GetUpperLimit: Single;
    procedure SetUpperLimit(const Value: Single);
    function GetEnableMotor: Boolean;
    procedure SetEnableMotor(const Value: Boolean);
    function GetMotorSpeed: Single;
    procedure SetMotorSpeed(const Value: Single);
    function GetMaxMotorForce: Single;
    procedure SetMaxMotorForce(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LocalAnchorA: TVector2 read GetLocalAnchorA;
    property LocalAnchorB: TVector2 read GetLocalAnchorB;
    property Axis: TVector2 read GetAxis;
    property ReferenceAngle: Single read GetReferenceAngle;
    property EnableLimit: Boolean read GetEnableLimit write SetEnableLimit;
    property Limits: TBounds1 read GetLimits write SetLimits;
    property LowerLimit: Single read GetLowerLimit write SetLowerLimit;
    property UpperLimit: Single read GetUpperLimit write SetUpperLimit;
    property EnableMotor: Boolean read GetEnableMotor write SetEnableMotor;
    property MotorSpeed: Single read GetMotorSpeed write SetMotorSpeed;
    property MaxMotorForce: Single read GetMaxMotorForce write SetMaxMotorForce;

  end;

  TPulleyJoint = class(TJoint<b2PulleyJointWrapper>)
  public type

    TDef = class(TDef<b2PulleyJointDef>)
    private
      function GetGroundAnchorA: TVector2;
      procedure SetGroundAnchorA(const Value: TVector2);
      function GetGroundAnchorB: TVector2;
      procedure SetGroundAnchorB(const Value: TVector2);
      function GetAnchorA: TVector2;
      procedure SetAnchorA(const Value: TVector2);
      function GetAnchorB: TVector2;
      procedure SetAnchorB(const Value: TVector2);

    protected
      function CreateData: b2PulleyJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AGroundAnchorA, AGroundAnchorB, AAnchorA, AAnchorB: TVector2;
        ARatio: Single = 1.0); reintroduce; overload;

      property GroundAnchorA: TVector2 read GetGroundAnchorA write SetGroundAnchorA;
      property GroundAnchorB: TVector2 read GetGroundAnchorB write SetGroundAnchorB;
      property AnchorA: TVector2 read GetAnchorA write SetAnchorA;
      property AnchorB: TVector2 read GetAnchorB write SetAnchorB;
      property Ratio: Single read FData.Ratio write FData.Ratio;
      property LengthA: Single read FData.LengthA write FData.LengthA;
      property LengthB: Single read FData.LengthB write FData.LengthB;

    end;

  private
    function GetCurrentLengthA: Single;
    function GetCurrentLengthB: Single;
    function GetGroundAnchorA: TVector2;
    function GetGroundAnchorB: TVector2;
    function GetLengthA: Single;
    function GetLengthB: Single;
    function GetRatio: Single;

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property GroundAnchorA: TVector2 read GetGroundAnchorA;
    property GroundAnchorB: TVector2 read GetGroundAnchorB;
    property Ratio: Single read GetRatio;
    property LengthA: Single read GetLengthA;
    property LengthB: Single read GetLengthB;
    property CurrentLengthA: Single read GetCurrentLengthA;
    property CurrentLengthB: Single read GetCurrentLengthB;

  end;

  /// <summary>Forces two revolute or prismatic joints to move in unison.</summary>
  /// <remarks>Both joints must have a non-dynamic as BodyA.</remarks>
  TGearJoint = class(TJoint<b2GearJointWrapper>)
  public type

    TDef = class(TDef<b2GearJointDef>)
    private
      function GetJoint1: TJoint;
      procedure SetJoint1(const Value: TJoint);
      function GetJoint2: TJoint;
      procedure SetJoint2(const Value: TJoint);

    protected
      function CreateData: b2GearJointDef; override;

    public
      constructor Create(AJoint1, AJoint2: TJoint; ARatio: Single = 1.0); reintroduce; overload;

      property Joint1: TJoint read GetJoint1 write SetJoint1;
      property Joint2: TJoint read GetJoint2 write SetJoint2;
      property Ratio: Single read FData.Ratio write FData.Ratio;

    end;

  private
    function GetJoint1: TJoint;
    function GetJoint2: TJoint;
    function GetRatio: Single;
    procedure SetRatio(const Value: Single);
    function GetBodyA: TBody;
    function GetBodyB: TBody;

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property BodyA: TBody read GetBodyA;
    property BodyB: TBody read GetBodyB;
    property Joint1: TJoint read GetJoint1;
    property Joint2: TJoint read GetJoint2;
    property Ratio: Single read GetRatio write SetRatio;

  end;

  TMouseJoint = class(TJoint<b2MouseJointWrapper>)
  public type

    TDef = class(TDef<b2MouseJointDef>)
    private
      function GetPosition: TVector2;
      procedure SetPosition(const Value: TVector2);

    protected
      function CreateData: b2MouseJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; APosition: TVector2); reintroduce; overload;

      property Position: TVector2 read GetPosition write SetPosition;
      property MaxForce: Single read FData.MaxForce write FData.MaxForce;
      property Frequncy: Single read FData.frequencyHz write FData.frequencyHz;
      property DampingRatio: Single read FData.DampingRatio write FData.DampingRatio;

    end;

  private
    function GetPosition: TVector2;
    procedure SetPosition(const Value: TVector2);
    function GetDampingRatio: Single;
    function GetFrequency: Single;
    function GetMaxForce: Single;
    procedure SetDampingRatio(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetMaxForce(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property Position: TVector2 read GetPosition write SetPosition;
    property MaxForce: Single read GetMaxForce write SetMaxForce;
    property Frequncy: Single read GetFrequency write SetFrequency;
    property DampingRatio: Single read GetDampingRatio write SetDampingRatio;

  end;

  TWheelJoint = class(TJoint<b2WheelJointWrapper>)
  public type

    TDef = class(TDef<b2WheelJointDef>)
    private
      function GetAnchorA: TVector2;
      function GetAnchorB: TVector2;
      function GetAxis: TVector2;
      procedure SetAnchorA(const Value: TVector2);
      procedure SetAnchorB(const Value: TVector2);
      procedure SetAxis(const Value: TVector2);

    protected
      function CreateData: b2WheelJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AAnchor, AAxis: TVector2); reintroduce; overload;

      property AnchorA: TVector2 read GetAnchorA write SetAnchorA;
      property AnchorB: TVector2 read GetAnchorB write SetAnchorB;
      property Axis: TVector2 read GetAxis write SetAxis;
      property EnableMotor: Boolean read FData.EnableMotor write FData.EnableMotor;
      property MaxMotorTorque: Single read FData.MaxMotorTorque write FData.MaxMotorTorque;
      property Frequency: Single read FData.frequencyHz write FData.frequencyHz;
      property DampingRatio: Single read FData.DampingRatio write FData.DampingRatio;

    end;

  private
    function GetLocalAnchorA: TVector2;
    function GetLocalAnchorB: TVector2;
    function GetLocalAxis: TVector2;
    function GetMaxMotorTorque: Single;
    function GetMotorEnabled: Boolean;
    function GetMotorSpeed: Single;
    function GetSpeed: Single;
    function GetSpringDampingRatio: Single;
    function GetSpringFrequency: Single;
    function GetTranslation: Single;
    procedure SetMaxMotorTorque(const Value: Single);
    procedure SetMotorEnabled(const Value: Boolean);
    procedure SetMotorSpeed(const Value: Single);
    procedure SetSpringDampingRatio(const Value: Single);
    procedure SetSpringFrequency(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LocalAnchorA: TVector2 read GetLocalAnchorA;
    property LocalAnchorB: TVector2 read GetLocalAnchorB;
    property LocalAxis: TVector2 read GetLocalAxis;
    property Translation: Single read GetTranslation;
    property Speed: Single read GetSpeed;
    property MotorEnabled: Boolean read GetMotorEnabled write SetMotorEnabled;
    property MotorSpeed: Single read GetMotorSpeed write SetMotorSpeed;
    property MaxMotorTorque: Single read GetMaxMotorTorque write SetMaxMotorTorque;
    function MotorTorque(AInvDeltaTime: Single): Single;
    property SpringFrequncy: Single read GetSpringFrequency write SetSpringFrequency;
    property SpringDampingRatio: Single read GetSpringDampingRatio write SetSpringDampingRatio;

  end;

  TWeldJoint = class(TJoint<b2WeldJointWrapper>)
  public type

    TDef = class(TDef<b2WeldJointDef>)
    private
      function GetAnchorA: TVector2;
      function GetAnchorB: TVector2;
      procedure SetAnchorA(const Value: TVector2);
      procedure SetAnchorB(const Value: TVector2);

    protected
      function CreateData: b2WeldJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AAnchor: TVector2); reintroduce; overload;

      property AnchorA: TVector2 read GetAnchorA write SetAnchorA;
      property AnchorB: TVector2 read GetAnchorB write SetAnchorB;
      property ReferenceAngle: Single read FData.ReferenceAngle write FData.ReferenceAngle;
      property Frequency: Single read FData.frequencyHz write FData.frequencyHz;
      property DampingRatio: Single read FData.DampingRatio write FData.DampingRatio;

    end;

  private
    function GetDampingRatio: Single;
    function GetFrequency: Single;
    function GetLocalAnchorA: TVector2;
    function GetLocalAnchorB: TVector2;
    function GetReferenceAngle: Single;
    procedure SetDampingRatio(const Value: Single);
    procedure SetFrequency(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LocalAnchorA: TVector2 read GetLocalAnchorA;
    property LocalAnchorB: TVector2 read GetLocalAnchorB;
    property ReferenceAngle: Single read GetReferenceAngle;
    property Frequency: Single read GetFrequency write SetFrequency;
    property DampingRatio: Single read GetDampingRatio write SetDampingRatio;

  end;

  TRopeJoint = class(TJoint<b2RopeJointWrapper>)
  public type

    TDef = class(TDef<b2RopeJointDef>)
    private
      function GetAnchorA: TVector2;
      function GetAnchorB: TVector2;
      procedure SetAnchorA(const Value: TVector2);
      procedure SetAnchorB(const Value: TVector2);

    protected
      function CreateData: b2RopeJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AAnchorA, AAnchorB: TVector2); reintroduce; overload;
      constructor Create(ABodyA, ABodyB: TBody; AAnchorA, AAnchorB: TVector2; ALength: Single); reintroduce; overload;

      property AnchorA: TVector2 read GetAnchorA write SetAnchorA;
      property AnchorB: TVector2 read GetAnchorB write SetAnchorB;
      property MaxLength: Single read FData.MaxLength write FData.MaxLength;

    end;

  private
    function GetLocalAnchorA: TVector2;
    function GetLocalAnchorB: TVector2;
    function GetMaxLength: Single;
    procedure SetMaxLength(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LocalAnchorA: TVector2 read GetLocalAnchorA;
    property LocalAnchorB: TVector2 read GetLocalAnchorB;
    property MaxLength: Single read GetMaxLength write SetMaxLength;

  end;

  TFrictionJoint = class(TJoint<b2FrictionJointWrapper>)
  public type

    TDef = class(TDef<b2FrictionJointDef>)
    private
      function GetAnchorA: TVector2;
      function GetAnchorB: TVector2;
      procedure SetAnchorA(const Value: TVector2);
      procedure SetAnchorB(const Value: TVector2);

    protected
      function CreateData: b2FrictionJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody; AAnchor: TVector2); reintroduce; overload;

      property AnchorA: TVector2 read GetAnchorA write SetAnchorA;
      property AnchorB: TVector2 read GetAnchorB write SetAnchorB;
      property MaxForce: Single read FData.MaxForce write FData.MaxForce;
      property MaxTorque: Single read FData.MaxTorque write FData.MaxTorque;

    end;

  private
    function GetLocalAnchorA: TVector2;
    function GetLocalAnchorB: TVector2;
    function GetMaxForce: Single;
    function GetMaxTorque: Single;
    procedure SetMaxForce(const Value: Single);
    procedure SetMaxTorque(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LocalAnchorA: TVector2 read GetLocalAnchorA;
    property LocalAnchorB: TVector2 read GetLocalAnchorB;
    property MaxForce: Single read GetMaxForce write SetMaxForce;
    property MaxTorque: Single read GetMaxTorque write SetMaxTorque;

  end;

  TMotorJoint = class(TJoint<b2MotorJointWrapper>)
  public type

    TDef = class(TDef<b2MotorJointDef>)
    private
      function GetLinearOffset: TVector2;
      procedure SetLinearOffset(const Value: TVector2);

    protected
      function CreateData: b2MotorJointDef; override;

    public
      constructor Create(ABodyA, ABodyB: TBody); reintroduce; overload;

      property LinearOffset: TVector2 read GetLinearOffset write SetLinearOffset;
      property AngularOffset: Single read FData.AngularOffset write FData.AngularOffset;
      property MaxForce: Single read FData.MaxForce write FData.MaxForce;
      property MaxTorque: Single read FData.MaxTorque write FData.MaxTorque;
      property CorrectionFactor: Single read FData.CorrectionFactor write FData.CorrectionFactor;

    end;

  private
    function GetAngularOffset: Single;
    function GetCorrectionFactor: Single;
    function GetLinearOffset: TVector2;
    function GetMaxForce: Single;
    function GetMaxTorque: Single;
    procedure SetAngularOffset(const Value: Single);
    procedure SetCorrectionFactor(const Value: Single);
    procedure SetLinearOffset(const Value: TVector2);
    procedure SetMaxForce(const Value: Single);
    procedure SetMaxTorque(const Value: Single);

  public
    constructor Create(AWorld: TWorld; ADef: TDef); reintroduce;

    property LinearOffset: TVector2 read GetLinearOffset write SetLinearOffset;
    property Angular: Single read GetAngularOffset write SetAngularOffset;
    property MaxForce: Single read GetMaxForce write SetMaxForce;
    property MaxTorque: Single read GetMaxTorque write SetMaxTorque;
    property CorrectionFactor: Single read GetCorrectionFactor write SetCorrectionFactor;

  end;

  /// <summary>Contains bodies, joints, gravity and some more properties.</summary>
  TWorld = class
  public type

    TListener = class(TInterfacedObject, Ib2ContactListener)
    private
      FWorld: TWorld;

    public
      constructor Create(AWorld: TWorld);

      property World: TWorld read FWorld;

      // Ib2ContactListener
      procedure BeginContact(AContact: b2ContactHandle); cdecl;
      procedure EndContact(AContact: b2ContactHandle); cdecl;
      procedure PreSolve(AContact: b2ContactHandle; AOldManifold: Pb2Manifold); cdecl;
      procedure PostSolve(AContact: b2ContactHandle; AImpulse: Pb2ContactImpulse); cdecl;

    end;

    TIterator = class(TInterfacedObject)
    private
      FWorld: TWorld;

    public
      constructor Create(AWorld: TWorld);

    end;

    TBodyIterator = class(TIterator, IIterator<TBody>)
    private
      FCurrent: TBody;
      FNext: TBody;

      function GetCurrent: TBody;

    public
      constructor Create(AWorld: TWorld);

      function MoveNext: Boolean;

    end;

    IBodies = interface(IIterable<TBody>)
      function GetFirst: TBody;

      property First: TBody read GetFirst;

      function Add(ABodyDef: TBody.TDef): TBody; overload;
      function Add(ABodyType: TBody.TType; APosition: TVector2): TBody; overload;

    end;

    TBodies = class(TInterfacedObject, IBodies, IIterable<TBody>)
    private
      FWorld: TWorld;

      // IBodies
      function GetFirst: TBody;

    public
      constructor Create(AWorld: TWorld);

      // IIterable<TBody>
      function GetEnumerator: IIterator<TBody>;
      function Iterate: IIterate<TBody>;

      // IBodies
      property First: TBody read GetFirst;

      function Add(ABodyDef: TBody.TDef): TBody; overload;
      function Add(ABodyType: TBody.TType; APosition: TVector2): TBody; overload;

    end;

    IJoints = interface(IIterable<TJoint>)
      function GetFirst: TJoint;

      property First: TJoint read GetFirst;

      function Add(ADef: TJoint.TDef): TJoint;

    end;

    TJoints = class(TInterfacedObject, IJoints, IIterable<TJoint>)
    private
      FWorld: TWorld;

      // IJoints
      function GetFirst: TJoint;

    public
      constructor Create(AWorld: TWorld);

      // IIterable<TJoint>
      function GetEnumerator: IIterator<TJoint>;
      function Iterate: IIterate<TJoint>;

      // IJoints
      property First: TJoint read GetFirst;

      function Add(ADef: TJoint.TDef): TJoint;

    end;

    TContact = record
    private
      FWrapper: b2ContactWrapper;

      function GetChainIndexA: Integer;
      function GetChainIndexB: Integer;
      function GetEnabled: Boolean;
      function GetFixtureA: TFixture;
      function GetFixtureB: TFixture;
      function GetFriction: Single;
      function GetRestitution: Single;
      function GetTangentSpeed: Single;
      function GetTouching: Boolean;
      procedure SetEnabled(const Value: Boolean);
      procedure SetFriction(const Value: Single);
      procedure SetRestition(const Value: Single);
      procedure SetTangentSpeed(const Value: Single);
      function GetBodyA: TBody;
      function GetBodyB: TBody;
      function GetUserDataA: TObject;
      function GetUserDataB: TObject;

    public
      constructor Create(AWrapper: b2ContactWrapper);

      property Wrapper: b2ContactWrapper read FWrapper;

      property Touching: Boolean read GetTouching;

      property Enabled: Boolean read GetEnabled write SetEnabled;

      function GetNext: TContact;

      property FixtureA: TFixture read GetFixtureA;
      property ChainIndexA: Integer read GetChainIndexA;
      property BodyA: TBody read GetBodyA;
      property UserDataA: TObject read GetUserDataA;

      property FixtureB: TFixture read GetFixtureB;
      property ChainIndexB: Integer read GetChainIndexB;
      property BodyB: TBody read GetBodyB;
      property UserDataB: TObject read GetUserDataB;

      property Firction: Single read GetFriction write SetFriction;
      procedure ResetFriction;

      property Restitution: Single read GetRestitution write SetRestition;
      procedure ResetRestitution;

      property TangetSpeed: Single read GetTangentSpeed write SetTangentSpeed;

      // TODO: Evaluate
      // TODO: GetManifold
      // TODO: GetWorldManifold

    end;

    TContactIterator = class(TIterator, IIterator<TContact>)
    private
      FCurrent: TContact;
      FNext: TContact;

      function GetCurrent: TContact;

    public
      constructor Create(AWorld: TWorld);

      function MoveNext: Boolean;

    end;

    IContacts = interface(IIterable<TContact>)
      function GetFirst: TContact;

      property First: TContact read GetFirst;

    end;

    TContacts = class(TInterfacedObject, IContacts, IIterable<TContact>)
    private
      FWorld: TWorld;

      // IContacts
      function GetFirst: TContact;

    public
      constructor Create(AWorld: TWorld);

      // IIterable<TContact>
      function GetEnumerator: IIterator<TContact>;
      function Iterate: IIterate<TContact>;

      // IContacts
      property First: TContact read GetFirst;

    end;

    TContactEventType = (
      ceBeginContact,
      ceEndContact,
      cePreSolve,
      cePostSolve
      );

    TEventInfo = TEventInfo<TWorld>;

    TEvent = TEvent<TEventInfo>;

    TContactEventInfo = class(TEventInfo)
    private
      FEventType: TContactEventType;
      FContact: TContact;

    public
      constructor Create(ASender: TWorld; AEventType: TContactEventType; AContact: TContact);

      property EventType: TContactEventType read FEventType;
      property Contact: TContact read FContact;

    end;

    TContactEvent = TEvent<TContactEventInfo>;

  public const

    DefaultVelocityIterations = 20;
    DefaultPositionIterations = 10;

  private
    FWrapper: b2WorldWrapper;
    FVelocityIterations: Integer;
    FPositionIterations: Integer;
    FContactListener: b2ContactListenerWrapper;
    FOnContact: TContactEvent;

    function GetAllowSleeping: Boolean; inline;
    procedure SetAllowSleeping(const Value: Boolean); inline;
    function GetGravity: TVector2; inline;
    procedure SetGravity(const Value: TVector2); inline;

    function GetBodies: IBodies;
    function GetJoints: IJoints;
    function GetContacts: IContacts;

    procedure InitializeListeners;

  public
    constructor Create(AGravity: TVector2; AAllowSleeping: Boolean = True);
    destructor Destroy; override;

    property Gravity: TVector2 read GetGravity write SetGravity;
    property AllowSleeping: Boolean read GetAllowSleeping write SetAllowSleeping;

    property Wrapper: b2WorldWrapper read FWrapper;

    property Bodies: IBodies read GetBodies;
    property Joints: IJoints read GetJoints;
    property Contacts: IContacts read GetContacts;

    property VelocityIterations: Integer read FVelocityIterations write FVelocityIterations;
    property PositionIterations: Integer read FPositionIterations write FPositionIterations;

    procedure Step(ATimeStep: Single);
    procedure StepNoUpdate(ATimeStep: Single);

    procedure UpdateLocations;

    function OnContact: TContactEvent.TAccess;

  end;

const

  JointClasses: array [TJoint.TType] of TJointClass = (
    nil,
    TRevoluteJoint,
    TPrismaticJoint,
    TDistanceJoint,
    TPulleyJoint,
    TMouseJoint,
    TGearJoint,
    TWheelJoint,
    TWeldJoint,
    TFrictionJoint,
    TRopeJoint,
    TMotorJoint
    );

function b2VecConv(const AVector: TVector2): b2Vec2; inline; overload;
function b2VecConv(const AVector: b2Vec2): TVector2; inline; overload;

implementation

function b2VecConv(const AVector: TVector2): b2Vec2;
begin
  Result := b2Vec2(AVector);
end;

function b2VecConv(const AVector: b2Vec2): TVector2;
begin
  Result := TVector2(AVector);
end;

{ TShape }

function TShape.GetRadius: Single;
begin
  Result := Wrapper.m_radius;
end;

procedure TShape.SetRadius(const Value: Single);
begin
  Wrapper.m_radius := Value;
end;

procedure TShape.SetWrapper(AWrapper: b2ShapeHandle);
begin
  FWrapper := AWrapper;
end;

constructor TShape.Create;
begin
  // nothing
end;

{ TShape<W> }

function TShape<W>.GetWrapper: W;
begin
  Result := W(inherited Wrapper);
end;

{ TCircle }

function TCircle.GetPosition: TVector2;
begin
  Result := b2VecConv(Wrapper.m_p);
end;

procedure TCircle.SetPosition(const Value: TVector2);
begin
  Wrapper.m_p := b2VecConv(Value);
end;

constructor TCircle.Create;
begin
  inherited;
  SetWrapper(b2CircleShapeWrapper.Create);
end;

constructor TCircle.Create(ARadius: Single);
begin
  Create;
  Radius := ARadius;
end;

constructor TCircle.Create(ARadius: Single; APosition: TVector2);
begin
  Create(ARadius);
  Position := APosition;
end;

destructor TCircle.Destroy;
begin
  Wrapper.Destroy;
  inherited;
end;

{ TPolygon.TIterator<T> }

constructor TPolygon.TIterator<T>.Create(APolygon: TPolygon);
begin
  FPolygon := APolygon;
  FCurrent := -1;
end;

function TPolygon.TIterator<T>.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent <> FPolygon.Points.Count;
end;

{ TPolygon.TPointIterator }

function TPolygon.TPointIterator.GetCurrent: TVector2;
begin
  Result := FPolygon.Points[FCurrent];
end;

{ TPolygon.TPoints }

function TPolygon.TPoints.GetCount: Integer;
begin
  Result := FPolygon.Wrapper.GetVertexCount;
end;

function TPolygon.TPoints.GetItem(AIndex: Integer): TVector2;
begin
  Result := b2VecConv(FPolygon.Wrapper.GetVertex(AIndex)^);
end;

function TPolygon.TPoints.GetMaxIndex: Integer;
begin
  Result := Count - 1;
end;

function TPolygon.TPoints.GetFirst: TVector2;
begin
  Result := Items[0];
end;

function TPolygon.TPoints.GetLast: TVector2;
begin
  Result := Items[MaxIndex];
end;

constructor TPolygon.TPoints.Create(APolygon: TPolygon);
begin
  FPolygon := APolygon;
end;

function TPolygon.TPoints.GetEnumerator: IIterator<TVector2>;
begin
  Result := TPointIterator.Create(FPolygon);
end;

function TPolygon.TPoints.Iterate: IIterate<TVector2>;
begin
  Result := TIterableIterate<TVector2>.Create(Self);
end;

function TPolygon.TPoints.Empty: Boolean;
begin
  Result := Count = 0;
end;

function TPolygon.TPoints.Contains(AItem: TVector2): Boolean;
begin
  Result := IndexOf(AItem) <> -1;
end;

function TPolygon.TPoints.IndexOf(AItem: TVector2): Integer;
begin
  for Result := 0 to MaxIndex do
    if Items[Result] = AItem then
      Exit;
  Result := -1;
end;

function TPolygon.TPoints.Reverse: IIterate<TVector2>;
begin
  raise ENotImplemented.Create('TPolygon.TPoints.Reverse');
end;

{ TPolygon }

function TPolygon.GetPoints: IPoints;
begin
  Result := TPoints.Create(Self);
end;

function TPolygon.GetSkinThickness: Single;
begin
  Result := Radius;
end;

procedure TPolygon.SetSkinThickness(const Value: Single);
begin
  Radius := Value;
end;

constructor TPolygon.Create;
begin
  inherited;
  SetWrapper(b2PolygonShapeWrapper.Create);
end;

destructor TPolygon.Destroy;
begin
  Wrapper.Destroy;
  inherited;
end;

procedure TPolygon.SetPoints(APoints: IList<TVector2>);
begin
  Wrapper.&Set(APoints.DataPointer, APoints.Count);
end;

procedure TPolygon.SetPoints(APoints: TArray<TVector2>);
begin
  Wrapper.&Set(@APoints[0], Length(APoints));
end;

procedure TPolygon.SetPoints(APoints: PVector2; ACount: Integer);
begin
  Wrapper.&Set(Pointer(APoints), ACount);
end;

procedure TPolygon.SetBox(ASize: TVector2);
begin
  Wrapper.SetAsBox(ASize.x, ASize.y);
end;

procedure TPolygon.SetBox(ASize, ACenter: TVector2; AAngleRad: Single);
begin
  Wrapper.SetAsBox(ASize.x, ASize.y, b2VecConv(ACenter), AAngleRad);
end;

function TPolygon.ValidateConvexity: Boolean;
begin
   Result := Wrapper.Validate;
end;

{ TEdge }

function TEdge.GetLine: TLine2;
begin
  Result := Vertex1.LineTo(Vertex2);
end;

procedure TEdge.SetLine(const Value: TLine2);
begin
  Vertex1 := Value.Tail;
  Vertex2 := Value.Head;
end;

function TEdge.GetVertex1: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex1);
end;

procedure TEdge.SetVertex1(const Value: TVector2);
begin
  Wrapper.m_vertex1 := b2VecConv(Value);
end;

function TEdge.GetVertex2: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex2);
end;

procedure TEdge.SetVertex2(const Value: TVector2);
begin
  Wrapper.m_vertex2 := b2VecConv(Value);
end;

function TEdge.GetPrev: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex0);
end;

procedure TEdge.SetPrev(const Value: TVector2);
begin
  Wrapper.m_vertex0 := b2VecConv(Value);
end;

function TEdge.GetNext: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex3);
end;

procedure TEdge.SetNext(const Value: TVector2);
begin
  Wrapper.m_vertex3 := b2VecConv(Value);
end;

constructor TEdge.Create;
begin
  inherited;
  SetWrapper(b2EdgeShapeWrapper.Create);
end;

destructor TEdge.Destroy;
begin
  Wrapper.Destroy;
  inherited;
end;

function TEdge.HasPrev: Boolean;
begin
  Result := Wrapper.m_hasVertex0;
end;

function TEdge.HasNext: Boolean;
begin
  Result := Wrapper.m_hasVertex3;
end;

{ TChain.TIterator<T> }

constructor TChain.TIterator<T>.Create(AChain: TChain);
begin
  FChain := AChain;
end;

function TChain.TIterator<T>.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent <> FChain.Points.Count;
end;

{ TChain.TPointIterator }

function TChain.TPointIterator.GetCurrent: TVector2;
begin
  Result := FChain.Points[FCurrent];
end;

{ TChain.TPoints }

function TChain.TPoints.GetCount: Integer;
begin
  Result := FChain.Wrapper.GetChildCount;
end;

function TChain.TPoints.GetItem(AIndex: Integer): TVector2;
var
  Edge: b2EdgeShapeWrapper;
begin
  FChain.Wrapper.GetChildEdge(NativeUInt(@Edge), AIndex);
  Result := b2VecConv(Edge.m_vertex1);
end;

function TChain.TPoints.GetMaxIndex: Integer;
begin
  Result := Count - 1;
end;

function TChain.TPoints.GetFirst: TVector2;
begin
  Result := Items[0];
end;

function TChain.TPoints.GetLast: TVector2;
begin
  Result := Items[MaxIndex];
end;

constructor TChain.TPoints.Create(AChain: TChain);
begin
  FChain := AChain;
end;

function TChain.TPoints.GetEnumerator: IIterator<TVector2>;
begin
  Result := TPointIterator.Create(FChain);
end;

function TChain.TPoints.Iterate: IIterate<TVector2>;
begin
  Result := TIterableIterate<TVector2>.Create(Self);
end;

function TChain.TPoints.Empty: Boolean;
begin
  Result := Count = 0;
end;

function TChain.TPoints.Contains(AItem: TVector2): Boolean;
begin
  Result := IndexOf(AItem) <> -1;
end;

function TChain.TPoints.IndexOf(AItem: TVector2): Integer;
begin
  for Result := 0 to MaxIndex do
    if Items[Result] = AItem then
      Exit;
  Result := -1;
end;

function TChain.TPoints.Reverse: IIterate<TVector2>;
begin
  raise ENotImplemented.Create('TChain.TPoints.Reverse');
end;

{ TChain }

function TChain.GetPoints: IPoints;
begin
  Result := TPoints.Create(Self);
end;

function TChain.GetNext: TVector2;
begin
  Result := b2VecConv(Wrapper.m_nextVertex);
end;

function TChain.GetPrev: TVector2;
begin
  Result := b2VecConv(Wrapper.m_prevVertex);
end;

procedure TChain.SetNext(const Value: TVector2);
begin
  Wrapper.m_nextVertex := b2VecConv(Value);
end;

procedure TChain.SetPrev(const Value: TVector2);
begin
  Wrapper.m_prevVertex := b2VecConv(Value);
end;

constructor TChain.Create;
begin
  inherited;
  SetWrapper(b2ChainShapeWrapper.Create);
end;

destructor TChain.Destroy;
begin
  Wrapper.Destroy;
  inherited;
end;

function TChain.HasPrev: Boolean;
begin
  Result := Wrapper.m_hasPrevVertex;
end;

function TChain.HasNext: Boolean;
begin
  Result := Wrapper.m_hasNextVertex;
end;

procedure TChain.SetChain(APoints: IList<TVector2>);
begin
  Wrapper.CreateChain(APoints.DataPointer, APoints.Count);
end;

procedure TChain.SetChain(APoints: TArray<TVector2>);
begin
  Wrapper.CreateChain(@APoints[0], Length(APoints));
end;

procedure TChain.SetChain(APoints: PVector2; ACount: Integer);
begin
  Wrapper.CreateChain(Pointer(APoints), ACount);
end;

procedure TChain.SetLoop(APoints: IList<TVector2>);
begin
  Wrapper.CreateLoop(APoints.DataPointer, APoints.Count);
end;

procedure TChain.SetLoop(APoints: TArray<TVector2>);
begin
  Wrapper.CreateLoop(@APoints[0], Length(APoints));
end;

procedure TChain.SetLoop(APoints: PVector2; ACount: Integer);
begin
  Wrapper.CreateLoop(Pointer(APoints), ACount);
end;

{ TFixture.TDef }

function TFixture.TDef.GetData: Pb2FixtureDef;
begin
  Result := @FData;
end;

constructor TFixture.TDef.Create(AShape: TShape);
begin
  inherited Create;
  FData := b2FixtureDef.Create;
  FData.Shape := AShape.Wrapper;
end;

{ TFixture }

function TFixture.GetBody: TBody;
var
  B: b2BodyWrapper;
begin
  B := Wrapper.GetBody;
  if B.FHandle = 0 then
    Exit(nil);
  Result := TBody(B.GetUserData);
end;

function TFixture.GetDenstity: Single;
begin
  Result := Wrapper.GetDensity;
end;

procedure TFixture.SetDensity(const Value: Single);
begin
  Wrapper.SetDensity(Value);
end;

function TFixture.GetFriction: Single;
begin
  Result := Wrapper.GetFriction;
end;

procedure TFixture.SetFriction(const Value: Single);
begin
  Wrapper.SetFriction(Value);
end;

function TFixture.GetRestitution: Single;
begin
  Result := Wrapper.GetRestitution;
end;

procedure TFixture.SetRestitution(const Value: Single);
begin
  Wrapper.SetRestitution(Value);
end;

constructor TFixture.Create(ABody: TBody; ADef: TFixture.TDef);
begin
  ADef.FData.UserData := Self;
  FWrapper := ABody.Wrapper.CreateFixture(ADef.Data)^;
end;

destructor TFixture.Destroy;
begin
  if Wrapper.GetUserData <> nil then
    Body.Wrapper.DestroyFixture(@Wrapper);
  inherited;
end;

function TFixture.GetNext: TFixture;
var
  Next: Pb2Fixture;
begin
  Next := Wrapper.GetNext;
  if Next = nil then
    Exit(nil);
  Result := TFixture(Next.GetUserData);
end;

{ TBody.TDef }

function TBody.TDef.GetData: Pb2BodyDef;
begin
  Result := @FData;
end;

function TBody.TDef.GetType: TType;
begin
  Result := TType(FData.&Type);
end;

procedure TBody.TDef.SetType(const Value: TType);
begin
  FData.&Type := b2BodyType(Value);
end;

function TBody.TDef.GetPosition: TVector2;
begin
  Result := b2VecConv(FData.Position);
end;

procedure TBody.TDef.SetPosition(const Value: TVector2);
begin
  FData.Position := b2VecConv(Value);
end;

function TBody.TDef.GetRotation: Single;
begin
  Result := RadToDeg(RotationRad);
end;

procedure TBody.TDef.SetRotation(const Value: Single);
begin
  RotationRad := DegToRad(Value);
end;

function TBody.TDef.GetLinearVelocity: TVector2;
begin
  Result := b2VecConv(FData.LinearVelocity);
end;

procedure TBody.TDef.SetLinearVelocity(const Value: TVector2);
begin
  FData.LinearVelocity := b2VecConv(Value);
end;

constructor TBody.TDef.Create(AType: TType; APosition: TVector2);
begin
  inherited Create;
  FData := b2BodyDef.Create;
  &Type := AType;
  Position := APosition;
end;

{ TBody.TFixturesIterator }

function TBody.TFixtureIterator.GetCurrent: TFixture;
begin
  Result := FCurrent;
end;

constructor TBody.TFixtureIterator.Create(ABody: TBody);
begin
  FNext := ABody.Fixtures.First;
end;

function TBody.TFixtureIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FNext <> nil;
  if Result then
    FNext := FNext.GetNext;
end;

{ TBody.TFixtures }

function TBody.TFixtures.GetFirst: TFixture;
begin
  Result := TFixture(FBody.Wrapper.GetFixtureList.GetUserData);
end;

constructor TBody.TFixtures.Create(ABody: TBody);
begin
  FBody := ABody;
end;

function TBody.TFixtures.GetEnumerator: IIterator<TFixture>;
begin
  Result := TFixtureIterator.Create(FBody);
end;

function TBody.TFixtures.Iterate: IIterate<TFixture>;
begin
  Result := TIterableIterate<TFixture>.Create(Self);
end;

function TBody.TFixtures.Add(AFixtureDef: TFixture.TDef): TFixture;
begin
  Result := TFixture.Create(FBody, AFixtureDef);
end;

function TBody.TFixtures.Add(AShape: TShape): TFixture;
var
  FixtureDef: TFixture.TDef;
begin
  FixtureDef := TFixture.TDef.Create(AShape);
  Result := TFixture.Create(FBody, FixtureDef);
  FixtureDef.Free;
end;

{ TBody.TJointEdge }

function TBody.TJointEdge.GetJoint: TJoint;
begin
  Result := TJoint(b2JointWrapper(FData.Joint).GetUserData);
end;

function TBody.TJointEdge.GetOther: TBody;
begin
  Result := TBody(b2BodyWrapper(FData.Other).GetUserData);
end;

constructor TBody.TJointEdge.Create(AJointEdge: Pb2JointEdge);
begin
  FData := AJointEdge;
end;

function TBody.TJointEdge.Next: TJointEdge;
begin
  Result.Create(FData.Next);
end;

function TBody.TJointEdge.HasNext: Boolean;
begin
  Result := FData.Next <> nil;
end;

function TBody.TJointEdge.Prev: TJointEdge;
begin
  Result.Create(FData.Prev);
end;

function TBody.TJointEdge.HasPrev: Boolean;
begin
  Result := FData.Prev <> nil;
end;

{ TBody.TJointEdgeIterator }

function TBody.TJointEdgeIterator.GetCurrent: TJointEdge;
begin
  Result := FCurrent;
end;

constructor TBody.TJointEdgeIterator.Create(ABody: TBody);
begin
  FNext := ABody.JointEdges.First;
end;

function TBody.TJointEdgeIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FCurrent.HasNext;
  if Result then
    FNext := FCurrent.Next;
end;

{ TBody.TJointEdges }

function TBody.TJointEdges.GetFirst: TJointEdge;
begin
  Result.Create(FBody.Wrapper.GetJointList);
end;

constructor TBody.TJointEdges.Create(ABody: TBody);
begin
  FBody := ABody;
end;

function TBody.TJointEdges.GetEnumerator: IIterator<TJointEdge>;
begin
  Result := TJointEdgeIterator.Create(FBody);
end;

function TBody.TJointEdges.Iterate: IIterate<TJointEdge>;
begin
  Result := TIterableIterate<TJointEdge>.Create(Self);
end;

{ TBody }

function TBody.GetType: TType;
begin
  Result := TType(Wrapper.GetType);
end;

procedure TBody.SetType(const Value: TType);
begin
  Wrapper.SetType(b2BodyType(Value));
end;

function TBody.GetFixtures: IFixtures;
begin
  Result := TFixtures.Create(Self);
end;

function TBody.GetJointEdges: IJointEdges;
begin
  Result := TJointEdges.Create(Self);
end;

function TBody.GetPosition: TVector2;
begin
  Result := b2VecConv(Wrapper.GetPosition^);
end;

procedure TBody.SetPosition(const Value: TVector2);
begin
  Wrapper.SetTransform(b2VecConv(Value), RotationRad);
end;

function TBody.GetRotationRad: Single;
begin
  Result := Wrapper.GetAngle;
end;

procedure TBody.SetRotationRad(const Value: Single);
begin
  Wrapper.SetTransform(b2VecConv(Position), Value);
end;

function TBody.GetRotation: Single;
begin
  Result := RadToDeg(RotationRad);
end;

procedure TBody.SetRotation(const Value: Single);
begin
  RotationRad := DegToRad(Value);
end;

function TBody.GetWorldCenter: TVector2;
begin
  Result := b2VecConv(Wrapper.GetWorldCenter^);
end;

function TBody.GetLocalCenter: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalCenter^);
end;

function TBody.GetLinearVelocity: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLinearVelocity^);
end;

procedure TBody.SetLinearVelocity(const Value: TVector2);
begin
  Wrapper.SetLinearVelocity(b2VecConv(Value));
end;

function TBody.GetLinearVelocityAtWorldPoint(APoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLinearVelocityFromWorldPoint(b2VecConv(APoint)));
end;

function TBody.GetLinearVelocityAtLocalPoint(APoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLinearVelocityFromLocalPoint(b2VecConv(APoint)));
end;

function TBody.GetAngularVelocity: Single;
begin
  Result := Wrapper.GetAngularVelocity;
end;

procedure TBody.SetAngularVelocity(const Value: Single);
begin
  Wrapper.SetAngularVelocity(Value);
end;

function TBody.GetLinearDamping: Single;
begin
  Result := Wrapper.GetLinearDamping;
end;

procedure TBody.SetLinearDamping(const Value: Single);
begin
  Wrapper.SetLinearDamping(Value);
end;

function TBody.GetAngularDamping: Single;
begin
  Result := Wrapper.GetAngularDamping;
end;

procedure TBody.SetAngularDamping(const Value: Single);
begin
  Wrapper.SetAngularDamping(Value);
end;

function TBody.GetAwake: Boolean;
begin
  Result := Wrapper.IsAwake;
end;

procedure TBody.SetAwake(const Value: Boolean);
begin
  Wrapper.SetAwake(Value);
end;

function TBody.GetSleepingAllowed: Boolean;
begin
  Result := Wrapper.IsSleepingAllowed;
end;

procedure TBody.SetSleepingAllowed(const Value: Boolean);
begin
  Wrapper.SetSleepingAllowed(Value);
end;

function TBody.GetActive: Boolean;
begin
  Result := Wrapper.IsActive;
end;

procedure TBody.SetActive(const Value: Boolean);
begin
  Wrapper.SetActive(Value);
end;

function TBody.GetFixedRotation: Boolean;
begin
  Result := Wrapper.IsFixedRotation;
end;

procedure TBody.SetFixedRotation(const Value: Boolean);
begin
  Wrapper.SetFixedRotation(Value);
end;

function TBody.GetBullet: Boolean;
begin
  Result := Wrapper.IsBullet;
end;

procedure TBody.SetBullet(const Value: Boolean);
begin
  Wrapper.SetBullet(Value);
end;

function TBody.GetGravityScale: Single;
begin
  Result := Wrapper.GetGravityScale;
end;

procedure TBody.SetGravityScale(const Value: Single);
begin
  Wrapper.SetGravityScale(Value);
end;

function TBody.GetMass: Single;
begin
  Result := Wrapper.GetMass;
end;

function TBody.GetInertia: Single;
begin
  Result := Wrapper.GetInertia;
end;

function TBody.GetWorldPoint(ALocalPoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetWorldPoint(b2VecConv(ALocalPoint)));
end;

function TBody.GetWorldVector(ALocalVector: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetWorldVector(b2VecConv(ALocalVector)));
end;

function TBody.GetLocalPoint(AWorldPoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalPoint(b2VecConv(AWorldPoint)));
end;

function TBody.GetLocalVector(AWorldVector: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalVector(b2VecConv(AWorldVector)));
end;

constructor TBody.Create(AWorld: TWorld; ADef: TDef);
begin
  FWorld := AWorld;
  ADef.Data.UserData := Self;
  FWrapper := World.Wrapper.CreateBody(ADef.Data);
  FLocations := TList<TLocation2>.Create;
end;

destructor TBody.Destroy;
var
  Fixture: TFixture;
  JointEdge: TJointEdge;
begin
  for JointEdge in JointEdges do
  begin
    JointEdge.Joint.Wrapper.SetUserData(nil);
    JointEdge.Joint.Free;
  end;
  for Fixture in Fixtures do
  begin
    Fixture.Wrapper.SetUserData(nil);
    Fixture.Free;
  end;
  if World <> nil then
    World.Wrapper.DestroyBody(Wrapper);
  inherited;
end;

function TBody.GetNext: TBody;
var
  Next: b2BodyWrapper;
begin
  Next := Wrapper.GetNext;
  if Next.FHandle = 0 then
    Exit(nil);
  Result := TBody(Next.GetUserData);
end;

procedure TBody.ApplyForce(AForce, APoint: TVector2; Awake: Boolean);
begin
  Wrapper.ApplyForce(b2VecConv(AForce), b2VecConv(APoint), Awake);
end;

procedure TBody.ApplyForceToCenter(AForce: TVector2; Awake: Boolean);
begin
  Wrapper.ApplyForceToCenter(b2VecConv(AForce), Awake);
end;

procedure TBody.ApplyTorque(ATorque: Single; Awake: Boolean);
begin
  Wrapper.ApplyTorque(ATorque, Awake);
end;

procedure TBody.ApplyLinearImpulse(AImpulse, APoint: TVector2; Awake: Boolean);
begin
  Wrapper.ApplyLinearImpulse(b2VecConv(AImpulse), b2VecConv(APoint), Awake);
end;

procedure TBody.ApplyAngularImpulse(AImpulse: Single; Awake: Boolean);
begin
  Wrapper.ApplyAngularImpulse(AImpulse, Awake);
end;

procedure TBody.AddLocation(ALocation: TLocation2);
begin
  FLocations.Add(ALocation);
end;

procedure TBody.RemoveLocation(ALocation: TLocation2);
begin
  FLocations.Remove(ALocation);
end;

function TBody.OnLocationChanged: TEvent.TAccess;
begin
  Result := FOnLocationChanged.Access;
end;

procedure TBody.UpdateLocations;
var
  Location: TLocation2;
begin
  for Location in FLocations do
  begin
    Location.Pos := Position;
    Location.Rotation := Rotation;
  end;
  if FOnLocationChanged.HasHandler then
    FOnLocationChanged.Execute(TEventInfo.Create(Self));
end;

{ TJoint.TDef }

function TJoint.TDef.GetBodyA: TBody;
var
  Body: b2BodyWrapper;
begin
  Body := Data.BodyA;
  if Body.FHandle = 0 then
    Exit(nil);
  Result := TBody(Body.GetUserData);
end;

procedure TJoint.TDef.SetBodyA(const Value: TBody);
begin
  Data.BodyA := Value.Wrapper;
end;

function TJoint.TDef.GetBodyB: TBody;
var
  Body: b2BodyWrapper;
begin
  Body := Data.BodyB;
  if Body.FHandle = 0 then
    Exit(nil);
  Result := TBody(Body.GetUserData);
end;

procedure TJoint.TDef.SetBodyB(const Value: TBody);
begin
  Data.BodyB := Value.Wrapper;
end;

function TJoint.TDef.GetCollideConnected: Boolean;
begin
  Result := Data.CollideConnected;
end;

procedure TJoint.TDef.SetCollideConnected(const Value: Boolean);
begin
  Data.CollideConnected := Value;
end;

constructor TJoint.TDef.Create;
begin
  inherited;
end;

function TJoint.TDef.GetType: TType;
begin
  Result := TType(Data.&Type);
end;

{ TJoint.TDef<T> }

function TJoint.TDef<T>.GetData: Pb2JointDef;
begin
  Result := @FData;
end;

function TJoint.TDef<T>.GetDataTyped: PT;
begin
  Result := PT(GetData);
end;

constructor TJoint.TDef<T>.Create;
begin
  inherited;
  FData := CreateData;
end;

{ TJoint.TIterator }

function TJoint.TIterator.GetCurrent: TJoint;
begin
  Result := FCurrent;
end;

constructor TJoint.TIterator.Create(AFirst: TJoint);
begin
  FNext := AFirst;
end;

function TJoint.TIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FCurrent <> nil;
  if Result then
    FNext := FNext.GetNext;
end;

{ TJoint }

function TJoint.GetBodyA: TBody;
begin
  Result := TBody(b2BodyWrapper(Wrapper.GetBodyA()).GetUserData);
end;

function TJoint.GetBodyB: TBody;
begin
  Result := TBody(b2BodyWrapper(Wrapper.GetBodyB()).GetUserData);
end;

function TJoint.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetAnchorA);
end;

function TJoint.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetAnchorB);
end;

function TJoint.GetCollideConnected: Boolean;
begin
  Result := Wrapper.GetCollideConnected;
end;

function TJoint.GetActive: Boolean;
begin
  Result := Wrapper.IsActive;
end;

constructor TJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  ADef.Data.UserData := Self;
  FWrapper := AWorld.Wrapper.CreateJoint(ADef.Data);;
  Assert(GetType = ADef.GetType, 'JointDef-Type and Object-Type must match.');
  FWorld := AWorld;
end;

destructor TJoint.Destroy;
begin
  if World <> nil then
    World.Wrapper.DestroyJoint(Wrapper);
  inherited;
end;

function TJoint.GetType: TType;
begin
  Result := TType(Wrapper.GetType);
end;

function TJoint.ReactionForce(AInvDeltaTime: Single): TVector2;
begin
  Result := b2VecConv(Wrapper.GetReactionForce(AInvDeltaTime));
end;

function TJoint.ReactionTorque(AInvDeltaTime: Single): Single;
begin
  Result := Wrapper.GetReactionTorque(AInvDeltaTime);
end;

procedure TJoint.ShiftOrigin(ANewOrigin: TVector2);
begin
  Wrapper.ShiftOrigin(b2VecConv(ANewOrigin));
end;

function TJoint.GetNext: TJoint;
var
  Joint: b2JointWrapper;
begin
  Joint := Wrapper.GetNext;
  if Joint.FHandle = 0 then
    Exit(nil);
  Result := TJoint(Joint.GetUserData);
end;

{ TJoint<T> }

function TJoint<T>.GetWrapper: T;
begin
  Result := T(inherited Wrapper);
end;

{ TDistanceJoint.TDef }

function TDistanceJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(FData.LocalAnchorA);
end;

function TDistanceJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(FData.LocalAnchorB);
end;

procedure TDistanceJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  FData.LocalAnchorA := b2VecConv(Value);
end;

procedure TDistanceJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  FData.LocalAnchorB := b2VecConv(Value);
end;

function TDistanceJoint.TDef.CreateData: b2DistanceJointDef;
begin
  Result := b2DistanceJointDef.Create;
end;

constructor TDistanceJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchorA, AAnchorB: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchorA), b2VecConv(AAnchorB));
end;

{ TDistanceJoint }

function TDistanceJoint.GetLength: Single;
begin
  Result := Wrapper.GetLength;
end;

procedure TDistanceJoint.SetLength(const Value: Single);
begin
  Wrapper.SetLength(Value);
end;

function TDistanceJoint.GetFrequency: Single;
begin
  Result := Wrapper.GetFrequency;
end;

procedure TDistanceJoint.SetFrequency(const Value: Single);
begin
  Wrapper.SetFrequency(Value);
end;

function TDistanceJoint.GetDampingRatio: Single;
begin
  Result := Wrapper.GetDampingRatio;
end;

procedure TDistanceJoint.SetDampingRatio(const Value: Single);
begin
  Wrapper.SetDampingRatio(Value);
end;

function TDistanceJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TDistanceJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

constructor TDistanceJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TRevoluteJoint.TDef }

function TRevoluteJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorA);
end;

procedure TRevoluteJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  Data.LocalAnchorA := b2VecConv(Value);
end;

function TRevoluteJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorB);
end;

procedure TRevoluteJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  Data.LocalAnchorB := b2VecConv(Value);
end;

function TRevoluteJoint.TDef.GetLimitsRad: TBounds1;
begin
  Result.Create(LowerAngleRad, UpperAngleRad);
end;

procedure TRevoluteJoint.TDef.SetLimitsRad(const Value: TBounds1);
begin
  LowerAngleRad := Value.C1;
  UpperAngleRad := Value.C2;
end;

function TRevoluteJoint.TDef.GetLimits: TBounds1;
begin
  Result.Create(LowerAngle, UpperAngle);
end;

procedure TRevoluteJoint.TDef.SetLimits(const Value: TBounds1);
begin
  LowerAngle := Value.C1;
  UpperAngle := Value.C2;
end;

function TRevoluteJoint.TDef.GetLowerAngle: Single;
begin
  Result := RadToDeg(LowerAngleRad);
end;

function TRevoluteJoint.TDef.GetUpperAngle: Single;
begin
  Result := RadToDeg(UpperAngleRad);
end;

procedure TRevoluteJoint.TDef.SetLowerAngle(const Value: Single);
begin
  LowerAngleRad := DegToRad(Value);
end;

procedure TRevoluteJoint.TDef.SetUpperAngle(const Value: Single);
begin
  UpperAngleRad := DegToRad(Value);
end;

function TRevoluteJoint.TDef.CreateData: b2RevoluteJointDef;
begin
  Result := b2RevoluteJointDef.Create;
end;

constructor TRevoluteJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor));
end;

{ TRevoluteJoint }

constructor TRevoluteJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

function TRevoluteJoint.GetEnableLimit: Boolean;
begin
  Result := Wrapper.IsLimitEnabled;
end;

function TRevoluteJoint.GetEnableMotor: Boolean;
begin
  Result := Wrapper.IsMotorEnabled;
end;

function TRevoluteJoint.GetLimits: TBounds1;
begin
  Result.Create(LowerLimit, UpperLimit);
end;

function TRevoluteJoint.GetLimitsRad: TBounds1;
begin
  Result.Create(Wrapper.GetLowerLimit, Wrapper.GetUpperLimit);
end;

function TRevoluteJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TRevoluteJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

function TRevoluteJoint.GetLowerLimit: Single;
begin
  Result := RadToDeg(LowerLimitRad);
end;

function TRevoluteJoint.GetLowerLimitRad: Single;
begin
  Result := Wrapper.GetLowerLimit;
end;

function TRevoluteJoint.GetMaxMotorTorque: Single;
begin
  Result := Wrapper.GetMaxMotorTorque;
end;

function TRevoluteJoint.GetMotorSpeed: Single;
begin
  Result := Wrapper.GetMotorSpeed;
end;

function TRevoluteJoint.GetReferenceAngle: Single;
begin
  Result := Wrapper.GetReferenceAngle;
end;

function TRevoluteJoint.GetUpperLimit: Single;
begin
  Result := RadToDeg(UpperLimitRad);
end;

function TRevoluteJoint.GetUpperLimitRad: Single;
begin
  Result := Wrapper.GetUpperLimit;
end;

procedure TRevoluteJoint.SetEnableLimit(const Value: Boolean);
begin
  Wrapper.EnableLimit(Value);
end;

procedure TRevoluteJoint.SetEnableMotor(const Value: Boolean);
begin
  Wrapper.EnableMotor(Value);
end;

procedure TRevoluteJoint.SetLimits(const Value: TBounds1);
begin
  Wrapper.SetLimits(DegToRad(Value.C1), DegToRad(Value.C2));
end;

procedure TRevoluteJoint.SetLimitsRad(const Value: TBounds1);
begin
  Wrapper.SetLimits(Value.C1, Value.C2);
end;

procedure TRevoluteJoint.SetLowerLimit(const Value: Single);
begin
  LowerLimitRad := DegToRad(Value);
end;

procedure TRevoluteJoint.SetLowerLimitRad(const Value: Single);
begin
  Wrapper.SetLimits(Value, UpperLimitRad);
end;

procedure TRevoluteJoint.SetMaxMotorTorque(const Value: Single);
begin
  Wrapper.SetMaxMotorTorque(Value);
end;

procedure TRevoluteJoint.SetMotorSpeed(const Value: Single);
begin
  Wrapper.SetMotorSpeed(Value);
end;

procedure TRevoluteJoint.SetUpperLimit(const Value: Single);
begin
  UpperLimit := DegToRad(Value);
end;

procedure TRevoluteJoint.SetUpperLimitRad(const Value: Single);
begin
  Wrapper.SetLimits(LowerLimitRad, Value);
end;

{ TPrismaticJoint.TDef }

function TPrismaticJoint.TDef.GetAxis: TVector2;
begin
  Result := b2VecConv(Data.localAxisA);
end;

procedure TPrismaticJoint.TDef.SetAxis(const Value: TVector2);
begin
  Data.localAxisA := b2VecConv(Value);
end;

function TPrismaticJoint.TDef.GetLimits: TBounds1;
begin
  Result.Create(LowerTranslation, UpperTranslation);
end;

procedure TPrismaticJoint.TDef.SetLimits(const Value: TBounds1);
begin
  LowerTranslation := Value.C1;
  UpperTranslation := Value.C2;
end;

function TPrismaticJoint.TDef.CreateData: b2PrismaticJointDef;
begin
  Result := b2PrismaticJointDef.Create;
end;

constructor TPrismaticJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor, AAxis: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor), b2VecConv(AAxis));
end;

{ TPrismaticJoint }

function TPrismaticJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TPrismaticJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

function TPrismaticJoint.GetAxis: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAxisA^);
end;

function TPrismaticJoint.GetReferenceAngle: Single;
begin
  Result := Wrapper.GetReferenceAngle;
end;

function TPrismaticJoint.GetEnableLimit: Boolean;
begin
  Result := Wrapper.IsLimitEnabled;
end;

procedure TPrismaticJoint.SetEnableLimit(const Value: Boolean);
begin
  Wrapper.EnableLimit(Value);
end;

function TPrismaticJoint.GetLimits: TBounds1;
begin
  Result.Create(LowerLimit, UpperLimit);
end;

procedure TPrismaticJoint.SetLimits(const Value: TBounds1);
begin
  Wrapper.SetLimits(Value.C1, Value.C2);
end;

function TPrismaticJoint.GetLowerLimit: Single;
begin
  Result := Wrapper.GetLowerLimit;
end;

procedure TPrismaticJoint.SetLowerLimit(const Value: Single);
begin
  Wrapper.SetLimits(Value, UpperLimit);
end;

function TPrismaticJoint.GetUpperLimit: Single;
begin
  Result := Wrapper.GetUpperLimit;
end;

procedure TPrismaticJoint.SetUpperLimit(const Value: Single);
begin
  Wrapper.SetLimits(LowerLimit, Value);
end;

function TPrismaticJoint.GetEnableMotor: Boolean;
begin
  Result := Wrapper.IsMotorEnabled;
end;

procedure TPrismaticJoint.SetEnableMotor(const Value: Boolean);
begin
  Wrapper.EnableMotor(Value);
end;

function TPrismaticJoint.GetMotorSpeed: Single;
begin
  Result := Wrapper.GetMotorSpeed;
end;

procedure TPrismaticJoint.SetMotorSpeed(const Value: Single);
begin
  Wrapper.SetMotorSpeed(Value);
end;

function TPrismaticJoint.GetMaxMotorForce: Single;
begin
  Result := Wrapper.GetMaxMotorForce;
end;

procedure TPrismaticJoint.SetMaxMotorForce(const Value: Single);
begin
  Wrapper.SetMaxMotorForce(Value);
end;

constructor TPrismaticJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TPulleyJoint.TDef }

function TPulleyJoint.TDef.GetGroundAnchorA: TVector2;
begin
  Result := b2VecConv(Data.GroundAnchorA);
end;

procedure TPulleyJoint.TDef.SetGroundAnchorA(const Value: TVector2);
begin
  Data.GroundAnchorA := b2VecConv(Value);
end;

function TPulleyJoint.TDef.GetGroundAnchorB: TVector2;
begin
  Result := b2VecConv(Data.GroundAnchorB);
end;

procedure TPulleyJoint.TDef.SetGroundAnchorB(const Value: TVector2);
begin
  Data.GroundAnchorB := b2VecConv(Value);
end;

function TPulleyJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorA);
end;

procedure TPulleyJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  Data.LocalAnchorA := b2VecConv(Value);
end;

function TPulleyJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorB);
end;

procedure TPulleyJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  Data.LocalAnchorB := b2VecConv(Value);
end;

function TPulleyJoint.TDef.CreateData: b2PulleyJointDef;
begin
  Result := b2PulleyJointDef.Create;
end;

constructor TPulleyJoint.TDef.Create(ABodyA, ABodyB: TBody;
  AGroundAnchorA, AGroundAnchorB, AAnchorA, AAnchorB: TVector2; ARatio: Single);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper,
    b2VecConv(AGroundAnchorA), b2VecConv(AGroundAnchorB), b2VecConv(AAnchorA), b2VecConv(AAnchorB), ARatio);
end;

{ TPulleyJoint }

function TPulleyJoint.GetCurrentLengthA: Single;
begin
  Result := Wrapper.GetCurrentLengthA;
end;

function TPulleyJoint.GetCurrentLengthB: Single;
begin
  Result := Wrapper.GetCurrentLengthB;
end;

function TPulleyJoint.GetGroundAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetGroundAnchorA);
end;

function TPulleyJoint.GetGroundAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetGroundAnchorB);
end;

function TPulleyJoint.GetLengthA: Single;
begin
  Result := Wrapper.GetLengthA;
end;

function TPulleyJoint.GetLengthB: Single;
begin
  Result := Wrapper.GetLengthB;
end;

function TPulleyJoint.GetRatio: Single;
begin
  Result := Wrapper.GetRatio;
end;

constructor TPulleyJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TGearJoint.TDef }

function TGearJoint.TDef.GetJoint1: TJoint;
begin
  Result := TJoint(b2JointWrapper(Data.Joint1).GetUserData);
end;

procedure TGearJoint.TDef.SetJoint1(const Value: TJoint);
begin
  Data.Joint1 := Value.Wrapper;
end;

function TGearJoint.TDef.GetJoint2: TJoint;
begin
  Result := TJoint(b2JointWrapper(Data.Joint2).GetUserData);
end;

procedure TGearJoint.TDef.SetJoint2(const Value: TJoint);
begin
  Data.Joint2 := Value.Wrapper;
end;

function TGearJoint.TDef.CreateData: b2GearJointDef;
begin
  Result := b2GearJointDef.Create;
end;

constructor TGearJoint.TDef.Create(AJoint1, AJoint2: TJoint; ARatio: Single);
begin
  inherited Create;
  BodyA := AJoint1.BodyB;
  BodyB := AJoint2.BodyB;
  Joint1 := AJoint1;
  Joint2 := AJoint2;
  Ratio := ARatio;
end;

{ TGearJoint }

function TGearJoint.GetJoint1: TJoint;
begin
  Result := TJoint(b2JointWrapper(Wrapper.GetJoint1()).GetUserData);
end;

function TGearJoint.GetJoint2: TJoint;
begin
  Result := TJoint(b2JointWrapper(Wrapper.GetJoint2()).GetUserData);
end;

function TGearJoint.GetRatio: Single;
begin
  Result := Wrapper.GetRatio;
end;

procedure TGearJoint.SetRatio(const Value: Single);
begin
  Wrapper.SetRatio(Value);
end;

function TGearJoint.GetBodyA: TBody;
begin
  Result := TBody(b2BodyWrapper(Wrapper.GetBodyA()).GetUserData);
end;

function TGearJoint.GetBodyB: TBody;
begin
  Result := TBody(b2BodyWrapper(Wrapper.GetBodyB()).GetUserData);
end;

constructor TGearJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TMouseJoint.TDef }

function TMouseJoint.TDef.GetPosition: TVector2;
begin
  Result := b2VecConv(Data.target);
end;

procedure TMouseJoint.TDef.SetPosition(const Value: TVector2);
begin
  Data.target := b2VecConv(Value);
end;

function TMouseJoint.TDef.CreateData: b2MouseJointDef;
begin
  Result := b2MouseJointDef.Create;
end;

constructor TMouseJoint.TDef.Create(ABodyA, ABodyB: TBody; APosition: TVector2);
begin
  inherited Create;
  BodyA := ABodyA;
  BodyB := ABodyB;
  Position := APosition;
  CollideConnected := True;
end;

{ TMouseJoint }

function TMouseJoint.GetPosition: TVector2;
begin
  Result := b2VecConv(Wrapper.GetTarget^);
end;

procedure TMouseJoint.SetPosition(const Value: TVector2);
begin
  Wrapper.SetTarget(b2VecConv(Value));
end;

function TMouseJoint.GetDampingRatio: Single;
begin
  Result := Wrapper.GetDampingRatio;
end;

function TMouseJoint.GetFrequency: Single;
begin
  Result := Wrapper.GetFrequency;
end;

function TMouseJoint.GetMaxForce: Single;
begin
  Result := Wrapper.GetMaxForce;
end;

procedure TMouseJoint.SetDampingRatio(const Value: Single);
begin
  Wrapper.SetDampingRatio(Value);
end;

procedure TMouseJoint.SetFrequency(const Value: Single);
begin
  Wrapper.SetFrequency(Value);
end;

procedure TMouseJoint.SetMaxForce(const Value: Single);
begin
  Wrapper.SetMaxForce(Value);
end;

constructor TMouseJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TWheelJoint.TDef }

function TWheelJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorA);
end;

function TWheelJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorB);
end;

function TWheelJoint.TDef.GetAxis: TVector2;
begin
  Result := b2VecConv(Data.localAxisA);
end;

procedure TWheelJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  Data.LocalAnchorA := b2VecConv(Value);
end;

procedure TWheelJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  Data.LocalAnchorB := b2VecConv(Value);
end;

procedure TWheelJoint.TDef.SetAxis(const Value: TVector2);
begin
  Data.localAxisA := b2VecConv(Value);
end;

function TWheelJoint.TDef.CreateData: b2WheelJointDef;
begin
  Result := b2WheelJointDef.Create;
end;

constructor TWheelJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor, AAxis: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor), b2VecConv(AAxis));
end;

{ TWheelJoint }

function TWheelJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TWheelJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

function TWheelJoint.GetLocalAxis: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAxisA^);
end;

function TWheelJoint.GetMaxMotorTorque: Single;
begin
  Result := Wrapper.GetMaxMotorTorque;
end;

function TWheelJoint.GetMotorEnabled: Boolean;
begin
  Result := Wrapper.IsMotorEnabled;
end;

function TWheelJoint.GetMotorSpeed: Single;
begin
  Result := Wrapper.GetMotorSpeed;
end;

function TWheelJoint.GetSpeed: Single;
begin
  Result := Wrapper.GetJointSpeed;
end;

function TWheelJoint.GetSpringDampingRatio: Single;
begin
  Result := Wrapper.GetSpringDampingRatio;
end;

function TWheelJoint.GetSpringFrequency: Single;
begin
  Result := Wrapper.GetSpringFrequencyHz;
end;

function TWheelJoint.GetTranslation: Single;
begin
  Result := Wrapper.GetJointTranslation;
end;

procedure TWheelJoint.SetMaxMotorTorque(const Value: Single);
begin
  Wrapper.SetMaxMotorTorque(Value);
end;

procedure TWheelJoint.SetMotorEnabled(const Value: Boolean);
begin
  Wrapper.EnableMotor(Value);
end;

procedure TWheelJoint.SetMotorSpeed(const Value: Single);
begin
  Wrapper.SetMotorSpeed(Value);
end;

procedure TWheelJoint.SetSpringDampingRatio(const Value: Single);
begin
  Wrapper.SetSpringDampingRatio(Value);
end;

procedure TWheelJoint.SetSpringFrequency(const Value: Single);
begin
  Wrapper.SetSpringFrequencyHz(Value);
end;

constructor TWheelJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

function TWheelJoint.MotorTorque(AInvDeltaTime: Single): Single;
begin
  Result := Wrapper.GetMotorTorque(AInvDeltaTime);
end;

{ TWeldJoint.TDef }

function TWeldJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorA);
end;

function TWeldJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorB);
end;

procedure TWeldJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  Data.LocalAnchorA := b2VecConv(Value);
end;

procedure TWeldJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  Data.LocalAnchorB := b2VecConv(Value);
end;

function TWeldJoint.TDef.CreateData: b2WeldJointDef;
begin
  Result := b2WeldJointDef.Create;
end;

constructor TWeldJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor));
end;

{ TWeldJoint }

function TWeldJoint.GetDampingRatio: Single;
begin
  Result := Wrapper.GetDampingRatio;
end;

function TWeldJoint.GetFrequency: Single;
begin
  Result := Wrapper.GetFrequency;
end;

function TWeldJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TWeldJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

function TWeldJoint.GetReferenceAngle: Single;
begin
  Result := Wrapper.GetReferenceAngle;
end;

procedure TWeldJoint.SetDampingRatio(const Value: Single);
begin
  Wrapper.SetDampingRatio(Value);
end;

procedure TWeldJoint.SetFrequency(const Value: Single);
begin
  Wrapper.SetFrequency(Value);
end;

constructor TWeldJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TRopeJoint.TDef }

function TRopeJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorA);
end;

function TRopeJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorB);
end;

procedure TRopeJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  Data.LocalAnchorA := b2VecConv(Value);
end;

procedure TRopeJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  Data.LocalAnchorB := b2VecConv(Value);
end;

function TRopeJoint.TDef.CreateData: b2RopeJointDef;
begin
  Result := b2RopeJointDef.Create;
end;

constructor TRopeJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchorA, AAnchorB: TVector2);
begin
  inherited Create;
  BodyA := ABodyA;
  BodyB := ABodyB;
  AnchorA := AAnchorA;
  AnchorB := AAnchorB;
  CollideConnected := True;
  MaxLength := BodyA.Position.DistanceTo(BodyB.Position);
end;

constructor TRopeJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchorA, AAnchorB: TVector2; ALength: Single);
begin
  inherited Create;
  BodyA := ABodyA;
  BodyB := ABodyB;
  AnchorA := AAnchorA;
  AnchorB := AAnchorB;
  CollideConnected := True;
  MaxLength := ALength;
end;

{ TRopeJoint }

function TRopeJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TRopeJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

function TRopeJoint.GetMaxLength: Single;
begin
  Result := Wrapper.GetMaxLength;
end;

procedure TRopeJoint.SetMaxLength(const Value: Single);
begin
  Wrapper.SetMaxLength(Value);
end;

constructor TRopeJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TFrictionJoint.TDef }

function TFrictionJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorA);
end;

function TFrictionJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorB);
end;

procedure TFrictionJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  Data.LocalAnchorA := b2VecConv(Value);
end;

procedure TFrictionJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  Data.LocalAnchorB := b2VecConv(Value);
end;

function TFrictionJoint.TDef.CreateData: b2FrictionJointDef;
begin
  Result := b2FrictionJointDef.Create;
end;

constructor TFrictionJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyA.Wrapper, b2VecConv(AAnchor));
end;

{ TFrictionJoint }

function TFrictionJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TFrictionJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

function TFrictionJoint.GetMaxForce: Single;
begin
  Result := Wrapper.GetMaxForce;
end;

function TFrictionJoint.GetMaxTorque: Single;
begin
  Result := Wrapper.GetMaxTorque;
end;

procedure TFrictionJoint.SetMaxForce(const Value: Single);
begin
  Wrapper.SetMaxForce(Value);
end;

procedure TFrictionJoint.SetMaxTorque(const Value: Single);
begin
  Wrapper.SetMaxTorque(Value);
end;

constructor TFrictionJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TMotorJoint.TDef }

function TMotorJoint.TDef.GetLinearOffset: TVector2;
begin
  Result := b2VecConv(Data.LinearOffset);
end;

procedure TMotorJoint.TDef.SetLinearOffset(const Value: TVector2);
begin
  Data.LinearOffset := b2VecConv(Value);
end;

function TMotorJoint.TDef.CreateData: b2MotorJointDef;
begin
  Result := b2MotorJointDef.Create;
end;

constructor TMotorJoint.TDef.Create(ABodyA, ABodyB: TBody);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper);
end;

{ TMotorJoint }

function TMotorJoint.GetAngularOffset: Single;
begin
  Result := Wrapper.GetAngularOffset;
end;

function TMotorJoint.GetCorrectionFactor: Single;
begin
  Result := Wrapper.GetCorrectionFactor;
end;

function TMotorJoint.GetLinearOffset: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLinearOffset^);
end;

function TMotorJoint.GetMaxForce: Single;
begin
  Result := Wrapper.GetMaxForce;
end;

function TMotorJoint.GetMaxTorque: Single;
begin
  Result := Wrapper.GetMaxTorque;
end;

procedure TMotorJoint.SetAngularOffset(const Value: Single);
begin
  Wrapper.SetAngularOffset(Value);
end;

procedure TMotorJoint.SetCorrectionFactor(const Value: Single);
begin
  Wrapper.SetCorrectionFactor(Value);
end;

procedure TMotorJoint.SetLinearOffset(const Value: TVector2);
begin
  Wrapper.SetLinearOffset(b2VecConv(Value));
end;

procedure TMotorJoint.SetMaxForce(const Value: Single);
begin
  Wrapper.SetMaxForce(Value);
end;

procedure TMotorJoint.SetMaxTorque(const Value: Single);
begin
  Wrapper.SetMaxTorque(Value);
end;

constructor TMotorJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

{ TWorld.TDestructionListener }

constructor TWorld.TListener.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

procedure TWorld.TListener.BeginContact(AContact: b2ContactHandle);
begin
  World.FOnContact.Execute(TContactEventInfo.Create(World, ceBeginContact, TContact.Create(AContact)));
end;

procedure TWorld.TListener.EndContact(AContact: b2ContactHandle);
begin
  World.FOnContact.Execute(TContactEventInfo.Create(World, ceEndContact, TContact.Create(AContact)));
end;

procedure TWorld.TListener.PreSolve(AContact: b2ContactHandle; AOldManifold: Pb2Manifold);
begin
  World.FOnContact.Execute(TContactEventInfo.Create(World, cePreSolve, TContact.Create(AContact)));
end;

procedure TWorld.TListener.PostSolve(AContact: b2ContactHandle; AImpulse: Pb2ContactImpulse);
begin
  World.FOnContact.Execute(TContactEventInfo.Create(World, cePostSolve, TContact.Create(AContact)));
end;

{ TWorld.TIterator }

constructor TWorld.TIterator.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

{ TWorld.TBodyIterator }

constructor TWorld.TBodyIterator.Create(AWorld: TWorld);
begin
  inherited;
  FNext := FWorld.Bodies.First;
end;

function TWorld.TBodyIterator.GetCurrent: TBody;
begin
  Result := FCurrent;
end;

function TWorld.TBodyIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FCurrent <> nil;
  if Result then
    FNext := FNext.GetNext;
end;

{ TWorld.TBodies }

function TWorld.TBodies.GetFirst: TBody;
begin
  Result := TBody(b2BodyWrapper(b2BodyHandle(FWorld.Wrapper.GetBodyList)).GetUserData);
end;

constructor TWorld.TBodies.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

function TWorld.TBodies.GetEnumerator: IIterator<TBody>;
begin
  Result := TBodyIterator.Create(FWorld);
end;

function TWorld.TBodies.Iterate: IIterate<TBody>;
begin
  Result := TIterableIterate<TBody>.Create(Self);
end;

function TWorld.TBodies.Add(ABodyDef: TBody.TDef): TBody;
begin
  Result := TBody.Create(FWorld, ABodyDef);
end;

function TWorld.TBodies.Add(ABodyType: TBody.TType; APosition: TVector2): TBody;
var
  BodyDef: TBody.TDef;
begin
  BodyDef := TBody.TDef.Create(ABodyType, APosition);
  Result := TBody.Create(FWorld, BodyDef);
  BodyDef.Free;
end;

{ TWorld.TJoints }

function TWorld.TJoints.GetFirst: TJoint;
begin
  Result := TJoint(b2JointWrapper(b2JointHandle(FWorld.Wrapper.GetJointList)).GetUserData);
end;

constructor TWorld.TJoints.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

function TWorld.TJoints.GetEnumerator: IIterator<TJoint>;
begin
  raise ENotImplemented.Create('TWorld.TJoints.GetEnumerator');
  // Result := TJointIterator.Create(FWorld);
end;

function TWorld.TJoints.Iterate: IIterate<TJoint>;
begin
  Result := TIterableIterate<TJoint>.Create(Self);
end;

function TWorld.TJoints.Add(ADef: TJoint.TDef): TJoint;
begin
  Result := TJoint.Create(FWorld, ADef);
end;

// --- FORMATTED END ---

{ TWorld }

constructor TWorld.Create(AGravity: TVector2; AAllowSleeping: Boolean);
begin
  FWrapper := b2WorldWrapper.Create(b2VecConv(AGravity));
  AllowSleeping := AAllowSleeping;
  FVelocityIterations := DefaultVelocityIterations;
  FPositionIterations := DefaultPositionIterations;
  InitializeListeners;
end;

destructor TWorld.Destroy;
var
  Body: TBody;
begin
  for Body in Bodies do
  begin
    Body.FWorld := nil;
    Body.Free;
  end;
  Wrapper.Destroy;
  Destroy_b2ContactListener_delegate(FContactListener);
  inherited;
end;

function TWorld.GetAllowSleeping: Boolean;
begin
  Result := Wrapper.GetAllowSleeping;
end;

function TWorld.GetBodies: IBodies;
begin
  Result := TBodies.Create(Self);
end;

function TWorld.GetContacts: IContacts;
begin
  Result := TContacts.Create(Self);
end;

function TWorld.GetGravity: TVector2;
begin
  Result := b2VecConv(Wrapper.GetGravity);
end;

function TWorld.GetJoints: IJoints;
begin
  Result := TJoints.Create(Self);
end;

procedure TWorld.InitializeListeners;
var
  Listener: TListener;
begin
  Listener := TListener.Create(Self);
  FContactListener := Create_b2ContactListener_delegate(Listener);
  Wrapper.SetContactListener(FContactListener);
end;

procedure TWorld.SetAllowSleeping(const Value: Boolean);
begin
  Wrapper.SetAllowSleeping(Value);
end;

procedure TWorld.SetGravity(const Value: TVector2);
begin
  Wrapper.SetGravity(b2VecConv(Gravity));
end;

procedure TWorld.Step(ATimeStep: Single);
begin
  Wrapper.Step(ATimeStep, VelocityIterations, PositionIterations);
  UpdateLocations;
end;

procedure TWorld.StepNoUpdate(ATimeStep: Single);
begin
  Wrapper.Step(ATimeStep, VelocityIterations, PositionIterations);
end;

procedure TWorld.UpdateLocations;
var
  Body: TBody;
begin
  for Body in Bodies do
    Body.UpdateLocations;
end;

{ TWorld.TContact }

constructor TWorld.TContact.Create(AWrapper: b2ContactWrapper);
begin
  FWrapper := AWrapper;
end;

function TWorld.TContact.GetBodyA: TBody;
begin
  Result := FixtureA.Body;
end;

function TWorld.TContact.GetBodyB: TBody;
begin
  Result := FixtureB.Body;
end;

function TWorld.TContact.GetChainIndexA: Integer;
begin
  Result := Wrapper.GetChildIndexA;
end;

function TWorld.TContact.GetChainIndexB: Integer;
begin
  Result := Wrapper.GetChildIndexB;
end;

function TWorld.TContact.GetEnabled: Boolean;
begin
  Result := Wrapper.IsEnabled;
end;

function TWorld.TContact.GetFixtureA: TFixture;
begin
  Result := TFixture(b2Fixture(Wrapper.GetFixtureA^).GetUserData);
end;

function TWorld.TContact.GetFixtureB: TFixture;
begin
  Result := TFixture(b2Fixture(Wrapper.GetFixtureB^).GetUserData);
end;

function TWorld.TContact.GetFriction: Single;
begin
  Result := Wrapper.GetFriction;
end;

function TWorld.TContact.GetNext: TContact;
begin
  Result.Create(Wrapper.GetNext);
end;

function TWorld.TContact.GetRestitution: Single;
begin
  Result := Wrapper.GetRestitution;
end;

function TWorld.TContact.GetTangentSpeed: Single;
begin
  Result := Wrapper.GetTangentSpeed;
end;

function TWorld.TContact.GetTouching: Boolean;
begin
  Result := Wrapper.IsTouching;
end;

function TWorld.TContact.GetUserDataA: TObject;
begin
  Result := BodyA.UserData;
end;

function TWorld.TContact.GetUserDataB: TObject;
begin
  Result := BodyB.UserData;
end;

procedure TWorld.TContact.ResetFriction;
begin
  Wrapper.ResetFriction;
end;

procedure TWorld.TContact.ResetRestitution;
begin
  Wrapper.ResetRestitution;
end;

procedure TWorld.TContact.SetEnabled(const Value: Boolean);
begin
  Wrapper.SetEnabled(Value);
end;

procedure TWorld.TContact.SetFriction(const Value: Single);
begin
  Wrapper.SetFriction(Value);
end;

procedure TWorld.TContact.SetRestition(const Value: Single);
begin
  Wrapper.SetRestitution(Value);
end;

procedure TWorld.TContact.SetTangentSpeed(const Value: Single);
begin
  Wrapper.SetTangentSpeed(Value);
end;

{ TWorld.TContactIterator }

constructor TWorld.TContactIterator.Create(AWorld: TWorld);
begin
  inherited;
  FNext := AWorld.Contacts.First;
end;

function TWorld.TContactIterator.GetCurrent: TContact;
begin
  Result := FCurrent;
end;

function TWorld.TContactIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FCurrent.Wrapper.FHandle <> 0;
  if Result then
    FNext := FNext.GetNext;
end;

{ TWorld.TContactEventInfo }

constructor TWorld.TContactEventInfo.Create(ASender: TWorld; AEventType: TContactEventType; AContact: TContact);
begin
  inherited Create(ASender);
  FEventType := AEventType;
  FContact := AContact;
end;

end.
