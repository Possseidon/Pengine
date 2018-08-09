unit Pengine.Box2D;

{$Pointermath ON}

interface

uses
  System.Math,
  System.SysUtils,

  Box2D.Dynamics,
  Box2D.Common,
  Box2D.Collision,
  Box2D.Rope,
  Box2DTypes,

  Pengine.TimeManager,
  Pengine.Vector,
  Pengine.CollectionInterfaces,
  Pengine.Collections,
  Pengine.EventHandling;

type

  TDefBase = class
  private
    FFreeAfterUse: Boolean;

  public
    constructor Create;

    property FreeAfterUse: Boolean read FFreeAfterUse write FFreeAfterUse;

  end;

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

  TShapeClass = class of TShape;

  TShape<W: record> = class abstract(TShape)
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

    TSideIterator = class(TIterator<TLine2>)
    protected
      function GetCurrent: TLine2; override;

    end;

    TPointsWrapper = record
    private
      FPolygon: TPolygon;

      function GetPoint(AIndex: Integer): TVector2;
      function GetCount: Integer;

      constructor Create(APolygon: TPolygon);

    public
      property Points[AIndex: Integer]: TVector2 read GetPoint; default;
      property Count: Integer read GetCount;
      function GetEnumerator: IIterator<TVector2>; inline;
      function ToArray: TArray<TVector2>; inline;

    end;

    TSidesWrapper = record
    private
      FPolygon: TPolygon;

      function GetCount: Integer;
      function GetSide(AIndex: Integer): TLine2;

      constructor Create(APolygon: TPolygon);

    public
      property Sides[AIndex: Integer]: TLine2 read GetSide; default;
      property Count: Integer read GetCount;
      function GetEnumerator: IIterator<TLine2>; inline;
      function ToArray: TArray<TLine2>; inline;

    end;

  private
    function GetPoints: TPointsWrapper; inline;
    function GetSkinThickness: Single; inline;
    procedure SetSkinThickness(const Value: Single); inline;
    function GetSides: TSidesWrapper;

  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>The skin thickness, used to prevent tunneling.</summary>
    property SkinThickness: Single read GetSkinThickness write SetSkinThickness;

    procedure SetPoints(APoints: TArray<TVector2>); overload;
    procedure SetPoints(AFirst: PVector2; ACount: Integer); overload;
    procedure SetBox(ASize: TVector2); overload;
    procedure SetBox(ASize: TVector2; ACenter: TVector2; AAngleRad: Single); overload;

    property Points: TPointsWrapper read GetPoints;
    property Sides: TSidesWrapper read GetSides;

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

    TEdgeIterator = class(TIterator<TLine2>)
    protected
      function GetCurrent: TLine2; override;

    end;

    TPointsWrapper = record
    private
      FChain: TChain;

      function GetPoint(AIndex: Integer): TVector2;
      function GetCount: Integer;

      constructor Create(AChain: TChain);

    public
      property Points[AIndex: Integer]: TVector2 read GetPoint; default;
      property Count: Integer read GetCount;
      function GetEnumerator: IIterator<TVector2>; inline;
      function ToArray: TArray<TVector2>; inline;

    end;

    TEdgesWrapper = record
    private
      FChain: TChain;

      function GetCount: Integer;
      function GetEdge(AIndex: Integer): TLine2;

      constructor Create(AChain: TChain);

    public
      property Edges[AIndex: Integer]: TLine2 read GetEdge; default;
      property Count: Integer read GetCount;
      function GetEnumerator: IIterator<TLine2>; inline;
      function ToArray: TArray<TLine2>; inline;

    end;

  private
    function GetEdges: TEdgesWrapper;
    function GetPoints: TPointsWrapper;
    function GetNext: TVector2;
    function GetPrev: TVector2;
    procedure SetNext(const Value: TVector2);
    procedure SetPrev(const Value: TVector2);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetChain(APoints: TArray<TVector2>); overload;
    procedure SetChain(AFirst: PVector2; ACount: Integer); overload;
    procedure SetLoop(APoints: TArray<TVector2>); overload;
    procedure SetLoop(AFirst: PVector2; ACount: Integer); overload;

    property Points: TPointsWrapper read GetPoints;
    property Edges: TEdgesWrapper read GetEdges;

    function HasPrev: Boolean;
    property Prev: TVector2 read GetPrev write SetPrev;
    function HasNext: Boolean;
    property Next: TVector2 read GetNext write SetNext;

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
    function GetFriction: Single;
    function GetRestitution: Single;
    procedure SetDensity(const Value: Single);
    procedure SetFriction(const Value: Single);
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
      function GetLinearVelocity: TVector2;
      function GetPosition: TVector2;
      function GetType: TType;
      procedure SetLinearVelocity(const Value: TVector2);
      procedure SetPosition(const Value: TVector2);
      procedure SetType(const Value: TType);
      function GetRotation: Single;
      procedure SetRotation(const Value: Single);

    public
      constructor Create(AType: TType; APosition: TVector2);

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

      property Data: Pb2BodyDef read GetData;

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

    TFixturesWrapper = record
    private
      FBody: TBody;

      constructor Create(ABody: TBody);

    public
      function Add(AFixture: TFixture.TDef): TFixture; overload;
      function Add(AShape: TShape): TFixture; overload;

      function First: TFixture;
      function GetEnumerator: IIterator<TFixture>;
      function ToArray: TRefArray<TFixture>;

    end;

    TJointIterator = class(TInterfacedObject, IIterator<TJoint>)
    private
      FCurrent: Pb2JointEdge;
      FNext: Pb2JointEdge;

      function GetCurrent: TJoint;

    public
      constructor Create(AJointEdge: Pb2JointEdge);

      function MoveNext: Boolean;

    end;

    TJointsWrapper = record
    private
      FBody: TBody;

      constructor Create(ABody: TBody);

    public
      function First: TJoint;
      function GetEnumerator: IIterator<TJoint>;
      function ToArray: TRefArray<TJoint>;

    end;

    TLocations = TRefArray<TLocation2>;

    TEventInfo = TSenderEventInfo<TBody>;

    TEvent = TEvent<TEventInfo>;

  private
    FWrapper: b2BodyWrapper;
    FWorld: TWorld;
    FLocations: TLocations;
    FOnLocationChanged: TEvent;

    function GetFixtures: TFixturesWrapper;

    function GetPosition: TVector2;
    function GetRotationRad: Single;
    function GetRotation: Single;

    function GetLinearVelocity: TVector2;
    procedure SetLinearVelocity(const Value: TVector2);
    function GetLinearDamping: Single;
    procedure SetLinearDamping(const Value: Single);

    function GetAngularVelocity: Single;
    procedure SetAngularVelocity(const Value: Single);
    function GetAngularDamping: Single;
    procedure SetAngularDamping(const Value: Single);

    function GetAwake: Boolean;
    procedure SetAwake(const Value: Boolean);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetPosition(const Value: TVector2);
    procedure SetRotation(const Value: Single);
    procedure SetRotationRad(const Value: Single);
    function GetJoints: TJointsWrapper;
    function GetFixedRotation: Boolean;
    procedure SetFixedRotation(const Value: Boolean);
    function GetBullet: Boolean;
    procedure SetBullet(const Value: Boolean);
    function GetGravityScale: Single;
    procedure SetGravityScale(const Value: Single);
    function GetInertia: Single;
    function GetLinearVelocityAtLocalPoint(APoint: TVector2): TVector2;
    function GetLinearVelocityAtWorldPoint(APoint: TVector2): TVector2;
    function GetLocalCenter: TVector2;
    function GetLocalPoint(AWorldPoint: TVector2): TVector2;
    function GetLocalVector(AWorldVector: TVector2): TVector2;
    function GetMass: Single;
    function GetSleepingAllowed: Boolean;
    function GetType: TType;
    function GetWorldCenter: TVector2;
    function GetWorldPoint(ALocalPoint: TVector2): TVector2;
    function GetWorldVector(ALocalVector: TVector2): TVector2;
    procedure SetSleepingAllowed(const Value: Boolean);
    procedure SetType(const Value: TType);

  public
    constructor Create(AWorld: TWorld; ADef: TDef);
    destructor Destroy; override;

    property Wrapper: b2BodyWrapper read FWrapper;
    property World: TWorld read FWorld;
    property &Type: TType read GetType write SetType;

    function GetNext: TBody;

    property Fixtures: TFixturesWrapper read GetFixtures;

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

    /// <summary>Add a location, that automatically gets its transform updated by the Step function.</summary>
    procedure AddLocation(ALocation: TLocation2);
    /// <summary>Removes a location from being automatically updated by the Step function.</summary>
    procedure RemoveLocation(ALocation: TLocation2);

    function OnLocationChanged: TEvent.TAccess;

    procedure UpdateLocations;

    property Joints: TJointsWrapper read GetJoints;

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

    TDef<T: record> = class(TDef)
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

  TJoint<T: record> = class abstract(TJoint)
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

    TDestructionListener = class(TInterfacedObject, Ib2DestructionListener)
    private
      FWorld: TWorld;

    public
      constructor Create(AWorld: TWorld);

      procedure SayGoodbye(AJoint: b2JointHandle); overload; cdecl;
      procedure SayGoodbye(AFixture: Pb2Fixture); overload; cdecl;

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

    TBodiesWrapper = record
    private
      FWorld: TWorld;

      constructor Create(AWorld: TWorld);

    public
      function Add(ABodyDef: TBody.TDef): TBody; overload;
      function Add(ABodyType: TBody.TType; APosition: TVector2): TBody; overload;

      function First: TBody;
      function GetEnumerator: IIterator<TBody>;
      function ToArray: TRefArray<TBody>;

    end;

    TJointsWrapper = record
    private
      FWorld: TWorld;

      constructor Create(AWorld: TWorld);

    public
      function Add(ADef: TJoint.TDef): TJoint;

      function First: TJoint;
      function GetEnumerator: IIterator<TJoint>;
      function ToArray: TRefArray<TJoint>;

    end;

  public const

    DefaultVelocityIterations = 20;
    DefaultPositionIterations = 10;

  private
    FWrapper: b2WorldWrapper;
    FVelocityIterations: Integer;
    FPositionIterations: Integer;
    FDestructionListener: b2DestructionListenerWrapper;

    function GetAllowSleeping: Boolean; inline;
    procedure SetAllowSleeping(const Value: Boolean); inline;
    function GetGravity: TVector2; inline;
    procedure SetGravity(const Value: TVector2); inline;
    function GetBodies: TBodiesWrapper;
    function GetJoints: TJointsWrapper;

  public
    constructor Create(AGravity: TVector2; AAllowSleeping: Boolean = True);
    destructor Destroy; override;

    property Gravity: TVector2 read GetGravity write SetGravity;
    property AllowSleeping: Boolean read GetAllowSleeping write SetAllowSleeping;

    property Wrapper: b2WorldWrapper read FWrapper;

    property Bodies: TBodiesWrapper read GetBodies;
    property Joints: TJointsWrapper read GetJoints;

    property VelocityIterations: Integer read FVelocityIterations write FVelocityIterations;
    property PositionIterations: Integer read FPositionIterations write FPositionIterations;

    procedure Step(ATimeStep: TSeconds);
    procedure StepNoUpdate(ATimeStep: TSeconds);

    procedure UpdateLocations;

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
  Result := Pb2Vec2(@AVector)^;
end;

function b2VecConv(const AVector: b2Vec2): TVector2;
begin
  Result := PVector2(@AVector)^;
end;

{ TWorld }

constructor TWorld.Create(AGravity: TVector2; AAllowSleeping: Boolean);
begin
  FWrapper := b2WorldWrapper.Create(b2VecConv(AGravity));
  AllowSleeping := AAllowSleeping;
  FVelocityIterations := DefaultVelocityIterations;
  FPositionIterations := DefaultPositionIterations;
  FDestructionListener := Create_b2DestructionListener_delegate(TDestructionListener.Create(Self));
  Wrapper.SetDestructionListener(FDestructionListener);
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
  Destroy_b2DestructionListener_delegate(FDestructionListener);
  inherited;
end;

function TWorld.GetAllowSleeping: Boolean;
begin
  Result := Wrapper.GetAllowSleeping;
end;

function TWorld.GetBodies: TBodiesWrapper;
begin
  Result.Create(Self);
end;

function TWorld.GetGravity: TVector2;
begin
  Result := b2VecConv(Wrapper.GetGravity);
end;

function TWorld.GetJoints: TJointsWrapper;
begin
  Result.Create(Self);
end;

procedure TWorld.SetAllowSleeping(const Value: Boolean);
begin
  Wrapper.SetAllowSleeping(Value);
end;

procedure TWorld.SetGravity(const Value: TVector2);
begin
  Wrapper.SetGravity(b2VecConv(Gravity));
end;

procedure TWorld.Step(ATimeStep: TSeconds);
begin
  Wrapper.Step(ATimeStep, VelocityIterations, PositionIterations);
  UpdateLocations;
end;

procedure TWorld.StepNoUpdate(ATimeStep: TSeconds);
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

{ TShape<W> }

function TShape<W>.GetWrapper: W;
begin
  Result := W(inherited Wrapper);
end;

{ TBody }

procedure TBody.AddLocation(ALocation: TLocation2);
begin
  FLocations.Add(ALocation);
end;

procedure TBody.ApplyAngularImpulse(AImpulse: Single; Awake: Boolean);
begin
  Wrapper.ApplyAngularImpulse(AImpulse, Awake);
end;

procedure TBody.ApplyForce(AForce, APoint: TVector2; Awake: Boolean);
begin
  Wrapper.ApplyForce(b2VecConv(AForce), b2VecConv(APoint), Awake);
end;

procedure TBody.ApplyForceToCenter(AForce: TVector2; Awake: Boolean);
begin
  Wrapper.ApplyForceToCenter(b2VecConv(AForce), Awake);
end;

procedure TBody.ApplyLinearImpulse(AImpulse, APoint: TVector2; Awake: Boolean);
begin
  Wrapper.ApplyLinearImpulse(b2VecConv(AImpulse), b2VecConv(APoint), Awake);
end;

procedure TBody.ApplyTorque(ATorque: Single; Awake: Boolean);
begin
  Wrapper.ApplyTorque(ATorque, Awake);
end;

constructor TBody.Create(AWorld: TWorld; ADef: TDef);
begin
  FWorld := AWorld;
  ADef.Data.userData := Self;
  FWrapper := World.Wrapper.CreateBody(ADef.Data);
  if ADef.FreeAfterUse then
    ADef.Free;
  FLocations := TLocations.Create;
end;

destructor TBody.Destroy;
var
  Fixture: TFixture;
  Joint: TJoint;
begin
  FLocations.Free;
  for Joint in Joints do
  begin
    Joint.Wrapper.SetUserData(nil);
    Joint.Free;
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

function TBody.GetActive: Boolean;
begin
  Result := Wrapper.IsActive;
end;

function TBody.GetAngularDamping: Single;
begin
  Result := Wrapper.GetAngularDamping;
end;

function TBody.GetAngularVelocity: Single;
begin
  Result := Wrapper.GetAngularVelocity;
end;

function TBody.GetAwake: Boolean;
begin
  Result := Wrapper.IsAwake;
end;

function TBody.GetBullet: Boolean;
begin
  Result := Wrapper.IsBullet;
end;

function TBody.GetFixedRotation: Boolean;
begin
  Result := Wrapper.IsFixedRotation;
end;

function TBody.GetFixtures: TFixturesWrapper;
begin
  Result := TFixturesWrapper.Create(Self);
end;

function TBody.GetGravityScale: Single;
begin
  Result := Wrapper.GetGravityScale;
end;

function TBody.GetInertia: Single;
begin
  Result := Wrapper.GetInertia;
end;

function TBody.GetJoints: TJointsWrapper;
begin
  Result.Create(Self);
end;

function TBody.GetLinearDamping: Single;
begin
  Result := Wrapper.GetLinearDamping;
end;

function TBody.GetLinearVelocity: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLinearVelocity^);
end;

function TBody.GetLinearVelocityAtLocalPoint(APoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLinearVelocityFromLocalPoint(b2VecConv(APoint)));
end;

function TBody.GetLinearVelocityAtWorldPoint(APoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLinearVelocityFromWorldPoint(b2VecConv(APoint)));
end;

function TBody.GetLocalCenter: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalCenter^);
end;

function TBody.GetLocalPoint(AWorldPoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalPoint(b2VecConv(AWorldPoint)));
end;

function TBody.GetLocalVector(AWorldVector: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalVector(b2VecConv(AWorldVector)));
end;

function TBody.GetMass: Single;
begin
  Result := Wrapper.GetMass;
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

function TBody.GetPosition: TVector2;
begin
  Result := b2VecConv(Wrapper.GetPosition^);
end;

function TBody.GetRotation: Single;
begin
  Result := RadToDeg(RotationRad);
end;

function TBody.GetRotationRad: Single;
begin
  Result := Wrapper.GetAngle;
end;

function TBody.GetSleepingAllowed: Boolean;
begin
  Result := Wrapper.IsSleepingAllowed;
end;

function TBody.GetType: TType;
begin
  Result := TType(Wrapper.GetType);
end;

function TBody.GetWorldCenter: TVector2;
begin
  Result := b2VecConv(Wrapper.GetWorldCenter^);
end;

function TBody.GetWorldPoint(ALocalPoint: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetWorldPoint(b2VecConv(ALocalPoint)));
end;

function TBody.GetWorldVector(ALocalVector: TVector2): TVector2;
begin
  Result := b2VecConv(Wrapper.GetWorldVector(b2VecConv(ALocalVector)));
end;

function TBody.OnLocationChanged: TEvent.TAccess;
begin
  Result := FOnLocationChanged.Access;
end;

procedure TBody.SetActive(const Value: Boolean);
begin
  Wrapper.SetActive(Value);
end;

procedure TBody.SetAngularDamping(const Value: Single);
begin
  Wrapper.SetAngularDamping(Value);
end;

procedure TBody.SetAngularVelocity(const Value: Single);
begin
  Wrapper.SetAngularVelocity(Value);
end;

procedure TBody.SetAwake(const Value: Boolean);
begin
  Wrapper.SetAwake(Value);
end;

procedure TBody.SetBullet(const Value: Boolean);
begin
  Wrapper.SetBullet(Value);
end;

procedure TBody.SetFixedRotation(const Value: Boolean);
begin
  Wrapper.SetFixedRotation(Value);
end;

procedure TBody.SetGravityScale(const Value: Single);
begin
  Wrapper.SetGravityScale(Value);
end;

procedure TBody.SetLinearDamping(const Value: Single);
begin
  Wrapper.SetLinearDamping(Value);
end;

procedure TBody.SetLinearVelocity(const Value: TVector2);
begin
  Wrapper.SetLinearVelocity(b2VecConv(Value));
end;

procedure TBody.SetPosition(const Value: TVector2);
begin
  Wrapper.SetTransform(b2VecConv(Value), RotationRad);
end;

procedure TBody.SetRotation(const Value: Single);
begin
  RotationRad := DegToRad(Value);
end;

procedure TBody.SetRotationRad(const Value: Single);
begin
  Wrapper.SetTransform(b2VecConv(Position), Value);
end;

procedure TBody.SetSleepingAllowed(const Value: Boolean);
begin
  Wrapper.SetSleepingAllowed(Value);
end;

procedure TBody.SetType(const Value: TType);
begin
  Wrapper.SetType(b2BodyType(Value));
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

procedure TBody.RemoveLocation(ALocation: TLocation2);
begin
  FLocations.Remove(ALocation);
end;

{ TBody.TFixturesWrapper }

function TBody.TFixturesWrapper.Add(AFixture: TFixture.TDef): TFixture;
begin
  Result := TFixture.Create(FBody, AFixture);
end;

function TBody.TFixturesWrapper.Add(AShape: TShape): TFixture;
begin
  Result := Add(TFixture.TDef.Create(AShape));
end;

constructor TBody.TFixturesWrapper.Create(ABody: TBody);
begin
  FBody := ABody;
end;

function TBody.TFixturesWrapper.First: TFixture;
var
  Fixture: Pb2Fixture;
begin
  Fixture := FBody.Wrapper.GetFixtureList;
  if Fixture = nil then
    Exit(nil);
  Result := TFixture(Fixture.GetUserData);
end;

function TBody.TFixturesWrapper.GetEnumerator: IIterator<TFixture>;
begin
  Result := TFixtureIterator.Create(FBody);
end;

function TBody.TFixturesWrapper.ToArray: TRefArray<TFixture>;
begin
  Result := TRefArray<TFixture>.Create;
  Result.Add(GetEnumerator);
end;

{ TFixture }

constructor TFixture.Create(ABody: TBody; ADef: TFixture.TDef);
begin
  ADef.FData.userData := Self;
  FWrapper := ABody.Wrapper.CreateFixture(ADef.Data)^;
  if ADef.FreeAfterUse then
    ADef.Free;
end;

destructor TFixture.Destroy;
begin
  if Wrapper.GetUserData <> nil then
    Body.Wrapper.DestroyFixture(@Wrapper);
  inherited;
end;

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

function TFixture.GetFriction: Single;
begin
  Result := Wrapper.GetFriction;
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

function TFixture.GetRestitution: Single;
begin
  Result := Wrapper.GetRestitution;
end;

procedure TFixture.SetDensity(const Value: Single);
begin
  Wrapper.SetDensity(Value);
end;

procedure TFixture.SetFriction(const Value: Single);
begin
  Wrapper.SetFriction(Value);
end;

procedure TFixture.SetRestitution(const Value: Single);
begin
  Wrapper.SetRestitution(Value);
end;

{ TBody.TFixturesIterator }

constructor TBody.TFixtureIterator.Create(ABody: TBody);
begin
  FNext := ABody.Fixtures.First;
end;

function TBody.TFixtureIterator.GetCurrent: TFixture;
begin
  Result := FCurrent;
end;

function TBody.TFixtureIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FNext <> nil;
  if Result then
    FNext := FNext.GetNext;
end;

{ TShape }

constructor TShape.Create;
begin
  // nothing
end;

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

{ TEdge }

function TEdge.GetVertex1: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex1);
end;

function TEdge.GetVertex2: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex2);
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

function TEdge.GetLine: TLine2;
begin
  Result := Vertex1.LineTo(Vertex2);
end;

function TEdge.GetNext: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex3);
end;

function TEdge.GetPrev: TVector2;
begin
  Result := b2VecConv(Wrapper.m_vertex0);
end;

function TEdge.HasNext: Boolean;
begin
  Result := Wrapper.m_hasVertex3;
end;

function TEdge.HasPrev: Boolean;
begin
  Result := Wrapper.m_hasVertex0;
end;

procedure TEdge.SetVertex1(const Value: TVector2);
begin
  Wrapper.m_vertex1 := b2VecConv(Value);
end;

procedure TEdge.SetVertex2(const Value: TVector2);
begin
  Wrapper.m_vertex2 := b2VecConv(Value);
end;

procedure TEdge.SetLine(const Value: TLine2);
begin
  Vertex1 := Value.Tail;
  Vertex2 := Value.Head;
end;

procedure TEdge.SetNext(const Value: TVector2);
begin
  Wrapper.m_vertex3 := b2VecConv(Value);
end;

procedure TEdge.SetPrev(const Value: TVector2);
begin
  Wrapper.m_vertex0 := b2VecConv(Value);
end;

{ TPolygon }

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

function TPolygon.GetPoints: TPointsWrapper;
begin
  Result.Create(Self);
end;

function TPolygon.GetSides: TSidesWrapper;
begin
  Result.Create(Self);
end;

function TPolygon.GetSkinThickness: Single;
begin
  Result := Radius;
end;

procedure TPolygon.SetBox(ASize: TVector2);
begin
  Wrapper.SetAsBox(ASize.x, ASize.y);
end;

procedure TPolygon.SetBox(ASize, ACenter: TVector2; AAngleRad: Single);
begin
  Wrapper.SetAsBox(ASize.x, ASize.y, b2VecConv(ACenter), AAngleRad);
end;

procedure TPolygon.SetPoints(AFirst: PVector2; ACount: Integer);
begin
  Wrapper.&Set(Pointer(AFirst), ACount);
end;

procedure TPolygon.SetPoints(APoints: TArray<TVector2>);
begin
  Wrapper.&Set(APoints.DataPointer, APoints.Count);
end;

procedure TPolygon.SetSkinThickness(const Value: Single);
begin
  Radius := Value;
end;

{ TPolygon.TPointsWrapper }

function TPolygon.TPointsWrapper.GetCount: Integer;
begin
  Result := FPolygon.Wrapper.m_count;
end;

constructor TPolygon.TPointsWrapper.Create(APolygon: TPolygon);
begin
  FPolygon := APolygon;
end;

function TPolygon.TPointsWrapper.GetEnumerator: IIterator<TVector2>;
begin
  Result := TPointIterator.Create(FPolygon);
end;

function TPolygon.TPointsWrapper.GetPoint(AIndex: Integer): TVector2;
begin
  Result := b2VecConv(FPolygon.Wrapper.m_vertices[AIndex]);
end;

function TPolygon.TPointsWrapper.ToArray: TArray<TVector2>;
begin
  Result := TArray<TVector2>.Create;
  Result.Add(GetEnumerator);
end;

{ TPolygon.TPointIterator }

function TPolygon.TPointIterator.GetCurrent: TVector2;
begin
  Result := FPolygon.Points[FCurrent];
end;

{ TPolygon.TSidesWrapper }

constructor TPolygon.TSidesWrapper.Create(APolygon: TPolygon);
begin
  FPolygon := APolygon;
end;

function TPolygon.TSidesWrapper.GetCount: Integer;
begin
  Result := FPolygon.Wrapper.m_count;
end;

function TPolygon.TSidesWrapper.GetEnumerator: IIterator<TLine2>;
begin
  Result := TSideIterator.Create(FPolygon);
end;

function TPolygon.TSidesWrapper.GetSide(AIndex: Integer): TLine2;
begin
  Result := FPolygon.Points[AIndex].LineTo(FPolygon.Points[AIndex mod Count]);
end;

function TPolygon.TSidesWrapper.ToArray: TArray<TLine2>;
begin
  Result := TArray<TLine2>.Create;
  Result.Add(GetEnumerator);
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

{ TPolygon.TSideIterator }

function TPolygon.TSideIterator.GetCurrent: TLine2;
begin
  Result := FPolygon.Sides[FCurrent];
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

{ TChain.TEdgesWrapper }

constructor TChain.TEdgesWrapper.Create(AChain: TChain);
begin
  FChain := AChain;
end;

function TChain.TEdgesWrapper.GetCount: Integer;
begin
  Result := FChain.Wrapper.m_count - 1;
end;

function TChain.TEdgesWrapper.GetEdge(AIndex: Integer): TLine2;
begin
  Result := FChain.Points[AIndex].LineTo(FChain.Points[AIndex + 1]);
end;

function TChain.TEdgesWrapper.GetEnumerator: IIterator<TLine2>;
begin
  Result := TEdgeIterator.Create(FChain);
end;

function TChain.TEdgesWrapper.ToArray: TArray<TLine2>;
begin
  Result := TArray<TLine2>.Create;
  Result.Add(GetEnumerator);
end;

{ TChain }

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

function TChain.GetEdges: TEdgesWrapper;
begin
  Result.Create(Self);
end;

function TChain.GetNext: TVector2;
begin
  Result := b2VecConv(Wrapper.m_nextVertex);
end;

function TChain.GetPoints: TPointsWrapper;
begin
  Result.Create(Self);
end;

function TChain.GetPrev: TVector2;
begin
  Result := b2VecConv(Wrapper.m_prevVertex);
end;

function TChain.HasNext: Boolean;
begin
  Result := Wrapper.m_hasNextVertex;
end;

function TChain.HasPrev: Boolean;
begin
  Result := Wrapper.m_hasPrevVertex;
end;

procedure TChain.SetChain(APoints: TArray<TVector2>);
begin
  Wrapper.CreateChain(APoints.DataPointer, APoints.Count);
end;

procedure TChain.SetChain(AFirst: PVector2; ACount: Integer);
begin
  Wrapper.CreateChain(Pointer(AFirst), ACount);
end;

procedure TChain.SetLoop(APoints: TArray<TVector2>);
begin
  Wrapper.CreateLoop(APoints.DataPointer, APoints.Count);
end;

procedure TChain.SetLoop(AFirst: PVector2; ACount: Integer);
begin
  Wrapper.CreateLoop(Pointer(AFirst), ACount);
end;

procedure TChain.SetNext(const Value: TVector2);
begin
  Wrapper.m_nextVertex := b2VecConv(Value);
end;

procedure TChain.SetPrev(const Value: TVector2);
begin
  Wrapper.m_prevVertex := b2VecConv(Value);
end;

{ TChain.TPointIterator }

function TChain.TPointIterator.GetCurrent: TVector2;
begin
  Result := FChain.Points[FCurrent];
end;

{ TChain.TEdgeIterator }

function TChain.TEdgeIterator.GetCurrent: TLine2;
begin
  Result := FChain.Edges[FCurrent];
end;

{ TChain.TPointsWrapper }

constructor TChain.TPointsWrapper.Create(AChain: TChain);
begin
  FChain := AChain;
end;

function TChain.TPointsWrapper.GetCount: Integer;
begin
  Result := FChain.Wrapper.m_count;
end;

function TChain.TPointsWrapper.GetEnumerator: IIterator<TVector2>;
begin
  Result := TPointIterator.Create(FChain);
end;

function TChain.TPointsWrapper.GetPoint(AIndex: Integer): TVector2;
begin
  Result := PVector2(FChain.Wrapper.m_vertices + AIndex)^;
end;

function TChain.TPointsWrapper.ToArray: TArray<TVector2>;
begin
  Result := TArray<TVector2>.Create;
  Result.Capacity := Count;
  Result.ForceCount(Result.Capacity);
  Move(FChain.Wrapper.m_vertices^, Result.DataPointer^, SizeOf(TVector2) * Result.Capacity);
end;

{ TWorld.TBodiesWrapper }

function TWorld.TBodiesWrapper.Add(ABodyDef: TBody.TDef): TBody;
begin
  Result := TBody.Create(FWorld, ABodyDef);
end;

function TWorld.TBodiesWrapper.Add(ABodyType: TBody.TType; APosition: TVector2): TBody;
begin
  Result := Add(TBody.TDef.Create(ABodyType, APosition));
end;

constructor TWorld.TBodiesWrapper.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

function TWorld.TBodiesWrapper.First: TBody;
var
  Body: b2BodyWrapper;
begin
  Body := FWorld.Wrapper.GetBodyList;
  if Body.FHandle = 0 then
    Exit(nil);
  Result := TBody(Body.GetUserData);
end;

function TWorld.TBodiesWrapper.GetEnumerator: IIterator<TBody>;
begin
  Result := TBodyIterator.Create(FWorld);
end;

function TWorld.TBodiesWrapper.ToArray: TRefArray<TBody>;
begin
  Result := TRefArray<TBody>.Create;
  Result.Add(GetEnumerator);
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

{ TWorld.TIterator }

constructor TWorld.TIterator.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

{ TCircle }

constructor TCircle.Create;
begin
  inherited;
  SetWrapper(b2CircleShapeWrapper.Create);
end;

constructor TCircle.Create(ARadius: Single; APosition: TVector2);
begin
  Create(ARadius);
  Position := APosition;
end;

constructor TCircle.Create(ARadius: Single);
begin
  Create;
  Radius := ARadius;
end;

destructor TCircle.Destroy;
begin
  Wrapper.Destroy;
  inherited;
end;

function TCircle.GetPosition: TVector2;
begin
  Result := b2VecConv(Wrapper.m_p);
end;

procedure TCircle.SetPosition(const Value: TVector2);
begin
  Wrapper.m_p := b2VecConv(Value);
end;

{ TBody.TDef }

constructor TBody.TDef.Create(AType: TType; APosition: TVector2);
begin
  inherited Create;
  FData := b2BodyDef.Create;
  &Type := AType;
  Position := APosition;
end;

function TBody.TDef.GetData: Pb2BodyDef;
begin
  Result := @FData;
end;

function TBody.TDef.GetLinearVelocity: TVector2;
begin
  Result := b2VecConv(FData.LinearVelocity);
end;

function TBody.TDef.GetPosition: TVector2;
begin
  Result := b2VecConv(FData.Position);
end;

function TBody.TDef.GetRotation: Single;
begin
  Result := RadToDeg(RotationRad);
end;

function TBody.TDef.GetType: TType;
begin
  Result := TType(FData.&Type);
end;

procedure TBody.TDef.SetLinearVelocity(const Value: TVector2);
begin
  FData.LinearVelocity := b2VecConv(Value);
end;

procedure TBody.TDef.SetPosition(const Value: TVector2);
begin
  FData.Position := b2VecConv(Value);
end;

procedure TBody.TDef.SetRotation(const Value: Single);
begin
  RotationRad := DegToRad(Value);
end;

procedure TBody.TDef.SetType(const Value: TType);
begin
  FData.&Type := b2BodyType(Value);
end;

{ TFixture.TDef }

constructor TFixture.TDef.Create(AShape: TShape);
begin
  inherited Create;
  FData := b2FixtureDef.Create;
  FData.Shape := AShape.Wrapper;
end;

function TFixture.TDef.GetData: Pb2FixtureDef;
begin
  Result := @FData;
end;

{ TWorld.TDestructionListener }

constructor TWorld.TDestructionListener.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

procedure TWorld.TDestructionListener.SayGoodbye(AFixture: Pb2Fixture);
begin
  // raise ENotImplemented.Create('Implicit destruction of fixtures.');
end;

procedure TWorld.TDestructionListener.SayGoodbye(AJoint: b2JointHandle);
begin
  // raise ENotImplemented.Create('Implicit destruction of joints.');
end;

{ TDefBase }

constructor TDefBase.Create;
begin
  FFreeAfterUse := True;
end;

{ TJoint<T> }

function TJoint<T>.GetWrapper: T;
begin
  Result := T(inherited Wrapper);
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

destructor TJoint.Destroy;
begin
  if World <> nil then
    World.Wrapper.DestroyJoint(Wrapper);
  inherited;
end;

function TJoint.GetActive: Boolean;
begin
  Result := Wrapper.IsActive;
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

function TJoint.GetNext: TJoint;
var
  Joint: b2JointWrapper;
begin
  Joint := Wrapper.GetNext;
  if Joint.FHandle = 0 then
    Exit(nil);
  Result := TJoint(Joint.GetUserData);
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

constructor TJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  ADef.Data.userData := Self;
  FWrapper := AWorld.Wrapper.CreateJoint(ADef.Data);;
  Assert(GetType = ADef.GetType, 'JointDef-Type and Object-Type must match.');
  FWorld := AWorld;
  if ADef.FreeAfterUse then
    ADef.Free;
end;

{ TJoint.TDef }

constructor TJoint.TDef.Create;
begin
  inherited;
end;

function TJoint.TDef.GetBodyA: TBody;
var
  Body: b2BodyWrapper;
begin
  Body := Data.BodyA;
  if Body.FHandle = 0 then
    Exit(nil);
  Result := TBody(Body.GetUserData);
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

function TJoint.TDef.GetCollideConnected: Boolean;
begin
  Result := Data.CollideConnected;
end;

function TJoint.TDef.GetType: TType;
begin
  Result := TType(Data.&Type);
end;

procedure TJoint.TDef.SetBodyA(const Value: TBody);
begin
  Data.BodyA := Value.Wrapper;
end;

procedure TJoint.TDef.SetBodyB(const Value: TBody);
begin
  Data.BodyB := Value.Wrapper;
end;

procedure TJoint.TDef.SetCollideConnected(const Value: Boolean);
begin
  Data.CollideConnected := Value;
end;

{ TJoint.TDef<T> }

constructor TJoint.TDef<T>.Create;
begin
  inherited;
  FData := CreateData;
end;

function TJoint.TDef<T>.GetData: Pb2JointDef;
begin
  Result := @FData;
end;

function TJoint.TDef<T>.GetDataTyped: PT;
begin
  Result := PT(GetData);
end;

{ TDistanceJoint.TDef }

constructor TDistanceJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchorA, AAnchorB: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchorA), b2VecConv(AAnchorB));
end;

function TDistanceJoint.TDef.CreateData: b2DistanceJointDef;
begin
  Result := b2DistanceJointDef.Create;
end;

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

{ TWorld.TJointsWrapper }

function TWorld.TJointsWrapper.Add(ADef: TJoint.TDef): TJoint;
begin
  Result := JointClasses[ADef.GetType].Create(FWorld, ADef);
end;

constructor TWorld.TJointsWrapper.Create(AWorld: TWorld);
begin
  FWorld := AWorld;
end;

function TWorld.TJointsWrapper.First: TJoint;
var
  Joint: b2JointWrapper;
begin
  Joint := FWorld.Wrapper.GetJointList;
  if Joint.FHandle = 0 then
    Exit(nil);
  Result := TJoint(Joint.GetUserData);
end;

function TWorld.TJointsWrapper.GetEnumerator: IIterator<TJoint>;
begin
  Result := TJoint.TIterator.Create(FWorld.Joints.First);
end;

function TWorld.TJointsWrapper.ToArray: TRefArray<TJoint>;
begin
  Result := TRefArray<TJoint>.Create;
  Result.Add(GetEnumerator);
end;

{ TJoint.TIterator }

constructor TJoint.TIterator.Create(AFirst: TJoint);
begin
  FNext := AFirst;
end;

function TJoint.TIterator.GetCurrent: TJoint;
begin
  Result := FCurrent;
end;

function TJoint.TIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FCurrent <> nil;
  if Result then
    FNext := FNext.GetNext;
end;

{ TBody.TJointsWrapper }

constructor TBody.TJointsWrapper.Create(ABody: TBody);
begin
  FBody := ABody;
end;

function TBody.TJointsWrapper.First: TJoint;
var
  Joint: Pb2JointEdge;
begin
  Joint := FBody.Wrapper.GetJointList;
  if Joint = nil then
    Exit(nil);
  Result := TJoint(b2JointWrapper(Joint.Joint).GetUserData);
end;

function TBody.TJointsWrapper.GetEnumerator: IIterator<TJoint>;
begin
  Result := TJointIterator.Create(FBody.Wrapper.GetJointList);
end;

function TBody.TJointsWrapper.ToArray: TRefArray<TJoint>;
begin
  Result := TRefArray<TJoint>.Create;
  Result.Add(GetEnumerator);
end;

{ TBody.TJointIterator }

constructor TBody.TJointIterator.Create(AJointEdge: Pb2JointEdge);
begin
  FNext := AJointEdge;
end;

function TBody.TJointIterator.GetCurrent: TJoint;
begin
  Result := TJoint(b2JointWrapper(FCurrent.Joint).GetUserData);
end;

function TBody.TJointIterator.MoveNext: Boolean;
begin
  FCurrent := FNext;
  Result := FCurrent <> nil;
  if Result then
    FNext := FNext.Next;
end;

{ TDistanceJoint }

constructor TDistanceJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

function TDistanceJoint.GetDampingRatio: Single;
begin
  Result := Wrapper.GetDampingRatio;
end;

function TDistanceJoint.GetFrequency: Single;
begin
  Result := Wrapper.GetFrequency;
end;

function TDistanceJoint.GetLength: Single;
begin
  Result := Wrapper.GetLength;
end;

function TDistanceJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TDistanceJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

procedure TDistanceJoint.SetDampingRatio(const Value: Single);
begin
  Wrapper.SetDampingRatio(Value);
end;

procedure TDistanceJoint.SetFrequency(const Value: Single);
begin
  Wrapper.SetFrequency(Value);
end;

procedure TDistanceJoint.SetLength(const Value: Single);
begin
  Wrapper.SetLength(Value);
end;

{ TRevoluteJoint.TDef }

constructor TRevoluteJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor));
end;

function TRevoluteJoint.TDef.CreateData: b2RevoluteJointDef;
begin
  Result := b2RevoluteJointDef.Create;
end;

function TRevoluteJoint.TDef.GetAnchorA: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorA);
end;

function TRevoluteJoint.TDef.GetAnchorB: TVector2;
begin
  Result := b2VecConv(Data.LocalAnchorB);
end;

function TRevoluteJoint.TDef.GetLimits: TBounds1;
begin
  Result.Create(LowerAngle, UpperAngle);
end;

function TRevoluteJoint.TDef.GetLimitsRad: TBounds1;
begin
  Result.Create(LowerAngleRad, UpperAngleRad);
end;

function TRevoluteJoint.TDef.GetLowerAngle: Single;
begin
  Result := RadToDeg(LowerAngleRad);
end;

function TRevoluteJoint.TDef.GetUpperAngle: Single;
begin
  Result := RadToDeg(UpperAngleRad);
end;

procedure TRevoluteJoint.TDef.SetAnchorA(const Value: TVector2);
begin
  Data.LocalAnchorA := b2VecConv(Value);
end;

procedure TRevoluteJoint.TDef.SetAnchorB(const Value: TVector2);
begin
  Data.LocalAnchorB := b2VecConv(Value);
end;

procedure TRevoluteJoint.TDef.SetLimits(const Value: TBounds1);
begin
  LowerAngle := Value.C1;
  UpperAngle := Value.C2;
end;

procedure TRevoluteJoint.TDef.SetLimitsRad(const Value: TBounds1);
begin
  LowerAngleRad := Value.C1;
  UpperAngleRad := Value.C2;
end;

procedure TRevoluteJoint.TDef.SetLowerAngle(const Value: Single);
begin
  LowerAngleRad := DegToRad(Value);
end;

procedure TRevoluteJoint.TDef.SetUpperAngle(const Value: Single);
begin
  UpperAngleRad := DegToRad(Value);
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

constructor TPrismaticJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor, AAxis: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor), b2VecConv(AAxis));
end;

function TPrismaticJoint.TDef.CreateData: b2PrismaticJointDef;
begin
  Result := b2PrismaticJointDef.Create;
end;

function TPrismaticJoint.TDef.GetAxis: TVector2;
begin
  Result := b2VecConv(Data.localAxisA);
end;

function TPrismaticJoint.TDef.GetLimits: TBounds1;
begin
  Result.Create(LowerTranslation, UpperTranslation);
end;

procedure TPrismaticJoint.TDef.SetAxis(const Value: TVector2);
begin
  Data.localAxisA := b2VecConv(Value);
end;

procedure TPrismaticJoint.TDef.SetLimits(const Value: TBounds1);
begin
  LowerTranslation := Value.C1;
  UpperTranslation := Value.C2;
end;

{ TPrismaticJoint }

constructor TPrismaticJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

function TPrismaticJoint.GetAxis: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAxisA^);
end;

function TPrismaticJoint.GetEnableLimit: Boolean;
begin
  Result := Wrapper.IsLimitEnabled;
end;

function TPrismaticJoint.GetEnableMotor: Boolean;
begin
  Result := Wrapper.IsMotorEnabled;
end;

function TPrismaticJoint.GetLimits: TBounds1;
begin
  Result.Create(LowerLimit, UpperLimit);
end;

function TPrismaticJoint.GetLocalAnchorA: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorA^);
end;

function TPrismaticJoint.GetLocalAnchorB: TVector2;
begin
  Result := b2VecConv(Wrapper.GetLocalAnchorB^);
end;

function TPrismaticJoint.GetLowerLimit: Single;
begin
  Result := Wrapper.GetLowerLimit;
end;

function TPrismaticJoint.GetMaxMotorForce: Single;
begin
  Result := Wrapper.GetMaxMotorForce;
end;

function TPrismaticJoint.GetMotorSpeed: Single;
begin
  Result := Wrapper.GetMotorSpeed;
end;

function TPrismaticJoint.GetReferenceAngle: Single;
begin
  Result := Wrapper.GetReferenceAngle;
end;

function TPrismaticJoint.GetUpperLimit: Single;
begin
  Result := Wrapper.GetUpperLimit;
end;

procedure TPrismaticJoint.SetEnableLimit(const Value: Boolean);
begin
  Wrapper.EnableLimit(Value);
end;

procedure TPrismaticJoint.SetEnableMotor(const Value: Boolean);
begin
  Wrapper.EnableMotor(Value);
end;

procedure TPrismaticJoint.SetLimits(const Value: TBounds1);
begin
  Wrapper.SetLimits(Value.C1, Value.C2);
end;

procedure TPrismaticJoint.SetLowerLimit(const Value: Single);
begin
  Wrapper.SetLimits(Value, UpperLimit);
end;

procedure TPrismaticJoint.SetMaxMotorForce(const Value: Single);
begin
  Wrapper.SetMaxMotorForce(Value);
end;

procedure TPrismaticJoint.SetMotorSpeed(const Value: Single);
begin
  Wrapper.SetMotorSpeed(Value);
end;

procedure TPrismaticJoint.SetUpperLimit(const Value: Single);
begin
  Wrapper.SetLimits(LowerLimit, Value);
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

function TPulleyJoint.TDef.CreateData: b2PulleyJointDef;
begin
  Result := b2PulleyJointDef.Create;
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

constructor TPulleyJoint.TDef.Create(ABodyA, ABodyB: TBody;
  AGroundAnchorA, AGroundAnchorB, AAnchorA, AAnchorB: TVector2; ARatio: Single);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper,
    b2VecConv(AGroundAnchorA), b2VecConv(AGroundAnchorB), b2VecConv(AAnchorA), b2VecConv(AAnchorB), ARatio);
end;

{ TPulleyJoint }

constructor TPulleyJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

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

{ TGearJoint.TDef }

constructor TGearJoint.TDef.Create(AJoint1, AJoint2: TJoint; ARatio: Single);
begin
  inherited Create;
  BodyA := AJoint1.BodyB;
  BodyB := AJoint2.BodyB;
  Joint1 := AJoint1;
  Joint2 := AJoint2;
  Ratio := ARatio;
end;

function TGearJoint.TDef.CreateData: b2GearJointDef;
begin
  Result := b2GearJointDef.Create;
end;

function TGearJoint.TDef.GetJoint1: TJoint;
begin
  Result := TJoint(b2JointWrapper(Data.Joint1).GetUserData);
end;

function TGearJoint.TDef.GetJoint2: TJoint;
begin
  Result := TJoint(b2JointWrapper(Data.Joint2).GetUserData);
end;

procedure TGearJoint.TDef.SetJoint1(const Value: TJoint);
begin
  Data.Joint1 := Value.Wrapper;
end;

procedure TGearJoint.TDef.SetJoint2(const Value: TJoint);
begin
  Data.Joint2 := Value.Wrapper;
end;

{ TGearJoint }

constructor TGearJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

function TGearJoint.GetBodyA: TBody;
begin
  Result := TBody(b2BodyWrapper(Wrapper.GetBodyA()).GetUserData);
end;

function TGearJoint.GetBodyB: TBody;
begin
  Result := TBody(b2BodyWrapper(Wrapper.GetBodyB()).GetUserData);
end;

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

{ TMouseJoint.TDef }

constructor TMouseJoint.TDef.Create(ABodyA, ABodyB: TBody; APosition: TVector2);
begin
  inherited Create;
  BodyA := ABodyA;
  BodyB := ABodyB;
  Position := APosition;
  CollideConnected := True;
end;

function TMouseJoint.TDef.CreateData: b2MouseJointDef;
begin
  Result := b2MouseJointDef.Create;
end;

function TMouseJoint.TDef.GetPosition: TVector2;
begin
  Result := b2VecConv(Data.target);
end;

procedure TMouseJoint.TDef.SetPosition(const Value: TVector2);
begin
  Data.target := b2VecConv(Value);
end;

{ TMouseJoint }

constructor TMouseJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
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

function TMouseJoint.GetPosition: TVector2;
begin
  Result := b2VecConv(Wrapper.GetTarget^);
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

procedure TMouseJoint.SetPosition(const Value: TVector2);
begin
  Wrapper.SetTarget(b2VecConv(Value));
end;

{ TWheelJoint.TDef }

constructor TWheelJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor, AAxis: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor), b2VecConv(AAxis));
end;

function TWheelJoint.TDef.CreateData: b2WheelJointDef;
begin
  Result := b2WheelJointDef.Create;
end;

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

{ TWheelJoint }

constructor TWheelJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

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

function TWheelJoint.MotorTorque(AInvDeltaTime: Single): Single;
begin
  Result := Wrapper.GetMotorTorque(AInvDeltaTime);
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

{ TWeldJoint.TDef }

constructor TWeldJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper, b2VecConv(AAnchor));
end;

function TWeldJoint.TDef.CreateData: b2WeldJointDef;
begin
  Result := b2WeldJointDef.Create;
end;

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

{ TWeldJoint }

constructor TWeldJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

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

{ TRopeJoint.TDef }

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

function TRopeJoint.TDef.CreateData: b2RopeJointDef;
begin
  Result := b2RopeJointDef.Create;
end;

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

{ TRopeJoint }

constructor TRopeJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

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

{ TFrictionJoint.TDef }

constructor TFrictionJoint.TDef.Create(ABodyA, ABodyB: TBody; AAnchor: TVector2);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyA.Wrapper, b2VecConv(AAnchor));
end;

function TFrictionJoint.TDef.CreateData: b2FrictionJointDef;
begin
  Result := b2FrictionJointDef.Create;
end;

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

{ TFrictionJoint }

constructor TFrictionJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

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

{ TMotorJoint.TDef }

constructor TMotorJoint.TDef.Create(ABodyA, ABodyB: TBody);
begin
  inherited Create;
  Data.Initialize(ABodyA.Wrapper, ABodyB.Wrapper);
end;

function TMotorJoint.TDef.CreateData: b2MotorJointDef;
begin
  Result := b2MotorJointDef.Create;
end;

function TMotorJoint.TDef.GetLinearOffset: TVector2;
begin
  Result := b2VecConv(Data.LinearOffset);
end;

procedure TMotorJoint.TDef.SetLinearOffset(const Value: TVector2);
begin
  Data.LinearOffset := b2VecConv(Value);
end;

{ TMotorJoint }

constructor TMotorJoint.Create(AWorld: TWorld; ADef: TDef);
begin
  inherited Create(AWorld, ADef);
end;

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

end.
