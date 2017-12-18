unit Pengine.GLState;

interface

uses          
  dglOpenGL, 

  System.SysUtils,
  {$IFDEF DEBUG}
  System.RTTI,
  {$ENDIF}

  Pengine.Interfaces,
  Pengine.GLEnums,
  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Color,
  Pengine.Equaller,
  Pengine.TimeManager,
  Pengine.ResourceManager,
  Pengine.Hasher;

type

  TGLObjectType = (
    otTexture = GL_TEXTURE,

    otVertexArray = GL_VERTEX_ARRAY,

    otBuffer = GL_BUFFER,
    otShader,
    otProgram,
    otQuery,
    otProgramPipeline,

    otSampler = GL_SAMPLER,
    otDisplayList,

    otFramebuffer = GL_FRAMEBUFFER,
    otRenderbuffer,

    otTransformFeedback = GL_TRANSFORM_FEEDBACK
  );

  TGLState = class;

  TGLObjectParam = class(TResourceParameter)
  private
    FGLState: TGLState;

  protected
    function GetHash: Cardinal; override;
    function Equals(AOther: TResourceParameter): Boolean; override;

  public
    constructor Create(AGLState: TGLState);

    property GLState: TGLState read FGLState;
    
  end;

  TGLObjectBase = class abstract(TInterfaceBase)
  private
    FGLLabel: AnsiString;
    FGLState: TGLState;


  protected
    function GetGLNameUInt: GLuint; virtual; abstract;

    procedure SetGLLabel(const Value: AnsiString); virtual;

  public
    constructor Create(AGLState: TGLState);

    class function GetObjectType: TGLObjectType; virtual; abstract;

    procedure Bind; virtual; abstract;
    procedure Unbind; virtual; abstract;

    property GLState: TGLState read FGLState;
    property GLLabel: AnsiString read FGLLabel write SetGLLabel;

  end;

  TGLObjectBaseClass = class of TGLObjectBase;

  TGLObject = class(TGLObjectBase)
  private
    FGLName: GLuint;

  protected
    procedure GenObject(out AGLName: GLuint); virtual; abstract;
    procedure DeleteObject(const AGLName: GLuint); virtual; abstract;

    function GetGLNameUInt: Cardinal; override;

    procedure SetGLLabel(const Value: AnsiString); override;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    property GLName: GLuint read FGLName;

  end;

  TGLHandleObject = class(TGLObjectBase)
  private
    FGLName: GLHandle;

  protected
    procedure GenObject(out AGLName: GLHandle); virtual; abstract;
    procedure DeleteObject(const AGLName: GLHandle); virtual; abstract;

    function GetGLNameUInt: Cardinal; override;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    property GLName: GLHandle read FGLName;

  end;

  TGLSingleState = class abstract
  public type
    // TODO: XmlDoc
    TType = (
      // Boolean
      stDepthTest,
      stSeamlessCubemap,
      stDepthClamp,
      stBlend,
      stCullFace,
      stDebugOutput,
      stDebugOutputSynced,
      // Still Boolean but with custom function
      stDepthMask,

      // TColorRGBA
      stClearColor,

      // TGLCompareFunction
      stDepthFunc,

      // TGLCullFace
      stCullFaceMode,

      // TGLBlendFunc
      stBlendFunc
      );

    TTypes = set of TType;

    TBooleanType = stDepthTest .. stDepthMask;
    TColorRGBAType = stClearColor .. stClearColor;
    TCompareFuncType = stDepthFunc .. stDepthFunc;
    TCullFaceType = stCullFaceMode .. stCullFaceMode;
    TBlendFuncType = stBlendFunc .. stBlendFunc;

  protected
    procedure Assign(AState: TGLSingleState); virtual; abstract;
    
  public 
    procedure SendState; virtual; abstract;

    function GetType: TType; virtual; abstract;

    function Copy: TGLSingleState;
    
  end;

  TGLSingleStateClass = class of TGLSingleState;

  // TODO: XmlDoc
  TGLSingleState<T> = class abstract(TGLSingleState)
  private
    FState: T;

  protected       
    procedure SetState(const Value: T); virtual; abstract;

    procedure Assign(AState: TGLSingleState); override;

  public
    constructor Create(const APengineDefault, AGLDefault: T); overload;
    constructor Create(const ADefault: T); overload;

    property State: T read FState write SetState;

  end;

  TGLSingleState<T; E: TEqualler<T>> = class abstract(TGLSingleState<T>)
  protected
    procedure SetState(const Value: T); override;
  end;

  // glEnable/glDisable
  TGLFlagState = class(TGLSingleState<Boolean, TBoolEqualler>)
  private
    FType: TGLSingleState.TType;
    FFlag: TGLenum;

  protected
    procedure Assign(AState: TGLSingleState); override;

  public
    constructor Create(AType: TGLSingleState.TType; AFlag: TGLenum; APengineDefault: Boolean; AGLDefault: Boolean = False);

    procedure SendState; override;

    function GetType: TGLSingleState.TType; override;

  end;

  TGLClearColorState = class(TGLSingleState<TColorRGBA, TColorRGBAEqualler>)
  public
    procedure SendState; override;
    function GetType: TGLSingleState.TType; override;
  end;

  TGLDepthMaskState = class(TGLSingleState<Boolean, TBoolEqualler>)
  public
    procedure SendState; override;
    function GetType: TGLSingleState.TType; override;
  end;

  TGLEnumState<T> = class(TGLSingleState<T, TEnumEqualler<T>>);

  TGLDepthFuncState = class(TGLEnumState<TGLCompareFunction>)
  public
    procedure SendState; override;
    function GetType: TGLSingleState.TType; override;
  end;

  TGLCullFaceState = class(TGLEnumState<TGLCullFace>)
  public
    procedure SendState; override;
    function GetType: TGLSingleState.TType; override;
  end;

  TGLBlendFuncState = class(TGLEnumState<TGLBlendFunc>)
  public
    procedure SendState; override;
    function GetType: TGLSingleState.TType; override;
  end;

  TGLStateRevertSet = class
  public type
    TStates = TObjectArray<TGLSingleState>;

  private
    FStates: TStates;
    FChangedStates: TGLSingleState.TTypes;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Save(AState: TGLSingleState);

  end;

  TGLObjectBinding = class
  private
    FBoundObject: TGLObjectBase;

  public
    property BoundObject: TGLObjectBase read FBoundObject write FBoundObject;

  end;

  TBoundGLObjects = class
  public type
    TBindingMap = TClassMap<TGLObjectBase>;

  private
    FBindings: TBindingMap;
    function GetBinding(AObjectType: TGLObjectBaseClass): TGLObjectBinding;
    function GetBindings: TBindingMap.TReader;

  public
    constructor Create;
    destructor Destroy; override;

    property Bindings[AObjectType: TGLObjectBaseClass]: TGLObjectBinding read GetBinding;

  end;

  // TODO: XmlDoc
  TGLState = class
  public type
    // TODO: XmlDoc
    TAllStates = array [TGLSingleState.TType] of TGLSingleState;
    TChangeStack = TObjectStack<TGLStateRevertSet>;

  private
    FStates: TAllStates;
    FChangeStack: TChangeStack;
    FTimer: TDeltaTimer;

    procedure InitStates;

    function GetState<T>(AState: TGLSingleState.TType): T; overload;
    procedure SetState<T>(AState: TGLSingleState.TType; const Value: T); overload;

    function GetState(AState: TGLSingleState.TBooleanType): Boolean; overload;
    procedure SetState(AState: TGLSingleState.TBooleanType; const Value: Boolean); overload;

    function GetState(AState: TGLSingleState.TColorRGBAType): TColorRGBA; overload;
    procedure SetState(AState: TGLSingleState.TColorRGBAType; const Value: TColorRGBA); overload;

    function GetState(AState: TGLSingleState.TCompareFuncType): TGLCompareFunction; overload;
    procedure SetState(AState: TGLSingleState.TCompareFuncType; const Value: TGLCompareFunction); overload;
                                  
    function GetState(AState: TGLSingleState.TCullFaceType): TGLCullFace; overload;
    procedure SetState(AState: TGLSingleState.TCullFaceType; const Value: TGLCullFace); overload;

    function GetState(AState: TGLSingleState.TBlendFuncType): TGLBlendFunc; overload;
    procedure SetState(AState: TGLSingleState.TBlendFuncType; const Value: TGLBlendFunc); overload;
    function GetDeltaTime: Single;
    function GetResourceParam: TGLObjectParam;

  public
    constructor Create(ATimer: TDeltaTimer);
    destructor Destroy; override;

    procedure Push;
    procedure Pop;

    property State[AState: TGLSingleState.TBooleanType]: Boolean read GetState write SetState; default;
    property State[AState: TGLSingleState.TColorRGBAType]: TColorRGBA read GetState write SetState; default;
    property State[AState: TGLSingleState.TCompareFuncType]: TGLCompareFunction read GetState write SetState; default;
    property State[AState: TGLSingleState.TCullFaceType]: TGLCullFace read GetState write SetState; default;
    property State[AState: TGLSingleState.TBlendFuncType]: TGLBlendFunc read GetState write SetState; default;

    property DeltaTime: Single read GetDeltaTime;

    property ResourceParam: TGLObjectParam read GetResourceParam;

  end;

implementation

{ TGLContextState }

procedure TGLState.InitStates;
{$IFDEF DEBUG}
var
  S: TGLSingleState.TType;
  Uninitialized: string;
{$ENDIF}
begin
  // Flag (glEnable/glDisable)
  FStates[stDepthTest] := TGLFlagState.Create(stDepthTest, GL_DEPTH_TEST, True);
  FStates[stSeamlessCubemap] := TGLFlagState.Create(stSeamlessCubemap, GL_TEXTURE_CUBE_MAP_SEAMLESS, True);
  FStates[stDepthClamp] := TGLFlagState.Create(stDepthClamp, GL_DEPTH_CLAMP, True);
  FStates[stBlend] := TGLFlagState.Create(stBlend, GL_BLEND, False);
  FStates[stCullFace] := TGLFlagState.Create(stCullFace, GL_CULL_FACE, True);
  FStates[stDebugOutput] := TGLFlagState.Create(stDebugOutput, GL_DEBUG_OUTPUT, False);
  FStates[stDebugOutputSynced] := TGLFlagState.Create(stDebugOutputSynced, GL_DEBUG_OUTPUT_SYNCHRONOUS, False);
  // Other booleans (using their own separate functions)
  FStates[stDepthMask] := TGLDepthMaskState.Create(True);
  // TColorRGBA
  FStates[stClearColor] := TGLClearColorState.Create(ColorTransparent);
  // GLEnums
  FStates[stDepthFunc] := TGLDepthFuncState.Create(cfLess);
  FStates[stCullFaceMode] := TGLCullFaceState.Create(cfBack);
  FStates[stBlendFunc] := TGLBlendFuncState.Create(TGLBlendFunc.Make);

  {$IFDEF DEBUG}
  Uninitialized := '';
  for S := Low(TGLSingleState.TType) to High(TGLSingleState.TType) do
  begin
    if FStates[S] = nil then
    begin
      if Uninitialized = '' then
        Uninitialized := TRttiEnumerationType.GetName(S)
      else      
        Uninitialized := Uninitialized + ', ' + TRttiEnumerationType.GetName(S);
    end;
  end;
  if Uninitialized <> '' then
    raise ENotImplemented.CreateFmt('Not initialized OpenGL-States: %s', [Uninitialized]);
  {$ENDIF}
end;

function TGLState.GetState(AState: TGLSingleState.TCompareFuncType): TGLCompareFunction;
begin
  Result := GetState<TGLCompareFunction>(AState);
end;

function TGLState.GetState(AState: TGLSingleState.TCullFaceType): TGLCullFace;
begin
  Result := GetState<TGLCullFace>(AState);
end;

function TGLState.GetDeltaTime: Single;
begin
  Result := FTimer.DeltaTime;
end;

function TGLState.GetResourceParam: TGLObjectParam;
begin
  Result := TGLObjectParam.Create(Self);
end;

function TGLState.GetState(AState: TGLSingleState.TBlendFuncType): TGLBlendFunc;
begin
  Result := GetState<TGLBlendFunc>(AState);
end;

function TGLState.GetState<T>(AState: TGLSingleState.TType): T;
begin
  Result := TGLSingleState<T>(FStates[AState]).State;
end;

procedure TGLState.SetState(AState: TGLSingleState.TCompareFuncType; const Value: TGLCompareFunction);
begin
  SetState<TGLCompareFunction>(AState, Value);
end;

procedure TGLState.SetState(AState: TGLSingleState.TCullFaceType; const Value: TGLCullFace);
begin
  SetState<TGLCullFace>(AState, Value);
end;

procedure TGLState.SetState(AState: TGLSingleState.TBlendFuncType; const Value: TGLBlendFunc);
begin                                 
  SetState<TGLBlendFunc>(AState, Value);
end;

procedure TGLState.SetState<T>(AState: TGLSingleState.TType; const Value: T);
begin
  if not FChangeStack.Empty then
    FChangeStack.Top.Save(FStates[AState]);
  TGLSingleState<T>(FStates[AState]).State := Value
end;

function TGLState.GetState(AState: TGLSingleState.TBooleanType): Boolean;
begin
  Result := GetState<Boolean>(AState);
end;

procedure TGLState.SetState(AState: TGLSingleState.TBooleanType; const Value: Boolean);
begin
  SetState<Boolean>(AState, Value);
end;

function TGLState.GetState(AState: TGLSingleState.TColorRGBAType): TColorRGBA;
begin
  Result := GetState<TColorRGBA>(AState);
end;

procedure TGLState.SetState(AState: TGLSingleState.TColorRGBAType; const Value: TColorRGBA);
begin
  SetState<TColorRGBA>(AState, Value);
end;

constructor TGLState.Create(ATimer: TDeltaTimer);
begin
  FTimer := ATimer;
  InitStates;
  FChangeStack := TChangeStack.Create(True);
end;

destructor TGLState.Destroy;
var
  S: TGLSingleState;
begin
  for S in FStates do
    S.Free;
  FChangeStack.Free;
  inherited;
end;

procedure TGLState.Push;
begin
  FChangeStack.Push(TGLStateRevertSet.Create);
end;

procedure TGLState.Pop;
begin
  FChangeStack.Pop;
end;

{ TGLFlagState }

procedure TGLFlagState.Assign(AState: TGLSingleState);
begin
  inherited;
  FType := TGLFlagState(AState).FType;
  FFlag := TGLFlagState(AState).FFlag;
end;

constructor TGLFlagState.Create(AType: TGLSingleState.TType; AFlag: TGLenum; APengineDefault: Boolean; AGLDefault: Boolean = False);
begin
  FType := AType;
  FFlag := AFlag;
  inherited Create(APengineDefault, AGLDefault);
end;

function TGLFlagState.GetType: TGLSingleState.TType;
begin
  Result := FType;
end;

procedure TGLFlagState.SendState;
begin
  if State then
    glEnable(FFlag)
  else
    glDisable(FFlag);
end;

{ TGLState<T, E> }

procedure TGLSingleState<T, E>.SetState(const Value: T);
begin
  if E.Equal(FState, Value) then
    Exit;
  FState := Value;
  SendState;
end;

{ TGLClearColorState }

function TGLClearColorState.GetType: TGLSingleState.TType;
begin
  Result := stClearColor;
end;

procedure TGLClearColorState.SendState;
begin
  glClearColor(State.R, State.G, State.B, State.A);
end;

{ TGLState<T> }

constructor TGLSingleState<T>.Create(const APengineDefault, AGLDefault: T);
begin
  FState := AGLDefault;
  State := APengineDefault;
end;

procedure TGLSingleState<T>.Assign(AState: TGLSingleState);
begin
  FState := TGLSingleState<T>(AState).FState;
end;

constructor TGLSingleState<T>.Create(const ADefault: T);
begin
  FState := ADefault;
end;

{ TGLDepthMaskState }

function TGLDepthMaskState.GetType: TGLSingleState.TType;
begin
  Result := stDepthMask;
end;

procedure TGLDepthMaskState.SendState;
begin
  glDepthMask(State);
end;

{ DepthFunc }

function TGLDepthFuncState.GetType: TGLSingleState.TType;
begin
  Result := stDepthFunc;
end;

procedure TGLDepthFuncState.SendState;
begin
  glDepthFunc(Ord(State));
end;

{ CullFace }

function TGLCullFaceState.GetType: TGLSingleState.TType;
begin
  Result := stCullFace;
end;

procedure TGLCullFaceState.SendState;
begin
  glCullFace(Ord(State));
end;

{ BlendFunc }

function TGLBlendFuncState.GetType: TGLSingleState.TType;
begin
  Result := stBlendFunc;
end;

procedure TGLBlendFuncState.SendState;
begin
  glBlendFunc(Ord(State.Src), Ord(State.Dest));
end;

{ TGLChangeSet }

constructor TGLStateRevertSet.Create;
begin
  FStates := TStates.Create(True);
end;

destructor TGLStateRevertSet.Destroy;
var
  State: TGLSingleState;
begin
  for State in FStates do
    State.SendState;
  FStates.Free;
  inherited;
end;

procedure TGLStateRevertSet.Save(AState: TGLSingleState);
begin
  if not (AState.GetType in FChangedStates) then
    FStates.Add(AState.Copy);
end;

{ TGLSingleState }

function TGLSingleState.Copy: TGLSingleState;
begin
  Result := TGLSingleStateClass(ClassType).Create;
  Result.Assign(Self);
end;

{ TGLObjectParam }

constructor TGLObjectParam.Create(AGLState: TGLState);
begin
  FGLState := AGLState;
end;

function TGLObjectParam.Equals(AOther: TResourceParameter): Boolean;
begin
  Result := inherited and (FGLState = TGLObjectParam(AOther).FGLState);
end;

function TGLObjectParam.GetHash: Cardinal;
begin
  Result := inherited xor TRefHasher<TGLState>.GetHash(FGLState);
end;

{ TGLUIntObject }

constructor TGLObject.Create(AGLState: TGLState);
begin
  inherited;
  GenObject(FGLName);
end;

destructor TGLObject.Destroy;
begin
  DeleteObject(FGLName);
  inherited;
end;

function TGLObject.GetGLNameUInt: Cardinal;
begin
  Result := GLName;
end;

procedure TGLObject.SetGLLabel(const Value: AnsiString);
begin
  Bind;
  inherited;
end;

{ TGLHandleObject }

constructor TGLHandleObject.Create(AGLState: TGLState);
begin
  inherited;
  GenObject(FGLName);
end;

destructor TGLHandleObject.Destroy;
begin
  DeleteObject(FGLName);
  inherited;
end;

function TGLHandleObject.GetGLNameUInt: Cardinal;
begin
  Result := GLName;
end;

{ TGLObjectBase }

constructor TGLObjectBase.Create(AGLState: TGLState);
begin
  FGLState := AGLState;
end;

procedure TGLObjectBase.SetGLLabel(const Value: AnsiString);
begin
  FGLLabel := Value;
  if Value = '' then
    glObjectLabel(Ord(GetObjectType), GetGLNameUInt, -1, nil)
  else
    glObjectLabel(Ord(GetObjectType), GetGLNameUInt, Length(Value), @Value[1]);
end;

{ TBoundGLObjects }

constructor TBoundGLObjects.Create;
begin
  FBindings := TBindingMap.Create;
end;

destructor TBoundGLObjects.Destroy;
begin
  FBindings.Free;
  inherited;
end;

function TBoundGLObjects.GetBinding(AObjectType: TGLObjectBaseClass): TGLObjectBinding;
begin
  if not FBindings.Get(AObjectType, Result) then
  begin

  end;
end;

function TBoundGLObjects.GetBindings: TBindingMap.TReader;
begin
  Result := FBindings.Reader;
end;

end.
