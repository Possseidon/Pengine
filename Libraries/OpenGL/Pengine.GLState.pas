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
  Pengine.Hasher,
  Pengine.IntMaths,
  Pengine.EventHandling;

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
    constructor Create; overload; override;
    constructor Create(AGLState: TGLState); reintroduce; overload;

    function GetHash: Cardinal; override;
    function Equals(AOther: TResourceParameter): Boolean; override;

  public
    procedure Assign(AResourceParameter: TResourceParameter); override;

    property GLState: TGLState read FGLState;

  end;

  TGLObjectBaseClass = class of TGLObjectBase;

  TGLObjectBinding = class;
  TGLObjectBindingClass = class of TGLObjectBinding;

  TGLObjectBase = class abstract(TInterfaceBase)
  private
    FGLLabel: AnsiString;
    FGLState: TGLState;
    FBinding: TGLObjectBinding;

  protected
    constructor Create(AGLState: TGLState);

    function GetGLNameUInt: GLuint; virtual; abstract;

    procedure SetGLLabel(const Value: AnsiString); virtual;

    procedure BindGLObject; virtual; abstract;
    procedure UnbindGLObject; virtual; abstract;

    property Binding: TGLObjectBinding read FBinding;

  public
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; virtual; abstract;
    class function GetBindingClass: TGLObjectBindingClass; virtual; abstract;

    procedure Bind;
    procedure Unbind;

    function Bound: Boolean;

    property GLState: TGLState read FGLState;
    property GLLabel: AnsiString read FGLLabel write SetGLLabel;

  end;

  TGLObject = class abstract(TGLObjectBase)
  private
    FGLName: GLuint;

  protected
    constructor Create(AGLState: TGLState);

    procedure GenObject(out AGLName: GLuint); virtual; abstract;
    procedure DeleteObject(const AGLName: GLuint); virtual; abstract;

    function GetGLNameUInt: Cardinal; override;

    procedure SetGLLabel(const Value: AnsiString); override;

  public
    destructor Destroy; override;

    property GLName: GLuint read FGLName;

  end;

  TGLHandleObject = class abstract(TGLObjectBase)
  private
    FGLName: GLHandle;

  protected
    constructor Create(AGLState: TGLState);

    procedure GenObject(out AGLName: GLHandle); virtual; abstract;
    procedure DeleteObject(const AGLName: GLHandle); virtual; abstract;

    function GetGLNameUInt: Cardinal; override;

  public
    destructor Destroy; override;

    property GLName: GLHandle read FGLName;

  end;

  TGLObjectBinding = class abstract
  private
    FBoundObject: TGLObjectBase;

  protected
    procedure SetBoundObject(const Value: TGLObjectBase); virtual;

    constructor Create; virtual;

  public
    property BoundObject: TGLObjectBase read FBoundObject write SetBoundObject;

  end;

  TGLObjectBinding<T: TGLObjectBase> = class(TGLObjectBinding)
  private
    function GetBoundObject: T;

  protected
    procedure SetBoundObject(const Value: T); reintroduce; virtual;

  public
    constructor Create; override;

    property BoundObject: T read GetBoundObject write SetBoundObject;

  end;

  TGLObjectBindings = class
  public type
    // from TGLObjectBindingClass to TGLObjectBinding
    TBindingMap = TClassObjectMap<TGLObjectBinding>;

  private
    FBindings: TBindingMap;

  public
    constructor Create;
    destructor Destroy; override;

    function Get<T: TGLObjectBase>: TGLObjectBinding<T>; overload;
    function Get(AGLObjectClass: TGLObjectBaseClass): TGLObjectBinding; overload;

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
    procedure AssignStateOnly(AState: TGLSingleState); virtual; abstract;

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
    procedure AssignStateOnly(AState: TGLSingleState); override;

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
    constructor Create(AType: TGLSingleState.TType; AFlag: TGLenum; APengineDefault: Boolean;
      AGLDefault: Boolean = False);

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

  TGLStateRevertSet = class;

  // TODO: XmlDoc
  TGLState = class
  public type
    // TODO: XmlDoc
    TAllStates = array [TGLSingleState.TType] of TGLSingleState;
    TChangeStack = TObjectStack<TGLStateRevertSet>;

  private
    FStates: TAllStates;
    FChangeStack: TChangeStack;
    FGLObjectBindings: TGLObjectBindings;
    FScreenSize: TIntVector2;

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

    function GetResourceParam: TGLObjectParam;
    
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetScreenSize(AScreenSize: TIntVector2);

    procedure Push;
    procedure Pop;

    property State[AState: TGLSingleState.TBooleanType]: Boolean read GetState write SetState; default;
    property State[AState: TGLSingleState.TColorRGBAType]: TColorRGBA read GetState write SetState; default;
    property State[AState: TGLSingleState.TCompareFuncType]: TGLCompareFunction read GetState write SetState; default;
    property State[AState: TGLSingleState.TCullFaceType]: TGLCullFace read GetState write SetState; default;
    property State[AState: TGLSingleState.TBlendFuncType]: TGLBlendFunc read GetState write SetState; default;

    property GLObjectBindings: TGLObjectBindings read FGLObjectBindings;

    property ResParam: TGLObjectParam read GetResourceParam;

    property ScreenSize: TIntVector2 read FScreenSize;

  end;

  TGLStateRevertSet = class
  public type
    TStates = TObjectArray<TGLSingleState>;

  private
    FStates: TGLState.TAllStates;
    FOldStates: TStates;
    FChangedStates: TGLSingleState.TTypes;

  public
    constructor Create(AStates: TGLState.TAllStates);
    destructor Destroy; override;

    procedure Save(AState: TGLSingleState);

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

procedure TGLState.SetScreenSize(AScreenSize: TIntVector2);
begin
  FScreenSize := AScreenSize;
end;

procedure TGLState.SetState(AState: TGLSingleState.TBlendFuncType; const Value: TGLBlendFunc);
begin
  SetState<TGLBlendFunc>(AState, Value);
end;

procedure TGLState.SetState<T>(AState: TGLSingleState.TType; const Value: T);
begin
  if not FChangeStack.Empty then
    FChangeStack.Top.Save(FStates[AState]);
  TGLSingleState<T>(FStates[AState]).State := Value;
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

constructor TGLState.Create;
begin
  InitStates;
  FChangeStack := TChangeStack.Create;
  FGLObjectBindings := TGLObjectBindings.Create;
end;

destructor TGLState.Destroy;
var
  S: TGLSingleState;
begin
  FGLObjectBindings.Free;
  FChangeStack.Free;
  for S in FStates do
    S.Free;
  inherited;
end;

procedure TGLState.Push;
begin
  FChangeStack.Push(TGLStateRevertSet.Create(FStates));
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

constructor TGLFlagState.Create(AType: TGLSingleState.TType; AFlag: TGLenum; APengineDefault: Boolean;
  AGLDefault: Boolean = False);
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

procedure TGLSingleState<T>.AssignStateOnly(AState: TGLSingleState);
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

{ TGLStateRevertSet }

constructor TGLStateRevertSet.Create(AStates: TGLState.TAllStates);
begin
  FStates := AStates;
  FOldStates := TStates.Create(True);
end;

destructor TGLStateRevertSet.Destroy;
var
  State: TGLSingleState;
begin
  for State in FOldStates do
  begin
    FStates[State.GetType].AssignStateOnly(State);
    State.SendState;
  end;
  FOldStates.Free;
  inherited;
end;

procedure TGLStateRevertSet.Save(AState: TGLSingleState);
var
  T: TGLSingleState.TType;
begin
  T := AState.GetType;
  if not(T in FChangedStates) then
  begin
    Include(FChangedStates, T);
    FOldStates.Add(AState.Copy);
  end;
end;

{ TGLSingleState }

function TGLSingleState.Copy: TGLSingleState;
begin
  Result := TGLSingleStateClass(ClassType).Create;
  Result.Assign(Self);
end;

{ TGLObjectParam }

procedure TGLObjectParam.Assign(AResourceParameter: TResourceParameter);
begin
  inherited;
  FGLState := TGLObjectParam(AResourceParameter).FGLState;
end;

constructor TGLObjectParam.Create(AGLState: TGLState);
begin
  FGLState := AGLState;
end;

constructor TGLObjectParam.Create;
begin

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

function TGLObjectBase.Bound: Boolean;
begin
  Result := Binding.BoundObject = Self;
end;

constructor TGLObjectBase.Create(AGLState: TGLState);
begin
  FGLState := AGLState;
  FBinding := GLState.GLObjectBindings.Get(TGLObjectBaseClass(ClassType));
end;

destructor TGLObjectBase.Destroy;
begin
  if Bound then
    Unbind;
  inherited;
end;

procedure TGLObjectBase.SetGLLabel(const Value: AnsiString);
begin
  FGLLabel := Value;
  if Value = '' then
    glObjectLabel(Ord(GetObjectType), GetGLNameUInt, -1, nil)
  else
    glObjectLabel(Ord(GetObjectType), GetGLNameUInt, Length(Value), @Value[1]);
end;

{ TGLObjectBase }

procedure TGLObjectBase.Bind;
begin
  if FBinding.BoundObject = Self then
    Exit;
  BindGLObject;
  FBinding.BoundObject := Self;
end;

procedure TGLObjectBase.Unbind;
begin
  if FBinding.BoundObject = nil then
    Exit;
  UnbindGLObject;
  FBinding.BoundObject := nil;
end;

{ TGLObjectBindings }

constructor TGLObjectBindings.Create;
begin
  FBindings := TBindingMap.Create;
end;

destructor TGLObjectBindings.Destroy;
begin
  FBindings.Free;
  inherited;
end;

function TGLObjectBindings.Get(AGLObjectClass: TGLObjectBaseClass): TGLObjectBinding;
var
  BindingClass: TGLObjectBindingClass;
begin
  BindingClass := AGLObjectClass.GetBindingClass;
  if not FBindings.Get(BindingClass, Result) then
  begin
    Result := BindingClass.Create;
    FBindings[BindingClass] := Result;
  end;
end;

function TGLObjectBindings.Get<T>: TGLObjectBinding<T>;
begin
  Result := TGLObjectBinding<T>(Get(T));
end;

{ TGLObjectBinding }

constructor TGLObjectBinding.Create;
begin
  // nothing
end;

procedure TGLObjectBinding.SetBoundObject(const Value: TGLObjectBase);
begin
  FBoundObject := Value;
end;

{ TGLObjectBinding<T> }

constructor TGLObjectBinding<T>.Create;
begin
  // nothing
end;

function TGLObjectBinding<T>.GetBoundObject: T;
begin
  Result := T(FBoundObject);
end;

procedure TGLObjectBinding<T>.SetBoundObject(const Value: T);
begin
  FBoundObject := Value;
end;

end.
