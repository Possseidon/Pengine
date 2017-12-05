unit Pengine.GLState;

interface

uses          
  dglOpenGL, 

  System.SysUtils,
  System.RTTI,
  
  Pengine.GLEnums,
  Pengine.Collections,
  Pengine.Color,
  Pengine.Equaller;

type

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

    TBooleanType = stDepthTest .. stDepthMask;
    TColorRGBAType = stClearColor .. stClearColor;
    TCompareFuncType = stDepthFunc .. stDepthFunc;
    TCullFaceType = stCullFaceMode .. stCullFaceMode;
    TBlendFuncType = stBlendFunc .. stBlendFunc;

  public
    // function GetType: TType; virtual; abstract;
    
  end;

  // TODO: XmlDoc
  TGLSingleState<T> = class abstract(TGLSingleState)
  private
    FState: T;

  protected
    procedure SendState; virtual; abstract;

    procedure SetState(const Value: T); virtual; abstract;

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
    FFlag: Integer;

  protected
    procedure SendState; override;

  public
    constructor Create(AFlag: TGLenum; APengineDefault: Boolean; AGLDefault: Boolean = False);

  end;

  TGLClearColorState = class(TGLSingleState<TColorRGBA, TColorRGBAEqualler>)
  protected
    procedure SendState; override;
  end;

  TGLDepthMaskState = class(TGLSingleState<Boolean, TBoolEqualler>)
  protected
    procedure SendState; override;
  end;

  TGLEnumState<T> = class(TGLSingleState<T, TEnumEqualler<T>>);

  TGLDepthFuncState = class(TGLEnumState<TGLCompareFunction>)
  protected
    procedure SendState; override;
  end;

  TGLCullFaceState = class(TGLEnumState<TGLCullFace>)
  protected
    procedure SendState; override;
  end;

  TGLBlendFuncState = class(TGLEnumState<TGLBlendFunc>)
  protected
    procedure SendState; override;
  end;

  TGLStateChange = class abstract;

  /// <summary>Automatically stores the old state on construction, which gets reverted on destruction of this object.</summary>
  TGLStateChange<T> = class(TGLStateChange)
  private
    FState: TGLSingleState<T>;
    FOldState: T;
  public
    constructor Create(AState: TGLSingleState<T>; const ANewState: T);
    destructor Destroy; override;
  end;

  // TODO: XmlDoc
  TGLState = class
  public type
    // TODO: XmlDoc
    TAllStates = array [TGLSingleState.TType] of TGLSingleState;
    TChanges = TRefArray<TGLStateChange>;
    TChangeStack = TRefStack<TChanges>;

  private
    FStates: TAllStates;
    FChangeStack: TChangeStack;

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

  public
    constructor Create;
    destructor Destroy; override;

    procedure Push;
    procedure Pop;

    property State[AState: TGLSingleState.TBooleanType]: Boolean read GetState write SetState; default;
    property State[AState: TGLSingleState.TColorRGBAType]: TColorRGBA read GetState write SetState; default;
    property State[AState: TGLSingleState.TCompareFuncType]: TGLCompareFunction read GetState write SetState; default;
    property State[AState: TGLSingleState.TCullFaceType]: TGLCullFace read GetState write SetState; default;
    property State[AState: TGLSingleState.TBlendFuncType]: TGLBlendFunc read GetState write SetState; default;

  end;

implementation

{ TGLContextState }

procedure TGLState.InitStates;
var
  S: TGLSingleState.TType;
  {$IFDEF DEBUG} 
  Uninitialized: string;
  {$ENDIF}      
begin
  // Flag (glEnable/glDisable)
  FStates[stDepthTest] := TGLFlagState.Create(GL_DEPTH_TEST, True);
  FStates[stSeamlessCubemap] := TGLFlagState.Create(GL_TEXTURE_CUBE_MAP_SEAMLESS, True);
  FStates[stDepthClamp] := TGLFlagState.Create(GL_DEPTH_CLAMP, True);
  FStates[stBlend] := TGLFlagState.Create(GL_BLEND, False);
  FStates[stCullFace] := TGLFlagState.Create(GL_CULL_FACE, True);
  FStates[stDebugOutput] := TGLFlagState.Create(GL_DEBUG_OUTPUT, False);
  FStates[stDebugOutputSynced] := TGLFlagState.Create(GL_DEBUG_OUTPUT_SYNCHRONOUS, False);
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
  if FChangeStack.Empty then
    TGLSingleState<T>(FStates[AState]).State := Value
  else
    FChangeStack.Top.Add(TGLStateChange<T>.Create(TGLSingleState<T>(FStates[AState]), Value));
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
  FChangeStack.Push(TChanges.Create(True));
end;

procedure TGLState.Pop;
begin
  FChangeStack.Pop;
end;

{ TGLFlagState }

constructor TGLFlagState.Create(AFlag: TGLenum; APengineDefault: Boolean; AGLDefault: Boolean = False);
begin
  FFlag := AFlag;
  inherited Create(APengineDefault, AGLDefault);
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

{ TGLStateChange<T> }

constructor TGLStateChange<T>.Create(AState: TGLSingleState<T>; const ANewState: T);
begin
  FState := AState;
  FOldState := AState.State;
  FState.State := ANewState;
end;

destructor TGLStateChange<T>.Destroy;
begin
  FState.State := FOldState;
  inherited;
end;

{ TGLClearColorState }

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

constructor TGLSingleState<T>.Create(const ADefault: T);
begin
  FState := ADefault;
end;

{ TGLDepthMaskState }

procedure TGLDepthMaskState.SendState;
begin
  glDepthMask(State);
end;

{ DepthFunc }

procedure TGLDepthFuncState.SendState;
begin
  glDepthFunc(Ord(State));
end;

{ CullFace }

procedure TGLCullFaceState.SendState;
begin
  glCullFace(Ord(State));
end;

{ BlendFunc }

procedure TGLBlendFuncState.SendState;
begin
  glBlendFunc(Ord(State.Src), Ord(State.Dest));
end;

end.
