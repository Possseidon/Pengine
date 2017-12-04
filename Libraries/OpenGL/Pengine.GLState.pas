unit Pengine.GLState;

interface

uses          
  dglOpenGL, 
  
  Pengine.Collections,
  Pengine.Color;

type

  TGLState = class abstract
  public type
    // TODO: XmlDoc
    TType = (
      // Boolean
      stDepthTest,
      stDepthMask,
      stSeamlessCubemap,
      stDepthClamp,
      stDebugOutput,
      stDebugOutputSynced,

      // TColorRGBA
      stClearColor,

      // TGLCompareFunction
      stDepthFunc,

      // TGLCullFace
      stCullFace,

      // TGLBlendingFactorSrc
      stBlendingFactorSrc,

      // TGLBlendingFactorDst
      stBlendingFactorDst,

      // TGLBlendingFunc
      stBlendingFunc
      );

    TBooleanType = stDepthTest .. stDebugOutputSynced;
    TColorRGBAType = stClearColor .. stClearColor;
    TDepthFuncType = stDepthFunc .. stDepthFunc;
    TCullFaceType = stCullFace .. stCullFace;
    TBlendingFactorSrcType = stBlendingFactorSrc .. stBlendingFactorSrc;
    TBlendingFactorDstType = stBlendingFactorDst .. stBlendingFactorDst;
    TBlendingFunc = stBlendingFunc .. stBlendingFunc;

  public
    function GetType: TType; virtual; abstract;
    procedure RevertTo(AState: TGLState); virtual; abstract;
    
  end;

  // TODO: XmlDoc
  TGLState<T> = class abstract(TGLState)
  private
    FState: T;

  protected
    class function StatesEqual(const AState1, AState2: T): Boolean; virtual; abstract;
    procedure SendState(const AState: T); virtual; abstract;

    procedure SetState(const Value: T); 

  public
    property State: T read FState write SetState;

    procedure RevertTo(AState: TGLState); override;

  end;

  TGLBooleanState = class abstract(TGLState<Boolean>)
  protected
    class function StatesEqual(const AState1, AState2: Boolean): Boolean; override;
  end;
             
  TGLColorRGBAState = class abstract(TGLState<TColorRGBA>)
  protected
    class function StatesEqual(const AState1, AState2: TColorRGBA): Boolean; override;
  end;

  // glEnable/glDisable
  TGLFlagState = class abstract(TGLBooleanState)
  private
    FFlag: Integer;
    FType: TGLState.TType;

  protected
    procedure SendState(const AState: Boolean); override;

  public
    constructor Create(AType: TGLState.TType; AFlag: TGLenum; APengineDefault: Boolean; AGLDefault: Boolean = False);
    
    function GetType: TGLState.TType; override;

  end;

  // TODO: XmlDoc
  TGLContextState = class
  public type
    // TODO: XmlDoc
    TAllStates = array [TGLState.TType] of TGLState;
    TChanges = TArray<TGLState>;
    TChangeStack = TRefStack<TChanges>;

  private
    FStates: TAllStates;
    FChangeStack: TChangeStack;

    procedure InitStates;
    procedure InitFlag(AType: TGLState.TType; AFlag: TGLenum; APengineDefault: Boolean; AGLDefault: Boolean = False); inline;
    
    function GetState(AState: TGLState.TBooleanType): Boolean; overload;
    procedure SetState(AState: TGLState.TBooleanType; const Value: Boolean); overload;

    function GetState(AState: TGLState.TColorRGBAType): TColorRGBA; overload;
    procedure SetState(AState: TGLState.TColorRGBAType; const Value: TColorRGBA); overload;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Push;
    procedure Pop;

    property State[AState: TGLState.TBooleanType]: Boolean read GetState write SetState; default;
    property State[AState: TGLState.TColorRGBAType]: TColorRGBA read GetState write SetState; default;

  end;

implementation

{ TGLContextState }

constructor TGLContextState.Create;
begin
  InitStates;
  FChangeStack := TChangeStack.Create(True);
end;

destructor TGLContextState.Destroy;
var
  S: TGLState;
begin
  for S in FStates do
    S.Free;
  FChangeStack.Free;
  inherited;
end;

function TGLContextState.GetState(AState: TGLState.TColorRGBAType): TColorRGBA;
begin
  Result := TGLColorRGBAState(FStates[AState]).State;
end;

function TGLContextState.GetState(AState: TGLState.TBooleanType): Boolean;
begin
  Result := TGLBooleanState(FStates[AState]).State;
end;

procedure TGLContextState.InitStates;
begin
  InitFlag(stDepthTest, GL_DEPTH_TEST, True, False);

  FStates[stClearColor] := TGLColorRGBAState.Create;
end;

procedure TGLContextState.Pop;
var
  Change: TGLState;
begin
  for Change in FChangeStack.Pop do
    FStates[Change.GetType].RevertTo(Change);
end;

procedure TGLContextState.InitFlag(AType: TGLState.TType; AFlag: TGLenum; APengineDefault, AGLDefault: Boolean);
begin
  FStates[AType] := TGLFlagState.Create(AType, AFlag, APengineDefault, AGLDefault);
end;

procedure TGLContextState.Push;
begin
  FChangeStack.Push(TChanges.Create);
end;

procedure TGLContextState.SetState(AState: TGLState.TColorRGBAType; const Value: TColorRGBA);
begin
  TGLColorRGBAState(FStates[AState]).State := Value;
end;

procedure TGLContextState.SetState(AState: TGLState.TBooleanType; const Value: Boolean);
begin                             
  TGLBooleanState(FStates[AState]).State := Value;  
end;

{ TGLBooleanState }

class function TGLBooleanState.StatesEqual(const AState1, AState2: Boolean): Boolean;
begin
  Result := AState1 = AState2;
end;

{ TGLFlagState }

constructor TGLFlagState.Create(AType: TGLState.TType; AFlag: TGLenum; APengineDefault: Boolean; AGLDefault: Boolean = False);
begin
  FType := AType;
  FFlag := AFlag;
  FState := AGLDefault;
  State := APengineDefault;
end;

function TGLFlagState.GetType: TGLState.TType;
begin
  Result := FType;
end;

procedure TGLFlagState.SendState(const AState: Boolean);
begin
  if AState then
    glEnable(FFlag)
  else
    glDisable(FFlag);
end;

{ TGLState<T> }

procedure TGLState<T>.RevertTo(AState: TGLState);
begin
  State := TGLState<T>(AState).State;
end;

procedure TGLState<T>.SetState(const Value: T);
begin
  if StatesEqual(FState, Value) then
    Exit;
  FState := Value;
  SendState(FState);
end;

{ TGLColorRGBAState }

class function TGLColorRGBAState.StatesEqual(const AState1, AState2: TColorRGBA): Boolean;
begin
  Result := AState1 = AState2;
end;

end.
