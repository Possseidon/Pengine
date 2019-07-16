unit Pengine.Coroutines;

interface

uses
  Winapi.Windows,

  System.SysUtils,

  Pengine.ICollections;

  (*
{$IFDEF CPUX64}


type

  PRuntimeFunction = ^TRuntimeFunction;

  TRuntimeFunction = record
    BeginAddress: Cardinal;
    EndAddress: Cardinal;
    case Integer of
      0:
        (UnwindInfoAddress: Cardinal);
      1:
        (UnwindData: Cardinal);
  end;

function RtlAddFunctionTable(FunctionTable: PRuntimeFunction; EntryCount: Cardinal; BaseAddress: Pointer): ByteBool;
  cdecl; external kernel32;

function RtlDeleteFunctionTable(FunctionTable: PRuntimeFunction): ByteBool; cdecl; external kernel32;

{$ENDIF}
    *)

type

  ECoroutine = class(Exception);

  /// <summary>A stackful coroutine, that can pause execution and resume later, whenever it yields.</summary>
  ICoroutine = interface
    /// <summary>Wether the coroutine has been resumed at least once.</summary>
    function Started: Boolean;
    /// <summary>Wether the coroutine has reached the end of execution.</summary>
    function Dead: Boolean;
    /// <summary>Wether the coroutine got terminated.</summary>
    function Terminated: Boolean;

    /// <summary>Resume and returns true, if the coroutine can still be resumed again afterwards.</summary>
    function Resume: Boolean;
    /// <summary>Resumes until the coroutine is dead.</summary>
    procedure Complete;
    /// <summary>Tries to terminate the coroutine by yielding false.</summary>
    procedure Terminate;

  end;

  /// <summary>An iterable generator, backed by a coroutine.</summary>
  IGenerator<T> = interface(IIterable<T>)
    /// <summary>Wether all values have been generated.</summary>
    function Depleted: Boolean;

  end;

  /// <summary>A consumer processing values, backed by a coroutine.</summary>
  IConsumer<T> = interface
    /// <summary>Wether the consumer is still accepting another value.</summary>
    function Accepting: Boolean;

    /// <summary>Sends a single value to the coroutine and returns true, if it still accpets another value.</summary>
    function Send(AValue: T): Boolean; overload;
    /// <summary>Sends as many values from the iterable to the coroutine as it accepts and returns true, if it still
    /// accepts another value.</summary>
    function Send(AIterable: IIterable<T>): Boolean; overload;

  end;

  /// <summary>A consumer, that generates new values from the consumed values, backed by a coroutine.</summary>
  IConsumingGenerator<T, V> = interface
    /// <summary>Sends a single value to the coroutine and returns an iterable of generated values.</summary>
    // function Send(AValue: T): IIterable<V>; overload;
    /// <summary>Sends as many values from the iterable to the coroutine as it accepts and returns an iterable of
    /// generated values.</summary>
    function Send(AIterable: IIterable<T>): IIterable<V>; overload;

  end;

  TCoroutine = class(TInterfacedObject, ICoroutine)
  public const

    DefaultStackSize = $4000;

  private
  class var
    FPageSize: Cardinal;
    class threadvar
      FCurrent: TCoroutine;

  private
    FParent: TCoroutine;
    FStack: PByte;
    FAddress: PByte;
    FException: TObject;
    FStarted: Boolean;
    FDead: Boolean;
    FTerminated: Boolean;
    (*
    {$IFDEF CPUX64}
    FRuntimeFunction: TRuntimeFunction;
    {$ENDIF}
    *)
    function Raised: Boolean;

    /// <summary>This gets the coroutine stack ready so that it starts running on its next switch.</summary>
    procedure FirstSwitch;
    /// <summary>This is called at the end of the first switch, runs the coroutine and handles exceptions.</summary>
    procedure Init;
    /// <summary>This is a symmetric switch to or from the coroutine.</summary>
    procedure Switch;

    class function GetCurrent: TCoroutine; static;

  protected
    procedure Execute; virtual; abstract;

    /// <summary>Yields from inside of the coroutine and returns false, if the coroutine should terminate.</summary>
    function Yield: Boolean;

  public
    class constructor Create;

    constructor Create(AStackSize: NativeUInt = DefaultStackSize);
    procedure FreeInstance; override;

    function Resume: Boolean;
    procedure Complete;
    procedure Terminate;

    function Started: Boolean;
    function Dead: Boolean;
    function Terminated: Boolean;

    class property Current: TCoroutine read GetCurrent;

  end;

  TYield = reference to function: Boolean;

  TSimpleCoroutine = class(TCoroutine)
  private
    FProc: TProc<TYield>;

  protected
    procedure Execute; override;

  public
    constructor Create(AProc: TProc<TYield>; AStackSize: NativeUInt = TCoroutine.DefaultStackSize);

  end;

  TGenerator<T> = class(TCoroutine, IGenerator<T>, IIterable<T>, IIterator<T>)
  private
    FGenerated: T;

    function GetCurrent: T;

  protected
    /// <summary>Generates a value from the generator and returns false, if the generator got terminated.</summary>
    function Generate(AValue: T): Boolean;

  public
    function IGenerator<T>.Depleted = Dead;

    function Iterate: IIterate<T>;
    function GetEnumerator: IIterator<T>;

    function IIterator<T>.MoveNext = Resume;

  end;

  TGenerate<T> = reference to
    function(AValue: T): Boolean;

  TSimpleGenerator<T> = class(TGenerator<T>)
  private
    FProc: TProc<TGenerate<T>>;

  protected
    procedure Execute; override;

  public
    constructor Create(AProc: TProc<TGenerate<T>>; AStackSize: NativeUInt = TCoroutine.DefaultStackSize);

  end;

  TConsumer<T> = class(TCoroutine, IConsumer<T>)
  private
    FValue: T;

  protected
    /// <summary>Consumes a single value and returns false, if the consumer got terminated.</summary>
    function Consume(out AValue: T): Boolean;

  public
    procedure AfterConstruction; override;

    function Accepting: Boolean;

    function Send(AValue: T): Boolean; overload;
    function Send(AIterable: IIterable<T>): Boolean; overload;

  end;

  TConsume<T> = reference to function(out AValue: T): Boolean;

  TSimpleConsumer<T> = class(TConsumer<T>)
  private
    FProc: TProc<TConsume<T>>;

  protected
    procedure Execute; override;

  public
    constructor Create(AProc: TProc<TConsume<T>>; AStackSize: NativeUInt = TCoroutine.DefaultStackSize);

  end;

  TConsumingGenerator<T, V> = class(TCoroutine, IConsumingGenerator<T, V>, IIterable<V>, IIterator<V>)
  private
    FConsumedIterator: IIterator<T>;
    FValueReady: Boolean;
    FGenerated: V;

    function AllConsumed: Boolean;

  protected
    /// <summary>Consumes a single value and returns false, if the consumer got terminated.</summary>
    function Consume(out AValue: T): Boolean;
    /// <summary>Generates a value from the generator and returns false, if the generator got terminated.</summary>
    function Generate(AValue: V): Boolean;

    function GetCurrent: V;

  public
    function Send(AIterable: IIterable<T>): IIterable<V>;

    function Iterate: IIterate<V>;
    function GetEnumerator: IIterator<V>;

    function MoveNext: Boolean;

  end;

  TSimpleConsumingGenerator<T, V> = class(TConsumingGenerator<T, V>)
  private
    FProc: TProc<TConsume<T>, TGenerate<V>>;

  protected
    procedure Execute; override;

  public
    constructor Create(AProc: TProc<TConsume<T>, TGenerate<V>>; AStackSize: NativeUInt = TCoroutine.DefaultStackSize);

  end;

implementation

{ TCoroutine }

function TCoroutine.Raised: Boolean;
begin
  Result := FException <> nil;
end;

procedure TCoroutine.FirstSwitch;
asm
  {$IFDEF WIN32}
  // Push all general purpose registers
  PUSHAD
  // Push XMM registers
  SUB esp, 16 * 8
  MOVDQU [esp + 16 * 0], xmm0
  MOVDQU [esp + 16 * 1], xmm1
  MOVDQU [esp + 16 * 2], xmm2
  MOVDQU [esp + 16 * 3], xmm3
  MOVDQU [esp + 16 * 4], xmm4
  MOVDQU [esp + 16 * 5], xmm5
  MOVDQU [esp + 16 * 6], xmm6
  MOVDQU [esp + 16 * 7], xmm7
  // Push Exception Handler, Stack Base Pointer and Stack Limit Pointer
  PUSH fs:[0]
  PUSH fs:[4]
  PUSH fs:[8]
  // Swap current Stack Pointer and the coroutines Stack Pointer
  XCHG esp, eax.FAddress
  // Setup Exception Handler, Stack Base Pointer and Stack Limit Pointer
  MOV fs:[0], 0         // Exception Handler
  MOV fs:[4], esp       // Stack Base Pointer
  MOV edx, eax.FStack
  MOV fs:[8], edx       // Stack Limit Pointer
  {$ELSE}
  // Push all general purpose registers
  // PUSH RAX
  // PUSH RCX
  // PUSH RDX
  PUSH RBX
  // PUSH RSP
  PUSH RBP
  PUSH RSI
  PUSH RDI
  // PUSH R8
  // PUSH R9
  // PUSH R10
  // PUSH R11
  PUSH R12
  PUSH R13
  PUSH R14
  PUSH R15
  // Push XMM registers
  SUB rsp, 16 * 16
  MOVDQU [rsp + 16 * 0], xmm0
  MOVDQU [rsp + 16 * 1], xmm1
  MOVDQU [rsp + 16 * 2], xmm2
  MOVDQU [rsp + 16 * 3], xmm3
  MOVDQU [rsp + 16 * 4], xmm4
  MOVDQU [rsp + 16 * 5], xmm5
  MOVDQU [rsp + 16 * 6], xmm6
  MOVDQU [rsp + 16 * 7], xmm7
  MOVDQU [rsp + 16 * 8], xmm8
  MOVDQU [rsp + 16 * 9], xmm9
  MOVDQU [rsp + 16 * 10], xmm10
  MOVDQU [rsp + 16 * 11], xmm11
  MOVDQU [rsp + 16 * 12], xmm12
  MOVDQU [rsp + 16 * 13], xmm13
  MOVDQU [rsp + 16 * 14], xmm14
  MOVDQU [rsp + 16 * 15], xmm15
  // Push Exception Handler, Stack Base Pointer and Stack Limit Pointer
  // PUSH gs:[0]
  // PUSH gs:[8]
  // PUSH gs:[16]
  // Swap current Stack Pointer and the coroutines Stack Pointer
  XCHG rsp, rcx.FAddress
  // Setup Exception Handler, Stack Base Pointer and Stack Limit Pointer
  // MOV gs:[0], 0         // Exception Handler
  // MOV gs:[8], rsp       // Stack Base Pointer
  // MOV rdx, rcx.FStack
  // MOV gs:[16], rdx       // Stack Limit Pointer
  {$ENDIF}
  // Call Init, which switches back immediately, priming the coroutine to run on its next switch
  JMP Init
end;

procedure TCoroutine.Init;
begin
  Switch;
  FCurrent := Self;
  FStarted := True;
  try
    Execute;
  except
    FException := AcquireExceptionObject;
  end;
  FDead := True;
  Switch;
end;

procedure TCoroutine.Switch;
asm
  {$IFDEF WIN32}
  // Push all general purpose registers
  PUSHAD
  // Push XMM registers
  SUB esp, 16 * 8
  MOVDQU [esp + 16 * 0], xmm0
  MOVDQU [esp + 16 * 1], xmm1
  MOVDQU [esp + 16 * 2], xmm2
  MOVDQU [esp + 16 * 3], xmm3
  MOVDQU [esp + 16 * 4], xmm4
  MOVDQU [esp + 16 * 5], xmm5
  MOVDQU [esp + 16 * 6], xmm6
  MOVDQU [esp + 16 * 7], xmm7
  // Push Exception Handler, Stack Base Pointer and Stack Limit Pointer
  PUSH fs:[0]
  PUSH fs:[4]
  PUSH fs:[8]
  // Swap current Stack Pointer and the coroutines Stack Pointer
  XCHG esp, eax.FAddress
  // Pop Exception Handler, Stack Base Pointer and Stack Limit Pointer
  POP fs:[8]
  POP fs:[4]
  POP fs:[0]
  // Pop XMM registers
  MOVDQU xmm0, [esp + 16 * 0]
  MOVDQU xmm1, [esp + 16 * 1]
  MOVDQU xmm2, [esp + 16 * 2]
  MOVDQU xmm3, [esp + 16 * 3]
  MOVDQU xmm4, [esp + 16 * 4]
  MOVDQU xmm5, [esp + 16 * 5]
  MOVDQU xmm6, [esp + 16 * 6]
  MOVDQU xmm7, [esp + 16 * 7]
  ADD esp, 16 * 8
  // Pop all general purpose registers
  POPAD
  {$ELSE}
  // Push all general purpose registers
  // PUSH RAX
  // PUSH RCX
  // PUSH RDX
  PUSH RBX
  // PUSH RSP
  PUSH RBP
  PUSH RSI
  PUSH RDI
  // PUSH R8
  // PUSH R9
  // PUSH R10
  // PUSH R11
  PUSH R12
  PUSH R13
  PUSH R14
  PUSH R15
  // Push XMM registers
  SUB rsp, 16 * 16
  MOVDQU [rsp + 16 * 0], xmm0
  MOVDQU [rsp + 16 * 1], xmm1
  MOVDQU [rsp + 16 * 2], xmm2
  MOVDQU [rsp + 16 * 3], xmm3
  MOVDQU [rsp + 16 * 4], xmm4
  MOVDQU [rsp + 16 * 5], xmm5
  MOVDQU [rsp + 16 * 6], xmm6
  MOVDQU [rsp + 16 * 7], xmm7
  MOVDQU [rsp + 16 * 8], xmm8
  MOVDQU [rsp + 16 * 9], xmm9
  MOVDQU [rsp + 16 * 10], xmm10
  MOVDQU [rsp + 16 * 11], xmm11
  MOVDQU [rsp + 16 * 12], xmm12
  MOVDQU [rsp + 16 * 13], xmm13
  MOVDQU [rsp + 16 * 14], xmm14
  MOVDQU [rsp + 16 * 15], xmm15
  // Push Exception Handler, Stack Base Pointer and Stack Limit Pointer
  // PUSH gs:[0]
  // PUSH gs:[8]
  // PUSH gs:[16]
  // Swap current Stack Pointer and the coroutines Stack Pointer
  XCHG rsp, rcx.FAddress
  // Pop Exception Handler, Stack Base Pointer and Stack Limit Pointer
  // POP gs:[16]
  // POP gs:[8]
  // POP gs:[0]
  // Pop XMM registers
  MOVDQU xmm0, [rsp + 16 * 0]
  MOVDQU xmm1, [rsp + 16 * 1]
  MOVDQU xmm2, [rsp + 16 * 2]
  MOVDQU xmm3, [rsp + 16 * 3]
  MOVDQU xmm4, [rsp + 16 * 4]
  MOVDQU xmm5, [rsp + 16 * 5]
  MOVDQU xmm6, [rsp + 16 * 6]
  MOVDQU xmm7, [rsp + 16 * 7]
  MOVDQU xmm8, [rsp + 16 * 8]
  MOVDQU xmm9, [rsp + 16 * 9]
  MOVDQU xmm10, [rsp + 16 * 10]
  MOVDQU xmm11, [rsp + 16 * 11]
  MOVDQU xmm12, [rsp + 16 * 12]
  MOVDQU xmm13, [rsp + 16 * 13]
  MOVDQU xmm14, [rsp + 16 * 14]
  MOVDQU xmm15, [rsp + 16 * 15]
  ADD rsp, 16 * 16
  // Push all general purpose registers
  POP R15
  POP R14
  POP R13
  POP R12
  // POP R11
  // POP R10
  // POP R9
  // POP R8
  POP RDI
  POP RSI
  POP RBP
  // POP RSP
  POP RBX
  // POP RDX
  // POP RCX
  // POP RAX
  {$ENDIF}
end;

class function TCoroutine.GetCurrent: TCoroutine;
begin
  Result := FCurrent;
end;

function TCoroutine.Yield: Boolean;
begin
  Switch;
  Result := not FTerminated;
end;

class constructor TCoroutine.Create;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  FPageSize := SysInfo.dwPageSize;
end;

constructor TCoroutine.Create(AStackSize: NativeUInt);
var
  OldProtect: Cardinal;
begin
  {$IFDEF WIN32}
  Assert(AStackSize mod 4 = 0, 'Coroutine stacksize must be aligned by 4 bytes.');
  {$ELSE}
  Assert(AStackSize mod 16 = 0, 'Coroutine stacksize must be aligned by 16 bytes.');
  {$ENDIF}
  FStack := VirtualAlloc(nil, FPageSize + AStackSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if FStack = nil then
    raise ECoroutine.Create('Could not allocate stack memory for coroutine.');
  if not VirtualProtect(FStack, FPageSize, PAGE_NOACCESS, OldProtect) then
    raise ECoroutine.CreateFmt('Could not setup guard page for coroutine. Error: %d', [GetLastError]);
  (*
  {$IFDEF CPUX64}
  FRuntimeFunction.BeginAddress := 0;
  FRuntimeFunction.EndAddress := AStackSize + FPageSize;
  FRuntimeFunction.UnwindInfoAddress := 0;
  Assert(RtlAddFunctionTable(@FRuntimeFunction, 1, FStack));
  {$ENDIF}
  *)
  FAddress := FStack;
  Inc(FAddress, FPageSize + AStackSize);
  FirstSwitch;
end;

procedure TCoroutine.FreeInstance;
begin
  try
    if Started and not Dead and not Raised then
      Terminate;
  finally
    Assert(VirtualFree(FStack, 0, MEM_RELEASE));
    (*
    {$IFDEF CPUX64}
    Assert(RtlDeleteFunctionTable(@FRuntimeFunction));
    {$ENDIF}
    *)
    inherited;
  end;
end;

function TCoroutine.Resume: Boolean;
begin
  if Raised then
    raise ECoroutine.Create('Cannot resume coroutine, that raised an exception.');
  if Dead then
    raise ECoroutine.Create('Cannot resume dead coroutine.');
  FParent := FCurrent;
  FCurrent := Self;
  Switch;
  FCurrent := FParent;
  FParent := nil;
  Result := not Dead;
  if Raised then
    raise FException;
end;

procedure TCoroutine.Complete;
begin
  while Resume do
      ; // nothing
end;

procedure TCoroutine.Terminate;
begin
  if Raised then
    raise ECoroutine.Create('Cannot terminate coroutine, that raised an exception.');
  if not Started then
    raise ECoroutine.Create('Coroutine never started.');
  if Dead then
    raise ECoroutine.Create('Cannot terminate dead coroutine.');
  FTerminated := True;
  while Resume do
      ; // nothing
end;

function TCoroutine.Started: Boolean;
begin
  Result := FStarted;
end;

function TCoroutine.Dead: Boolean;
begin
  Result := FDead;
end;

function TCoroutine.Terminated: Boolean;
begin
  Result := FTerminated;
end;

{ TSimpleCoroutine }

constructor TSimpleCoroutine.Create(AProc: TProc<TYield>; AStackSize: NativeUInt);
begin
  inherited Create(AStackSize);
  FProc := AProc;
end;

procedure TSimpleCoroutine.Execute;
begin
  FProc(Yield);
end;

{ TGenerator<T> }

function TGenerator<T>.GetCurrent: T;
begin
  Result := FGenerated;
end;

function TGenerator<T>.GetEnumerator: IIterator<T>;
begin
  Result := Self;
end;

function TGenerator<T>.Iterate: IIterate<T>;
begin
  Result := TIterableIterate<T>.Create(Self);
end;

function TGenerator<T>.Generate(AValue: T): Boolean;
begin
  FGenerated := AValue;
  Result := Yield;
end;

{ TSimpleGenerator<T> }

constructor TSimpleGenerator<T>.Create(AProc: TProc<TGenerate<T>>; AStackSize: NativeUInt);
begin
  inherited Create(AStackSize);
  FProc := AProc;
end;

procedure TSimpleGenerator<T>.Execute;
begin
  FProc(Generate);
end;

{ TConsumer<T> }

function TConsumer<T>.Accepting: Boolean;
begin
  Result := not Dead;
end;

procedure TConsumer<T>.AfterConstruction;
begin
  inherited;
  Resume;
end;

function TConsumer<T>.Consume(out AValue: T): Boolean;
begin
  Result := Yield;
  AValue := FValue;
end;

function TConsumer<T>.Send(AValue: T): Boolean;
begin
  FValue := AValue;
  Result := Resume;
end;

function TConsumer<T>.Send(AIterable: IIterable<T>): Boolean;
var
  Value: T;
begin
  for Value in AIterable do
    if not Send(Value) then
      Exit(False);
  Result := True;
end;

{ TSimpleConsumer<T> }

constructor TSimpleConsumer<T>.Create(AProc: TProc<TConsume<T>>; AStackSize: NativeUInt);
begin
  inherited Create(AStackSize);
  FProc := AProc;
end;

procedure TSimpleConsumer<T>.Execute;
begin
  FProc(Consume);
end;

{ TConsumingGenerator<T, V> }

function TConsumingGenerator<T, V>.AllConsumed: Boolean;
begin
  Result := FConsumedIterator = nil;
end;

function TConsumingGenerator<T, V>.Consume(out AValue: T): Boolean;
begin
  Result := Yield;
  if Result then
  begin
    AValue := FConsumedIterator.Current;
    if not FConsumedIterator.MoveNext then
      FConsumedIterator := nil;
  end;
end;

function TConsumingGenerator<T, V>.Generate(AValue: V): Boolean;
begin
  FValueReady := True;
  FGenerated := AValue;
  Result := Yield;
  FValueReady := False;
end;

function TConsumingGenerator<T, V>.GetCurrent: V;
begin
  Result := FGenerated;
end;

function TConsumingGenerator<T, V>.GetEnumerator: IIterator<V>;
begin
  Result := Self;
end;

function TConsumingGenerator<T, V>.Iterate: IIterate<V>;
begin
  Result := TIterableIterate<V>.Create(Self);
end;

function TConsumingGenerator<T, V>.MoveNext: Boolean;
begin
  while True do
  begin
    if not Resume then
      Exit(False);
    if FValueReady then
      Exit(True);
    if AllConsumed then
      Exit(False);
  end;
end;

function TConsumingGenerator<T, V>.Send(AIterable: IIterable<T>): IIterable<V>;
begin
  FConsumedIterator := AIterable.GetEnumerator;
  if not FConsumedIterator.MoveNext then
    FConsumedIterator := nil;
  Result := Self;
end;

{ TSimpleConsumingGenerator<T, V> }

constructor TSimpleConsumingGenerator<T, V>.Create(AProc: TProc<TConsume<T>, TGenerate<V>>; AStackSize: NativeUInt);
begin
  inherited Create(AStackSize);
  FProc := AProc;
end;

procedure TSimpleConsumingGenerator<T, V>.Execute;
begin
  FProc(Consume, Generate);
end;

end.
