unit Pengine.Coroutines;

interface

uses
  Winapi.Windows,

  System.SysUtils,

  Pengine.ICollections;

type

  ECoroutine = class(Exception);

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

  IGenerator<T> = interface(IIterable<T>)
    /// <summary>Wether all values have been generated.</summary>
    function Depleted: Boolean;

  end;

  IConsumer<T> = interface
    /// <summary>Wether the consumer is still accepting another value.</summary>
    function Accepting: Boolean;

    /// <summary>Sends a single value to the coroutine and returns true, if it still accpets another value.</summary>
    function Send(AValue: T): Boolean; overload;
    /// <summary>Sends as many values from the iterable to the coroutine as it accepts and returns true, if it still
    /// accepts another value.</summary>
    function Send(AIterable: IIterable<T>): Boolean; overload;

  end;

  IConsumingGenerator<T, V> = interface
    /// <summary>Sends a single value to the coroutine and returns an iterable of generated values.</summary>
    function Send(AValue: T): IIterable<V>; overload;
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

  private
    FStack: PByte;
    FAddress: PByte;
    FException: TObject;
    FStarted: Boolean;
    FDead: Boolean;
    FTerminated: Boolean;

    function Raised: Boolean;

    class procedure Init(ACoroutine: TCoroutine); static;
    class procedure FirstSwitch(ACoroutine: TCoroutine); static;
    class procedure Switch(ACoroutine: TCoroutine); static;

    // class function GetCurrent: TCoroutine; static;

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

  TGenerate<T> = reference to function(AValue: T): Boolean;

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
    FConsuming: Boolean;
    FConsumed: T;
    FGenerated: V;

  protected
    /// <summary>Consumes a single value and returns false, if the consumer got terminated.</summary>
    function Consume(out AValue: T): Boolean;
    /// <summary>Generates a value from the generator and returns false, if the generator got terminated.</summary>
    function Generate(AValue: V): Boolean;

    function GetCurrent: V;

  public
    procedure AfterConstruction; override;

    function Send(AValue: T): IIterable<V>; overload;
    function Send(AIterable: IIterable<T>): IIterable<V>; overload;

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

class procedure TCoroutine.Init(ACoroutine: TCoroutine);
begin
  Switch(ACoroutine);
  ACoroutine.FStarted := True;
  try
    ACoroutine.Execute;
  except
    ACoroutine.FException := AcquireExceptionObject;
  end;
  ACoroutine.FDead := True;
  Switch(ACoroutine);
end;

class procedure TCoroutine.FirstSwitch(ACoroutine: TCoroutine);
asm
  PUSHAD
  PUSH fs:[0]
  PUSH fs:[4]
  PUSH fs:[8]
  XCHG esp, eax.FAddress
  MOV fs:[0], 0         // Exception Handler
  MOV fs:[4], esp       // Stack Base Pointer
  MOV ecx, eax.FStack
  MOV fs:[8], ecx       // Stack Limit Pointer
  CALL Init
end;

procedure TCoroutine.FreeInstance;
begin
  try
    if Started and not Dead and not Raised then
      Terminate;
  finally
    VirtualFree(FStack, 0, MEM_RELEASE);
    inherited;
  end;
end;

function TCoroutine.Started: Boolean;
begin
  Result := FStarted;
end;

class procedure TCoroutine.Switch(ACoroutine: TCoroutine);
asm
  PUSHAD
  PUSH fs:[0]
  PUSH fs:[4]
  PUSH fs:[8]
  XCHG esp, eax.FAddress
  POP fs:[8]
  POP fs:[4]
  POP fs:[0]
  POPAD
end;

constructor TCoroutine.Create(AStackSize: NativeUInt);
var
  OldProtection: Cardinal;
begin
  FStack := VirtualAlloc(nil, FPageSize + AStackSize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  if FStack = nil then
    raise ECoroutine.Create('Could not allocate stack memory for coroutine.');
  if not VirtualProtect(FStack, FPageSize, PAGE_NOACCESS, OldProtection) then
    raise ECoroutine.CreateFmt('Could not setup guard page for coroutine. Error: %d', [GetLastError]);

  FAddress := FStack;
  Inc(FAddress, FPageSize + AStackSize);
  FirstSwitch(Self);
end;

function TCoroutine.Dead: Boolean;
begin
  Result := FDead;
end;

procedure TCoroutine.Complete;
begin
  while Resume do
      ; // nothing
end;

class constructor TCoroutine.Create;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  FPageSize := SysInfo.dwPageSize;
end;

function TCoroutine.Yield: Boolean;
begin
  Switch(Self);
  Result := not FTerminated;
end;

function TCoroutine.Raised: Boolean;
begin
  Result := FException <> nil;
end;

function TCoroutine.Resume: Boolean;
begin
  if Raised then
    raise ECoroutine.Create('Cannot resume coroutine, that raised an exception.');
  if Dead then
    raise ECoroutine.Create('Cannot resume dead coroutine.');
  Switch(Self);
  Result := not Dead;
  if Raised then
    raise FException;
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

procedure TConsumingGenerator<T, V>.AfterConstruction;
begin
  inherited;
  Resume;
  Assert(FConsuming);
end;

function TConsumingGenerator<T, V>.Consume(out AValue: T): Boolean;
begin
  FConsuming := True;
  Result := Yield;
  AValue := FConsumed;
end;

function TConsumingGenerator<T, V>.Generate(AValue: V): Boolean;
begin
  FConsuming := False;
  FGenerated := AValue;
  Result := Yield;
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
  Result := Resume;
end;

function TConsumingGenerator<T, V>.Send(AValue: T): IIterable<V>;
begin
  // Does this work???
end;

function TConsumingGenerator<T, V>.Send(AIterable: IIterable<T>): IIterable<V>;
begin
  // Does this work???
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
