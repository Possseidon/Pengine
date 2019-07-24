unit Pengine.EventHandling;

interface

uses
  System.SysUtils,

  Pengine.ICollections;

type

  EEventHandler = class(Exception);

  IEventInfo = interface
  end;

  /// <summary>An event with a generic sender.</summary>
  IEventInfo<T> = interface(IEventInfo)
    function GetSender: T;

    property Sender: T read GetSender;

  end;

  TEventInfo = class(TInterfacedObject, IEventInfo);

  TEventInfo<T: class> = class(TEventInfo, IEventInfo<T>, IEventInfo)
  private
    FSender: T;
    function GetSender: T;

  public
    constructor Create(ASender: T);

    property Sender: T read GetSender;

  end;

  IEventSubscription = interface
  end;

  TEvent<T: IEventInfo> = record
  public type

    THandler = reference to procedure(AInfo: T);

    TSubscription = class(TInterfacedObject, IEventSubscription)
    private
      FHandlers: IList<THandler>;
      FHandler: THandler;

    public
      constructor Create(AHandlers: IList<THandler>; AHandler: THandler);
      destructor Destroy; override;

    end;

    TAccess = record
    private
      FHandlers: IList<THandler>;

    public
      constructor Create(AHandlers: IList<THandler>);

      /// <summary>Adds a new event-handler to the list.</summary>
      function Add(AHandler: THandler): IEventSubscription; overload;

    end;

  private
    FHandlers: IList<THandler>;
    FDisableCounter: Integer;

    function GetDisabled: Boolean; inline;
    function GetEnabled: Boolean; inline;
    function GetHandlers: IList<THandler>;

  public
    property Handlers: IList<THandler> read GetHandlers;

    /// <summary>Grants access to adding and removing handlers.</summary>
    function Access: TAccess;

    /// <summary>Execute each event handler with the given event-info object.</summary>
    procedure Execute(AInfo: T);

    /// <summary>Disable the execution of all event handlers.</summary>
    /// <remarks>Calling disable multiple times, prevent the execution until enable has been called for the same amount
    /// of times. This allows nesting.</remarks>
    procedure Disable;
    /// <summary>Re-enable the execution of all event handlers.</summary>
    /// <remarks>Enabled by default. If disable got called multiple times, enable must be called for the same amount of
    /// times, until the event is enabled again.</remarks>
    procedure Enable;

    /// <summary>Whether the event handlers are not executed.</summary>
    property Disabled: Boolean read GetDisabled;
    /// <summary>Whether the event handlers are executed.</summary>
    property Enabled: Boolean read GetEnabled;

    /// <returns>True, if the event has at least one event handler.</returns>
    function HasHandler: Boolean; inline;

  end;

implementation

{ TEventInfo<T> }

constructor TEventInfo<T>.Create(ASender: T);
begin
  FSender := ASender;
end;

function TEventInfo<T>.GetSender: T;
begin
  Result := FSender;
end;

{ TEvent<T>.TSubscription }

constructor TEvent<T>.TSubscription.Create(AHandlers: IList<THandler>; AHandler: THandler);
begin
  FHandlers := AHandlers;
  FHandler := AHandler;
  FHandlers.Add(FHandler);
end;

destructor TEvent<T>.TSubscription.Destroy;
begin
  Assert(FHandlers.Remove(FHandler), 'Event-Handler removed already');
  inherited;
end;

{ TEvent<T> }

function TEvent<T>.Access: TAccess;
begin
  Result.Create(Handlers);
end;

procedure TEvent<T>.Disable;
begin
  Inc(FDisableCounter);
end;

procedure TEvent<T>.Enable;
begin
  Dec(FDisableCounter);
end;

procedure TEvent<T>.Execute(AInfo: T);
var
  Handler: THandler;
begin
  if HasHandler and Enabled then
    for Handler in FHandlers do
      Handler(AInfo);
end;

function TEvent<T>.GetDisabled: Boolean;
begin
  Result := FDisableCounter > 0;
end;

function TEvent<T>.GetEnabled: Boolean;
begin
  Result := FDisableCounter <= 0;
end;

function TEvent<T>.GetHandlers: IList<THandler>;
begin
  if FHandlers = nil then
    FHandlers := TList<THandler>.Create;
  Result := FHandlers;
end;

function TEvent<T>.HasHandler: Boolean;
begin
  Result := (FHandlers <> nil) and not FHandlers.Empty;
end;

{ TEvent<T>.TAccess }

function TEvent<T>.TAccess.Add(AHandler: THandler): IEventSubscription;
begin
  Result := TSubscription.Create(FHandlers, AHandler);
end;

constructor TEvent<T>.TAccess.Create(AHandlers: IList<THandler>);
begin
  FHandlers := AHandlers;
end;

end.
