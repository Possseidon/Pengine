unit Pengine.EventHandling;

interface

uses
  System.SysUtils,

  Pengine.ICollections;

type

  EEventHandler = class(Exception);

  /// <summary>An event with a generic sender.</summary>
  ISenderEvent<T> = interface
    ['{E8AB15DA-3FBC-4FC2-8B4B-A9011DFD6B6B}']
    function Sender: T;
  end;

  /// <summary>An event, that can get canceled.</summary>
  /// <remarks>Once canceled, it is not possible to revert that.<p/>
  /// Do not test for canceled in an event handler.</remarks>
  ICancelableEvent = interface
    ['{1F5810A2-9EFC-4860-A9D7-26846ACCA4F8}']
    function Canceled: Boolean;
    procedure Cancel;
  end;

  TEventInfo = class(TInterfacedObject);

  TEvent<T: TEventInfo> = record
  public type

    THandler = reference to procedure(AInfo: T);

    IEventContext = interface
    end;

    TEventContext = class(TInterfacedObject, IEventContext)
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
      function Add(AHandler: THandler): IEventContext; overload;

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

  TSenderEventInfo<T> = class(TEventInfo, IEventSender<T>)
  private
    FSender: T;

  public
    constructor Create(ASender: T);

    function Sender: T;

  end;

  TCancelEventInfo<T> = class(TEventInfo, IEventCancelable)
  private
    FCanceled: Boolean;

  public
    function Canceled: Boolean;
    procedure Cancel;

  end;

  TSenderCancelEventInfo<T> = class(TSenderEventInfo<T>, IEventCancelable)
  private
    FCanceled: Boolean;

  public
    function Canceled: Boolean;
    procedure Cancel;

  end;

implementation

{ TEvent<T> }

function TEvent<T>.Access: TAccess;
begin
  Result := TAccess(FHandlers);
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
begin
  if HasHandler and Enabled then
  
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
  Result := FHandlers;
end;

function TEvent<T>.HasHandler: Boolean;
begin
  Result := (FHandlers = nil) or FHandlers.Empty;
end;

{ TEvent<T>.TEventContext }

constructor TEvent<T>.TEventContext.Create(AHandlers: IList<THandler>; AHandler: THandler);
begin
  FHandlers := AHandlers;
  FHandler := AHandler;
end;

destructor TEvent<T>.TEventContext.Destroy;
begin
  FHandlers.Remove(FHandler);
  inherited;
end;

end.
