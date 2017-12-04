unit Pengine.EventHandling;

interface

uses
  SysUtils;

type

  EHandlerNotFound = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>An event with a generic sender.</summary>
  IEventSender<T> = interface
    ['{E8AB15DA-3FBC-4FC2-8B4B-A9011DFD6B6B}']
    function Sender: T;
  end;

  /// <summary>An event, that can get canceled.</summary>
  /// <remarks>Once canceled, it is not possible to revert that.<p/>
  /// Do not test for canceled in an event handler.</remarks>
  IEventCancelable = interface
    ['{1F5810A2-9EFC-4860-A9D7-26846ACCA4F8}']
    function Canceled: Boolean;
    procedure Cancel;
  end;

  TEventInfo = class(TInterfacedObject)
  end;

  TObservableEvent = record
  public type
    THandlerStatic = procedure;
    THandler = procedure of object;

  private
    FDisableCounter: Integer;
    FHandlers: array of THandler;

    function Find(AHandler: THandler): Integer; inline;

    function GetDisabled: Boolean;
    function GetEnabled: Boolean;

  public
    /// <summary>Adds a new static event-handler to the list.</summary>
    procedure Add(AHandler: THandlerStatic); overload; inline;
    /// <summary>Adds a new event-handler to the list.</summary>
    procedure Add(AHandler: THandler); overload;
    /// <summary>Removes a static event-handler from the list.</summary>
    procedure Del(AHandler: THandlerStatic); overload; inline;
    /// <summary>Adds an event-handler from the list.</summary>
    procedure Del(AHandler: THandler); overload;

    /// <summary>Execute each event handler.</summary>
    procedure Execute; overload; inline;

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

  end;

  /// <summary>
  TObservableEvent<T: TEventInfo> = record
  public type
    THandlerStatic = procedure(AInfo: T);
    THandler = procedure(AInfo: T) of object;

  private
    FDisableCounter: Integer;
    FHandlers: array of THandler;

    function Find(AHandler: THandler): Integer; inline;

    function GetDisabled: Boolean;
    function GetEnabled: Boolean;

  public
    /// <summary>Adds a new static event-handler to the list.</summary>
    procedure Add(AHandler: THandlerStatic); overload; inline;
    /// <summary>Adds a new event-handler to the list.</summary>
    procedure Add(AHandler: THandler); overload;
    /// <summary>Removes a static event-handler from the list.</summary>
    procedure Del(AHandler: THandlerStatic); overload; inline;
    /// <summary>Adds an event-handler from the list.</summary>
    procedure Del(AHandler: THandler); overload;

    /// <summary>Execute each event handler with the given event-info object.</summary>
    procedure Execute(AInfo: T; AFreeInfo: Boolean = True); overload; inline;

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

  end;

implementation

{ EHandlerNotFound }

constructor EHandlerNotFound.Create;
begin
  inherited Create('Could not remove the event-handler as it does not exist.');
end;

{ TObservableEvent }

function TObservableEvent.Find(AHandler: THandler): Integer;
begin
  for Result := 0 to Length(FHandlers) - 1 do
    if @FHandlers[Result] = @AHandler then
      Exit;
  Result := -1;
end;

function TObservableEvent.GetDisabled: Boolean;
begin
  Result := FDisableCounter > 0;
end;

function TObservableEvent.GetEnabled: Boolean;
begin
  Result := FDisableCounter <= 0;
end;

procedure TObservableEvent.Add(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Add(THandler(M));
end;

procedure TObservableEvent.Add(AHandler: THandler);
begin
  SetLength(FHandlers, Length(FHandlers) + 1);
  FHandlers[Length(FHandlers) - 1] := AHandler;
end;

procedure TObservableEvent.Del(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Del(THandler(M));
end;

procedure TObservableEvent.Del(AHandler: THandler);
var
  I: Integer;
begin
  I := Find(AHandler);
  if I <> -1 then
  begin
    if Length(FHandlers) - I > 1 then
      Move(FHandlers[I + 1], FHandlers[I], SizeOf(THandler) *
        (Length(FHandlers) - I - 1));
    SetLength(FHandlers, Length(FHandlers) - 1);
  end
  else
    raise EHandlerNotFound.Create;
end;

procedure TObservableEvent.Execute;
var
  Handler: THandler;
begin
  if (FHandlers = nil) or Disabled then
    Exit;
  for Handler in FHandlers do
  begin
    if TMethod(Handler).Data = nil then
      THandlerStatic(TMethod(Handler).Code)
    else
      Handler;
  end;
end;

procedure TObservableEvent.Disable;
begin
  Inc(FDisableCounter);
end;

procedure TObservableEvent.Enable;
begin
  Dec(FDisableCounter);
end;

{ TObservableEvent<T> }

function TObservableEvent<T>.Find(AHandler: THandler): Integer;
begin
  for Result := 0 to Length(FHandlers) - 1 do
    if @FHandlers[Result] = @AHandler then
      Exit;
  Result := -1;
end;

function TObservableEvent<T>.GetDisabled: Boolean;
begin
  Result := FDisableCounter > 0;
end;

function TObservableEvent<T>.GetEnabled: Boolean;
begin
  Result := FDisableCounter <= 0;
end;

procedure TObservableEvent<T>.Add(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Add(THandler(M));
end;

procedure TObservableEvent<T>.Add(AHandler: THandler);
begin
  SetLength(FHandlers, Length(FHandlers) + 1);
  FHandlers[Length(FHandlers) - 1] := AHandler;
end;

procedure TObservableEvent<T>.Del(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Del(THandler(M));
end;

procedure TObservableEvent<T>.Del(AHandler: THandler);
var
  I: Integer;
begin
  I := Find(AHandler);
  if I <> -1 then
  begin
    if Length(FHandlers) - I > 1 then
      Move(FHandlers[I + 1], FHandlers[I], SizeOf(THandler) *
        (Length(FHandlers) - I - 1));
    SetLength(FHandlers, Length(FHandlers) - 1);
  end
  else
    raise EHandlerNotFound.Create;
end;

procedure TObservableEvent<T>.Execute(AInfo: T; AFreeInfo: Boolean = True);
var
  Handler: THandler;
begin
  if (FHandlers <> nil) and Enabled then
  begin
    for Handler in FHandlers do
    begin
      if TMethod(Handler).Data = nil then
        THandlerStatic(TMethod(Handler).Code)(AInfo)
      else
        Handler(AInfo);
    end;
  end;
  if AFreeInfo then
    AInfo.Free;
end;

procedure TObservableEvent<T>.Disable;
begin
  Inc(FDisableCounter);
end;

procedure TObservableEvent<T>.Enable;
begin
  Dec(FDisableCounter);
end;

end.
