unit Pengine.EventHandling;

interface

uses
  SysUtils;

type

  EEventHandlerNotFound = class(Exception)
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

  TEvent = record
  public type
    THandlerStatic = procedure;
    THandler = procedure of object;

    TAccess = record
    private
      FEvent: Pointer;

      function Find(AHandler: THandler): Integer; inline;

    public
      /// <summary>Adds a new static event-handler to the list.</summary>
      procedure Add(AHandler: THandlerStatic); overload; inline;
      /// <summary>Adds a new event-handler to the list.</summary>
      procedure Add(AHandler: THandler); overload;
      /// <summary>Removes a static event-handler from the list.</summary>
      procedure Del(AHandler: THandlerStatic); overload; inline;
      /// <summary>Adds an event-handler from the list.</summary>
      procedure Del(AHandler: THandler); overload;

    end;

  private
    FDisableCounter: Integer;
    FHandlers: TArray<THandler>;

    function GetDisabled: Boolean; inline;
    function GetEnabled: Boolean; inline;

  public
    /// <summary>Grants access to adding and removing handlers.</summary>
    function Access: TAccess; inline;

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

  TEvent<T: TEventInfo> = record
  public type
    THandlerStatic = procedure(AInfo: T);
    THandler = procedure(AInfo: T) of object;

    TAccess = record
    private
      FEvent: Pointer;

      function Find(AHandler: THandler): Integer; inline;

    public
      /// <summary>Adds a new static event-handler to the list.</summary>
      procedure Add(AHandler: THandlerStatic); overload; inline;
      /// <summary>Adds a new event-handler to the list.</summary>
      procedure Add(AHandler: THandler); overload;
      /// <summary>Removes a static event-handler from the list.</summary>
      procedure Del(AHandler: THandlerStatic); overload; inline;
      /// <summary>Adds an event-handler from the list.</summary>
      procedure Del(AHandler: THandler); overload;

    end;

  private
    FDisableCounter: Integer;
    FHandlers: TArray<THandler>;

    function GetDisabled: Boolean; inline;
    function GetEnabled: Boolean; inline;

  public
    /// <summary>Grants access to adding and removing handlers.</summary>
    function Access: TAccess;

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

{ EEventHandlerNotFound }

constructor EEventHandlerNotFound.Create;
begin
  inherited Create('Could not remove the event-handler as it does not exist.');
end;

{ TEvent.TAccess }

function TEvent.TAccess.Find(AHandler: THandler): Integer;
begin
  for Result := 0 to Length(TEvent(FEvent^).FHandlers) - 1 do
    if TMethod(TEvent(FEvent^).FHandlers[Result]) = TMethod(AHandler) then
      Exit;
  Result := -1;
end;

procedure TEvent.TAccess.Add(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Add(THandler(M));
end;

procedure TEvent.TAccess.Add(AHandler: THandler);
begin
  SetLength(TEvent(FEvent^).FHandlers, Length(TEvent(FEvent^).FHandlers) + 1);
  TEvent(FEvent^).FHandlers[Length(TEvent(FEvent^).FHandlers) - 1] := AHandler;
end;

procedure TEvent.TAccess.Del(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Del(THandler(M));
end;

procedure TEvent.TAccess.Del(AHandler: THandler);
var
  I: Integer;
begin
  I := Find(AHandler);
  if I <> -1 then
  begin
    if Length(TEvent(FEvent^).FHandlers) - I > 1 then
      Move(TEvent(FEvent^).FHandlers[I + 1], TEvent(FEvent^).FHandlers[I], SizeOf(THandler) *
        (Length(TEvent(FEvent^).FHandlers) - I - 1));
    SetLength(TEvent(FEvent^).FHandlers, Length(TEvent(FEvent^).FHandlers) - 1);
  end
  else
    raise EEventHandlerNotFound.Create;
end;

{ TEvent }

function TEvent.GetDisabled: Boolean;
begin
  Result := FDisableCounter > 0;
end;

function TEvent.GetEnabled: Boolean;
begin
  Result := FDisableCounter <= 0;
end;

function TEvent.Access: TAccess;
begin
  Result.FEvent := @Self;
end;

procedure TEvent.Execute;
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

procedure TEvent.Disable;
begin
  Inc(FDisableCounter);
end;

procedure TEvent.Enable;
begin
  Dec(FDisableCounter);
end;

{ TEvent<T>.TAccess }

function TEvent<T>.TAccess.Find(AHandler: THandler): Integer;
begin
  for Result := 0 to Length(TEvent<T>(FEvent^).FHandlers) - 1 do
    if TMethod(TEvent<T>(FEvent^).FHandlers[Result]) = TMethod(AHandler) then
      Exit;
  Result := -1;
end;

procedure TEvent<T>.TAccess.Add(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Add(THandler(M));
end;

procedure TEvent<T>.TAccess.Add(AHandler: THandler);
begin
  SetLength(TEvent<T>(FEvent^).FHandlers, Length(TEvent<T>(FEvent^).FHandlers) + 1);
  TEvent<T>(FEvent^).FHandlers[Length(TEvent<T>(FEvent^).FHandlers) - 1] := AHandler;
end;

procedure TEvent<T>.TAccess.Del(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Del(THandler(M));
end;

procedure TEvent<T>.TAccess.Del(AHandler: THandler);
var
  I: Integer;
begin
  I := Find(AHandler);
  if I <> -1 then
  begin
    if Length(TEvent<T>(FEvent^).FHandlers) - I > 1 then
      Move(TEvent<T>(FEvent^).FHandlers[I + 1], TEvent<T>(FEvent^).FHandlers[I], SizeOf(THandler) *
        (Length(TEvent<T>(FEvent^).FHandlers) - I - 1));
    SetLength(TEvent<T>(FEvent^).FHandlers, Length(TEvent<T>(FEvent^).FHandlers) - 1);
  end
  else
    raise EEventHandlerNotFound.Create;
end;

{ TEvent<T> }

function TEvent<T>.GetDisabled: Boolean;
begin
  Result := FDisableCounter > 0;
end;

function TEvent<T>.GetEnabled: Boolean;
begin
  Result := FDisableCounter <= 0;
end;

procedure TEvent<T>.Execute(AInfo: T; AFreeInfo: Boolean = True);
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

function TEvent<T>.Access: TAccess;
begin
  Result.FEvent := @Self;
end;

procedure TEvent<T>.Disable;
begin
  Inc(FDisableCounter);
end;

procedure TEvent<T>.Enable;
begin
  Dec(FDisableCounter);
end;

end.
