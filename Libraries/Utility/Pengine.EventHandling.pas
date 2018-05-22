unit Pengine.EventHandling;

interface

uses
  System.IOUtils, // TODO: Remove this

  System.SysUtils;

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
      procedure Remove(AHandler: THandlerStatic); overload; inline;
      /// <summary>Removes an event-handler from the list.</summary>
      procedure Remove(AHandler: THandler); overload;

    end;

  private
    FDisableCounter: Integer;

    function GetDisabled: Boolean; inline;
    function GetEnabled: Boolean; inline;

  public
    Handlers: TArray<THandler>;

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

    /// <returns>True, if the event has at least one event handler.</returns>
    function HasHandler: Boolean;

  end;

  TEvent<T: TEventInfo> = record
  public type
    THandlerStatic = procedure(AInfo: T);
    THandler = procedure(AInfo: T) of object;

    THandlerRec = record
    case Info: Boolean of
      False: (HandlerInfo: THandler);
      True: (Handler: TEvent.THandler);
    end;


    TAccess = record
    private
      FEvent: Pointer;

      function Find(AHandler: THandler): Integer; inline;

    public
      /// <summary>Adds a new static event-handler to the list.</summary>
      procedure Add(AHandler: THandlerStatic); overload; inline;
      /// <summary>Adds a new event-handler to the list.</summary>
      procedure Add(AHandler: THandler); overload;
      /// <summary>Adds a new static event-handler without info to the list.</summary>
      procedure Add(AHandler: TEvent.THandlerStatic); overload; inline;
      /// <summary>Adds a new event-handler without info to the list.</summary>
      procedure Add(AHandler: TEvent.THandler); overload;
      /// <summary>Removes a static event-handler from the list.</summary>
      procedure Remove(AHandler: THandlerStatic); overload; inline;
      /// <summary>Removes an event-handler from the list.</summary>
      procedure Remove(AHandler: THandler); overload;
      /// <summary>Removes a static event-handler without info from the list.</summary>
      procedure Remove(AHandler: TEvent.THandlerStatic); overload; inline;
      /// <summary>Removes an event-handler without info from the list.</summary>
      procedure Remove(AHandler: TEvent.THandler); overload;

    end;

  private
    FDisableCounter: Integer;

    function GetDisabled: Boolean; inline;
    function GetEnabled: Boolean; inline;

  public
    Handlers: TArray<THandlerRec>;

    /// <summary>Grants access to adding and removing handlers.</summary>
    function Access: TAccess;

    /// <summary>Execute each event handler with the given event-info object.</summary>
    procedure Execute(AInfo: T; AFreeInfo: Boolean = True); inline;

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
    function HasHandler: Boolean;

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

{ EEventHandlerNotFound }

constructor EEventHandlerNotFound.Create;
begin
  inherited Create('Could not remove the event-handler as it does not exist.');
end;

{ TEvent.TAccess }

function TEvent.TAccess.Find(AHandler: THandler): Integer;
var
  Method, FindMethod: TMethod;
begin
  FindMethod := TMethod(AHandler);
  for Result := Length(TEvent(FEvent^).Handlers) - 1 downto 0 do
  begin
    Method := TMethod(TEvent(FEvent^).Handlers[Result]);
    if (Method.Code = FindMethod.Code) and
       ((Method.Data = FindMethod.Data) or (Method.Data = nil)) then
      Exit;
  end;
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
  SetLength(TEvent(FEvent^).Handlers, Length(TEvent(FEvent^).Handlers) + 1);
  TEvent(FEvent^).Handlers[Length(TEvent(FEvent^).Handlers) - 1] := AHandler;
end;

procedure TEvent.TAccess.Remove(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Remove(THandler(M));
end;

procedure TEvent.TAccess.Remove(AHandler: THandler);
var
  I: Integer;
begin
  I := Find(AHandler);
  if I = -1 then
    raise EEventHandlerNotFound.Create;
  if Length(TEvent(FEvent^).Handlers) - I > 1 then
    Move(TEvent(FEvent^).Handlers[I + 1], TEvent(FEvent^).Handlers[I], SizeOf(THandler) *
      (Length(TEvent(FEvent^).Handlers) - I - 1));
  SetLength(TEvent(FEvent^).Handlers, Length(TEvent(FEvent^).Handlers) - 1);
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

function TEvent.HasHandler: Boolean;
begin
  Result := Length(Handlers) > 0;
end;

function TEvent.Access: TAccess;
begin
  Result.FEvent := @Self;
end;

procedure TEvent.Execute;
var
  Handler: THandler;
begin
  if (Handlers = nil) or Disabled then
    Exit;
  for Handler in Handlers do
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
var
  Method, FindMethod: TMethod;
begin
  FindMethod := TMethod(AHandler);
  for Result := Length(TEvent<T>(FEvent^).Handlers) - 1 downto 0 do
  begin
    Method := TMethod(TEvent<T>(FEvent^).Handlers[Result].Handler);
    if (Method.Code = FindMethod.Code) and
       ((Method.Data = FindMethod.Data) or (Method.Data = nil)) then
      Exit;
  end;
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
  // TFile.AppendAllText('log.txt', Format('%.16x add %.16x.%.16x' + sLineBreak, [UInt64(@Self), UInt64(TMethod(AHandler).Data), UInt64(TMethod(AHandler).Code)]));
  SetLength(TEvent<T>(FEvent^).Handlers, Length(TEvent<T>(FEvent^).Handlers) + 1);
  TEvent<T>(FEvent^).Handlers[Length(TEvent<T>(FEvent^).Handlers) - 1].Info := True;
  TEvent<T>(FEvent^).Handlers[Length(TEvent<T>(FEvent^).Handlers) - 1].HandlerInfo := AHandler;
end;

procedure TEvent<T>.TAccess.Add(AHandler: TEvent.THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Add(TEvent.THandler(M));
end;

procedure TEvent<T>.TAccess.Add(AHandler: TEvent.THandler);
begin
  // TFile.AppendAllText('log.txt', Format('%.16x add %.16x.%.16x' + sLineBreak, [UInt64(@Self), UInt64(TMethod(AHandler).Data), UInt64(TMethod(AHandler).Code)]));
  SetLength(TEvent<T>(FEvent^).Handlers, Length(TEvent<T>(FEvent^).Handlers) + 1);
  TEvent<T>(FEvent^).Handlers[Length(TEvent<T>(FEvent^).Handlers) - 1].Info := False;
  TEvent<T>(FEvent^).Handlers[Length(TEvent<T>(FEvent^).Handlers) - 1].HandlerInfo := THandler(AHandler);
end;

procedure TEvent<T>.TAccess.Remove(AHandler: THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Remove(THandler(M));
end;

procedure TEvent<T>.TAccess.Remove(AHandler: THandler);
var
  I: Integer;
begin
  // TFile.AppendAllText('log.txt', Format('%.16x del %.16x.%.16x' + sLineBreak, [UInt64(@Self), UInt64(TMethod(AHandler).Data), UInt64(TMethod(AHandler).Code)]));
  I := Find(AHandler);
  if I <> -1 then
  begin
    if Length(TEvent<T>(FEvent^).Handlers) - I > 1 then
      Move(TEvent<T>(FEvent^).Handlers[I + 1], TEvent<T>(FEvent^).Handlers[I], SizeOf(THandlerRec) *
        (Length(TEvent<T>(FEvent^).Handlers) - I - 1));
    SetLength(TEvent<T>(FEvent^).Handlers, Length(TEvent<T>(FEvent^).Handlers) - 1);
  end
  else
    raise EEventHandlerNotFound.Create;
end;

procedure TEvent<T>.TAccess.Remove(AHandler: TEvent.THandlerStatic);
var
  M: TMethod;
begin
  M.Code := @AHandler;
  M.Data := nil;
  Remove(TEvent.THandler(M));
end;

procedure TEvent<T>.TAccess.Remove(AHandler: TEvent.THandler);
var
  I: Integer;
begin
  // TFile.AppendAllText('log.txt', Format('%.16x del %.16x.%.16x' + sLineBreak, [UInt64(@Self), UInt64(TMethod(AHandler).Data), UInt64(TMethod(AHandler).Code)]));
  I := Find(THandler(AHandler));
  if I = -1 then
    raise EEventHandlerNotFound.Create;
  if Length(TEvent<T>(FEvent^).Handlers) - I > 1 then
    Move(TEvent<T>(FEvent^).Handlers[I + 1], TEvent<T>(FEvent^).Handlers[I], SizeOf(THandler) *
      (Length(TEvent<T>(FEvent^).Handlers) - I - 1));
  SetLength(TEvent<T>(FEvent^).Handlers, Length(TEvent<T>(FEvent^).Handlers) - 1);
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

function TEvent<T>.HasHandler: Boolean;
begin
  Result := Length(Handlers) > 0;
end;

procedure TEvent<T>.Execute(AInfo: T; AFreeInfo: Boolean = True);
var
  Handler: THandlerRec;
begin
  try
    if (Handlers <> nil) and Enabled then
    begin
      for Handler in Handlers do
      begin
        if Handler.Info then
        begin
          if TMethod(Handler.Handler).Data = nil then
            THandlerStatic(TMethod(Handler.HandlerInfo).Code)(AInfo)
          else
            Handler.HandlerInfo(AInfo);
        end
        else
        begin
          if TMethod(Handler.Handler).Data = nil then
            TEvent.THandlerStatic(TMethod(Handler.Handler).Code)
          else
            Handler.Handler;
        end;
      end;
    end;
  finally
    if AFreeInfo then
      AInfo.Free;
  end;
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

{ TSenderEventInfo<T> }

constructor TSenderEventInfo<T>.Create(ASender: T);
begin
  FSender := ASender;
end;

function TSenderEventInfo<T>.Sender: T;
begin
  Result := FSender;
end;

{ TCancelEventInfo<T> }

procedure TCancelEventInfo<T>.Cancel;
begin
  FCanceled := True;
end;

function TCancelEventInfo<T>.Canceled: Boolean;
begin
  Result := FCanceled;
end;

{ TSenderCancelEventInfo<T> }

procedure TSenderCancelEventInfo<T>.Cancel;
begin
  FCanceled := True;
end;

function TSenderCancelEventInfo<T>.Canceled: Boolean;
begin
  Result := FCanceled;
end;

end.
