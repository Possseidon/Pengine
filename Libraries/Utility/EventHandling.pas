unit EventHandling;

interface

uses
  SysUtils;

type

  EHandlerNotFound = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>An event with a generic Sender</summary>
  IEventSender<T> = interface
  ['{E8AB15DA-3FBC-4FC2-8B4B-A9011DFD6B6B}']
    function Sender: T;
  end;

  /// <summary>An event, that can get canceled</summary> 
  /// <remarks>
  /// Once canceled, it is not possible to revert that.<para/>
  /// It is usually not a good idea to test for Canceled in a Handler.
  /// </remarks>
  IEventCancelable = interface
  ['{1F5810A2-9EFC-4860-A9D7-26846ACCA4F8}']
    function Canceled: Boolean;
    procedure Cancel;
  end;
  
  TEventInfo = class(TInterfacedObject)
  end;

  TObservableEvent = record
  public type
    THandler = procedure of object;
    TStaticHandler = procedure;

  private
    FDisableCounter: Integer;
    FHandlers: array of THandler;
                  
    function Find(AHandler: THandler): Integer; inline;     

    function GetDisabled: Boolean;
    function GetEnabled: Boolean;

  public
    procedure Add(AHandler: THandler); overload;
    procedure Add(AHandler: TStaticHandler); overload;
    procedure Del(AHandler: THandler); overload;
    procedure Del(AHandler: TStaticHandler); overload;

    procedure Execute; inline;

    /// <summary>Disable the execution of all event Handlers</summary>
    /// <remarks>
    /// Calling disable multiple times, prevent the execution until
    /// Enable has been called as often as Disable. This allows nesting.
    /// </remarks>
    procedure Disable;
    /// <summary>Re-enable the execution of all event Handlers</summary>
    /// <remarks>
    /// Enabled by default. If Disable got called multiple times,
    /// enable must be called the same amount of times, until it is enabled again
    /// </remarks>
    procedure Enable;

    property Disabled: Boolean read GetDisabled;
    property Enabled: Boolean read GetEnabled;

  end;

  TObservableEvent<T: TEventInfo> = record
  public type
    THandler = procedure (AInfo: T) of object;
    TStaticHandler = procedure (AInfo: T);

  private
    FDisableCounter: Integer;
    FHandlers: array of THandler;

    function Find(AHandler: THandler): Integer; inline; 

    function GetDisabled: Boolean;
    function GetEnabled: Boolean;

  public
    procedure Add(AHandler: THandler); overload;
    procedure Add(AHandler: TStaticHandler); overload;
    procedure Del(AHandler: THandler); overload;
    procedure Del(AHandler: TStaticHandler); overload;

    procedure Execute(AInfo: T; AFreeInfo: Boolean = True); overload; inline;

    /// <summary>Disable the execution of all event Handlers</summary>
    /// <remarks>
    /// Calling disable multiple times, prevent the execution until
    /// Enable has been called as often as Disable. This allows nesting.
    /// </remarks>
    procedure Disable;
    /// <summary>Re-enable the execution of all event Handlers</summary>
    /// <remarks>
    /// Enabled by default. If Disable got called multiple times,
    /// enable must be called the same amount of times, until it is enabled again
    /// </remarks>
    procedure Enable;

    property Disabled: Boolean read GetDisabled;
    property Enabled: Boolean read GetEnabled;
    
  end;

implementation

{ TObservableEvent<T> }

procedure TObservableEvent<T>.Add(AHandler: TStaticHandler);
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

procedure TObservableEvent<T>.Del(AHandler: TStaticHandler);
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
      Move(FHandlers[I + 1], FHandlers[I], SizeOf(THandler) * (Length(FHandlers) - I - 1));
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
        TStaticHandler(TMethod(Handler).Code)(AInfo)
      else
        Handler(AInfo);
    end;
  end;
  if AFreeInfo then
    AInfo.Free;
end;

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

procedure TObservableEvent<T>.Enable;
begin
  Dec(FDisableCounter);
end;

procedure TObservableEvent<T>.Disable;
begin
  Inc(FDisableCounter);
end;

{ TObservableEvent }

procedure TObservableEvent.Add(AHandler: TStaticHandler);
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

procedure TObservableEvent.Del(AHandler: TStaticHandler);
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
      Move(FHandlers[I + 1], FHandlers[I], SizeOf(THandler) * (Length(FHandlers) - I - 1));
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
      TStaticHandler(TMethod(Handler).Code)
    else
      Handler;
  end;
end;

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

procedure TObservableEvent.Enable;
begin
  Dec(FDisableCounter);
end;

procedure TObservableEvent.Disable;
begin
  Inc(FDisableCounter);
end;

{ EHandlerNotFound }

constructor EHandlerNotFound.Create;
begin
  inherited Create('Could not remove the Event-Handler as it doesn''t exist');
end;


end.