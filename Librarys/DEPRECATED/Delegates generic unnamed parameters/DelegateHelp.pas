{$IF DEFINED(INTERFACE_SECTION)}

  TDelegate{$IFDEF DELEGATE1}<{$DEFINE GENERIC}{$I DelegateSnippets}{$UNDEF GENERIC}>{$ENDIF} = record
  public type
    THandler = procedure ({$DEFINE PARAMS}{$I DelegateSnippets}{$UNDEF PARAMS}) of object;
    TStaticHandler = procedure ({$DEFINE PARAMS}{$I DelegateSnippets}{$UNDEF PARAMS});

  private
    FHandlers: array of THandler;

    function Find(const AHandler: THandler): Integer;

  public
    procedure Add(const AHandler: THandler); overload;
    procedure Add(const AHandler: TStaticHandler); overload;
    procedure Del(const AHandler: THandler); overload;
    procedure Del(const AHandler: TStaticHandler); overload;

    procedure Call({$DEFINE PARAMS}{$I DelegateSnippets}{$UNDEF PARAMS}); inline;
  end;

{$ELSEIF DEFINED(IMPLEMENTATION_SECTION)}

procedure TDelegate{$IFDEF DELEGATE1}<{$DEFINE GENERIC}{$I DelegateSnippets}{$UNDEF GENERIC}>{$ENDIF}.Del(const AHandler: THandler);
var
  I: Integer;
begin
  I := Find(AHandler);
  if I <> -1 then
  begin
    if Length(FHandlers) - I > 1 then
      Move(FHandlers[I + 1], FHandlers[I], SizeOf(THandler) * (Length(FHandlers) - I - 1));
    SetLength(FHandlers, Length(FHandlers) - 1)
  end
  else
    raise EDelegateNotFound.Create;
end;

procedure TDelegate{$IFDEF DELEGATE1}<{$DEFINE GENERIC}{$I DelegateSnippets}{$UNDEF GENERIC}>{$ENDIF}.Del(const AHandler: TStaticHandler);
var
  Method: TMethod;
begin
  Method.Data := nil;
  Method.Code := @AHandler;
  Del(THandler(Method));
end;

function TDelegate{$IFDEF DELEGATE1}<{$DEFINE GENERIC}{$I DelegateSnippets}{$UNDEF GENERIC}>{$ENDIF}.Find(const AHandler: THandler): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(FHandlers) - 1 do
    {$IFDEF FPC}
    if CompareByte(FHandlers[I], AHandler, SizeOf(THandler)) = 0 then {$ELSE}
    if @FHandlers[I] = @AHandler then {$ENDIF}
      Exit(I);
  Result := -1;
end;

procedure TDelegate{$IFDEF DELEGATE1}<{$DEFINE GENERIC}{$I DelegateSnippets}{$UNDEF GENERIC}>{$ENDIF}.Add(const AHandler: THandler);
begin
  SetLength(FHandlers, Length(FHandlers) + 1);
  FHandlers[Length(FHandlers) - 1] := AHandler;
end;

procedure TDelegate{$IFDEF DELEGATE1}<{$DEFINE GENERIC}{$I DelegateSnippets}{$UNDEF GENERIC}>{$ENDIF}.Add(const AHandler: TStaticHandler);
begin
  SetLength(FHandlers, Length(FHandlers) + 1);
  with TMethod(FHandlers[Length(FHandlers) - 1]) do
  begin
    Code := @AHandler;
    Data := nil;
  end;
end;

procedure TDelegate{$IFDEF DELEGATE1}<{$DEFINE GENERIC}{$I DelegateSnippets}{$UNDEF GENERIC}>{$ENDIF}.Call({$DEFINE PARAMS}{$I DelegateSnippets}{$UNDEF PARAMS});
var
  Handler: THandler;
begin
  if FHandlers = nil then
    Exit;
  for Handler in FHandlers do
  begin
    if TMethod(Handler).Data = nil then
      TStaticHandler(TMethod(Handler).Code)({$DEFINE CALL}{$I DelegateSnippets}{$UNDEF CALL})
    else
      Handler({$DEFINE CALL}{$I DelegateSnippets}{$UNDEF CALL});
  end;
end;

{$ENDIF}
