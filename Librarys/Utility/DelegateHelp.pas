{$IF DEFINED(INTERFACE_SECTION)}

  {$DEFINE HEADER}{$I DelegateSnippets}{$UNDEF HEADER}<T> = record
  private
    type
      TCall = procedure (const {$DEFINE PARAMLIST}{$I DelegateSnippets}{$UNDEF PARAMLIST}: Pointer) of object;
      {$IFNDEF FPC}
      PCall = ^TCall;
      {$ENDIF}
  private
    FItems: array of T;

    procedure CallFunction(const {$DEFINE PARAMLIST}{$I DelegateSnippets}{$UNDEF PARAMLIST}: Pointer);
    function GetCallFunction: T;

    function Find(const AMethod: T): Integer;

  public
    procedure Add(const AMethod: T);
    procedure Del(const AMethod: T);

    property Call: T read GetCallFunction;
  end;

{$ELSEIF DEFINED(IMPLEMENTATION_SECTION)}

function {$DEFINE HEADER}{$I DelegateSnippets}{$UNDEF HEADER}<T>.GetCallFunction: T;
var
  {$IFDEF FPC}
  Func: TMethod; {$ELSE}
  Func: TCall;   {$ENDIF}
begin
  {$IFDEF FPC}
  Func.Code := @CallFunction;
  Func.Data := @Self;
  {$ELSE}
  Func := CallFunction;
  {$ENDIF}
  Move(Func, Result{%H-}, SizeOf(TMethod));
end;

procedure {$DEFINE HEADER}{$I DelegateSnippets}{$UNDEF HEADER}<T>.Del(const AMethod: T);
var
  I: Integer;
begin
  I := Find(AMethod);
  if I <> -1 then
  begin
    if Length(FItems) - I > 1 then
      Move(FItems[I + 1], FItems[I], SizeOf(TMethod) * (Length(FItems) - I - 1));
    SetLength(FItems, Length(FItems) - 1)
  end;
end;

function {$DEFINE HEADER}{$I DelegateSnippets}{$UNDEF HEADER}<T>.Find(const AMethod: T): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
    {$IFDEF FPC}
    if CompareByte(FItems[I], AMethod, SizeOf(TMethod)) = 0 then {$ELSE}
    if CompareMem(@FItems[I], @AMethod, SizeOf(TMethod)) then {$ENDIF}
      Exit(I);
  Result := -1;
end;

procedure {$DEFINE HEADER}{$I DelegateSnippets}{$UNDEF HEADER}<T>.CallFunction(const {$DEFINE PARAMLIST}{$I DelegateSnippets}{$UNDEF PARAMLIST}: Pointer);
var
  Func: T;
begin
  for Func in FItems do
    {$IFDEF FPC}
    TCall(Func)({$DEFINE PARAMLIST}{$I DelegateSnippets}{$UNDEF PARAMLIST}); {$ELSE}
    PCall(@Func)^({$DEFINE PARAMLIST}{$I DelegateSnippets}{$UNDEF PARAMLIST}); {$ENDIF}
end;

procedure {$DEFINE HEADER}{$I DelegateSnippets}{$UNDEF HEADER}<T>.Add(const AMethod: T);
begin
  if Find(AMethod) = -1 then
  begin
    SetLength(FItems, Length(FItems) + 1);
    FItems[Length(FItems) - 1] := AMethod;
  end;
end;
{$ENDIF}
