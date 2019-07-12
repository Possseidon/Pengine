unit Pengine.Coroutines;

interface

uses
  System.SysUtils;

type

  TCoroutine = class
  private
    FStarted: Boolean;
    FProc: TProc<TCoroutine>;
    FStack: array of Byte;
    FAddress: PByte;

    procedure FirstSwitch;
    procedure Switch;

  public
    constructor Create(AProc: TProc<TCoroutine>);

    procedure Yield;
    procedure Resume;

  end;

implementation

{ TCoroutine }

constructor TCoroutine.Create;
begin
  SetLength(FStack, 1024);
  FAddress := @FStack[0];
  Inc(FAddress, Length(FStack));
end;

procedure TCoroutine.FirstSwitch;
asm
  PUSHAD
  XCHG ESP, Self.FStack
end;

procedure TCoroutine.Resume;
begin
  if not FStarted then
  begin
    FirstSwitch;
    FStarted := True;
    FProc(Self);
  end
  else
    Switch;
end;

procedure TCoroutine.Switch;
asm
  PUSHAD
  XCHG ESP, Self.FStack
  POPAD
end;

procedure TCoroutine.Yield;
begin
  Switch;
end;

end.

