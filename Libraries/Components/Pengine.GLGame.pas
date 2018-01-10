unit Pengine.GLGame;

interface

uses
  Pengine.InputHandler,
  Pengine.GLState,
  Pengine.EventHandling,
  Pengine.TimeManager,
  Pengine.IntMaths;

type

  TGLGame = class
  private
    FGLState: TGLState;
    FInput: TInputHandler;
    FTimer: TDeltaTimer;
    FResolution: TIntVector2;

    FOnResize: TEvent;
    FOnUpdate: TEvent;
    FOnRender: TEvent;

    function GetOnResize: TEvent.TAccess;
    function GetOnUpdate: TEvent.TAccess;
    function GetOnRender: TEvent.TAccess;
    function GetDeltaTime: Single;
    function GetSeconds: Single;
    function GetFPS: Single;
    function GetAspect: Single;

  public
    constructor Create(AGLState: TGLState; AInput: TInputHandler; ATimer: TDeltaTimer; AResolution: TIntVector2);

    procedure Resize(AResolution: TIntVector2);
    procedure Update;
    procedure Render;

    property GLState: TGLState read FGLState;

    property Input: TInputHandler read FInput;

    property Timer: TDeltaTimer read FTimer;
    property DeltaTime: Single read GetDeltaTime;
    property Seconds: Single read GetSeconds;
    property FPS: Single read GetFPS;

    property Resolution: TIntVector2 read FResolution;
    property Aspect: Single read GetAspect;

    property OnResize: TEvent.TAccess read GetOnResize;
    property OnUpdate: TEvent.TAccess read GetOnUpdate;
    property OnRender: TEvent.TAccess read GetOnRender;

  end;

implementation

{ TGLGame }

function TGLGame.GetOnResize: TEvent.TAccess;
begin
  Result := FOnResize.Access;
end;

function TGLGame.GetOnUpdate: TEvent.TAccess;
begin
  Result := FOnUpdate.Access;
end;

function TGLGame.GetAspect: Single;
begin
  Result := Resolution.X / Resolution.Y;
end;

function TGLGame.GetDeltaTime: Single;
begin
  Result := Timer.DeltaTime;
end;

function TGLGame.GetFPS: Single;
begin
  Result := Timer.FPS;
end;

function TGLGame.GetSeconds: Single;
begin
  Result := Timer.Seconds;
end;

function TGLGame.GetOnRender: TEvent.TAccess;
begin
  Result := FOnRender.Access;
end;

constructor TGLGame.Create(AGLState: TGLState; AInput: TInputHandler; ATimer: TDeltaTimer; AResolution: TIntVector2);
begin
  FGLState := AGLState;
  FInput := AInput;
  FTimer := ATimer;
  FResolution := AResolution;
end;

procedure TGLGame.Resize(AResolution: TIntVector2);
begin
  FResolution := AResolution;
  FOnResize.Execute;
end;

procedure TGLGame.Update;
begin
  FOnUpdate.Execute;
end;

procedure TGLGame.Render;
begin
  FOnRender.Execute;
end;

end.
