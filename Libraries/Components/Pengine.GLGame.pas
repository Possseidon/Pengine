unit Pengine.GLGame;

interface

uses
  System.IOUtils,

  Pengine.InputHandler,
  Pengine.GLState,
  Pengine.EventHandling,
  Pengine.TimeManager,
  Pengine.IntMaths,
  Pengine.GLContext;

type

  TGLGame = class
  private
    FGLContext: TGLContext;
    FInput: TInputHandler;
    FTimer: TDeltaTimer;
    FResolution: TIntVector2;

    FOnResize: TEvent;
    FOnUpdate: TEvent;
    FOnRender: TEvent;

    function GetOnResize: TEvent.TAccess;
    function GetOnUpdate: TEvent.TAccess;
    function GetOnRender: TEvent.TAccess;
    function GetDeltaTime: TSeconds;
    function GetTime: TSeconds;
    function GetFPS: Single;
    function GetAspect: Single;
    function GetGLState: TGLState;

  public
    constructor Create(AGLContext: TGLContext; AInput: TInputHandler; ATimer: TDeltaTimer; AResolution: TIntVector2);

    procedure Resize(AResolution: TIntVector2);
    procedure Update;
    procedure Render;

    property GLContext: TGLContext read FGLContext;
    property GLState: TGLState read GetGLState;

    property Input: TInputHandler read FInput;

    property Timer: TDeltaTimer read FTimer;
    property DeltaTime: TSeconds read GetDeltaTime;
    property Time: TSeconds read GetTime;
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

function TGLGame.GetDeltaTime: TSeconds;
begin
  Result := Timer.DeltaTime;
end;

function TGLGame.GetFPS: Single;
begin
  Result := Timer.FPS;
end;

function TGLGame.GetGLState: TGLState;
begin
  Result := GLContext.GLState;
end;

function TGLGame.GetTime: TSeconds;
begin
  Result := Timer.Time;
end;

function TGLGame.GetOnRender: TEvent.TAccess;
begin
  Result := FOnRender.Access;
end;

constructor TGLGame.Create(AGLContext: TGLContext; AInput: TInputHandler; ATimer: TDeltaTimer; AResolution: TIntVector2);
begin
  FGLContext := AGLContext;
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
