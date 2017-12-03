unit GameLogicDefine;

interface

uses
  Pengine.Lua;

type

  { TGameLogic }

  TGameLogic = class abstract
  private
    FLua: TLua;

  public
    constructor Create(ALua: TLua);
    destructor Destroy; override;

  end;

implementation

{ TGameLogic }

constructor TGameLogic.Create(ALua: TLua);
begin
  FLua := ALua;
end;

destructor TGameLogic.Destroy;
begin

  inherited;
end;

end.

