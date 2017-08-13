unit Game;

interface

uses
  EntityDefine, Lists;

type

  TGame = class
  private
    FEntities: TObjectArray<TEntity>;

  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TGame }

constructor TGame.Create;
begin
  FEntities := TObjectArray<TEntity>.Create;
end;

destructor TGame.Destroy;
begin
  FEntities.Free;
  inherited;
end;

end.
