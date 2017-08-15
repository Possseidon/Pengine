unit CustomModules;

interface

uses
  EntityDefine, VectorGeometry, VAOManager, ResourceManager;

type

  TWheelModule = class(TBotModule)
  private

  protected
    class function GetSourceVAO: TVAO; override;
    class function GetInitialHealth: Single; override;
    class function GetInitialName: AnsiString; override;

  public
    constructor Create(AParent: TBotCore; ASide: TBasicDir3); override;
    destructor Destroy; override;

  end;

implementation

{ TWheelModule }

class function TWheelModule.GetSourceVAO: TVAO;
var
  Params: TResCubeVAOParams;
begin
  Params := TResCubeVAOParams.Create;
  Params.Size := 0.75;
  Result := TResCubeVAO.Make(Params);
end;

class function TWheelModule.GetInitialHealth: Single;
begin
  Result := 20;
end;

class function TWheelModule.GetInitialName: AnsiString;
begin
  Result := 'Wheel-Module';
end;

destructor TWheelModule.Destroy;
begin
  SourceVAO.Free;
  inherited;
end;

constructor TWheelModule.Create(AParent: TBotCore; ASide: TBasicDir3);
begin
  inherited;
  Location.Offset := 0.125;
end;

end.

