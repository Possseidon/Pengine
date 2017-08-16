unit CustomModules;

interface

uses
  EntityDefine, VectorGeometry, VAOManager, Resources;

type

  TWheelModule = class(TBotModule)
  private
    FModelParams: TResCubeVAOParams;

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
begin
  FModelParams := TResCubeVAOParams.Create;
  FModelParams.Size := 0.75;
  FModelParams.Texture := 'holed_ironplating';
  Result := TResCubeVAO.Make(FModelParams);
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
  TResCubeVAO.Release(FModelParams);
  inherited;
end;

constructor TWheelModule.Create(AParent: TBotCore; ASide: TBasicDir3);
begin
  inherited;
  Location.Offset := 0.125;
end;

end.

