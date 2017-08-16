unit CustomModules;

interface

uses
  EntityDefine, VectorGeometry, VAOManager, Resources;

type

  TWheelModule = class(TBotModule)
  private
    class function GetModelParams: TResCubeVAOParams;

  protected
    class function GetSourceVAO: TVAO; override;
    class procedure FreeSourceVAO; override;
    class function GetInitialHealth: Single; override;
    class function GetInitialName: AnsiString; override;

  public
    constructor Create(AParent: TBotCore; ASide: TBasicDir3); override;

  end;

implementation

{ TWheelModule }

class function TWheelModule.GetSourceVAO: TVAO;
begin
  Result := TResCubeVAO.Make(GetModelParams);
end;

class function TWheelModule.GetInitialHealth: Single;
begin
  Result := 20;
end;

class function TWheelModule.GetInitialName: AnsiString;
begin
  Result := 'Wheel-Module';
end;

class function TWheelModule.GetModelParams: TResCubeVAOParams;
begin
  Result := TResCubeVAOParams.Create;
  Result.Size := 0.75;
  Result.Texture := 'holed_ironplating';
end;

class procedure TWheelModule.FreeSourceVAO;
begin
  TResCubeVAO.Release(GetModelParams);
end;

constructor TWheelModule.Create(AParent: TBotCore; ASide: TBasicDir3);
begin
  inherited;
  Location.Offset := 0.125;
end;

end.

