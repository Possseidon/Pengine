unit CustomModules;

interface

uses
  EntityDefine, VectorGeometry, VAOManager;

type

  TWheelModule = class(TBotModule)
  private

  protected
    class function GetSourceVAO: TVAO; override;
    class function GetInitialHealth: Single; override;

  public
    constructor Create(AParent: TBotCore; ASide: TBasicDir3); override;

  end;

implementation

{ TWheelModule }


class function TWheelModule.GetInitialHealth: Single;
begin
  Result := 20;
end;

class function TWheelModule.GetSourceVAO: TVAO;
begin
  Result := nil;
end;

{ TWheelModule }

constructor TWheelModule.Create(AParent: TBotCore; ASide: TBasicDir3);
begin
  inherited;
end;

end.

