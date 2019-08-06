program ResourceManagerTest;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,

  Pengine.DebugConsole,
  Pengine.ResourceManager;

type

  TMyObject = class
  private
    FText: string;

  public
    constructor Create(AText: string);
    destructor Destroy; override;

  end;

  TMyResource = class(TParamResource<TMyObject, string>)
  protected
    class function CreateData(AParam: string): TMyObject; override;

  end;

{ TMyResource }

class function TMyResource.CreateData(AParam: string): TMyObject;
begin
  Result := TMyObject.Create(AParam);
end;

{ TMyObject }

constructor TMyObject.Create(AText: string);
begin
  FText := AText;
  Writeln(FText, ' says hello!');
end;

destructor TMyObject.Destroy;
begin
  Writeln(FText, ' says goodbye!');
  inherited;
end;

procedure Test;
var
  A, B, C, D, E, F, G, H, I, J: IResource<TMyObject>;
begin
  A := TMyResource.Get('abc');
  B := TMyResource.Get('abc');
  C := TMyResource.Get('is');

  ResourceManager.Test;

  D := TMyResource.Get('this');
  E := TMyResource.Get('world');
  F := TMyResource.Get('abc');
  G := TMyResource.Get('abc');
  H := TMyResource.Get('world');
  I := TMyResource.Get('abc');
  J := TMyResource.Get('this');

  ResourceManager.Test;

end;

begin
  try
    ReportMemoryLeaksOnShutdown := True;
    Test;
    ResourceManager.Test;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
