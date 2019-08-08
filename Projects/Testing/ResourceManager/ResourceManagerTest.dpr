program ResourceManagerTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,

  System.SysUtils,
  System.StrUtils,

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
    function CreateData(AParam: string): TMyObject; override;

  end;

{ TMyResource }

function TMyResource.CreateData(AParam: string): TMyObject;
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
  A := TMyResource.Get('hello');
  B := TMyResource.Get('world');
end;

begin

  try
    ReportMemoryLeaksOnShutdown := True;
    Test;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
