unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Menus,

  GdiPlus,
  GdiPlusHelpers,

  Pengine.DebugConsole,
  Pengine.Vector,

  Pengine.Factorio.General,

  FactoryDefine,
  RecipeForm,
  FactoryFrame;

type
  TfrmMain = class(TForm)
    mmMain: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    New1: TMenuItem;
    Saveas1: TMenuItem;
    N2: TMenuItem;
    frmFactory: TfrmFactory;
  private
    FFactorio: TFactorio;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Factorio: TFactorio read FFactorio;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  FFactorio := TFactorio.Create;
  inherited;
end;

destructor TfrmMain.Destroy;
begin
  inherited;
  FFactorio.Free;
end;

end.
