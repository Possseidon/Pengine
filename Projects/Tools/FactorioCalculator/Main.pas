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
  Pengine.ActionRecorder,

  Pengine.Factorio.General,

  FactoryDefine,
  RecipeForm,
  FactoryFrame,
  System.Actions,
  Vcl.ActnList;

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
    alMain: TActionList;
    actNew: TAction;
    actSaveAs: TAction;
    actSave: TAction;
    actOpen: TAction;
    actExit: TAction;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    procedure actExitExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta:
      Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FFactorio: TFactorio;
    FFilename: string;
    procedure SetFilename(const Value: string);
    procedure UpdateCaption;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Factorio: TFactorio read FFactorio;

    property Filename: string read FFilename write SetFilename;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  FFactorio := TFactorio.Create;
  FFactorio.Load;
  inherited;
  UpdateCaption;
end;

destructor TfrmMain.Destroy;
begin
  inherited;
  FFactorio.Free;
end;

procedure TfrmMain.UpdateCaption;
begin
  if Filename.IsEmpty then
    Caption := 'Factorio Calculator'
  else
    Caption := Format('%s - Factorio Calculator', [ChangeFileExt(ExtractFileName(Filename), '')]);
end;

procedure TfrmMain.SetFilename(const Value: string);
begin
  if Filename = Value then
    Exit;
  FFilename := Value;
  UpdateCaption;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  frmFactory.Factory.Clear;
  Filename := '';
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  if not odOpen.Execute then
    Exit;
  Filename := odOpen.Filename;
  frmFactory.Factory.LoadFromFile(Filename);
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
begin
  if not sdSave.Execute then
    Exit;
  Filename := sdSave.Filename;
  frmFactory.Factory.SaveToFile(Filename);
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if Filename.IsEmpty then
    actSaveAsExecute(Sender)
  else
    frmFactory.Factory.SaveToFile(Filename);
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  MousePos := frmFactory.ScreenToClient(MousePos);
  if frmFactory.BoundsRect.Contains(MousePos) then
    frmFactory.OnMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

end.
