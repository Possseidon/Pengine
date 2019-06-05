unit FactoryFrame;

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

  GdiPlus,

  Pengine.Vector,
  Pengine.Utility,

  FactoryDefine;

type
  TfrmFactory = class(TFrame)
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FFactory: TFactory;
    FDragMachineArray: TMachineArray;
    FConfigureMachineArray: TMachineArray;
    FDragInput: TMachineInput;
    FDragOutput: TMachineOutput;
    FDragPos: TOpt<TVector2>;
    FCamera: TVector2;

    procedure SetCamera(const Value: TVector2);

    procedure DrawConnection(G: IGPGraphics);

  protected
    procedure PaintWindow(DC: HDC); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Factory: TFactory read FFactory;

    property Camera: TVector2 read FCamera write SetCamera;
    function MouseToFactory(APos: TVector2): TVector2; overload;
    function MouseToFactory(X, Y: Single): TVector2; overload;

  end;

implementation

{$R *.dfm}


uses
  Main,
  RecipeForm;

{ frmFactory }

constructor TfrmFactory.Create(AOwner: TComponent);
begin
  inherited;
  ControlState := ControlState + [csCustomPaint];
  ControlStyle := ControlStyle + [csOpaque];
  FFactory := TFactory.Create(frmMain.Factorio);
  Factory.OnMachineArrayAdd.Add(Invalidate);
  Factory.OnMachineArrayRemove.Add(Invalidate);
  Factory.OnMachineArrayChange.Add(Invalidate);
  Factory.OnConnectionAdd.Add(Invalidate);
  Factory.OnConnectionRemove.Add(Invalidate);
  Factory.OnConnectionChange.Add(Invalidate);
end;

destructor TfrmFactory.Destroy;
begin
  FFactory.Free;
  inherited;
end;

procedure TfrmFactory.DrawConnection(G: IGPGraphics);
var
  Pen: IGPPen;
  Start, Stop: TVector2;
begin
  if (FDragInput = nil) and (FDragOutput = nil) then
    Exit;
  Pen := TGPPen.Create($7F7F7FFF, 16);
  Pen.StartCap := LineCapRound;
  Pen.EndCap := LineCapArrowAnchor;
  if FDragInput <> nil then
  begin
    Start := FDragPos;
    Stop := FDragInput.Pos + Vec2(0, 16);
  end
  else
  begin
    Start := FDragOutput.Pos + Vec2(32, 16);
    Stop := FDragPos;
  end;
  G.DrawLine(Pen, Start.X, Start.Y, Stop.X, Stop.Y);
end;

procedure TfrmFactory.FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
  Input: TMachineInput;
  Output: TMachineOutput;
begin
  if Button in [mbLeft, mbMiddle] then
    FDragPos := Vec2(X, Y);
  MachineArray := FFactory.MachineArrayAt(MouseToFactory(X, Y));
  if MachineArray <> nil then
  begin
    if Button = mbLeft then
    begin
      Input := MachineArray.InputAt(MouseToFactory(X, Y));
      if Input <> nil then
      begin
        FDragInput := Input;
        Exit;
      end;
      Output := MachineArray.OutputAt(MouseToFactory(X, Y));
      if Output <> nil then
      begin
        FDragOutput := Output;
        Exit;
      end;
      FDragMachineArray := MachineArray;
    end
    else if Button = mbRight then
      FConfigureMachineArray := MachineArray;
  end
  else
  begin
    if Button = mbRight then
    begin
      MachineArray := FFactory.AddMachineArray;
      MachineArray.Pos := MouseToFactory(X, Y) - MachineArray.Bounds.Size / 2;
      MachineArray.CraftingMachine := frmMain.Factorio.CraftingMachine['assembling-machine-1'];
    end;
  end;
end;

procedure TfrmFactory.FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
  Input: TMachineInput;
  Output: TMachineOutput;
begin
  if FDragMachineArray <> nil then
    FDragMachineArray.Pos := FDragMachineArray.Pos + Vec2(X, Y) - FDragPos
  else if FDragInput <> nil then
  begin
    Invalidate;
    MachineArray := Factory.MachineArrayAt(MouseToFactory(X, Y));
    if MachineArray = FDragInput.MachineArray then
    begin
      Input := MachineArray.InputAt(MouseToFactory(X, Y));
      if (Input <> nil) and (Input <> FDragInput) then
        FDragInput.Index := Input.Index;
    end;
  end
  else if FDragOutput <> nil then
  begin
    Invalidate;
    MachineArray := Factory.MachineArrayAt(MouseToFactory(X, Y));
    if MachineArray = FDragOutput.MachineArray then
    begin
      Output := MachineArray.OutputAt(MouseToFactory(X, Y));
      if (Output <> nil) and (Output <> FDragOutput) then
        FDragOutput.Index := Output.Index;
    end;
  end
  else if FDragPos.HasValue then
    Camera := FCamera + Vec2(X, Y) - FDragPos;

  if FDragPos.HasValue then
    FDragPos := Vec2(X, Y);
end;

procedure TfrmFactory.FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
  Output: TMachineOutput;
  Input: TMachineInput;
begin
  FDragPos.Clear;
  FDragMachineArray := nil;

  MachineArray := Factory.MachineArrayAt(MouseToFactory(X, Y));
  if MachineArray <> nil then
  begin
    if FConfigureMachineArray = MachineArray then
    begin
      frmRecipes.Execute(MachineArray);
      FConfigureMachineArray := nil;
      Exit;
    end;
    Input := MachineArray.InputAt(MouseToFactory(X, Y));
    Output := MachineArray.OutputAt(MouseToFactory(X, Y));
  end
  else
  begin
    Input := nil;
    Output := nil;
  end;

  if FDragOutput <> nil then
  begin
    Invalidate;
    if Input <> nil then
      FDragOutput.ToggleConnection(Input);
    FDragOutput := nil;
  end;
  if FDragInput <> nil then
  begin
    Invalidate;
    if Output <> nil then
      FDragInput.ToggleConnection(Output);
    FDragInput := nil;
  end;

  FConfigureMachineArray := nil;
end;

function TfrmFactory.MouseToFactory(X, Y: Single): TVector2;
begin
  Result := MouseToFactory(Vec2(X, Y));
end;

function TfrmFactory.MouseToFactory(APos: TVector2): TVector2;
begin
  Result := APos - FCamera;
end;

procedure TfrmFactory.PaintWindow(DC: HDC);
var
  G: IGPGraphics;
begin
  G := TGPGraphics.Create(DC);
  G.TranslateTransform(Camera.X, Camera.Y);
  G.SmoothingMode := SmoothingModeAntiAlias;
  G.TextRenderingHint := TextRenderingHintAntiAlias;
  Factory.Draw(G);
  DrawConnection(G);
end;

procedure TfrmFactory.SetCamera(const Value: TVector2);
begin
  if Camera = Value then
    Exit;
  FCamera := Value;
  Invalidate;
end;

end.
