unit FactoryFrame;

interface

uses
  Winapi.Windows,

  System.Classes,
  System.Math,

  Vcl.Controls,
  Vcl.Forms,

  GdiPlus,

  Pengine.Vector,
  Pengine.Utility,
  Pengine.EventHandling,
  Pengine.IntMaths,

  Pengine.Factorio.General,

  FactoryDefine;

type

  TCamera = class
  private
    FOnChange: TEvent<TCamera>;
    FPos: TVector2;
    FScale: Single;

    procedure SetPos(const Value: TVector2);
    procedure SetScale(const Value: Single);
    function GetOnChange: TEvent<TCamera>.TAccess;

  public
    constructor Create;

    procedure Assign(AFrom: TCamera);
    function Copy: TCamera;

    property OnChange: TEvent<TCamera>.TAccess read GetOnChange;

    procedure Reset;

    property Pos: TVector2 read FPos write SetPos;
    property Scale: Single read FScale write SetScale;

    procedure Move(AOffset: TVector2);
    procedure Zoom(AFactor: Single; APos: TVector2);

    function CalculateMatrix(ASize: TIntVector2): IGPMatrix;

  end;

  TfrmFactory = class(TFrame)
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure FrameResize(Sender: TObject);
  private
    FFactory: TFactory;
    FDragMachineArray: TMachineArray;
    FDragOffset: TVector2;
    FConfigureMachineArray: TMachineArray;
    FDragMachinePort: TMachinePort;
    FDragCreate: Boolean;
    FDragPos: TOpt<TVector2>;
    FCamera: TCamera;
    FGridSnap: TOpt<Single>;

    procedure DrawConnection(G: IGPGraphics);

    function CalculateCameraMatrix: IGPMatrix;

    function TransformHelper(APos: TVector2; ABack: Boolean): TVector2;
    // function FactoryToMouse(APos: TVector2): TVector2;
    function MouseToFactory(X, Y: Single): TVector2; overload;
    function MouseToFactory(APos: TVector2): TVector2; overload;

    function SnapToGrid(APos: TVector2): TVector2;

  protected
    procedure PaintWindow(DC: HDC); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Factory: TFactory read FFactory;

    property Camera: TCamera read FCamera;
    property GridSnap: TOpt<Single> read FGridSnap write FGridSnap;

  end;

implementation

{$R *.dfm}


uses
  Main,
  RecipeForm;

{ frmFactory }

function TfrmFactory.CalculateCameraMatrix: IGPMatrix;
begin
  Result := FCamera.CalculateMatrix(IVec2(Width, Height));
end;

constructor TfrmFactory.Create(AOwner: TComponent);
begin
  inherited;
  ControlState := ControlState + [csCustomPaint];
  ControlStyle := ControlStyle + [csOpaque];

  FGridSnap := 8;

  FCamera := TCamera.Create;
  FCamera.OnChange.Add(Invalidate);

  FFactory := TFactory.Create(frmMain.Factorio);
  Factory.OnMachineArrayAdd.Add(Invalidate);
  Factory.OnMachineArrayRemove.Add(Invalidate);
  Factory.OnMachineArrayChange.Add(Invalidate);
  Factory.OnConnectionNetAdd.Add(Invalidate);
  Factory.OnConnectionNetRemove.Add(Invalidate);
  Factory.OnConnectionNetChange.Add(Invalidate);
end;

destructor TfrmFactory.Destroy;
begin
  FFactory.Free;
  FCamera.Free;
  inherited;
end;

procedure TfrmFactory.DrawConnection(G: IGPGraphics);
var
  Pen: IGPPen;
  Start, Stop: TVector2;
  MachineArray: TMachineArray;
begin
  if FDragMachinePort = nil then
    Exit;
  MachineArray := Factory.MachineArrayAt(MouseToFactory(FDragPos));

  if (FDragMachinePort <> nil) and (MachineArray = FDragMachinePort.MachineArray) then
    Exit;

  if FDragCreate then
    Pen := TGPPen.Create($7FFF7F7F, 16)
  else
    Pen := TGPPen.Create($7F7F7FFF, 16);
  Pen.StartCap := LineCapRound;
  Pen.EndCap := LineCapArrowAnchor;
  if FDragMachinePort is TMachineInput then
  begin
    Start := MouseToFactory(FDragPos);
    Stop := FDragMachinePort.Pos + Vec2(0, 16);
  end
  else
  begin
    Start := FDragMachinePort.Pos + Vec2(32, 16);
    Stop := MouseToFactory(FDragPos);
  end;
  Start := Start;
  Stop := Stop;
  G.DrawLine(Pen, Start.X, Start.Y, Stop.X, Stop.Y);
end;

procedure TfrmFactory.FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
  Port: TMachinePort;
begin
  MouseCapture := True;
  if Button in [mbLeft, mbMiddle] then
    FDragPos := Vec2(X, Y);
  MachineArray := FFactory.MachineArrayAt(MouseToFactory(X, Y));
  if MachineArray <> nil then
  begin
    if Button = mbLeft then
    begin
      Port := MachineArray.PortAt(MouseToFactory(X, Y));
      if Port <> nil then
      begin
        FDragMachinePort := Port;
        Exit;
      end;
      FDragMachineArray := MachineArray;
      FDragOffset := MouseToFactory(X, Y) - MachineArray.Pos;
    end
    else if Button = mbRight then
    begin
      FConfigureMachineArray := MachineArray;
      Port := MachineArray.InputAt(MouseToFactory(X, Y));
      if Port <> nil then
      begin
        FDragMachinePort := Port;
        FDragCreate := True;
        FDragPos := Vec2(X, Y);
        Exit;
      end;
    end;
  end
  else
  begin
    if Button = mbRight then
    begin
      MachineArray := FFactory.AddMachineArray;
      MachineArray.Pos := SnapToGrid(MouseToFactory(X, Y) - MachineArray.Bounds.Size / 2);
      MachineArray.CraftingMachine := frmMain.Factorio.CraftingMachine['assembling-machine-1'];
    end;
  end;
end;

procedure TfrmFactory.FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Port: TMachinePort;
begin
  if FDragMachineArray <> nil then
    FDragMachineArray.Pos := SnapToGrid(MouseToFactory(X, Y) - FDragOffset)
  else if FDragMachinePort <> nil then
  begin
    Invalidate;
    Port := Factory.MachinePortAt(MouseToFactory(X, Y));
    if (Port <> nil) and (Port <> FDragMachinePort)
      and (Port.MachineArray = FDragMachinePort.MachineArray)
      and (Port.ClassType = FDragMachinePort.ClassType) then
      FDragMachinePort.Index := Port.Index;
  end
  else if FDragPos.HasValue then
    Camera.Move(FDragPos - Vec2(X, Y));

  if FDragPos.HasValue then
    FDragPos := Vec2(X, Y);
end;

procedure TfrmFactory.FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
  Port: TMachinePort;
  Output: TMachineOutput;
  Recipe: TFactorio.TRecipe;
begin
  MouseCapture := False;
  FDragPos.Clear;
  FDragMachineArray := nil;

  MachineArray := Factory.MachineArrayAt(MouseToFactory(X, Y));
  if MachineArray <> nil then
  begin
    if FConfigureMachineArray = MachineArray then
    begin
      frmRecipes.Execute(MachineArray);
      FConfigureMachineArray := nil;
      FDragMachinePort := nil;
      Invalidate;
      FDragCreate := False;
      Exit;
    end;
    Port := MachineArray.PortAt(MouseToFactory(X, Y));
  end
  else
    Port := nil;
  FConfigureMachineArray := nil;

  if FDragMachinePort <> nil then
  begin
    Invalidate;

    if FDragCreate then
    begin
      Assert(FDragMachinePort is TMachineInput);
      Recipe := TMachineInput(FDragMachinePort).Ingredient.Item.FindRecipe;
      if Recipe <> nil then
      begin
        MachineArray := Factory.AddMachineArray;
        MachineArray.Recipe := Recipe;
        MachineArray.Pos := SnapToGrid(MouseToFactory(X, Y) - MachineArray.Bounds.Size / 2);
        for Output in MachineArray.Outputs do
        begin
          if Output.ItemStack.Item = FDragMachinePort.ItemStack.Item then
          begin
            Factory.ToggleConnection(FDragMachinePort, Output);
            Break;
          end;
        end;
      end;
      FDragCreate := False;
    end
    else if (Port <> nil) and (FDragMachinePort <> Port) then
      Factory.ToggleConnection(FDragMachinePort, Port);
    FDragMachinePort := nil;
  end;
end;

function TfrmFactory.MouseToFactory(X, Y: Single): TVector2;
begin
  Result := TransformHelper(Vec2(X, Y), True);
end;

procedure TfrmFactory.PaintWindow(DC: HDC);
var
  G: IGPGraphics;
begin
  G := TGPGraphics.Create(DC);
  G.SmoothingMode := SmoothingModeAntiAlias;
  G.TextRenderingHint := TextRenderingHintAntiAlias;

  G.Transform := CalculateCameraMatrix;

  Factory.Draw(G);
  DrawConnection(G);
end;

{
  function TfrmFactory.FactoryToMouse(APos: TVector2): TVector2;
  begin
  Result := TransformHelper(APos, False);
  end;
}

procedure TfrmFactory.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Zoom: Single;
begin
  Zoom := Power(1.1, WheelDelta / WHEEL_DELTA);
  Camera.Zoom(Zoom, Vec2(MousePos.X, MousePos.Y) - Vec2(Width, Height) / 2);
  Handled := True;
end;

procedure TfrmFactory.FrameResize(Sender: TObject);
begin
  Invalidate;
end;

function TfrmFactory.SnapToGrid(APos: TVector2): TVector2;
begin
  if not GridSnap.HasValue then
    Exit;
  Result := (APos / GridSnap.Value + 0.5).Floor;
  Result := Result * GridSnap.Value;
end;

function TfrmFactory.MouseToFactory(APos: TVector2): TVector2;
begin
  Result := TransformHelper(APos, True);
end;

function TfrmFactory.TransformHelper(APos: TVector2; ABack: Boolean): TVector2;
var
  Matrix: IGPMatrix;
  P: TGPPointF;
begin
  Matrix := CalculateCameraMatrix;
  if ABack then
    Matrix.Invert;
  P := TGPPointF.Create(APos.X, APos.Y);
  Matrix.TransformPoint(P);
  Result.Create(P.X, P.Y);
end;

{ TCamera }

procedure TCamera.Assign(AFrom: TCamera);
begin
  Pos := AFrom.Pos;
  Scale := AFrom.Scale;
end;

function TCamera.CalculateMatrix(ASize: TIntVector2): IGPMatrix;
begin
  Result := TGPMatrix.Create;
  Result.Translate(ASize.X / 2, ASize.Y / 2);
  Result.Translate(-Pos.X, -Pos.Y);
  Result.Scale(Scale, Scale);
end;

function TCamera.Copy: TCamera;
begin
  Result := TCamera.Create;
  Result.Assign(Self);
end;

constructor TCamera.Create;
begin
  Reset;
end;

function TCamera.GetOnChange: TEvent<TCamera>.TAccess;
begin
  Result := FOnChange.Access;
end;

procedure TCamera.Move(AOffset: TVector2);
begin
  Pos := Pos + AOffset;
end;

procedure TCamera.Reset;
begin
  Pos := 0;
  Scale := 1;
end;

procedure TCamera.SetPos(const Value: TVector2);
begin
  if Pos = Value then
    Exit;
  FPos := Value;
  FOnChange.Execute(Self);
end;

procedure TCamera.SetScale(const Value: Single);
begin
  if Scale = Value then
    Exit;
  FScale := Value;
  FOnChange.Execute(Self);
end;

procedure TCamera.Zoom(AFactor: Single; APos: TVector2);
var
  OldPos: TVector2;
begin
  OldPos := (Pos + APos) / Scale;
  Scale := EnsureRange(Scale * AFactor, 0.2, 20);
  Pos := OldPos * Scale - APos;
end;

end.
