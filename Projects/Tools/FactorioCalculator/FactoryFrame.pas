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
  public type

    TEventInfo = TSenderEventInfo<TCamera>;

    TEvent = TEvent<TEventInfo>;

  private
    FOnChange: TEvent;
    FPos: TVector2;
    FScale: Single;

    procedure SetPos(const Value: TVector2);
    procedure SetScale(const Value: Single);
    function GetOnChange: TEvent.TAccess;

  public
    constructor Create;

    procedure Assign(AFrom: TCamera);
    function Copy: TCamera;

    property OnChange: TEvent.TAccess read GetOnChange;

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
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta:
        Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FrameResize(Sender: TObject);
  private
    FFactory: TFactory;
    FDragMachineArray: TMachineArray;
    FConfigureMachineArray: TMachineArray;
    FDragInput: TMachineInput;
    FDragOutput: TMachineOutput;
    FDragCreate: Boolean;
    FDragPos: TOpt<TVector2>;
    FCamera: TCamera;

    procedure DrawConnection(G: IGPGraphics);

    function CalculateCameraMatrix: IGPMatrix;

    function TransformHelper(APos: TVector2; ABack: Boolean): TVector2;
    function FactoryToMouse(APos: TVector2): TVector2;
    function MouseToFactory(X, Y: Single): TVector2; overload;
    function MouseToFactory(APos: TVector2): TVector2; overload;

  protected
    procedure PaintWindow(DC: HDC); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Factory: TFactory read FFactory;

    property Camera: TCamera read FCamera;

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

  FCamera := TCamera.Create;
  FCamera.OnChange.Add(Invalidate);

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
  FCamera.Free;
  inherited;
end;

procedure TfrmFactory.DrawConnection(G: IGPGraphics);
var
  Pen: IGPPen;
  Start, Stop: TVector2;
  MachineArray: TMachineArray;
begin
  if (FDragInput = nil) and (FDragOutput = nil) then
    Exit;
  MachineArray := Factory.MachineArrayAt(MouseToFactory(FDragPos));

  if (FDragInput <> nil) and (MachineArray = FDragInput.MachineArray) then
      Exit;
  if (FDragOutput <> nil) and (MachineArray = FDragOutput.MachineArray) then
      Exit;

  if FDragCreate then
    Pen := TGPPen.Create($7FFF7F7F, 16)
  else
    Pen := TGPPen.Create($7F7F7FFF, 16);
  Pen.StartCap := LineCapRound;
  Pen.EndCap := LineCapArrowAnchor;
  if FDragInput <> nil then
  begin
    Start := MouseToFactory(FDragPos);
    Stop := FDragInput.Pos + Vec2(0, 16);
  end
  else
  begin
    Start := FDragOutput.Pos + Vec2(32, 16);
    Stop := MouseToFactory(FDragPos);
  end;
  Start := Start;
  Stop := Stop;
  G.DrawLine(Pen, Start.X, Start.Y, Stop.X, Stop.Y);
end;

procedure TfrmFactory.FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
  Input: TMachineInput;
  Output: TMachineOutput;
begin
  MouseCapture := True;
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
    begin
      FConfigureMachineArray := MachineArray;
      Input := MachineArray.InputAt(MouseToFactory(X, Y));
      if Input <> nil then
      begin
        FDragInput := Input;
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
    FDragMachineArray.Pos := FDragMachineArray.Pos + MouseToFactory(X, Y) - MouseToFactory(FDragPos)
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
    Camera.Move(FDragPos - Vec2(X, Y));

  if FDragPos.HasValue then
    FDragPos := Vec2(X, Y);
end;

procedure TfrmFactory.FrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MachineArray: TMachineArray;
  Output: TMachineOutput;
  Input: TMachineInput;
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
      FDragInput := nil;
      FDragOutput := nil;
      Invalidate;
      FDragCreate := False;
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
    if FDragCreate then
    begin
      Recipe := FDragInput.Ingredient.Item.FindRecipe;
      if Recipe <> nil then
      begin
        MachineArray := Factory.AddMachineArray;
        MachineArray.Recipe := Recipe;
        MachineArray.Pos := MouseToFactory(X, Y) - MachineArray.Bounds.Size / 2;
        for Output in MachineArray.Outputs do
        begin
          if Output.ItemStack.Item = FDragInput.ItemStack.Item then
          begin
            FDragInput.Connect(Output);
            Break;
          end;
        end;
      end;
      FDragCreate := False;
    end
    else if Output <> nil then
      FDragInput.ToggleConnection(Output);
    FDragInput := nil;
  end;

  FConfigureMachineArray := nil;
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

function TfrmFactory.FactoryToMouse(APos: TVector2): TVector2;
begin
  Result := TransformHelper(APos, False);
end;

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

function TCamera.GetOnChange: TEvent.TAccess;
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
  FOnChange.Execute(TEventInfo.Create(Self));
end;

procedure TCamera.SetScale(const Value: Single);
begin
  if Scale = Value then
    Exit;
  FScale := Value;
  FOnChange.Execute(TEventInfo.Create(Self));
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
