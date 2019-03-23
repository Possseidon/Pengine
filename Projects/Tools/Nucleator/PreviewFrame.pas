unit PreviewFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,
  System.Math,

  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,

  dglOpenGL,

  Pengine.GLContext,
  Pengine.IntMaths,
  Pengine.GLState,
  Pengine.Camera,
  Pengine.TextureAtlas,
  Pengine.GLProgram,
  Pengine.Vector,
  Pengine.SkyBox,
  Pengine.VAO,
  Pengine.GLEnums,
  Pengine.ResourceManager,
  Pengine.Color,
  Pengine.Utility,

  ReactorDefine,
  ReactorEvolutionDefine;

type

  TSkyboxGLProgram = class(TSkyboxGLProgramBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

  TModelGLProgram = class(TGLProgramResource)
  public type

    TData = record
      Pos: TVector3;
      Color: TColorRGB;
      TexCoord: TTexCoord2;
      Border: TBounds2;
      Normal: TVector3;
    end;

  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
    class procedure GetData(out AName: string; out AResource: Boolean); override;
  end;

  TReactorVAO = class(TVAOMutable<TModelGLProgram.TData>)
  private
    FReactor: TReactor;
    FCutoff: Integer;
    FTextureAtlas: TTextureAtlas;

    procedure BuildVAO;

    procedure SetReactor(const Value: TReactor);
    procedure SetCutoff(Value: Integer);

  public
    constructor Create(AGLProgram: TGLProgram; ATextureAtlas: TTextureAtlas);

    property Reactor: TReactor read FReactor write SetReactor;
    property Cutoff: Integer read FCutoff write SetCutoff;

  end;

  TfrmPreview = class(TFrame)
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled:
      Boolean);
  private
    FContext: TGLContext;
    FCamera: TCamera;
    FTextureAtlas: TTextureAtlas;
    FReactor: TRatedReactor;
    FModelGLProgram: TGLProgram;
    FSkyboxGLProgram: TGLProgram;
    FSkybox: TSkybox;
    FReactorVAO: TReactorVAO;

    FDragStart: TOpt<TIntVector2>;

    procedure InitGL;

    function GetSize: TIntVector2;

    procedure Render;
    procedure SetReactor(const Value: TRatedReactor);

  protected
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

  public
    destructor Destroy; override;

    property Size: TIntVector2 read GetSize;
    property Reactor: TRatedReactor read FReactor write SetReactor;

  end;

implementation

{$R *.dfm}


uses Main;

{ TfrmPreview }

procedure TfrmPreview.InitGL;
var
  BlockData: TReactor.TBlockData;
begin
  if FContext <> nil then
    Exit;

  TResourceManager.Init;

  ControlStyle := [];

  FContext := TGLContext.Create(GetDC(Handle), Size, Render);
  FContext.AutoFinish := True;
  FContext.Samples := FContext.MaxSamples;

  FTextureAtlas := TTextureAtlas.Create(FContext.GLState);
  FTextureAtlas.Texture.MagFilter := magNearest;
  for BlockData in TReactor.BlockData do
  begin
    if BlockData.TextureName.IsEmpty then
      Continue;
    FTextureAtlas.AddFromResource(BlockData.TextureName, BlockData.TextureName);
  end;

  FSkyboxGLProgram := TSkyboxGLProgram.Make(FContext.GLState.ResParam);
  FModelGLProgram := TModelGLProgram.Make(FContext.GLState.ResParam);

  FCamera := TCamera.Create(60, Size.X / Size.Y, 0.1, 100);
  FCamera.AddUniforms(FModelGLProgram);
  FCamera.AddUniforms(FSkyboxGLProgram);
  FCamera.Location.OffsetZ := 2;
  FCamera.Location.TurnAngle := 30;
  FCamera.Location.PitchAngle := -20;

  FSkybox := TSkybox.Create(FSkyboxGLProgram);
  FSkybox.AddStripe(ColorRGB(0.7, 1.0, 0.9), -90);
  FSkybox.AddStripe(ColorRGB(0.4, 0.6, 0.9), 0);
  FSkybox.AddStripe(ColorRGB(0.1, 0.2, 0.9), +90);
  FCamera.AddRenderable(FSkybox);

  FReactorVAO := TReactorVAO.Create(FModelGLProgram, FTextureAtlas);
  FCamera.AddRenderable(FReactorVAO);

  Invalidate;
end;

procedure TfrmPreview.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FDragStart := IVec2(X, Y);
  MouseCapture := True;
end;

procedure TfrmPreview.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Offset: TIntVector2;
begin
  inherited;
  if (FContext = nil) or not FDragStart.HasValue then
    Exit;
  Offset := FDragStart - IVec2(X, Y);
  FDragStart := IVec2(X, Y);
  FCamera.Location.Turn(-Offset.X / Height * 180);
  FCamera.Location.Pitch(Offset.Y / Height * 180);
  Invalidate;
end;

procedure TfrmPreview.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FDragStart.Clear;
  MouseCapture := False;
end;

function TfrmPreview.GetSize: TIntVector2;
begin
  Result := IVec2(ClientWidth, ClientHeight);
end;

procedure TfrmPreview.Render;
begin
  FCamera.Render;
end;

procedure TfrmPreview.Resize;
begin
  inherited;
  if FContext = nil then
    Exit;
  FContext.Size := Size;
  FCamera.Aspect := Size.X / Size.Y;
  FContext.Render;
end;

procedure TfrmPreview.SetReactor(const Value: TRatedReactor);
begin
  FReactor := Value;
  InitGL;
  FReactorVAO.Reactor := Value;
  Invalidate;
end;

procedure TfrmPreview.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_PAINT:
      begin
        if FContext = nil then
        begin
          inherited;
          Exit;
        end;
        FContext.Render;
        ValidateRect(Handle, ClientRect);
        Message.Result := 0;
      end
  else
    inherited;
  end;
end;

destructor TfrmPreview.Destroy;
begin
  if FContext <> nil then
  begin
    FReactorVAO.Free;
    FSkybox.Free;
    FCamera.Free;
    TModelGLProgram.Release(FContext.GLState.ResParam);
    TSkyboxGLProgram.Release(FContext.GLState.ResParam);
    FTextureAtlas.Free;
    FContext.Free;
    TResourceManager.Finalize;
  end;
  inherited;
end;

procedure TfrmPreview.FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var
  Handled: Boolean);
begin
  if FContext = nil then
    Exit;
  FReactorVAO.Cutoff := FReactorVAO.Cutoff - Sign(WheelDelta);
  Handled := True;
  Invalidate;
end;

{ TSkyboxGLProgram }

class procedure TSkyboxGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  AName := 'SKYBOX';
end;

{ TModelGLProgram }

class function TModelGLProgram.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := [
    'vpos',
    'vcolor',
    'vtexcoord',
    'vborderlow',
    'vborderhigh',
    'vnormal'
    ];
end;

class procedure TModelGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  AName := 'MODEL';
end;

{ TReactorVAO }

procedure TReactorVAO.BuildVAO;
var
  Writer: IWriter;
  Bounds: TIntBounds3;
  Pos: TIntVector3;
  DataSize, MaxSize: Integer;
  CubePlane: TPlane3;
  BlockType: TReactor.TBlockType;
  TexTile: TTexTile;
  TexCoord: TTexCoord2;
begin
  Bounds := IBounds3(Reactor.Size);
  if Cutoff >= 0 then
    Bounds.C2.Y := Bounds.C2.Y - Cutoff
  else
    Bounds.C1.Y := Bounds.C1.Y - Cutoff;

  MaxSize := Max(Max(Bounds.Width, Bounds.Height), Bounds.Depth);

  DataSize := 0;
  for Pos in Bounds do
    if Reactor.Blocks[Pos] <> rbAir then
      Inc(DataSize, QuadSideCount * 6);

  VBO.Generate(DataSize, buStaticDraw);
  Writer := VBO.Map;
  for Pos in Bounds do
  begin
    BlockType := Reactor.Blocks[Pos];
    if BlockType = rbAir then
      Continue;
    TexTile := FTextureAtlas[TReactor.BlockData[BlockType].TextureName];

    for CubePlane in CubePlanes do
    begin
      Writer[2].Border := TexTile.BoundsHalfPixelInset;
      Writer[5].Border := TexTile.BoundsHalfPixelInset;
      for TexCoord in QuadTexCoords do
      begin
        Writer.Current.Pos := (CubePlane[TexCoord] + Pos) / MaxSize - 0.5;
        Writer.Current.Color := ColorWhite;
        Writer.Current.TexCoord := TexTile.Bounds[TexCoord];
        Writer.Current.Normal := CubePlane.Normal;
        Writer.NextBufferPos;
      end;
    end;
  end;
end;

constructor TReactorVAO.Create(AGLProgram: TGLProgram; ATextureAtlas: TTextureAtlas);
begin
  inherited Create(AGLProgram);
  FTextureAtlas := ATextureAtlas;
end;

procedure TReactorVAO.SetCutoff(Value: Integer);
begin
  Value := EnsureRange(Value, 1 - Reactor.Size.Y, Reactor.Size.Y - 1);
  if Cutoff = Value then
    Exit;
  FCutoff := Value;
  BuildVAO;
end;

procedure TReactorVAO.SetReactor(const Value: TReactor);
begin
  FReactor := Value;
  FCutoff := 0;
  BuildVAO;
end;

end.
