unit Pengine.GLFormless;

interface

uses
  Vcl.Forms,

  GdiPlus,

  dglOpenGL,

  Pengine.Settings,
  Pengine.GLEnums,
  Pengine.GLContext,
  Pengine.Texture,
  Pengine.FBO,
  Pengine.IntMaths,
  Pengine.GLState;

type

  /// <summary>Creates an OpenGL context without a hidden form, rendering to a texture using an FBO instead.</summary>
  TGLFormless = class
  private
    FForm: TForm;
    FContext: TGLContext;
    FTexture: TTexture2D;
    FDepthTexture: TTexture2D;
    FFBO: TFBO;

    function GetSize: TIntVector2;
    procedure SetSize(const Value: TIntVector2);
    function GetGLState: TGLState;
    function GetAspect: Single;

  public
    constructor Create;
    destructor Destroy; override;

    property Context: TGLContext read FContext;
    property GLState: TGLState read GetGLState;

    property Size: TIntVector2 read GetSize write SetSize;
    property Aspect: Single read GetAspect;

    procedure Clear;
    procedure Bind;

    function ToImage: IGPBitmap;

  end;

  TBackgroundGLSettings = class(TSettings)
  private
    FGL: TGLFormless;

  protected
    constructor Create(ARoot: TRootSettings); override;

  public
    destructor Destroy; override;

    class function SkipSave: Boolean; override;

    property GL: TGLFormless read FGL;

  end;

implementation

{ TGLFormless }

procedure TGLFormless.Bind;
begin
  FFBO.Bind;
end;

procedure TGLFormless.Clear;
begin
  FFBO.Bind;
  glClear(ToGLBitfield([amColor, amDepth]));
end;

constructor TGLFormless.Create;
begin
  FForm := TForm.Create(nil);
  FContext := TGLContext.Create(FForm.Canvas.Handle, 0, nil);
  FTexture := TTexture2D.Create(GLState, 1);
  FDepthTexture := TTexture2D.Create(GLState, Size, pfDepthComponent);
  FFBO := TFBO.Create(GLState, IBounds2(Size));
  FFBO.Add(TTexture2DAttachment.Create(TColorAttachment.Create, FTexture));
  FFBO.Add(TTexture2DAttachment.Create(TDepthAttachment.Create, FDepthTexture));
  FFBO.Complete;
end;

destructor TGLFormless.Destroy;
begin
  FFBO.Free;
  FDepthTexture.Free;
  FTexture.Free;
  FContext.Free;
  FForm.Free;
  inherited;
end;

function TGLFormless.GetAspect: Single;
begin
  Result := Size.X / Size.Y;
end;

function TGLFormless.GetGLState: TGLState;
begin
  Result := Context.GLState;
end;

function TGLFormless.GetSize: TIntVector2;
begin
  Result := FTexture.Size;
end;

procedure TGLFormless.SetSize(const Value: TIntVector2);
begin
  FTexture.Size := Value;
  FDepthTexture.Size := Value;
  FFBO.Viewport := IBounds2(Value);
end;

function TGLFormless.ToImage: IGPBitmap;
var
  TextureData: TTextureData;
begin
  TextureData := FTexture.ToTextureData;
  Result := TextureData.ToImage;
  TextureData.Free;
end;

{ TBackgroundGLSettings }

constructor TBackgroundGLSettings.Create(ARoot: TRootSettings);
begin
  inherited;
  FGL := TGLFormless.Create;
end;

destructor TBackgroundGLSettings.Destroy;
begin
  FGL.Free;
  inherited;
end;

class function TBackgroundGLSettings.SkipSave: Boolean;
begin
  Result := True;
end;

end.
