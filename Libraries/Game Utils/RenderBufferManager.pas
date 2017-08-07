unit RenderBufferManager;

interface

uses
  dglOpenGL, GLEnums;

type

  { TBasicRenderBuffer }

  TBasicRenderBuffer = class abstract
  private
    FID: Cardinal;

    var
      BoundRenderBuffer: TBasicRenderBuffer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Bind;

    property ID: Cardinal read FID;
  end;

  { TRenderBuffer }

  TRenderBuffer = class (TBasicRenderBuffer)
  private
    FFormat: TGLPixelFormat;
    FWidth: Cardinal;
    FHeight: Cardinal;

  public
    constructor Create(AWidth, AHeight: Cardinal; AFormat: TGLPixelFormat);

    property Format: TGLPixelFormat read FFormat;
    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
  end;

  { TRenderBufferMS }

  TRenderBufferMS = class (TBasicRenderBuffer)
   private
    FFormat: TGLPixelFormat;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FSamples: Cardinal;

  public
    constructor Create(AWidth, AHeight: Cardinal; AFormat: TGLPixelFormat; ASamples: Cardinal);

    property Format: TGLPixelFormat read FFormat;
    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
    property Samples: Cardinal read FSamples;
  end;

implementation

{ TBasicRenderBuffer }

constructor TBasicRenderBuffer.Create;
begin
  glGenRenderbuffers(1, @FID);
end;

destructor TBasicRenderBuffer.Destroy;
begin
  glDeleteRenderbuffers(1, @FID);
  inherited Destroy;
end;

procedure TBasicRenderBuffer.Bind;
begin
  if Pointer(BoundRenderBuffer) <> Pointer(Self) then
  begin
    glBindRenderbuffer(GL_RENDERBUFFER, FID);
    BoundRenderBuffer := Self;
  end;
end;

{ TRenderBufferMS }

constructor TRenderBufferMS.Create(AWidth, AHeight: Cardinal; AFormat: TGLPixelFormat; ASamples: Cardinal);
begin
  inherited Create;
  FFormat := AFormat;
  FWidth := AWidth;
  FHeight := AHeight;
  FSamples := ASamples;
  Bind;
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, ASamples, Ord(AFormat), AWidth, AHeight);
end;

{ TRenderBuffer }

constructor TRenderBuffer.Create(AWidth, AHeight: Cardinal; AFormat: TGLPixelFormat);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FFormat := AFormat;
  Bind;
  glRenderbufferStorage(GL_RENDERBUFFER, Ord(AFormat), AWidth, AHeight);
end;

end.
