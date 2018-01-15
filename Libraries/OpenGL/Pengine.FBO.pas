unit Pengine.FBO;

interface

uses
  dglOpenGL,

  System.SysUtils,

  Pengine.GLEnums,
  Pengine.Renderbuffer,
  Pengine.Texture,
  Pengine.GLState,
  Pengine.IntMaths,
  Pengine.Collections;

type

  EFramebufferNotComplete = class(Exception)
  public
    constructor Create;
  end;

  EFramebufferViewportDifferent = class(Exception)
  public
    constructor Create;
  end;

  TAttachmentPoint = class
  public
    function GLEnum: Cardinal; virtual; abstract;
  end;

  TColorAttachment = class(TAttachmentPoint)
  private
    FIndex: Integer;
  public
    constructor Create(AIndex: Integer = 0);
    property Index: Integer read FIndex;
    function GLEnum: Cardinal; override;
  end;

  TDepthAttachment = class(TAttachmentPoint)
  public
    function GLEnum: Cardinal; override;
  end;

  TStencilAttachment = class(TAttachmentPoint)
  public
    function GLEnum: Cardinal; override;
  end;

  TDepthStencilAttachment = class(TAttachmentPoint)
  public
    function GLEnum: Cardinal; override;
  end;

  TAttachment = class abstract
  private
    FAttachmentPoint: TAttachmentPoint;

  public
    constructor Create(AAttachmentPoint: TAttachmentPoint);
    destructor Destroy; override;

    property AttachmentPoint: TAttachmentPoint read FAttachmentPoint;

    procedure Attach; virtual; abstract;

  end;

  TTexture2DAttachment = class(TAttachment)
  private
    FTexture: TTexture2D;

  public
    constructor Create(AAttachmentPoint: TAttachmentPoint; ATexture: TTexture2D);

    property Texture: TTexture2D read FTexture;

    procedure Attach; override;

  end;

  TTextureLayerAttachment = class(TAttachment)
  private
    FTexture: TTexture2DArray;
    FLayer: Integer;

  public
    constructor Create(AAttachmentPoint: TAttachmentPoint; ATexture: TTexture2DArray; ALayer: Integer);

    property Texture: TTexture2DArray read FTexture;
    property Layer: Integer read FLayer;

    procedure Attach; override;

  end;

  TCubeMapLayerAttachment = class(TAttachment)
  private
    FTexture: TTextureCubeMapArray;
    FSide: TGLCubeMapSide;
    FLayer: Integer;

  public
    constructor Create(AAttachmentPoint: TAttachmentPoint; ATexture: TTextureCubeMapArray; ASide: TGLCubeMapSide; ALayer: Integer);

    property Texture: TTextureCubeMapArray read FTexture;
    property Side: TGLCubeMapSide read FSide;
    property Layer: Integer read FLayer;

    procedure Attach; override;

  end;

  TRenderbufferAttachment = class(TAttachment)
  private
    FRenderbuffer: TRenderbuffer;

  public
    constructor Create(AAttachmentPoint: TAttachmentPoint; ARenderbuffer: TRenderbuffer);

    property Renderbuffer: TRenderbuffer read FRenderbuffer;

    procedure Attach; override;

  end;

  TFBO = class(TGLObject)
  public type

    TAttachments = TObjectArray<TAttachment>;

  private
    FAttachments: TAttachments;
    FViewport: TIntBounds2;

    function GetAttachments: TAttachments.TReader;

    procedure SetViewport(const Value: TIntBounds2);

  protected
    procedure GenObject(out AGLName: Cardinal); override;
    procedure DeleteObject(const AGLName: Cardinal); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

  public
    constructor Create(AGLState: TGLState; AViewport: TIntBounds2);
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    property Attachments: TAttachments.TReader read GetAttachments;

    procedure Add(AAttachment: TAttachment);
    procedure Complete;

    procedure CopyTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
    procedure StretchTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
    procedure CopyToScreen(AMask: TGLAttribMaskFlags);
    procedure StretchToScreen(AMask: TGLAttribMaskFlags; ASize: TIntVector2);

    property Viewport: TIntBounds2 read FViewport write SetViewport;

    // Use when you don't actually have an FBO Object at hand but need to unbind
    class procedure BindScreen(AViewport: TIntBounds2; AFBOBinding: TGLObjectBinding<TFBO>);
    class procedure ClearScreen(AViewport: TIntBounds2; AFBOBinding: TGLObjectBinding<TFBO>; AMask: TGLAttribMaskFlags);

  end;

implementation

{ TFBO }

procedure TFBO.GenObject(out AGLName: Cardinal);
begin
  glGenFramebuffers(1, @AGLName);
end;

procedure TFBO.DeleteObject(const AGLName: Cardinal);
begin
  glDeleteFramebuffers(1, @GLName);
end;

procedure TFBO.Add(AAttachment: TAttachment);
begin
  FAttachments.Add(AAttachment);
  Bind;
  AAttachment.Attach;
end;

procedure TFBO.BindGLObject;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, GLName);
  glViewport(Viewport.C1.X, Viewport.C1.Y, Viewport.C2.X, Viewport.C2.Y);
end;

procedure TFBO.UnbindGLObject;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, GLState.ScreenSize.X, GLState.ScreenSize.Y);
end;

constructor TFBO.Create(AGLState: TGLState; AViewport: TIntBounds2);
begin
  inherited Create(AGLState);
  FViewport := AViewport;
  FAttachments := TAttachments.Create;
end;

destructor TFBO.Destroy;
begin
  FAttachments.Free;
  inherited;
end;

function TFBO.GetAttachments: TAttachments.TReader;
begin
  Result := FAttachments.Reader;
end;

class function TFBO.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TGLObjectBinding<TFBO>;
end;

class function TFBO.GetObjectType: TGLObjectType;
begin
  Result := otFramebuffer;
end;

class procedure TFBO.BindScreen(AViewport: TIntBounds2; AFBOBinding: TGLObjectBinding<TFBO>);
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(AViewport.C1.X, AViewport.C1.Y, AViewport.C2.X, AViewport.C2.Y);
  AFBOBinding.BoundObject := nil;
end;

class procedure TFBO.ClearScreen(AViewport: TIntBounds2; AFBOBinding: TGLObjectBinding<TFBO>; AMask: TGLAttribMaskFlags);
begin
  BindScreen(AViewport, AFBOBinding);
  glClear(ToGLBitfield(AMask));
end;

procedure TFBO.Complete;
const
  DrawBuffers: TGLenum = GL_COLOR_ATTACHMENT0;
begin
  Bind;
  glDrawBuffers(1, @DrawBuffers);
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
    raise EFramebufferNotComplete.Create;
end;

procedure TFBO.CopyTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
begin
  if Viewport.Size <> AFBO.Viewport.Size then
    raise EFramebufferViewportDifferent.Create;

  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, AFBO.GLName);
  Binding.BoundObject := AFBO;

  glBlitFramebuffer(
    Viewport.C1.X, Viewport.C1.Y, Viewport.C2.X, Viewport.C2.Y,
    AFBO.Viewport.C1.X, AFBO.Viewport.C1.Y, AFBO.Viewport.C2.X, AFBO.Viewport.C2.Y,
    ToGLBitfield(AMask), GL_NEAREST);
end;

procedure TFBO.SetViewport(const Value: TIntBounds2);
begin
  if Viewport = Value then
    Exit;
  FViewport := Value;
  if Bound then
    glViewport(Viewport.C1.X, Viewport.C1.Y, Viewport.C2.X, Viewport.C2.Y);
end;

procedure TFBO.StretchTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, AFBO.GLName);
  Binding.BoundObject := AFBO;

  glBlitFramebuffer(
    Viewport.C1.X, Viewport.C1.Y, Viewport.C2.X, Viewport.C2.Y,
    AFBO.Viewport.C1.X, AFBO.Viewport.C1.Y, AFBO.Viewport.C2.X, AFBO.Viewport.C2.Y,
    ToGLBitfield(AMask), GL_NEAREST);
end;

procedure TFBO.CopyToScreen(AMask: TGLAttribMaskFlags);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  Binding.BoundObject := nil;

  glBlitFramebuffer(
    Viewport.C1.X, Viewport.C1.Y, Viewport.C2.X, Viewport.C2.Y,
    Viewport.C1.X, Viewport.C1.Y, Viewport.C2.X, Viewport.C2.Y,
    ToGLBitfield(AMask), GL_NEAREST);
end;

procedure TFBO.StretchToScreen(AMask: TGLAttribMaskFlags; ASize: TIntVector2);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  Binding.BoundObject := nil;

  glBlitFramebuffer(
    Viewport.C1.X, Viewport.C1.Y, Viewport.C2.X, Viewport.C2.Y,
    0, 0, ASize.X, ASize.Y,
    ToGLBitfield(AMask), GL_NEAREST);
end;
 {
procedure TFBO.Resize(ASize: TIntVector2);
var
  I: TGLFBOAttachment;
  F: TGLPixelFormat;
  S: Cardinal;
begin
  FSize := ASize;
  for I := Low(TGLFBOAttachment) to High(TGLFBOAttachment) do
  begin
    if FOutputs[I] is TTexture2D then
    begin
      TTexture2D(FOutputs[I]).Size := FSize;
    end
    else if FOutputs[I] is TRenderbufferMS then
    begin
      F := TRenderbufferMS(FOutputs[I]).Format;
      S := TRenderbufferMS(FOutputs[I]).Samples;
      FreeAndNil(FOutputs[I]);
      EnableRenderBufferMS(I, F, S);
    end
    else if FOutputs[I] is TRenderbuffer then
    begin
      F := TRenderbuffer(FOutputs[I]).Format;
      FreeAndNil(FOutputs[I]);
      EnableRenderBuffer(I, F);
    end
  end;
end;

procedure TFBO.SetSamples(ASamples: Integer);
var
  Attachment: TGLFBOAttachment;
  PixelFormat: TGLPixelFormat;
begin
  for Attachment := Low(TGLFBOAttachment) to High(TGLFBOAttachment) do
  begin
    if FOutputs[Attachment] is TTexture2DMS then
    begin
      TTexture2DMS(FOutputs[Attachment]).Samples := ASamples;
    end
    else if FOutputs[Attachment] is TRenderbufferMS then
    begin
      PixelFormat := TRenderbufferMS(FOutputs[Attachment]).Format;
      FreeAndNil(FOutputs[Attachment]);
      EnableRenderBufferMS(Attachment, PixelFormat, ASamples);
    end;
  end;
end; }

{ TColorAttachment }

constructor TColorAttachment.Create(AIndex: Integer);
begin
  FIndex := AIndex;
end;

function TColorAttachment.GLEnum: Cardinal;
begin
  Result := GL_COLOR_ATTACHMENT0 + Index;
end;

{ TDepthAttachment }

function TDepthAttachment.GLEnum: Cardinal;
begin
  Result := GL_DEPTH_ATTACHMENT;
end;

{ TStencilAttachment }

function TStencilAttachment.GLEnum: Cardinal;
begin
  Result := GL_STENCIL_ATTACHMENT;
end;

{ TDepthStencilAttachment }

function TDepthStencilAttachment.GLEnum: Cardinal;
begin
  Result := GL_DEPTH_STENCIL_ATTACHMENT;
end;

{ EFramebufferNotComplete }

constructor EFramebufferNotComplete.Create;
begin
  inherited Create('The framebuffer is not complete.');
end;

{ TAttachment }

constructor TAttachment.Create(AAttachmentPoint: TAttachmentPoint);
begin
  FAttachmentPoint := AAttachmentPoint;
end;

destructor TAttachment.Destroy;
begin
  FAttachmentPoint.Free;
  inherited;
end;

{ TTexture2DAttachment }

procedure TTexture2DAttachment.Attach;
begin
  glFramebufferTexture2D(GL_FRAMEBUFFER, AttachmentPoint.GLEnum, FTexture.TargetType, FTexture.GLName, 0);
end;

constructor TTexture2DAttachment.Create(AAttachmentPoint: TAttachmentPoint; ATexture: TTexture2D);
begin
  inherited Create(AAttachmentPoint);
  FTexture := ATexture;
end;

{ TRenderbufferAttachment }

procedure TRenderbufferAttachment.Attach;
begin
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, AttachmentPoint.GLEnum, GL_RENDERBUFFER, Renderbuffer.GLName);
end;

constructor TRenderbufferAttachment.Create(AAttachmentPoint: TAttachmentPoint; ARenderbuffer: TRenderbuffer);
begin
  inherited Create(AAttachmentPoint);
  FRenderbuffer := ARenderbuffer;
end;

{ TTextureLayerAttachment }

procedure TTextureLayerAttachment.Attach;
begin
  glFramebufferTextureLayer(GL_FRAMEBUFFER, AttachmentPoint.GLEnum, Texture.GLName, 0, Layer);
end;

constructor TTextureLayerAttachment.Create(AAttachmentPoint: TAttachmentPoint; ATexture: TTexture2DArray;
  ALayer: Integer);
begin
  inherited Create(AAttachmentPoint);
  FTexture := ATexture;
  FLayer := ALayer;
end;

{ EFramebufferViewportDifferent }

constructor EFramebufferViewportDifferent.Create;
begin
  inherited Create('Framebuffer viewports are not equal in size.');
end;

{ TCubeMapLayerAttachment }

procedure TCubeMapLayerAttachment.Attach;
begin
  glFramebufferTextureLayer(GL_FRAMEBUFFER, AttachmentPoint.GLEnum, Texture.GLName, 0, Layer * 6 + Ord(Side) - Ord(cmsPosX));
end;

constructor TCubeMapLayerAttachment.Create(AAttachmentPoint: TAttachmentPoint; ATexture: TTextureCubeMapArray; ASide: TGLCubeMapSide; ALayer: Integer);
begin
  inherited Create(AAttachmentPoint);
  FTexture := ATexture;
  FSide := ASide;
  FLayer := ALayer;
end;

end.
