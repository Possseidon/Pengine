unit FBOManager;

interface

uses
  GLEnums, dglOpenGL, TextureManager, RenderBufferManager, SysUtils;

type

  { TFBO }

  TFBO = class
  private
    FFBO: Cardinal;
    FOutputs: array [TGLFBOAttachment] of TObject;
    FReferenced: array [TGLFBOAttachment] of Boolean;

    FWidth: Cardinal;
    FHeight: Cardinal;

    class var
      BoundFBO: TFBO;

    function GetRenderBuffer(AType: TGLFBOAttachment): TBasicRenderBuffer;
    function GetTexture(AType: TGLFBOAttachment): TTexture;
  public
    constructor Create(AWidth, AHeight: Cardinal);
    destructor Destroy; override;

    class procedure BindScreen(AWidth, AHeight: Integer);
    procedure Bind;

    procedure EnableTexture2D(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat); overload;
    procedure EnableTexture2D(AType: TGLFBOAttachment; ATexture: TTexture2D); overload;

    procedure EnableTexture2DLayer(AType: TGLFBOAttachment; ATexture: TTexture2DArray; ALayer: Cardinal);
    procedure EnableTextureCubeMapLayer(AType: TGLFBOAttachment; ATexture: TTextureCubeMapArray; ALayer: Cardinal;
      ASide: TGLCubeMapSide);

    procedure EnableTexture2DMS(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat; ASamples: Cardinal);
    procedure EnableRenderBuffer(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat);
    procedure EnableRenderBufferMS(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat; ASamples: Cardinal);

    property Textures[AType: TGLFBOAttachment]: TTexture read GetTexture;
    property RenderBuffers[AType: TGLFBOAttachment]: TBasicRenderBuffer read GetRenderBuffer;

    function Finish: Boolean;

    procedure CopyTo(AFBO: TFBO; AMask: TGLAttribMask);
    procedure StretchTo(AFBO: TFBO; AMask: TGLAttribMask);
    procedure CopyToScreen(AMask: TGLAttribMask);
    procedure StretchToScreen(AMask: TGLAttribMask; AWidth, AHeight: Cardinal);

    procedure Resize(AWidth, AHeight: Cardinal);
    procedure SetSamples(ASamples: Cardinal);

    class property CurrentFBO: TFBO read BoundFBO;

  end;

implementation

{ TFBO }

function TFBO.GetRenderBuffer(AType: TGLFBOAttachment): TBasicRenderBuffer;
begin
  Result := FOutputs[AType] as TBasicRenderBuffer;
end;

function TFBO.GetTexture(AType: TGLFBOAttachment): TTexture;
begin
  Result := FOutputs[AType] as TTexture;
end;

constructor TFBO.Create(AWidth, AHeight: Cardinal);
begin
  FWidth := AWidth;
  FHeight := AHeight;

  glGenFramebuffers(1, @FFBO);
end;

destructor TFBO.Destroy;
var
  T: TGLFBOAttachment;
begin
  for T := Low(TGLFBOAttachment) to High(TGLFBOAttachment) do
    if (FOutputs[T] <> nil) and not FReferenced[T] then
      FOutputs[T].Free;

  glDeleteFramebuffers(1, @FFBO);
  inherited;
end;

class procedure TFBO.BindScreen(AWidth, AHeight: Integer);
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, AWidth, AHeight);
  BoundFBO := nil;
end;

procedure TFBO.Bind;
begin
  if Pointer(BoundFBO) <> Pointer(Self) then
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, FFBO);
    glViewport(0, 0, FWidth, FHeight);
    BoundFBO := Self;
  end;
end;

procedure TFBO.EnableTexture2D(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TEmptyTexture2D.Create(FWidth, FHeight, APixelFormat);

  Bind;
  glFramebufferTexture2D(GL_FRAMEBUFFER, Ord(AType), GL_TEXTURE_2D, Textures[AType].ID, 0);
end;

procedure TFBO.EnableTexture2D(AType: TGLFBOAttachment; ATexture: TTexture2D);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := ATexture;
  FReferenced[AType] := True;

  Bind;
  glFramebufferTexture2D(GL_FRAMEBUFFER, Ord(AType), ATexture.TargetType, Textures[AType].ID, 0);
end;

procedure TFBO.EnableTexture2DLayer(AType: TGLFBOAttachment; ATexture: TTexture2DArray; ALayer: Cardinal);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := ATexture;
  FReferenced[AType] := True;

  Bind;
  glFramebufferTextureLayer(GL_FRAMEBUFFER, Ord(AType), Textures[AType].ID, 0, ALayer);
end;

procedure TFBO.EnableTextureCubeMapLayer(AType: TGLFBOAttachment; ATexture: TTextureCubeMapArray; ALayer: Cardinal;
  ASide: TGLCubeMapSide);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := ATexture;
  FReferenced[AType] := True;

  Bind;
  glFramebufferTextureLayer(GL_FRAMEBUFFER, Ord(AType), Textures[AType].ID, 0, ALayer * 6 + Ord(ASide) - Ord(cmsPosX));
end;

procedure TFBO.EnableTexture2DMS(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat; ASamples: Cardinal);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TEmptyTexture2DMS.Create(FWidth, FHeight, APixelFormat, ASamples);

  Bind;
  glFramebufferTexture2D(GL_FRAMEBUFFER, Ord(AType), GL_TEXTURE_2D_MULTISAMPLE, Textures[AType].ID, 0);
end;

procedure TFBO.EnableRenderBuffer(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TRenderBuffer.Create(FWidth, FHeight, APixelFormat);

  Bind;
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, Ord(AType), GL_RENDERBUFFER, RenderBuffers[AType].ID);
end;

procedure TFBO.EnableRenderBufferMS(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat; ASamples: Cardinal);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TRenderBufferMS.Create(FWidth, FHeight, APixelFormat, ASamples);

  Bind;
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, Ord(AType), GL_RENDERBUFFER, RenderBuffers[AType].ID);
end;

function TFBO.Finish: Boolean;
const
  DrawBuffers: array [0 .. 0] of Cardinal = (GL_COLOR_ATTACHMENT0);
var
  Status: Cardinal;
begin
  Bind;
  glDrawBuffers(1, @DrawBuffers[0]);
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  Result := Status = GL_FRAMEBUFFER_COMPLETE;
end;

procedure TFBO.CopyTo(AFBO: TFBO; AMask: TGLAttribMask);
begin
  if (FWidth <> AFBO.FWidth) or (FHeight <> AFBO.FHeight) then
    raise Exception.Create('Framebuffer Dimension are not equal!');

  glBindFramebuffer(GL_READ_FRAMEBUFFER, Self.FFBO);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, AFBO.FFBO);

  glBlitFramebuffer(0, 0, FWidth, FHeight, 0, 0, FWidth, FHeight, Ord(AMask), GL_NEAREST);

  BoundFBO := nil;
end;

procedure TFBO.StretchTo(AFBO: TFBO; AMask: TGLAttribMask);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, Self.FFBO);
  GLErrorMessage;
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, AFBO.FFBO);
  GLErrorMessage;

  glBlitFramebuffer(0, 0, FWidth, FHeight, 0, 0, AFBO.FWidth, AFBO.FHeight, Ord(AMask), GL_NEAREST);
  GLErrorMessage;

  BoundFBO := nil;
end;

procedure TFBO.CopyToScreen(AMask: TGLAttribMask);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, Self.FFBO);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

  glBlitFramebuffer(0, 0, FWidth, FHeight, 0, 0, FWidth, FHeight, Ord(AMask), GL_NEAREST);

  BoundFBO := nil;
end;

procedure TFBO.StretchToScreen(AMask: TGLAttribMask; AWidth, AHeight: Cardinal);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, Self.FFBO);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

  glBlitFramebuffer(0, 0, FWidth, FHeight, 0, 0, AWidth, AHeight, Ord(AMask), GL_NEAREST);

  BoundFBO := nil;
end;

procedure TFBO.Resize(AWidth, AHeight: Cardinal);
var
  I: TGLFBOAttachment;
  F: TGLPixelFormat;
  S: Cardinal;
begin
  FWidth := AWidth;
  FHeight := AHeight;
  for I := Low(TGLFBOAttachment) to High(TGLFBOAttachment) do
  begin
    if FOutputs[I] is TRenderBuffer then
    begin
      F := TRenderBuffer(FOutputs[I]).Format;
      FOutputs[I].Free;
      FOutputs[I] := nil;
      EnableRenderBuffer(I, F);
    end;
    if FOutputs[I] is TRenderBufferMS then
    begin
      F := TRenderBufferMS(FOutputs[I]).Format;
      S := TRenderBufferMS(FOutputs[I]).Samples;
      FOutputs[I].Free;
      FOutputs[I] := nil;
      EnableRenderBufferMS(I, F, S);
    end;
    if FOutputs[I] is TEmptyTexture2D then
      TEmptyTexture2D(FOutputs[I]).Resize(FWidth, FHeight);
    if FOutputs[I] is TEmptyTexture2DMS then
      TEmptyTexture2DMS(FOutputs[I]).Resize(FWidth, FHeight);
  end;
end;

procedure TFBO.SetSamples(ASamples: Cardinal);
var
  I: TGLFBOAttachment;
  F: TGLPixelFormat;
begin
  for I := Low(TGLFBOAttachment) to High(TGLFBOAttachment) do
  begin
    if FOutputs[I] is TEmptyTexture2DMS then
      TEmptyTexture2DMS(FOutputs[I]).SetSamples(ASamples);
    if FOutputs[I] is TRenderBufferMS then
    begin
      F := TRenderBufferMS(FOutputs[I]).Format;
      FOutputs[I].Free;
      FOutputs[I] := nil;
      EnableRenderBufferMS(I, F, ASamples);
    end;
  end;
end;

end.

