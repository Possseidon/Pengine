unit Pengine.FBO;

interface

uses
  dglOpenGL,

  System.SysUtils,

  Pengine.GLEnums,
  Pengine.RBO,
  Pengine.Texture,
  Pengine.GLState,
  Pengine.IntMaths;

type

  TFBO = class;

  TFBO = class(TGLObject)
  private
    FOutputs: array [TGLFBOAttachment] of TObject;
    FReferenced: array [TGLFBOAttachment] of Boolean;

    FSize: TIntVector2;

    function GetRenderBuffer(AType: TGLFBOAttachment): TBasicRenderBuffer;
    function GetTexture(AType: TGLFBOAttachment): TTexture;

  protected
    procedure GenObject(out AGLName: Cardinal); override;
    procedure DeleteObject(const AGLName: Cardinal); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

  public
    constructor Create(AGLState: TGLState; ASize: TIntVector2);
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

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

    procedure CopyTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
    procedure StretchTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
    procedure CopyToScreen(AMask: TGLAttribMaskFlags);
    procedure StretchToScreen(AMask: TGLAttribMaskFlags; ASize: TIntVector2);

    procedure Resize(ASize: TIntVector2);
    procedure SetSamples(ASamples: Integer);

    // Use when you don't actually have an FBO Object at hand but need to unbind
    class procedure BindScreen(ASize: TIntVector2; AFBOBinding: TGLObjectBinding<TFBO>);

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

procedure TFBO.GenObject(out AGLName: Cardinal);
begin
  glGenFramebuffers(1, @AGLName);
end;

procedure TFBO.DeleteObject(const AGLName: Cardinal);
begin
  glDeleteFramebuffers(1, @GLName);
end;

procedure TFBO.BindGLObject;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, GLName);
  glViewport(0, 0, FSize.X, FSize.Y);
end;

procedure TFBO.UnbindGLObject;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, GLState.ScreenSize.X, GLState.ScreenSize.Y);
end;

constructor TFBO.Create(AGLState: TGLState; ASize: TIntVector2);
begin
  inherited Create(AGLState);
  FSize := ASize;
end;

destructor TFBO.Destroy;
var
  T: TGLFBOAttachment;
begin
  for T := Low(TGLFBOAttachment) to High(TGLFBOAttachment) do
    if (FOutputs[T] <> nil) and not FReferenced[T] then
      FOutputs[T].Free;
  inherited;
end;

class function TFBO.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TGLObjectBinding<TFBO>;
end;

class function TFBO.GetObjectType: TGLObjectType;
begin
  Result := otFramebuffer;
end;

procedure TFBO.EnableTexture2D(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TEmptyTexture2D.Create(GLState, FSize.X, FSize.Y, APixelFormat);

  Bind;
  glFramebufferTexture2D(GL_FRAMEBUFFER, Ord(AType), GL_TEXTURE_2D, Textures[AType].GLName, 0);
end;

procedure TFBO.EnableTexture2D(AType: TGLFBOAttachment; ATexture: TTexture2D);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := ATexture;
  FReferenced[AType] := True;

  Bind;
  glFramebufferTexture2D(GL_FRAMEBUFFER, Ord(AType), ATexture.TargetType, Textures[AType].GLName, 0);
end;

procedure TFBO.EnableTexture2DLayer(AType: TGLFBOAttachment; ATexture: TTexture2DArray; ALayer: Cardinal);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := ATexture;
  FReferenced[AType] := True;

  Bind;
  glFramebufferTextureLayer(GL_FRAMEBUFFER, Ord(AType), Textures[AType].GLName, 0, ALayer);
end;

procedure TFBO.EnableTextureCubeMapLayer(AType: TGLFBOAttachment; ATexture: TTextureCubeMapArray; ALayer: Cardinal;
  ASide: TGLCubeMapSide);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := ATexture;
  FReferenced[AType] := True;

  Bind;
  glFramebufferTextureLayer(GL_FRAMEBUFFER, Ord(AType), Textures[AType].GLName, 0,
    ALayer * 6 + Ord(ASide) - Ord(cmsPosX));
end;

procedure TFBO.EnableTexture2DMS(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat; ASamples: Cardinal);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TEmptyTexture2DMS.Create(GLState, FSize.X, FSize.Y, APixelFormat, ASamples);

  Bind;
  glFramebufferTexture2D(GL_FRAMEBUFFER, Ord(AType), GL_TEXTURE_2D_MULTISAMPLE, Textures[AType].GLName, 0);
end;

procedure TFBO.EnableRenderBuffer(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TRenderBuffer.Create(FSize.X, FSize.Y, APixelFormat);

  Bind;
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, Ord(AType), GL_RENDERBUFFER, RenderBuffers[AType].ID);
end;

procedure TFBO.EnableRenderBufferMS(AType: TGLFBOAttachment; APixelFormat: TGLPixelFormat; ASamples: Cardinal);
begin
  if FOutputs[AType] <> nil then
    raise Exception.Create('Multiple Framebuffer outputs for same Type!');

  FOutputs[AType] := TRenderBufferMS.Create(FSize.X, FSize.Y, APixelFormat, ASamples);

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

class procedure TFBO.BindScreen(ASize: TIntVector2; AFBOBinding: TGLObjectBinding<TFBO>);
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, ASize.X, ASize.Y);
  AFBOBinding.BoundObject := nil;
end;

procedure TFBO.CopyTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
begin
  if FSize <> AFBO.FSize then
    raise Exception.Create('Framebuffer Dimension are not equal!');

  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, AFBO.GLName);
  Binding.BoundObject := AFBO;

  glBlitFramebuffer(0, 0, FSize.X, FSize.Y, 0, 0, FSize.X, FSize.Y, ToGLBitfield(AMask), GL_NEAREST);
end;

procedure TFBO.StretchTo(AFBO: TFBO; AMask: TGLAttribMaskFlags);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, AFBO.GLName);
  Binding.BoundObject := AFBO;

  glBlitFramebuffer(0, 0, FSize.X, FSize.Y, 0, 0, AFBO.FSize.X, AFBO.FSize.Y, ToGLBitfield(AMask), GL_NEAREST);
end;

procedure TFBO.CopyToScreen(AMask: TGLAttribMaskFlags);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  Binding.BoundObject := nil;

  glBlitFramebuffer(0, 0, FSize.X, FSize.Y, 0, 0, FSize.X, FSize.Y, ToGLBitfield(AMask), GL_NEAREST);
end;

procedure TFBO.StretchToScreen(AMask: TGLAttribMaskFlags; ASize: TIntVector2);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, GLName);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  Binding.BoundObject := nil;

  glBlitFramebuffer(0, 0, FSize.X, FSize.Y, 0, 0, ASize.X, ASize.Y, ToGLBitfield(AMask), GL_NEAREST);
end;

procedure TFBO.Resize(ASize: TIntVector2);
var
  I: TGLFBOAttachment;
  F: TGLPixelFormat;
  S: Cardinal;
begin
  FSize := ASize;
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
      TEmptyTexture2D(FOutputs[I]).Resize(FSize.X, FSize.Y);
    if FOutputs[I] is TEmptyTexture2DMS then
      TEmptyTexture2DMS(FOutputs[I]).Resize(FSize.X, FSize.Y);
  end;
end;

procedure TFBO.SetSamples(ASamples: Integer);
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
