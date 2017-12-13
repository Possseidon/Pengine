unit Pengine.GLObject;

interface

uses
  dglOpenGL, 

  Pengine.GLContext,
  Pengine.Interfaces;

type

  TGLObjectType = (
    otTexture = GL_TEXTURE,

    otVertexArray = GL_VERTEX_ARRAY,

    otBuffer = GL_BUFFER,
    otShader,
    otProgram,
    otQuery,
    otProgramPipeline,

    otSampler = GL_SAMPLER,
    otDisplayList,

    otFramebuffer = GL_FRAMEBUFFER,
    otRenderbuffer,

    otTransformFeedback = GL_TRANSFORM_FEEDBACK
  );

  TGLObject = class abstract(TInterfaceBase)
  private
    FGLLabel: AnsiString;
    FGLContext: TGLContext;

  protected
    function GetObjectType: TGLObjectType; virtual; abstract;

    procedure SetGLLabel(const Value: AnsiString); virtual; abstract;

  public
    procedure Bind; virtual; abstract;
    procedure Unbind; virtual; abstract;

    property GLLabel: AnsiString read FGLLabel write SetGLLabel;

    property ObjectType: TGLObjectType read GetObjectType;

  end;

  TGLUIntObject = class(TGLObject)
  private
    FGLName: GLuint;

  protected
    procedure GenObject(out AGLName: GLuint); virtual; abstract;
    procedure DeleteObject(const AGLName: GLuint); virtual; abstract;

    procedure SetGLLabel(const Value: AnsiString); override;

  public
    constructor Create;
    destructor Destroy; override;

    property GLName: GLuint read FGLName;

  end;

  TGLHandleObject = class(TGLObject)
  private
    FGLName: GLHandle;

  protected
    procedure GenObject(out AGLName: GLHandle); virtual; abstract;
    procedure DeleteObject(const AGLName: GLHandle); virtual; abstract;

    procedure SetGLLabel(const Value: AnsiString); override;

  public
    constructor Create;
    destructor Destroy; override;

    property GLName: GLHandle read FGLName;

  end;

implementation

{ TGLUIntObject }

constructor TGLUIntObject.Create;
begin
  GenObject(FGLName);
end;

destructor TGLUIntObject.Destroy;
begin
  DeleteObject(FGLName);
end;

procedure TGLUIntObject.SetGLLabel(const Value: AnsiString);
begin
  FGLLabel := Value;
  if Value = '' then
    glObjectLabel(Ord(GetObjectType), GLName, -1, nil)
  else
    glObjectLabel(Ord(GetObjectType), GLName, Length(Value), @Value[1]);
end;

{ TGLHandleObject }

constructor TGLHandleObject.Create;
begin
  GenObject(FGLName);
end;

destructor TGLHandleObject.Destroy;
begin
  DeleteObject(FGLName);
end;

procedure TGLHandleObject.SetGLLabel(const Value: AnsiString);
begin
  FGLLabel := Value;
  if Value = '' then
    glObjectLabel(Ord(GetObjectType), GLName, -1, nil)
  else
    glObjectLabel(Ord(GetObjectType), GLName, Length(Value), @Value[1]);
end;

end.
