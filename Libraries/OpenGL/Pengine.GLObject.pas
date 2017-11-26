unit Pengine.GLObject;

interface

uses
  dglOpenGL, IntfBase;

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

  { TGLObject }
  
  TGLObject = class abstract (TInterfaceBase)
  private
    FGLName: GLuint;
    FGLLabel: AnsiString;

  protected
    procedure GenObject(var AGLName: GLuint); virtual; abstract;
    procedure DeleteObject(var AGLName: GLuint); virtual; abstract;

    function GetObjectType: TGLObjectType; virtual; abstract;

    procedure SetGLLabel(const Value: AnsiString); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Bind; virtual; abstract;
    class procedure Unbind; virtual; abstract;

    property GLName: GLuint read FGLName;
    property GLLabel: AnsiString read FGLLabel write SetGLLabel;

  end;

implementation

{ TGLObject }

constructor TGLObject.Create;
begin
  GenObject(FGLName);
end;

destructor TGLObject.Destroy;
begin
  DeleteObject(FGLName);
end;

procedure TGLObject.SetGLLabel(const Value: AnsiString);
begin
  FGLLabel := Value;
  if Value = '' then
    glObjectLabel(Ord(GetObjectType), GLName, -1, nil)
  else
    glObjectLabel(Ord(GetObjectType), GLName, Length(Value), @Value[1]);
end;

end.
