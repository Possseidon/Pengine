unit Pengine.UBO;

interface

uses
  dglOpenGL,

  System.SysUtils,

  Pengine.GLEnums,
  Pengine.GLProgram,
  Pengine.GLState;

type

  { TUBO }

  TUBO = class(TGLObject)
  private
    FBindingPoint: Integer;

    class var
      Initialized: Boolean;
      MaxBindings: Integer;
      BindingCount: Integer;
      
    class procedure Init;

  protected
    procedure GenObject(out AGLName: Cardinal); override;
    procedure DeleteObject(const AGLName: Cardinal); override;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    procedure Generate(AData: Pointer; ASize: Integer; ABufferUsage: TGLBufferUsage); overload;
    procedure Generate(ASize: Integer; ABufferUsage: TGLBufferUsage); overload;

    procedure SubData(AOffset, ASize: Integer; const AData);

    procedure Bind; override;
    procedure Unbind; override;

    procedure BindToGLProgram(AGLProgram: TGLProgram; AName: PAnsiChar);

    class function GetObjectType: TGLObjectType; override;

  end;

implementation

{ TUBO }

class procedure TUBO.Init;
begin
  glGetIntegerv(GL_MAX_UNIFORM_BUFFER_BINDINGS, @MaxBindings);
  Initialized := True;
end;

procedure TUBO.GenObject(out AGLName: Cardinal);
begin
  glGenBuffers(1, @AGLName);
end;

procedure TUBO.DeleteObject(const AGLName: Cardinal);
begin
  glDeleteBuffers(1, @AGLName);
end;

constructor TUBO.Create(AGLState: TGLState);
begin
  if not Initialized then
    Init;

  if BindingCount >= MaxBindings then
    raise Exception.Create('Too many UBOs!');

  inherited;

  FBindingPoint := BindingCount;
  Inc(BindingCount);
end;

destructor TUBO.Destroy;
begin
  inherited;
end;

procedure TUBO.Generate(AData: Pointer; ASize: Integer; ABufferUsage: TGLBufferUsage);
begin
  Bind;
  glBufferData(GL_UNIFORM_BUFFER, ASize, AData, Ord(ABufferUsage));
end;

procedure TUBO.Generate(ASize: Integer; ABufferUsage: TGLBufferUsage);
begin
  Bind;
  glBufferData(GL_UNIFORM_BUFFER, ASize, nil, Ord(ABufferUsage));
end;

procedure TUBO.SubData(AOffset, ASize: Integer; const AData);
begin
  Bind;
  glBufferSubData(GL_UNIFORM_BUFFER, AOffset, ASize, @AData);
end;

procedure TUBO.Unbind;
begin
  glBindBuffer(GL_UNIFORM_BUFFER, 0);
end;

procedure TUBO.Bind;
begin
  glBindBuffer(GL_UNIFORM_BUFFER, GLName);
end;

procedure TUBO.BindToGLProgram(AGLProgram: TGLProgram; AName: PAnsiChar);
var
  BlockIndex: GLuint;
begin
  Bind;
  BlockIndex := glGetUniformBlockIndex(AGLProgram.GLName, AName);
  if BlockIndex = GL_INVALID_INDEX then
    raise Exception.Create('The UBO ' + AName + ' does not exist in the shader.');
  glBindBufferBase(GL_UNIFORM_BUFFER, FBindingPoint, GLName);
  glUniformBlockBinding(AGLProgram.GLName, BlockIndex, FBindingPoint);
end;

class function TUBO.GetObjectType: TGLObjectType;
begin
  Result := otBuffer;
end;

end.

