unit UBOManager;

interface

uses
  dglOpenGL, GLEnums, Shaders, SysUtils;

type

  { TUBO }

  TUBO = class
  private
    FUBO: Integer;
    FBinding: Integer;

    class var
      Initialized: Boolean;
      MaxBindings: Integer;
      BindingCount: Integer;
      BoundUBO: TUBO;

    class procedure Init;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Generate(AData: Pointer; ASize: Integer; ABufferUsage: TGLBufferUsage); overload;
    procedure Generate(ASize: Integer; ABufferUsage: TGLBufferUsage); overload;

    procedure SubData(AOffset, ASize: Integer; const AData);

    procedure Bind;

    procedure BindToShader(AShader: TShader; AName: PAnsiChar);
  end;

implementation

{ TUBO }

class procedure TUBO.Init;
begin
  glGetIntegerv(GL_MAX_UNIFORM_BUFFER_BINDINGS, @MaxBindings);
  Initialized := True;
end;

constructor TUBO.Create;
begin
  if not Initialized then
    Init;

  if BindingCount >= MaxBindings then
    raise Exception.Create('Too many UBOs!');

  FBinding := BindingCount;
  Inc(BindingCount);
  glGenBuffers(1, @FUBO);
end;

destructor TUBO.Destroy;
begin
  glDeleteBuffers(1, @FUBO);
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

procedure TUBO.Bind;
begin
  if Pointer(BoundUBO) <> Pointer(Self) then
  begin
    glBindBuffer(GL_UNIFORM_BUFFER, FUBO);
    BoundUBO := Self;
  end;
end;

procedure TUBO.BindToShader(AShader: TShader; AName: PAnsiChar);
var
  BlockIndex: Integer;
begin
  Bind;
  BlockIndex := glGetUniformBlockIndex(AShader.ProgramObject, AName);
  glBindBufferBase(GL_UNIFORM_BUFFER, FBinding, FUBO);
  glUniformBlockBinding(AShader.ProgramObject, BlockIndex, FBinding);
end;

end.

