unit Pengine.UBO;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,

  Pengine.GLEnums,
  Pengine.GLProgram,
  Pengine.GLState,
  Pengine.Bitfield;

type

  EBufferObjectTooMany = class(Exception)
  public
    constructor Create;
  end;

  TBufferObject = class(TGLObject)
  public type

    TBindingPoint = type Integer;

    TBinding = class(TGLObjectBinding<TBufferObject>)
    private
      FUsedBindingPoints: TBitfield;
      FFirstUnusedBindingPoint: TBindingPoint;

    public
      constructor Create; override;
      destructor Destroy; override;

      function Add: TBindingPoint;
      procedure Del(ABindingPoint: TBindingPoint);

    end;

  private
    FBindingPoint: TBindingPoint;

  protected
    procedure GenObject(out AGLName: Cardinal); override;
    procedure DeleteObject(const AGLName: Cardinal); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

    class function TargetType: GLenum; virtual; abstract;

    function Binding: TBinding;

    procedure DoBlockBinding(AGLProgram: TGLProgram; ABlockIndex: Integer); virtual; abstract;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    procedure BindToGLProgram(AGLProgram: TGLProgram; AName: PAnsiChar);

  end;

  TUBO<TData: record> = class(TBufferObject)
  public type

    PData = ^TData;

  protected
    class function TargetType: Cardinal; override;

    procedure DoBlockBinding(AGLProgram: TGLProgram; ABlockIndex: Integer); override;

  public
    constructor Create(AGLState: TGLState; ABufferUsage: TGLBufferUsage);

    procedure SubData(AOffset, ASize: Integer; const AData);

    procedure BindToGLProgram(AGLProgram: TGLProgram; AName: PAnsiChar);

  end;

  TSSBO = class(TBufferObject)
    // TODO
  end;

implementation

{ EBufferObjectTooMany }

constructor EBufferObjectTooMany.Create;
begin
  inherited Create('Too many buffer objects.');
end;

{ TBufferObject.TBinding }

constructor TBufferObject.TBinding.Create;
var
  BindingPoints: Integer;
begin
  glGetIntegerv(GL_MAX_UNIFORM_BUFFER_BINDINGS, @BindingPoints);
  FUsedBindingPoints := TBitfield.Create(BindingPoints);
end;

destructor TBufferObject.TBinding.Destroy;
begin
  FUsedBindingPoints.Free;
  inherited;
end;

function TBufferObject.TBinding.Add: TBindingPoint;
begin
  if FFirstUnusedBindingPoint = FUsedBindingPoints.Size then
    raise EBufferObjectTooMany.Create;
  FUsedBindingPoints[FFirstUnusedBindingPoint] := True;
  Result := FFirstUnusedBindingPoint;
  repeat
    Inc(FFirstUnusedBindingPoint)
  until not FUsedBindingPoints[FFirstUnusedBindingPoint];
end;

procedure TBufferObject.TBinding.Del(ABindingPoint: TBindingPoint);
begin
  FUsedBindingPoints[ABindingPoint] := False;
  FFirstUnusedBindingPoint := Min(FFirstUnusedBindingPoint, ABindingPoint);
end;

{ TBufferObject }

procedure TBufferObject.GenObject(out AGLName: Cardinal);
begin
  glGenBuffers(1, @AGLName);
end;

procedure TBufferObject.DeleteObject(const AGLName: Cardinal);
begin
  glDeleteBuffers(1, @AGLName);
end;

procedure TBufferObject.BindGLObject;
begin
  glBindBuffer(TargetType, GLName);
end;

procedure TBufferObject.UnbindGLObject;
begin
  glBindBuffer(TargetType, 0);
end;

function TBufferObject.Binding: TBinding;
begin
  Result := TBinding(inherited Binding);
end;

procedure TBufferObject.BindToGLProgram(AGLProgram: TGLProgram; AName: PAnsiChar);
var
  BlockIndex: GLuint;
begin
  Bind;
  BlockIndex := glGetUniformBlockIndex(AGLProgram.GLName, AName);
  if BlockIndex = GL_INVALID_INDEX then
    raise Exception.Create('The UBO ' + AName + ' does not exist in the shader.');
  glBindBufferBase(GL_UNIFORM_BUFFER, FBindingPoint, GLName);
  DoBlockBinding(AGLProgram, BlockIndex);
end;

constructor TBufferObject.Create(AGLState: TGLState);
begin
  inherited;
  FBindingPoint := Binding.Add;
end;

destructor TBufferObject.Destroy;
begin
  Binding.Del(FBindingPoint);
  inherited;
end;

class function TBufferObject.GetObjectType: TGLObjectType;
begin
  Result := otBuffer;
end;

class function TBufferObject.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TBinding;
end;

{ TUBO<TData> }

constructor TUBO<TData>.Create(AGLState: TGLState; ABufferUsage: TGLBufferUsage);
begin
  inherited Create(AGLState);
  Bind;
  glBufferData(GL_UNIFORM_BUFFER, SizeOf(TData), nil, Ord(ABufferUsage));
end;

procedure TUBO<TData>.DoBlockBinding(AGLProgram: TGLProgram; ABlockIndex: Integer);
begin
  glUniformBlockBinding(AGLProgram.GLName, ABlockIndex, FBindingPoint);
end;

procedure TUBO<TData>.SubData(AOffset, ASize: Integer; const AData);
begin
  Bind;
  glBufferSubData(GL_UNIFORM_BUFFER, AOffset, ASize, @AData);
end;

class function TUBO<TData>.TargetType: Cardinal;
begin
  Result := GL_UNIFORM_BUFFER;
end;

procedure TUBO<TData>.BindToGLProgram(AGLProgram: TGLProgram; AName: PAnsiChar);
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

end.

