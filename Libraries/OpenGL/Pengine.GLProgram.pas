unit Pengine.GLProgram;

interface

uses
  dglOpenGL,

  System.Types,
  System.UITypes,
  System.Classes,
  System.SysUtils,

  Vcl.Dialogs,
  Vcl.Controls,
  Vcl.Forms,

  Winapi.Windows,

  Pengine.Collections,
  Pengine.HashCollections,
  Pengine.Hasher,
  Pengine.Matrix,
  Pengine.GLEnums,
  Pengine.GLState, 
  Pengine.Vector,
  Pengine.ResourceManager;

type

  EUniformDataTypeUnsupported = class(Exception)
  public
    constructor Create(ADataType: TGLDataType);
  end;

  EAttribOrderSetAlready = class(Exception)
  public
    constructor Create;
  end;

  EAttribMissing = class(Exception)
  public
    constructor Create(AAttribute: AnsiString);
  end;

  EAttribNotUsed = class(Exception)
  public
    constructor Create(AAttribute: AnsiString);
  end;

  EUniformWrongDataType = class(Exception)
  public
    constructor Create(AUniform: AnsiString; AExpected, AGot: string);
  end;

  EGetInactiveUniform = class(Exception)
  public
    constructor Create(AUniform: AnsiString);
  end;

  EShaderCompilation = class(Exception)
  private
    FLog: AnsiString;
  public
    constructor Create(ALog: AnsiString);
    property Log: AnsiString read FLog;
  end;

  EShaderFileCompilation = class(Exception)
  public
    constructor Create(AFileName: string; ALog: AnsiString);
  end;

  EShaderResourceCompilation = class(Exception)
  public
    constructor Create(AResourceName: string; ALog: AnsiString);
  end;

  EShaderLinking = class(Exception)
  public
    constructor Create(ALog: AnsiString);
  end;

  TGLProgram = class(TGLHandleObject)
  public type

    TVariable = class abstract
    private
      FProgram: TGLProgram;
      FName: AnsiString;
      FDataType: TGLDataType;
      FCount: Integer;
      FDataSize: Integer;
      FBaseDataType: TGLBaseDataType;
      FBaseCount: Integer;
      FLocation: Integer;

      function GetActive: Boolean;

    protected
      function GetLocation: Integer; virtual; abstract;

    public
      constructor Create(AShader: TGLProgram; AName: AnsiString; ADataType: TGLDataType; ACount: Integer); overload;
      destructor Destroy; override;

      /// <summary>Creates a "dead" entry with a Location of -1</summary>
      constructor Create; overload;

      /// <summary>The Shader, to which this Variable belongs to</summary>
      property GLProgram: TGLProgram read FProgram;
      /// <summary>The Name of the Variable as written in the Shader</summary>
      property Name: AnsiString read FName;
      /// <summary>The DataType of the Variable</summary>
      property DataType: TGLDataType read FDataType;
      /// <summary>The ArrayLength of the Variable</summary>
      /// <remarks>For usual Non-Array Types this is always 1</remarks>
      property Count: Integer read FCount;
      /// <summary>The full size of the Variable, measured in bytes</summary>
      property DataSize: Integer read FDataSize;

      /// <summary>
      /// The underlying Basic data type.
      /// <p>float for vec3/mat4/...</p>
      /// </summary>
      property BaseDataType: TGLBaseDataType read FBaseDataType;
      /// <summary>
      /// The Count measured in the BaseDataType
      /// <code>
      /// Examples:<p/>
      /// vec3     -> 3<p/>
      /// mat4     -> 4x4 -> 16<p/>
      /// vec2[10] -> 20x2 -> 20
      /// </code>
      /// </summary>
      property BaseCount: Integer read FBaseCount;

      /// <summary>A unique identifier for this Variable in context to the underlying Shader</summary>
      property Location: Integer read FLocation;

      /// <summary>Wether this Variable is non-existent in the Shader (Location = -1)</summary>
      property Active: Boolean read GetActive;

    end;

    TAttribute = class(TVariable)
    private
      FOffset: Integer;

      function GetOffset: Integer;

    protected
      function GetLocation: Integer; override;

    public
      constructor Create(AShader: TGLProgram; AName: AnsiString; ADataType: TGLDataType; ACount: Integer); overload;
      constructor Create; overload;

      property Offset: Integer read GetOffset;

    end;

    TUniform = class abstract(TVariable)
    protected
      function GetLocation: Integer; override;

    end;

    TUniform<T> = class(TUniform)
    private
      FValue: T;

      function GetValue: T;
      procedure SetValue(const Value: T);

    public
      constructor Create(AShader: TGLProgram; AName: AnsiString; ADataType: TGLDataType; ACount: Integer); overload;
      constructor Create; overload;

      property Value: T read FValue write SetValue;

    end;

    TUniformSampler = TUniform<Integer>;

    TInterfaceVariableMap = TToObjectMap<AnsiString, TVariable, TAnsiStringHasher>;
    TAttributes = TRefArray<TAttribute>;

    TType = (
      stFragment,
      stVertex,
      stGeometry,
      stCompute
      );

    TAttributeOrder = array of AnsiString;

  public const

    GLShaderTypes: array [TType] of TGLShaderType = (
      TGLShaderType.stFragment,
      TGLShaderType.stVertex,
      TGLShaderType.stGeometry,
      TGLShaderType.stCompute
      );

    FileExtensions: array [TType] of string = ('.fs', '.vs', '.gs', '.cs');

    ResourceExtensions: array [TType] of string = ('_FS', '_VS', '_GS', '_CS');

  private
    FInterfaceVariables: TInterfaceVariableMap;
    FAttributes: TAttributes;
    FAttributeStride: Integer;

    procedure CheckShaderErrors(AShader: GLHandle);
    procedure CheckProgramErrors;

    procedure LoadAttributeLocations;
    procedure LoadUniformLocations;

    procedure AddShaderFromStream(AShaderType: TType; AStream: TStream);
    function GetAttributes: TAttributes.TReader;

  protected
    procedure GenObject(out AGLName: GLHandle); override;
    procedure DeleteObject(const AGLName: GLHandle); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

  public
    constructor Create(AGLState: TGLState);
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    procedure LoadFromFile(AFileName: string);
    procedure LoadFromResource(AResourceName: string);

    procedure AddShaderFromText(ShaderType: TType; const AText: AnsiString);
    procedure AddShaderFromFile(AShaderType: TType; AFileName: string);
    procedure AddShaderFromResource(AShaderType: TType; AResourceName: string);

    procedure Link;

    procedure SetAttributeOrder(AAttributes: TAttributeOrder);

    property Attributes: TAttributes.TReader read GetAttributes;
    property AttributeStride: Integer read FAttributeStride;

    function Uniform<T>(AName: AnsiString): TUniform<T>;
    function UniformSampler(AName: AnsiString): TUniformSampler;

  end;

  TGLProgramResource = class(TParamResource<TGLProgram, TGLObjectParam>)
  protected
    class function CreateData(AParam: TGLObjectParam): TGLProgram; override;

    class function GetFileName: string; virtual; abstract;
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; virtual; abstract;
  end;

  TGLProgramResourceClass = class of TGLProgramResource;

implementation

uses
  Pengine.IntMaths;

{ TShader.TAttribute }

constructor TGLProgram.TAttribute.Create(AShader: TGLProgram; AName: AnsiString; ADataType: TGLDataType; ACount: Integer);
begin
  inherited;
  FOffset := -1;
end;

constructor TGLProgram.TAttribute.Create;
begin
  inherited Create;
end;

function TGLProgram.TAttribute.GetLocation: Integer;
begin
  Result := glGetAttribLocation(GLProgram.GLName, @Name[1]);
end;

function TGLProgram.TAttribute.GetOffset: Integer;
begin
  if FOffset = -1 then
    raise EAttribNotUsed.Create(Name);
  Result := FOffset;
end;

{ TShader }

procedure TGLProgram.AddShaderFromFile(AShaderType: TType; AFileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    try
      AddShaderFromStream(AShaderType, FileStream);
    finally
      FileStream.Free;
    end;
  except
    on E: EShaderCompilation do
      raise EShaderFileCompilation.Create(AFileName, E.Log);
  end;
end;

procedure TGLProgram.AddShaderFromResource(AShaderType: TType; AResourceName: string);
var
  ResourceStream: TResourceStream;
begin
  ResourceStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    try
      AddShaderFromStream(AShaderType, ResourceStream);
    finally
      ResourceStream.Free;
    end;
  except
    on E: EShaderCompilation do
      raise EShaderResourceCompilation.Create(AResourceName, E.Log);
  end;
end;

procedure TGLProgram.AddShaderFromStream(AShaderType: TType; AStream: TStream);
var
  Text: AnsiString;
begin
  if AStream.Size = 0 then
    AddShaderFromText(AShaderType, '')
  else
  begin
    SetLength(Text, AStream.Size);
    AStream.Read(Text[1], AStream.Size);
    AddShaderFromText(AShaderType, Text);
  end;
end;

procedure TGLProgram.AddShaderFromText(ShaderType: TType; const AText: AnsiString);
var
  ShaderObject, ShaderLength: Integer;
  Data: PGLchar;
begin
  ShaderLength := Length(AText);

  ShaderObject := glCreateShader(Ord(GLShaderTypes[ShaderType]));

  Data := @AText[1];
  glShaderSource(ShaderObject, 1, @Data, @ShaderLength);
  glCompileShader(ShaderObject);

  try
    CheckShaderErrors(ShaderObject);
    glAttachShader(GLName, ShaderObject);
  finally
    glDeleteShader(ShaderObject);
  end;
end;

procedure TGLProgram.BindGLObject;
begin
  glUseProgram(GLName);
end;

procedure TGLProgram.CheckShaderErrors(AShader: GLHandle);
var
  blen, slen: Integer;
  InfoLog: AnsiString;
  Success: Integer;
begin
  glGetShaderiv(AShader, GL_COMPILE_STATUS, @Success);
  if Success <> 0 then
    Exit;
  glGetShaderiv(AShader, GL_INFO_LOG_LENGTH, @blen);
  if blen <= 1 then
    Exit;
  SetLength(InfoLog, blen);
  glGetShaderInfoLog(AShader, blen, @slen, @InfoLog[1]);
  raise EShaderCompilation.Create(InfoLog);
end;

procedure TGLProgram.CheckProgramErrors;
var
  blen: GLInt;
  slen: GLsizei;
  InfoLog: AnsiString;
  Success: Integer;
begin
  glGetProgramiv(GLName, GL_LINK_STATUS, @Success);
  if Success <> 0 then
    Exit;

  glGetProgramiv(GLName, GL_INFO_LOG_LENGTH, @blen);
  if blen <= 1 then
    raise EShaderLinking.Create('Unknown Error');

  SetLength(InfoLog, blen);
  glGetProgramInfoLog(GLName, blen, @slen, @InfoLog[1]);
  raise EShaderLinking.Create(InfoLog);
end;

constructor TGLProgram.Create;
begin
  inherited;
  FInterfaceVariables := TInterfaceVariableMap.Create;
end;

procedure TGLProgram.DeleteObject(const AGLName: GLHandle);
begin
  glDeleteProgram(AGLName);
end;

destructor TGLProgram.Destroy;
begin
  FAttributes.Free;
  FInterfaceVariables.Free;
  inherited;
end;

procedure TGLProgram.GenObject(out AGLName: GLHandle);
begin
  AGLName := glCreateProgram;
end;

function TGLProgram.GetAttributes: TAttributes.TReader;
begin
  Result := FAttributes.Reader;
end;

class function TGLProgram.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TGLObjectBinding<TGLProgram>;
end;

class function TGLProgram.GetObjectType: TGLObjectType;
begin
  Result := otProgram;
end;

procedure TGLProgram.UnbindGLObject;
begin
  glUseProgram(0);
end;

function TGLProgram.Uniform<T>(AName: AnsiString): TUniform<T>;
var
  InterfaceVar: TVariable;
begin
  if FInterfaceVariables.Get(AName, InterfaceVar) then
  begin
    if InterfaceVar is TUniform<T> then
      Result := TUniform<T>(InterfaceVar)
    else
      raise EUniformWrongDataType.Create(AName, TUniform<T>.ClassName, InterfaceVar.ClassName)
  end
  else
  begin
    Result := TUniform<T>.Create;
    FInterfaceVariables[AName] := Result;
  end;
end;

function TGLProgram.UniformSampler(AName: AnsiString): TUniformSampler;
begin
  Result := Uniform<Integer>(AName);
end;

procedure TGLProgram.Link;
begin
  glLinkProgram(GLName);
  CheckProgramErrors;
  Bind;
  LoadAttributeLocations;
  LoadUniformLocations;
end;

procedure TGLProgram.LoadAttributeLocations;
var
  AttributeCount, MaxLength, ActualLength, Size, I: Integer;
  Buffer: AnsiString;
  DataType: GLenum;
begin
  glGetProgramiv(GLName, GL_ACTIVE_ATTRIBUTES, @AttributeCount);
  if AttributeCount = 0 then
    Exit;
  glGetProgramiv(GLName, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH, @MaxLength);
  for I := 0 to AttributeCount - 1 do
  begin
    SetLength(Buffer, MaxLength);
    glGetActiveAttrib(GLName, I, MaxLength, ActualLength, Size, DataType, @Buffer[1]);
    SetLength(Buffer, ActualLength);
    FInterfaceVariables[Buffer] := TAttribute.Create(Self, Buffer, TGLDataType(DataType), Size);
  end;
end;

procedure TGLProgram.LoadFromFile(AFileName: string);
var
  ShaderType: TType;
  Name: string;
begin
  GLLabel := AnsiString(AFileName);
  for ShaderType := Low(TType) to High(TType) do
  begin
    Name := AFileName + FileExtensions[ShaderType];
    if FileExists(Name) then
      AddShaderFromFile(ShaderType, Name);
  end;
  Link;
end;

procedure TGLProgram.LoadFromResource(AResourceName: string);
var
  ShaderType: TType;
  Name: string;
begin
  GLLabel := AnsiString(AResourceName);
  for ShaderType := Low(TType) to High(TType) do
  begin
    Name := AResourceName + ResourceExtensions[ShaderType];
    if FindResource(HInstance, @Name[1], RT_RCDATA) <> 0 then
      AddShaderFromResource(ShaderType, Name);
  end;
  Link;
end;

procedure TGLProgram.LoadUniformLocations;
var
  UniformCount, MaxLength, ActualLength, Size, I: Integer;
  Buffer: AnsiString;
  GLDataType: GLenum;
  DataType: TGLDataType;
  Uniform: TUniform;
begin
  glGetProgramiv(GLName, GL_ACTIVE_UNIFORMS, @UniformCount);
  if UniformCount = 0 then
    Exit;
  glGetProgramiv(GLName, GL_ACTIVE_UNIFORM_MAX_LENGTH, @MaxLength);
  for I := 0 to UniformCount - 1 do
  begin
    SetLength(Buffer, MaxLength);
    glGetActiveUniform(GLName, I, MaxLength, ActualLength, Size, GLDataType, @Buffer[1]);
    SetLength(Buffer, ActualLength);
    DataType := TGLDataType(GLDataType);
    case DataType of
      dtInt:
        Uniform := TUniform<Integer>.Create(Self, Buffer, DataType, Size);
      dtUInt:
        Uniform := TUniform<Cardinal>.Create(Self, Buffer, DataType, Size);
      dtFloat:
        Uniform := TUniform<Single>.Create(Self, Buffer, DataType, Size);
      dtVec2:
        Uniform := TUniform<TVector2>.Create(Self, Buffer, DataType, Size);
      dtVec3:
        Uniform := TUniform<TVector3>.Create(Self, Buffer, DataType, Size);
      dtVec4:
        Uniform := TUniform<TVector4>.Create(Self, Buffer, DataType, Size);
      dtBoolean:
        Uniform := TUniform<Boolean>.Create(Self, Buffer, DataType, Size);
      dtMat2:
        Uniform := TUniform<TMatrix2>.Create(Self, Buffer, DataType, Size);
      dtMat3:
        Uniform := TUniform<TMatrix3>.Create(Self, Buffer, DataType, Size);
      dtMat4:
        Uniform := TUniform<TMatrix4>.Create(Self, Buffer, DataType, Size);
    else
      if GLDataTypeIsSampler(DataType) then
        Uniform := TUniformSampler.Create(Self, Buffer, DataType, Size)
      else
        raise EUniformDataTypeUnsupported.Create(DataType);
    end;
    FInterfaceVariables[Buffer] := Uniform;
  end;
end;

procedure TGLProgram.SetAttributeOrder(AAttributes: TAttributeOrder);
var
  Attribute: AnsiString;
  InterfaceVar: TVariable;
begin
  if FAttributes <> nil then
    raise EAttribOrderSetAlready.Create;

  FAttributes := TAttributes.Create;

  FAttributeStride := 0;
  for Attribute in AAttributes do
  begin
    if FInterfaceVariables.Get(Attribute, InterfaceVar) then
    begin
      if InterfaceVar is TAttribute then
      begin
        FAttributes.Add(TAttribute(InterfaceVar));
        TAttribute(InterfaceVar).FOffset := FAttributeStride;
        FAttributeStride := FAttributeStride + InterfaceVar.DataSize;
      end;
    end
    else
    begin
      raise EAttribMissing.Create(Attribute);
    end;
  end;
end;

{ EShaderCompilation }

constructor EShaderCompilation.Create(ALog: AnsiString);
begin
  inherited Create(string(ALog));
  FLog := ALog;
end;

{ EFileShaderCompilation }

constructor EShaderFileCompilation.Create(AFileName: string; ALog: AnsiString);
begin
  inherited Create('GLSL Compilation-Error in File: ' + AFileName + sLineBreak + string(ALog));
end;

{ EShaderResourceCompilation }

constructor EShaderResourceCompilation.Create(AResourceName: string; ALog: AnsiString);
begin
  inherited Create('GLSL Compilation-Error in Resource: ' + AResourceName + sLineBreak + string(ALog));
end;

{ EShaderLinking }

constructor EShaderLinking.Create(ALog: AnsiString);
begin
  inherited Create('GLSL Linking-Error:' + sLineBreak + string(ALog));
end;

{ TShader.TVariable }

constructor TGLProgram.TVariable.Create(AShader: TGLProgram; AName: AnsiString; ADataType: TGLDataType; ACount: Integer);
begin
  FProgram := AShader;
  FName := AName;
  FDataType := ADataType;
  FCount := ACount;
  FDataSize := GLDataTypeSize(DataType) * Count;
  FBaseDataType := GLBaseDataType(FDataType);
  Assert(GLDataTypeSize(FDataType) mod GLDataTypeSize(TGLDataType(FBaseDataType)) = 0);
  FBaseCount := GLDataTypeSize(FDataType) div GLDataTypeSize(TGLDataType(FBaseDataType));
  FLocation := GetLocation;
end;

constructor TGLProgram.TVariable.Create;
begin
  FLocation := -1;
end;

destructor TGLProgram.TVariable.Destroy;
begin
  inherited;
end;

function TGLProgram.TVariable.GetActive: Boolean;
begin
  Result := Location <> -1;
end;

{ TShader.TUniformBase }

function TGLProgram.TUniform.GetLocation: Integer;
begin
  Result := glGetUniformLocation(GLProgram.GLName, @Name[1]);
end;

{ EUniformWrongDataType }

constructor EUniformWrongDataType.Create(AUniform: AnsiString; AExpected, AGot: string);
begin
  inherited CreateFmt('Uniform "%s" got accessed as "%s" but is "%s"', [AUniform, AExpected, AGot]);
end;

{ TShader.TUniform<T> }

constructor TGLProgram.TUniform<T>.Create(AShader: TGLProgram; AName: AnsiString; ADataType: TGLDataType;
  ACount: Integer);
begin
  inherited;
  if Active then
    FValue := GetValue;
end;

constructor TGLProgram.TUniform<T>.Create;
begin
  inherited;
end;

function TGLProgram.TUniform<T>.GetValue: T;
var
  BoolHelp: Integer;
begin
  if not Active then
    raise EGetInactiveUniform.Create(Name);
  case GLBaseDataType(DataType) of
    bdtInt:
      if DataType = dtBoolean then
      begin
        glGetUniformiv(GLProgram.GLName, Location, @BoolHelp);
        PBoolean(@Result)^ := BoolHelp <> 0;
      end
      else
        glGetUniformiv(GLProgram.GLName, Location, PGLint(@Result));
    bdtUInt:
      glGetUniformuiv(GLProgram.GLName, Location, PGLuint(@Result));
    bdtFloat:
      glGetUniformfv(GLProgram.GLName, Location, PGLfloat(@Result));
    bdtDouble:
      glGetUniformdv(GLProgram.GLName, Location, PGLdouble(@Result));
  else
    raise ENotImplemented.Create('Missing BaseDataType for Uniform GetValue');
  end;
end;

procedure TGLProgram.TUniform<T>.SetValue(const Value: T);
var
  BoolHelp: Cardinal;
begin
  if not Active then
    Exit;
  FValue := Value;
  GLProgram.Bind;
  case DataType of
    dtFloat:
      glUniform1fv(Location, Count, PGLfloat(@Value));
    dtVec2:
      glUniform2fv(Location, Count, PGLfloat(@Value));
    dtVec3:
      glUniform3fv(Location, Count, PGLfloat(@Value));
    dtVec4:
      glUniform4fv(Location, Count, PGLfloat(@Value));

    dtMat2:
      glUniformMatrix2fv(Location, Count, False, PGLfloat(@Value));
    dtMat3:
      glUniformMatrix3fv(Location, Count, False, PGLfloat(@Value));
    dtMat4:
      glUniformMatrix4fv(Location, Count, False, PGLfloat(@Value));

    dtMat2x3:
      glUniformMatrix2x3fv(Location, Count, False, PGLfloat(@Value));
    dtMat2x4:
      glUniformMatrix2x4fv(Location, Count, False, PGLfloat(@Value));
    dtMat3x2:
      glUniformMatrix3x2fv(Location, Count, False, PGLfloat(@Value));
    dtMat3x4:
      glUniformMatrix3x4fv(Location, Count, False, PGLfloat(@Value));
    dtMat4x2:
      glUniformMatrix4x2fv(Location, Count, False, PGLfloat(@Value));
    dtMat4x3:
      glUniformMatrix4x3fv(Location, Count, False, PGLfloat(@Value));

    dtDouble:
      glUniform1dv(Location, Count, PGLdouble(@Value));
    dtDVec2:
      glUniform2dv(Location, Count, PGLdouble(@Value));
    dtDVec3:
      glUniform3dv(Location, Count, PGLdouble(@Value));
    dtDVec4:
      glUniform4dv(Location, Count, PGLdouble(@Value));

    dtDMat2:
      glUniformMatrix2dv(Location, Count, False, PGLdouble(@Value));
    dtDMat3:
      glUniformMatrix3dv(Location, Count, False, PGLdouble(@Value));
    dtDMat4:
      glUniformMatrix4dv(Location, Count, False, PGLdouble(@Value));

    dtDMat2x3:
      glUniformMatrix2x3dv(Location, Count, False, PGLdouble(@Value));
    dtDMat2x4:
      glUniformMatrix2x4dv(Location, Count, False, PGLdouble(@Value));
    dtDMat3x2:
      glUniformMatrix3x2dv(Location, Count, False, PGLdouble(@Value));
    dtDMat3x4:
      glUniformMatrix3x4dv(Location, Count, False, PGLdouble(@Value));
    dtDMat4x2:
      glUniformMatrix4x2dv(Location, Count, False, PGLdouble(@Value));
    dtDMat4x3:
      glUniformMatrix4x3dv(Location, Count, False, PGLdouble(@Value));

    dtInt:
      glUniform1iv(Location, Count, PGLint(@Value));
    dtIVec2:
      glUniform2iv(Location, Count, PGLint(@Value));
    dtIVec3:
      glUniform3iv(Location, Count, PGLint(@Value));
    dtIVec4:
      glUniform4iv(Location, Count, PGLint(@Value));

    dtBVec2:
      glUniform2iv(Location, Count, PGLint(@Value));
    dtBVec3:
      glUniform3iv(Location, Count, PGLint(@Value));
    dtBVec4:
      glUniform4iv(Location, Count, PGLint(@Value));

    dtUInt:
      glUniform1uiv(Location, Count, PGLuint(@Value));
    dtUVec2:
      glUniform2uiv(Location, Count, PGLuint(@Value));
    dtUVec3:
      glUniform3uiv(Location, Count, PGLuint(@Value));
    dtUVec4:
      glUniform4uiv(Location, Count, PGLuint(@Value));

    dtBoolean:
      begin
        if PBoolean(@Value)^ then
          BoolHelp := $FFFFFFFF
        else
          BoolHelp := 0;
        glUniform1iv(Location, Count, @BoolHelp);
      end

  else
    glUniform1iv(Location, Count, PGLint(@Value));
  end;
end;

{ EAttribNotUsed }

constructor EAttribNotUsed.Create(AAttribute: AnsiString);
begin
  CreateFmt('Vertex Attribute "%s" is not used.', [AAttribute]);
end;

{ EAttribOrderSetAlready }

constructor EAttribOrderSetAlready.Create;
begin
  inherited Create('The Attribute-Order can not be modified.');
end;

{ EUnsupportedDataType }

constructor EUniformDataTypeUnsupported.Create(ADataType: TGLDataType);
begin
  inherited CreateFmt('Uniform Data-Type "%s" is not supported.', [GLDataTypeName(ADataType)]);
end;

{ EGetInactiveUniform }

constructor EGetInactiveUniform.Create(AUniform: AnsiString);
begin
  inherited CreateFmt('Cannot get the Uniform "%s" as it does not exist.', [AUniform]);
end;

{ EAttribMissing }

constructor EAttribMissing.Create(AAttribute: AnsiString);
begin
  CreateFmt('Vertex Attribute "%s" does not exist or got optimized away.', [AAttribute]);
end;

{ TGLProgramResource }

class function TGLProgramResource.CreateData(AParam: TGLObjectParam): TGLProgram;
begin
  Result := TGLProgram.Create(AParam.GLState);
  try
    Result.LoadFromFile(GetFileName);
    Result.SetAttributeOrder(GetAttributeOrder);
  except
    Result.Free;
    raise;
  end;
end;

end.
