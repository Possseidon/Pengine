unit Shaders;

interface

uses
  {$IFDEF FPC}
  {$ELSE}
  Types,
  UITypes,
  {$ENDIF}
  dglOpenGL, Dialogs, Classes, SysUtils, GLEnums, Controls, Forms, Lists, VectorGeometry, Matrix, Windows;

type

  { EUnsupportedDataType }

  EUniformDataTypeUnsupported = class(Exception)
  public
    constructor Create(ADataType: TGLDataType);
  end;

  { EAttribOrderSetAlready }

  EAttribOrderSetAlready = class(Exception)
  public
    constructor Create;
  end;

  { EAttribMissing }

  EAttribMissing = class(Exception)
  public
    constructor Create(AAttribute: AnsiString);
  end;

  { EAttribNotUsed }

  EAttribNotUsed = class(Exception)
  public
    constructor Create(AAttribute: AnsiString);
  end;

  { EUniformWrongDataType }
  
  EUniformWrongDataType = class(Exception)
  public
    constructor Create(AUniform: AnsiString; AExpected, AGot: string);
  end;

  { EGetInactiveUniform }
  
  EGetInactiveUniform = class(Exception)
  public
    constructor Create(AUniform: AnsiString);  
  end;

  { EShaderCompilation }

  EShaderCompilation = class(Exception)
  private
    FLog: AnsiString;
  public
    constructor Create(ALog: AnsiString);
    property Log: AnsiString read FLog;
  end;

  { EShaderFileCompilation }

  EShaderFileCompilation = class(Exception)
  public
    constructor Create(AFileName: string; ALog: AnsiString);
  end;

  { EShaderResourceCompilation }

  EShaderResourceCompilation = class(Exception)
  public
    constructor Create(AResourceName: string; ALog: AnsiString);
  end;

  { ELinking }

  EShaderLinking = class(Exception)
  public
    constructor Create(ALog: AnsiString);
  end;

  TShader = class;

  { TShaderVariable }

  TShaderVariable = class abstract
  private
    FShader: TShader;
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
    constructor Create(AShader: TShader; AName: AnsiString; ADataType: TGLDataType; ACount: Integer); overload;

    /// <summary>Creates a "dead" entry with a Location of -1</summary>
    constructor Create; overload;

    /// <summary>The Shader, to which this Variable belongs to</summary>
    property Shader: TShader read FShader;
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
    /// <para>float for vec3/mat4/...</para>
    /// </summary>
    property BaseDataType: TGLBaseDataType read FBaseDataType;
    /// <summary>
    /// The Count measured in the BaseDataType
    /// <code>
    /// <para>vec3     -> 3</para>
    /// <para>mat4     -> 4x4 -> 16</para>
    /// <para>vec2[10] -> 20x2 -> 20</para>
    /// </code>
    /// </summary>
    property BaseCount: Integer read FBaseCount;

    /// <summary>A unique identifier for this Variable in context to the underlying Shader</summary>
    property Location: Integer read FLocation;

    /// <summary>Wether this Variable is non-existent in the Shader (Location = -1)</summary>
    property Active: Boolean read GetActive;

  end;

  TShaderAttribute = class(TShaderVariable)
  private
    FOffset: Integer;
    function GetOffset: Integer;

  protected
    function GetLocation: Integer; override;

  public
    constructor Create(AShader: TShader; AName: AnsiString; ADataType: TGLDataType; ACount: Integer); overload;
    constructor Create; overload;

    property Offset: Integer read GetOffset;
  end;

  TShaderUniformBase = class abstract(TShaderVariable)
  protected
    function GetLocation: Integer; override;
  end;

  TShaderUniform<T> = class(TShaderUniformBase)
  private
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    property Value: T read GetValue write SetValue;
  end;

  TShaderUniformSampler = TShaderUniform<Integer>;

  TShaderInterfaceVariableMap = TAnsiStringObjectMap<TShaderVariable>;
  TShaderAttributes = TRefArray<TShaderAttribute>;
  TShaderAttributesReader = TRefArrayReader<TShaderAttribute>;

  TShaderType = (
    stFragment,
    stVertex,
    stGeometry,
    stCompute
    );

  TShaderAttributeOrder = array of AnsiString;

  { TShader }

  TShader = class
  private
    FProgramObject: GLHandle;
    FInterfaceVariables: TShaderInterfaceVariableMap;
    FAttributes: TShaderAttributes;
    FAttributesReader: TShaderAttributesReader;
    FAttributeStride: Integer;

    procedure CheckShaderErrors(AShader: GLHandle);
    procedure CheckProgramErrors;

    class var ActiveShader: TShader;

    procedure LoadAttributeLocations;
    procedure LoadUniformLocations;

    procedure AddShaderFromStream(AShaderType: TShaderType; AStream: TStream);

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(AFileName: string);
    procedure LoadFromResource(AResourceName: string);

    procedure AddShaderFromText(ShaderType: TShaderType; const AText: AnsiString);
    procedure AddShaderFromFile(AShaderType: TShaderType; AFileName: string);
    procedure AddShaderFromResource(AShaderType: TShaderType; AResourceName: string);

    procedure Link;

    procedure SetAttributeOrder(AAttributes: TShaderAttributeOrder);

    property Attributes: TShaderAttributesReader read FAttributesReader;
    property AttributeStride: Integer read FAttributeStride;

    procedure Enable;
    class procedure Disable;

    property ProgramObject: GLHandle read FProgramObject;

    function Uniform<T>(AName: AnsiString): TShaderUniform<T>;
    function UniformSampler(AName: AnsiString): TShaderUniformSampler;

  const

    GLShaderTypes: array [TShaderType] of TGLShaderType = (
      TGLShaderType.stFragment,
      TGLShaderType.stVertex,
      TGLShaderType.stGeometry,
      TGLShaderType.stCompute
      );

    FileExtensions: array [TShaderType] of string = ('.fs', '.vs', '.gs', '.cs');

    ResourceExtensions: array [TShaderType] of string = ('_FS', '_VS', '_GS', '_CS');

  end;

implementation

{ TShaderAttribute }

constructor TShaderAttribute.Create(AShader: TShader; AName: AnsiString; ADataType: TGLDataType; ACount: Integer);
begin
  inherited;
  FOffset := -1;
end;

constructor TShaderAttribute.Create;
begin
  inherited Create;
end;

function TShaderAttribute.GetLocation: Integer;
begin
  Result := glGetAttribLocation(Shader.ProgramObject, @Name[1]);
end;

function TShaderAttribute.GetOffset: Integer;
begin
  if FOffset = -1 then
    raise EAttribNotUsed.Create(Name);
  Result := FOffset;
end;

{ TShader }

procedure TShader.AddShaderFromFile(AShaderType: TShaderType; AFileName: string);
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

procedure TShader.AddShaderFromResource(AShaderType: TShaderType; AResourceName: string);
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

procedure TShader.AddShaderFromStream(AShaderType: TShaderType; AStream: TStream);
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

procedure TShader.AddShaderFromText(ShaderType: TShaderType; const AText: AnsiString);
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
    glAttachShader(FProgramObject, ShaderObject);
  finally
    glDeleteShader(ShaderObject);
  end;
end;

procedure TShader.CheckShaderErrors(AShader: GLHandle);
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

procedure TShader.CheckProgramErrors;
var
  blen: GLInt;
  slen: GLsizei;
  InfoLog: AnsiString;
  Success: Integer;
begin
  glGetProgramiv(FProgramObject, GL_LINK_STATUS, @Success);
  if Success <> 0 then
    Exit;

  glGetProgramiv(FProgramObject, GL_INFO_LOG_LENGTH, @blen);
  if blen <= 1 then
    raise EShaderLinking.Create('Unknown Error');

  SetLength(InfoLog, blen);
  glGetProgramInfoLog(FProgramObject, blen, @slen, @InfoLog[1]);
  raise EShaderLinking.Create(InfoLog);
end;

constructor TShader.Create;
begin
  FProgramObject := glCreateProgram;
  FInterfaceVariables := TShaderInterfaceVariableMap.Create;
end;

destructor TShader.Destroy;
begin
  FAttributesReader.Free;
  FAttributes.Free;
  FInterfaceVariables.Free;
  glDeleteProgram(FProgramObject);
  inherited;
end;

class procedure TShader.Disable;
begin
  if ActiveShader <> nil then
  begin
    glUseProgram(0);
    ActiveShader := nil;
  end;
end;

procedure TShader.Enable;
begin
  if ActiveShader <> Self then
  begin
    glUseProgram(FProgramObject);
    ActiveShader := Self;
  end;
end;

function TShader.Uniform<T>(AName: AnsiString): TShaderUniform<T>;
var
  InterfaceVar: TShaderVariable;
begin
  if FInterfaceVariables.Get(AName, InterfaceVar) then
  begin
    if InterfaceVar is TShaderUniform<T> then
      Result := TShaderUniform<T>(InterfaceVar)
    else
      raise EUniformWrongDataType.Create(AName, TShaderUniform<T>.ClassName, InterfaceVar.ClassName)
  end
  else
  begin
    Result := TShaderUniform<T>.Create;
    FInterfaceVariables[AName] := Result;
  end;
end;

function TShader.UniformSampler(AName: AnsiString): TShaderUniformSampler;
begin
  Result := Uniform<Integer>(AName);
end;

procedure TShader.Link;
begin
  glLinkProgram(FProgramObject);
  CheckProgramErrors;
  Enable;
  LoadAttributeLocations;
  LoadUniformLocations;
end;

procedure TShader.LoadAttributeLocations;
var
  AttributeCount, MaxLength, ActualLength, Size, I: Integer;
  Buffer: AnsiString;
  DataType: GLenum;
begin
  glGetProgramiv(ProgramObject, GL_ACTIVE_ATTRIBUTES, @AttributeCount);
  if AttributeCount = 0 then
    Exit;
  glGetProgramiv(ProgramObject, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH, @MaxLength);
  for I := 0 to AttributeCount - 1 do
  begin
    SetLength(Buffer, MaxLength);
    glGetActiveAttrib(ProgramObject, I, MaxLength, ActualLength, Size, DataType, @Buffer[1]);
    SetLength(Buffer, ActualLength);
    FInterfaceVariables[Buffer] := TShaderAttribute.Create(Self, Buffer, TGLDataType(DataType), Size);
  end;
end;

procedure TShader.LoadFromFile(AFileName: string);
var
  ShaderType: TShaderType;
  Name: string;
begin
  for ShaderType := Low(TShaderType) to High(TShaderType) do
  begin
    Name := AFileName + FileExtensions[ShaderType];
    if FileExists(Name) then
      AddShaderFromFile(ShaderType, Name);
  end;
  Link;
end;

procedure TShader.LoadFromResource(AResourceName: string);
var
  ShaderType: TShaderType;
  Name: string;
begin
  for ShaderType := Low(TShaderType) to High(TShaderType) do
  begin
    Name := AResourceName + ResourceExtensions[ShaderType];
    if FindResource(HInstance, @Name[1], RT_RCDATA) <> 0 then
      AddShaderFromResource(ShaderType, Name);
  end;
  Link;
end;

procedure TShader.LoadUniformLocations;
var
  UniformCount, MaxLength, ActualLength, Size, I: Integer;
  Buffer: AnsiString;
  GLDataType: GLenum;
  DataType: TGLDataType;
  Uniform: TShaderUniformBase;
begin
  glGetProgramiv(ProgramObject, GL_ACTIVE_UNIFORMS, @UniformCount);
  if UniformCount = 0 then
    Exit;
  glGetProgramiv(ProgramObject, GL_ACTIVE_UNIFORM_MAX_LENGTH, @MaxLength);
  for I := 0 to UniformCount - 1 do
  begin
    SetLength(Buffer, MaxLength);
    glGetActiveUniform(ProgramObject, I, MaxLength, ActualLength, Size, GLDataType, @Buffer[1]);
    SetLength(Buffer, ActualLength);
    DataType := TGLDataType(GLDataType);
    case DataType of
      dtInt: Uniform := TShaderUniform<Integer>.Create(Self, Buffer, DataType, Size);
      dtUInt: Uniform := TShaderUniform<Cardinal>.Create(Self, Buffer, DataType, Size);
      dtFloat: Uniform := TShaderUniform<Single>.Create(Self, Buffer, DataType, Size);
      dtVec2: Uniform := TShaderUniform<TVector2>.Create(Self, Buffer, DataType, Size);
      dtVec3: Uniform := TShaderUniform<TVector3>.Create(Self, Buffer, DataType, Size);
      dtVec4: Uniform := TShaderUniform<TVector4>.Create(Self, Buffer, DataType, Size);
      dtBoolean: Uniform := TShaderUniform<Boolean>.Create(Self, Buffer, DataType, Size);
      dtMat2: Uniform := TShaderUniform<TMatrix2>.Create(Self, Buffer, DataType, Size);
      dtMat3: Uniform := TShaderUniform<TMatrix3>.Create(Self, Buffer, DataType, Size);
      dtMat4: Uniform := TShaderUniform<TMatrix4>.Create(Self, Buffer, DataType, Size);
    else
      if GLDataTypeIsSampler(DataType) then
        Uniform := TShaderUniformSampler.Create(Self, Buffer, DataType, Size)
      else
        raise EUniformDataTypeUnsupported.Create(DataType);
    end;
    FInterfaceVariables[Buffer] := Uniform;
  end;
end;

procedure TShader.SetAttributeOrder(AAttributes: TShaderAttributeOrder);
var
  Attribute: AnsiString;
  InterfaceVar: TShaderVariable;
begin
  if FAttributes <> nil then
    raise EAttribOrderSetAlready.Create;

  FAttributes := TShaderAttributes.Create;
  FAttributesReader := TShaderAttributesReader.Create(FAttributes);

  FAttributeStride := 0;
  for Attribute in AAttributes do
  begin
    if FInterfaceVariables.Get(Attribute, InterfaceVar) then
    begin
      if InterfaceVar is TShaderAttribute then
      begin
        FAttributes.Add(TShaderAttribute(InterfaceVar));
        TShaderAttribute(InterfaceVar).FOffset := FAttributeStride;
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

{ TShaderVariable }

constructor TShaderVariable.Create(AShader: TShader; AName: AnsiString; ADataType: TGLDataType;
  ACount: Integer);
begin
  FShader := AShader;
  FName := AName;
  FDataType := ADataType;
  FCount := ACount;
  FDataSize := GLDataTypeSize(DataType) * Count;
  FBaseDataType := GLBaseDataType(FDataType);
  Assert(GLDataTypeSize(FDataType) mod GLDataTypeSize(TGLDataType(FBaseDataType)) = 0);
  FBaseCount := GLDataTypeSize(FDataType) div GLDataTypeSize(TGLDataType(FBaseDataType));
  FLocation := GetLocation; 
end;

constructor TShaderVariable.Create;
begin
  FLocation := -1;
end;

function TShaderVariable.GetActive: Boolean;
begin
  Result := Location <> -1;
end;

{ TShaderUniformBase }

function TShaderUniformBase.GetLocation: Integer;
begin
  Result := glGetUniformLocation(Shader.ProgramObject, @Name[1]);
end;

{ EUniformWrongDataType }

constructor EUniformWrongDataType.Create(AUniform: AnsiString; AExpected, AGot: string);
begin
  inherited CreateFmt('Uniform "%s" got accessed as "%s" but is "%s"', [AUniform, AExpected, AGot]);
end;

{ TShaderUniform<T> }

function TShaderUniform<T>.GetValue: T;
var
  BoolHelp: Integer;
begin
  if not Active then
    raise EGetInactiveUniform.Create(Name);
  case GLBaseDataType(DataType) of
    bdtInt:
      if DataType = dtBoolean then
      begin
        glGetUniformiv(Shader.ProgramObject, Location, @BoolHelp);
        PBoolean(@Result)^ := BoolHelp <> 0;
      end
      else
        glGetUniformiv(Shader.ProgramObject, Location, PGLint(@Result));
    bdtUInt:
      glGetUniformuiv(Shader.ProgramObject, Location, PGLuint(@Result));
    bdtFloat:
      glGetUniformfv(Shader.ProgramObject, Location, PGLfloat(@Result));
    bdtDouble:
      glGetUniformdv(Shader.ProgramObject, Location, PGLdouble(@Result));
  else
    Assert(False, 'Missing BaseDataType for Uniform GetValue');
  end;
end;

procedure TShaderUniform<T>.SetValue(const Value: T);
var
  BoolHelp: Cardinal;
begin
  if not Active then
    Exit;
  Shader.Enable;
  case DataType of
    dtFloat: glUniform1fv(Location, Count, PGLfloat(@Value));
    dtVec2: glUniform2fv(Location, Count, PGLfloat(@Value));
    dtVec3: glUniform3fv(Location, Count, PGLfloat(@Value));
    dtVec4: glUniform4fv(Location, Count, PGLfloat(@Value));

    dtMat2: glUniformMatrix2fv(Location, Count, False, PGLfloat(@Value));
    dtMat3: glUniformMatrix3fv(Location, Count, False, PGLfloat(@Value));
    dtMat4: glUniformMatrix4fv(Location, Count, False, PGLfloat(@Value));

    dtMat2x3: glUniformMatrix2x3fv(Location, Count, False, PGLfloat(@Value));
    dtMat2x4: glUniformMatrix2x4fv(Location, Count, False, PGLfloat(@Value));
    dtMat3x2: glUniformMatrix3x2fv(Location, Count, False, PGLfloat(@Value));
    dtMat3x4: glUniformMatrix3x4fv(Location, Count, False, PGLfloat(@Value));
    dtMat4x2: glUniformMatrix4x2fv(Location, Count, False, PGLfloat(@Value));
    dtMat4x3: glUniformMatrix4x3fv(Location, Count, False, PGLfloat(@Value));

    dtDouble: glUniform1dv(Location, Count, PGLdouble(@Value));
    dtDVec2: glUniform2dv(Location, Count, PGLdouble(@Value));
    dtDVec3: glUniform3dv(Location, Count, PGLdouble(@Value));
    dtDVec4: glUniform4dv(Location, Count, PGLdouble(@Value));

    dtDMat2: glUniformMatrix2dv(Location, Count, False, PGLdouble(@Value));
    dtDMat3: glUniformMatrix3dv(Location, Count, False, PGLdouble(@Value));
    dtDMat4: glUniformMatrix4dv(Location, Count, False, PGLdouble(@Value));

    dtDMat2x3: glUniformMatrix2x3dv(Location, Count, False, PGLdouble(@Value));
    dtDMat2x4: glUniformMatrix2x4dv(Location, Count, False, PGLdouble(@Value));
    dtDMat3x2: glUniformMatrix3x2dv(Location, Count, False, PGLdouble(@Value));
    dtDMat3x4: glUniformMatrix3x4dv(Location, Count, False, PGLdouble(@Value));
    dtDMat4x2: glUniformMatrix4x2dv(Location, Count, False, PGLdouble(@Value));
    dtDMat4x3: glUniformMatrix4x3dv(Location, Count, False, PGLdouble(@Value));

    dtInt: glUniform1iv(Location, Count, PGLint(@Value));
    dtIVec2: glUniform2iv(Location, Count, PGLint(@Value));
    dtIVec3: glUniform3iv(Location, Count, PGLint(@Value));
    dtIVec4: glUniform4iv(Location, Count, PGLint(@Value));

    dtBVec2: glUniform2iv(Location, Count, PGLint(@Value));
    dtBVec3: glUniform3iv(Location, Count, PGLint(@Value));
    dtBVec4: glUniform4iv(Location, Count, PGLint(@Value));

    dtUInt: glUniform1uiv(Location, Count, PGLuint(@Value));
    dtUVec2: glUniform2uiv(Location, Count, PGLuint(@Value));
    dtUVec3: glUniform3uiv(Location, Count, PGLuint(@Value));
    dtUVec4: glUniform4uiv(Location, Count, PGLuint(@Value));

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
  CreateFmt('Vertex Attribute "%s" is not used', [AAttribute]);
end;

{ EAttribOrderSetAlready }

constructor EAttribOrderSetAlready.Create;
begin
  inherited Create('The Attribute-Order can not be modified');
end;

{ EUnsupportedDataType }

constructor EUniformDataTypeUnsupported.Create(ADataType: TGLDataType);
begin
  inherited CreateFmt('Uniform Data-Type "%s" is not supported', [GLDataTypeName(ADataType)]);
end;

{ EGetInactiveUniform }

constructor EGetInactiveUniform.Create(AUniform: AnsiString);
begin
  inherited CreateFmt('Cannot get the Uniform "%s" as it does not exist', [AUniform]);
end;

{ EAttribMissing }

constructor EAttribMissing.Create(AAttribute: AnsiString);
begin
  CreateFmt('Vertex Attribute "%s" does not exist or got optimized away', [AAttribute]);
end;

end.
