unit Shaders;

interface

uses
  dglOpenGL, Dialogs, Classes, SysUtils, GLEnums, Controls, Forms, Lists
  {$IFDEF FPC}
  , Windows // for RT_RCDATA
  {$ELSE}
  , Types
  , UITypes
  {$ENDIF}
  ;

type

  { EAttribNotFound }

  EAttribNotFound = class (Exception)
    constructor Create(Attribute: PAnsiChar);
  end;

  { EUniformNotFound }

  EUniformNotFound = class (Exception)
    constructor Create(Uniform: PAnsiChar);
  end;

  { EFileEmpty }

  EFileEmpty = class (Exception)
    constructor Create(AFileName: String);
  end;

  { TShader }

  TShader = class
  public
    type

      { TAttribute }

      TAttribute = class
      private
        FCount: Cardinal;
        FName: AnsiString;
        FDataType: TGLDataType;

      public
        constructor Create(ACount: Cardinal; AName: AnsiString; ADataType: TGLDataType);

        property Count: Cardinal read FCount;
        property Name: AnsiString read FName;
        property DataType: TGLDataType read FDataType;

      end;

  private
    FProgramObject: Integer;
    FLocations: TAnsiStringMap<Integer>;
    FAttributes: TObjectArray<TAttribute>;

    function CheckShaderErrors(AShaderName: String; AShader: GLHandle): Boolean;
    function CheckProgramErrors: Boolean;
    function GetAtributeCount: Integer;

    function GetAttribute(I: Integer): TAttribute;

    function GetAttribLocation(AName: PAnsiChar): Integer;
    function GetFragDataLocation(AName: PAnsiChar): Integer;
    function GetUniformLocation(AName: PAnsiChar): Integer;

    class var FActiveShader: TShader;

  public
    constructor Create;
    destructor Destroy; override;

    function AddShaderFromFile(ShaderType: TShaderType; Filename: String): Boolean;
    function AddShaderFromResource(ShaderType: TShaderType; ResourceName: String): Boolean;
    function Link: Boolean;

    function VertexFragmentShader(AName: String): Boolean;
    function VertexFragmentShaderFromResource(AName: String): Boolean;

    procedure AddAttribute(ACount: Cardinal; AName: AnsiString; ADataType: TGLDataType = dtFloat);
    property Attributes[I: Integer]: TAttribute read GetAttribute;
    property AttributeCount: Integer read GetAtributeCount;

    property UniformLocation[AName: PAnsiChar]: Integer read GetUniformLocation;
    property AttribLocation[AName: PAnsiChar]: Integer read GetAttribLocation;
    property FragDataLocation[AName: PAnsiChar]: Integer read GetFragDataLocation;

    procedure Enable;
    class procedure Disable;

    property ProgramObject: Integer read FProgramObject;

    class property ActiveShader: TShader read FActiveShader;

    procedure UniformFloat(ALocation: Integer; AValue: Single); overload;
    procedure UniformFloat(AName: PAnsiChar; AValue: Single); overload;

    procedure UniformInt(ALocation: Integer; AValue: Integer); overload;
    procedure UniformInt(AName: PAnsiChar; AValue: Integer); overload;

    procedure UniformBool(ALocation: Integer; AValue: Boolean); overload;
    procedure UniformBool(AName: PAnsiChar; AValue: Boolean); overload;

    const
      FragFileExt = '.fs';
      VertFileExt = '.vs';
      GeomFileExt = '.gs';
      CompFileExt = '.cs';

      FragResourceExt = '_FS';
      VertResourceExt = '_VS';
      GeomResourceExt = '_GS';
      CompResourceExt = '_CS';

  end;

implementation

{ EFileEmpty }

constructor EFileEmpty.Create(AFileName: String);
begin
  CreateFmt('"%s" doesn''t have any Data!', [AFileName]);
end;

{ TShader.TAttribute }

constructor TShader.TAttribute.Create(ACount: Cardinal; AName: AnsiString; ADataType: TGLDataType);
begin
  FCount := ACount;
  FName := AName;
  FDataType := ADataType;
end;

{ EUniformNotFound }

constructor EUniformNotFound.Create(Uniform: PAnsiChar);
begin
  CreateFmt('Shader Uniform "%s" could not be found!', [Uniform]);
end;

{ EAttribNotFound }

constructor EAttribNotFound.Create(Attribute: PAnsiChar);
begin
  CreateFmt('Vertex Attribute "%s" could not be found!', [Attribute]);
end;

{ TShader }

function TShader.AddShaderFromFile(ShaderType: TShaderType; Filename: String): Boolean;
  function LoadStringFromFile(Filename: String): AnsiString;
  var
    FS: TFileStream;
  begin
    FS := TFileStream.Create(Filename, fmOpenRead);
    try
      if FS.Size = 0 then
        raise EFileEmpty.Create(Filename);
      SetLength(Result, FS.Size);
      FS.ReadBuffer(Result[1], FS.Size);
    finally
      FS.Free;
      end;
  end;

var
  ShaderObject, ShaderLength: Integer;
  ShaderText: AnsiString;
begin
  ShaderObject := glCreateShader(Ord(ShaderType));

  try
    ShaderText := LoadStringFromFile(Filename);
    ShaderLength := Length(ShaderText);

    glShaderSource(ShaderObject, 1, @ShaderText, @ShaderLength);
    glCompileShader(ShaderObject);

    Result := CheckShaderErrors(ExtractFileName(Filename), ShaderObject);

    if Result then
      glAttachShader(FProgramObject, ShaderObject);

  finally
    glDeleteShader(ShaderObject);
  end;
end;


function TShader.AddShaderFromResource(ShaderType: TShaderType; ResourceName: String): Boolean;

  function LoadStringFromResource(Resourcename: String): AnsiString;
  begin
    with TResourceStream.Create(hInstance, Resourcename, RT_RCDATA) do
    begin
      SetLength(Result, Size);
      ReadBuffer(Result[1], Size);
      Free;
    end;
  end;

var
  ShaderObject, ShaderLength: Integer;
  ShaderText: AnsiString;
begin
  ShaderObject := glCreateShader(Ord(ShaderType));

  try
    ShaderText := LoadStringFromResource(ResourceName);
    ShaderLength := Length(ShaderText);

    glShaderSource(ShaderObject, 1, @ShaderText, @ShaderLength);
    glCompileShader(ShaderObject);

    Result := CheckShaderErrors(ResourceName, ShaderObject);

    if Result then
      glAttachShader(FProgramObject, ShaderObject);

  finally
    glDeleteShader(ShaderObject);
  end;

end;

function TShader.GetAttribLocation(AName: PAnsiChar): Integer;
begin
  if not FLocations.Get(AName, Result) then
  begin
    Result := glGetAttribLocation(FProgramObject, AName);
    FLocations[AName] := Result;
  end;
end;

function TShader.GetAttribute(I: Integer): TAttribute;
begin
  Result := FAttributes[I];
end;

function TShader.GetFragDataLocation(AName: PAnsiChar): Integer;
begin
  if not FLocations.Get(AName, Result) then
  begin
    Result := glGetFragDataLocation(FProgramObject, AName);
    FLocations[AName] := Result;
  end;
end;

function TShader.CheckShaderErrors(AShaderName: String; AShader: GLHandle): Boolean;
var
  blen, slen: Integer;
  InfoLog: AnsiString;
  Success: Integer;
begin
  glGetShaderiv(AShader, GL_COMPILE_STATUS, @Success);
  if Success <> 0 then
    Exit(True);
  glGetShaderiv(AShader, GL_INFO_LOG_LENGTH, @blen);
  if blen <= 1 then
    Exit(True);
  Result := False;
  SetLength(InfoLog, blen);
  glGetShaderInfoLog(AShader, blen, slen, @InfoLog[1]);
  MessageDlg(AShaderName + ':' + sLineBreak + InfoLog, mtError, [mbOK], 0);
end;

function TShader.CheckProgramErrors: Boolean;
const
  ErrorFmt = 'Linking: %s';
var
  blen: GLInt;
  slen: PGLsizei;
  InfoLog: AnsiString;
  Success: Integer;
begin
  glGetProgramiv(FProgramObject, GL_LINK_STATUS, @Success);
  if Success <> 0 then
    Exit(True);
  glGetProgramiv(FProgramObject, GL_INFO_LOG_LENGTH, @blen);
  if blen <= 1 then
    Exit(True);
  Result := False;
  SetLength(InfoLog, blen);
  glGetProgramInfoLog(FProgramObject, blen, slen, @InfoLog[1]);
  MessageDlg(Format(ErrorFmt, [sLineBreak + InfoLog]), mtError, [mbOK], 0);
end;

function TShader.GetAtributeCount: Integer;
begin
  Result := FAttributes.Count;
end;

constructor TShader.Create;
begin
  FProgramObject := glCreateProgram;
  FLocations := TAnsiStringMap<Integer>.Create;
  FAttributes := TObjectArray<TAttribute>.Create;
end;

destructor TShader.Destroy;
begin
  FLocations.Free;
  FAttributes.Free;
  glDeleteProgram(FProgramObject);
  inherited;
end;

class procedure TShader.Disable;
begin
  glUseProgram(0);
end;

procedure TShader.UniformFloat(ALocation: Integer; AValue: Single);
begin
  Enable;
  glUniform1f(ALocation, AValue);
end;

procedure TShader.UniformFloat(AName: PAnsiChar; AValue: Single);
begin
  UniformFloat(UniformLocation[AName], AValue);
end;

procedure TShader.UniformInt(ALocation: Integer; AValue: Integer);
begin
  Enable;
  glUniform1i(ALocation, AValue);
end;

procedure TShader.UniformInt(AName: PAnsiChar; AValue: Integer);
begin
  UniformInt(UniformLocation[AName], AValue);
end;

procedure TShader.UniformBool(ALocation: Integer; AValue: Boolean);
begin
  Enable;
  glUniform1i(ALocation, Integer(AValue));
end;

procedure TShader.UniformBool(AName: PAnsiChar; AValue: Boolean);
begin
  UniformBool(UniformLocation[AName], AValue);
end;

procedure TShader.Enable;
begin
  if Pointer(FActiveShader) <> Pointer(Self) then
  begin
    glUseProgram(FProgramObject);
    FActiveShader := Self;
  end;
end;

function TShader.Link: Boolean;
begin
  glLinkProgram(FProgramObject);
  Result := CheckProgramErrors;
  if Result then
    Enable;
end;

function TShader.GetUniformLocation(AName: PAnsiChar): Integer;
begin
  if not FLocations.Get(AName, Result) then
  begin
    Result := glGetUniformLocation(FProgramObject, AName);
    FLocations[AName] := Result;
  end;
end;

function TShader.VertexFragmentShader(AName: String): Boolean;
begin
  Result := AddShaderFromFile(stVertex, AName + VertFileExt) and
            AddShaderFromFile(stFragment, AName + FragFileExt) and
            Link;
end;

function TShader.VertexFragmentShaderFromResource(AName: String): Boolean;
begin
  Result := AddShaderFromResource(stVertex, AName + VertResourceExt) and
            AddShaderFromResource(stFragment, AName + FragResourceExt) and
            Link;
end;

procedure TShader.AddAttribute(ACount: Cardinal; AName: AnsiString; ADataType: TGLDataType);
begin
  FAttributes.Add(TAttribute.Create(ACount, AName, ADataType));
end;

end.
