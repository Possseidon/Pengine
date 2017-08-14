unit VAOManager;

interface

uses
  dglOpenGL, GLEnums, SysUtils, Shaders, Matrix, VectorGeometry, GLObjectBase, Camera, IntfBase;

type

  { EVBOFull }

  EVBOFull = class(Exception)
  public
    constructor Create;
  end;

  { EVBONotEnoughSpace }

  EVBONotEnoughSpace = class(Exception)
  public
    constructor Create(AMissing: Cardinal);
  end;

  { EVBOOutOfRange }

  EVBOOutOfRange = class(Exception)
  public
    constructor Create(AIndex, AMax: Cardinal);
  end;

  { EVBOAttribOutOfRange }

  EVBOAttribOutOfRange = class(Exception)
  public
    constructor Create(AIndex, AMax: Cardinal);
  end;

  { EVBONotMapped }

  EVBONotMapped = class(Exception)
  public
    constructor Create;
  end;

  { EVBOUnmappable }

  EVBOUnmappable = class(Exception)
  public
    constructor Create;
  end;

  EVBOBindLock = class(Exception)
  public
    constructor Create;
  end;

  { TVBO }

  TVBO = class(TGLObject)
  private
    FMappedData: PByte;
    FMappedOffset: Integer;

    FStride: Integer;
    FSize: Integer;
    FMaxSize: Integer;

    class var
      BoundVBO: TVBO;
      BindLock: Boolean;

  protected
    function GetObjectType: TGLObjectType; override;
    procedure GenObject(var AGLName: Cardinal); override;
    procedure DeleteObject(var AGLName: Cardinal); override;

  public
    constructor Create(AStride: Integer);

    procedure Bind; override;
    class procedure Unbind; override;

    procedure Generate(AMaxSize: Integer; AUsage: TGLBufferUsage); overload;
    procedure Generate(const AData; ADataSize: Integer; AUsage: TGLBufferUsage); overload;

    property MaxSize: Integer read FMaxSize;
    property Size: Integer read FSize;

     // Mapped Access
    procedure Map(AAccess: TGLBufferAccess);
    procedure Unmap;

    procedure AddVertex(const AData);
    procedure AddVertices(const AData; ACount: Integer);

    // Direct Access
    procedure SubVertex(const AData; AIndex: Integer);
  end;

  { TVAO }

  TVAO = class(TGLObject, IRenderable)
  private
    FVBO: TVBO;
    FBeginMode: TGLBeginMode;

    FShader: TShader;

    FVisible: Boolean;

    class var
      BoundVAO: TVAO;

    procedure GenAttributes;
    function GetMaxSize: Integer; inline;
    function GetSize: Integer; inline;

    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);

  protected

    procedure BeforeRender; virtual;
    procedure AfterRender; virtual;

    procedure GenObject(var AGLName: Cardinal); override;
    procedure DeleteObject(var AGLName: Cardinal); override;
    function GetObjectType: TGLObjectType; override;
    procedure SetGLLabel(const Value: AnsiString); override;

  public
    constructor Create(AShader: TShader; ABeginMode: TGLBeginMode = rmTriangles);
    destructor Destroy; override;

    procedure Bind; override;
    class procedure Unbind; override;

    procedure Render; virtual;
    procedure RenderTo(ACount: Integer); virtual;
    procedure RenderFrom(AFirst: Integer); virtual;
    procedure RenderFromTo(AFirst, ACount: Integer); virtual;

    property Visible: Boolean read GetVisible write SetVisible;
    function ModelMatrix: TMatrix4; virtual;
    function Bounds: TBounds3; virtual;
    function HasBounds: Boolean; virtual;

    property Shader: TShader read FShader;

    // --- VBO ---
    procedure Generate(AMaxSize: Integer; AUsage: TGLBufferUsage); overload; inline;
    procedure Generate(const AData; ADataSize: Integer; AUsage: TGLBufferUsage); overload; inline;

    property MaxSize: Integer read GetMaxSize;
    property Size: Integer read GetSize;

    // Mapped Access
    procedure Map(AAccess: TGLBufferAccess); inline;
    procedure Unmap; inline;

    procedure AddVertex(const AData); inline;
    procedure AddVertices(const AData; ACount: Integer); inline;

    // Direct Access
    procedure SubVertex(const AData; AIndex: Integer); inline;

  end;

  { TVAOProxy }

  TVAOProxy = class(TInterfaceBase, IRenderable)
  private
    FSourceVAO: TVAO;
    FLocation: TLocation;

  public
    constructor Create(ASourceVAO: TVAO);
    destructor Destroy; override;

    function GetVisible: Boolean;
    function Bounds: TBounds3;
    function HasBounds: Boolean;
    function ModelMatrix: TMatrix4;
    procedure Render;

    property Location: TLocation read FLocation;

    property SourceVAO: TVAO read FSourceVAO write FSourceVAO;

  end;

  { TAutoUpdateVAO }
  // Automatically calls BuildVAO if NotifyChanges got called or CheckForChanges returns true
  TAutoUpdateVAO = class abstract(TVAO)
  private
    FChanged: Boolean;
    procedure Build;
  protected
    procedure BuildVAO; virtual; abstract;
    function CheckForChanges: Boolean; virtual;
  public
    procedure NotifyChanges;

    procedure Render; override;
    procedure RenderTo(ACount: Integer); override;
    procedure RenderFrom(AFirst: Integer); override;
    procedure RenderFromTo(AFirst, ACount: Integer); override;
  end;

implementation

{ TAutoUpdateVAO }

procedure TAutoUpdateVAO.Build;
begin
  if not CheckForChanges then
    Exit;
  BuildVAO;
  FChanged := False;
end;

procedure TAutoUpdateVAO.NotifyChanges;
begin
  FChanged := True;
end;

function TAutoUpdateVAO.CheckForChanges: Boolean;
begin
  Result := FChanged;
end;

procedure TAutoUpdateVAO.Render;
begin
  Build;
  inherited Render;
end;

procedure TAutoUpdateVAO.RenderTo(ACount: Integer);
begin
  Build;
  inherited RenderTo(ACount);
end;

procedure TAutoUpdateVAO.RenderFrom(AFirst: Integer);
begin
  Build;
  inherited RenderFrom(AFirst);
end;

procedure TAutoUpdateVAO.RenderFromTo(AFirst, ACount: Integer);
begin
  Build;
  inherited RenderFromTo(AFirst, ACount);
end;

{ EVBOUnmappable }

constructor EVBOUnmappable.Create;
begin
  inherited Create('VBO mapping returned nil! Generated? Size 0? Not enough RAM? Attributes? Mapped twice?');
end;

{ EVBONotMapped }

constructor EVBONotMapped.Create;
begin
  inherited Create('Can''t write to VBO. Not mapped to client');
end;

{ EVBOAttribOutOfRange }

constructor EVBOAttribOutOfRange.Create(AIndex, AMax: Cardinal);
begin
  inherited CreateFmt('VBO-Buffer attribute index %d out of range. Attributes: %d', [AIndex, AMax]);
end;

{ EVBOOutOfRange }

constructor EVBOOutOfRange.Create(AIndex, AMax: Cardinal);
begin
  inherited CreateFmt('VBO-Buffer index %d out of range. Max: %d', [AIndex, AMax]);
end;

{ EVBONotEnoughSpace }

constructor EVBONotEnoughSpace.Create(AMissing: Cardinal);
begin
  inherited CreateFmt('Not enough space in VBO-Buffer. Space for %d more vertices required', [AMissing]);
end;

{ EVBOFull }

constructor EVBOFull.Create;
begin
  inherited Create('VBO-Buffer is full, can''t add more vertices!');
end;

{ TVAO }

procedure TVAO.Bind;
begin
  if BoundVAO <> Self then
  begin
    glBindVertexArray(GLName);
    BoundVAO := Self;
  end;
end;

function TVAO.Bounds: TBounds3;
begin
  Result := 0;
end;

procedure TVAO.BeforeRender;
begin
  FShader.Enable;
end;

procedure TVAO.AddVertex(const AData);
begin
  FVBO.AddVertex(AData);
end;

procedure TVAO.AddVertices(const AData; ACount: Integer);
begin
  FVBO.AddVertices(AData, ACount);
end;

procedure TVAO.AfterRender;
begin
  // do nothing after rendering at the moment by default
end;

constructor TVAO.Create(AShader: TShader; ABeginMode: TGLBeginMode);
begin
  inherited Create;
  FShader := AShader;
  FBeginMode := ABeginMode;
  FVBO := TVBO.Create(FShader.AttributeStride);
  GenAttributes;
  FVisible := True;
end;

procedure TVAO.DeleteObject(var AGLName: Cardinal);
begin
  glDeleteVertexArrays(1, @AGLName);
end;

destructor TVAO.Destroy;
begin
  FVBO.Free;
  inherited;
end;

procedure TVAO.GenObject(var AGLName: Cardinal);
begin
  glGenVertexArrays(1, @AGLName);
end;

procedure TVAO.GenAttributes;
var
  Attribute: TShaderAttribute;
begin
  Bind;
  FVBO.Bind;
  for Attribute in Shader.Attributes do
  begin
    if Attribute.Location <> -1 then
    begin
      glEnableVertexAttribArray(Attribute.Location);
      glVertexAttribPointer(
        Attribute.Location,
        Attribute.BaseCount,
        Ord(Attribute.BaseDataType),
        True,
        Shader.AttributeStride,
        Pointer(Attribute.Offset));
    end;
  end;
end;

procedure TVAO.Generate(const AData; ADataSize: Integer; AUsage: TGLBufferUsage);
begin
  FVBO.Generate(AData, ADataSize, AUsage);
end;

procedure TVAO.Generate(AMaxSize: Integer; AUsage: TGLBufferUsage);
begin
  FVBO.Generate(AMaxSize, AUsage);
end;

function TVAO.GetMaxSize: Integer;
begin
  Result := FVBO.MaxSize;
end;

function TVAO.GetObjectType: TGLObjectType;
begin
  Result := otVertexArray;
end;

function TVAO.GetSize: Integer;
begin
  Result := FVBO.Size;
end;

function TVAO.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TVAO.HasBounds: Boolean;
begin
  Result := False;
end;

procedure TVAO.Map(AAccess: TGLBufferAccess);
begin
  FVBO.Map(AAccess);
end;

function TVAO.ModelMatrix: TMatrix4;
begin
  Result.LoadIdentity;
end;

procedure TVAO.Render;
begin
  if (FVBO.Size = 0) or not Visible then
    Exit;
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), 0, FVBO.Size);
  AfterRender;
end;

procedure TVAO.RenderFrom(AFirst: Integer);
begin
  if (FVBO.Size = 0) or (Visible = False) then
    Exit;
  if AFirst >= FVBO.Size then
    raise EVBOOutOfRange.Create(AFirst, FVBO.MaxSize);
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), AFirst, FVBO.Size);
  AfterRender;
end;

procedure TVAO.RenderFromTo(AFirst, ACount: Integer);
begin
  if (FVBO.Size = 0) or (Visible = False) then
    Exit;
  if AFirst >= FVBO.MaxSize then
    raise EVBOOutOfRange.Create(AFirst, FVBO.MaxSize);
  if AFirst + ACount > FVBO.MaxSize then
    raise EVBOOutOfRange.Create(AFirst + ACount, FVBO.MaxSize);
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), AFirst, ACount);
  AfterRender;
end;

procedure TVAO.RenderTo(ACount: Integer);
begin
  if (FVBO.Size = 0) or (Visible = False) then
    Exit;
  if ACount >= FVBO.Size then
    raise EVBOOutOfRange.Create(ACount, FVBO.MaxSize);
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), 0, ACount);
  AfterRender;
end;

procedure TVAO.SetGLLabel(const Value: AnsiString);
begin
  inherited;
  FVBO.GLLabel := Value + ' [VBO]';
end;

procedure TVAO.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TVAO.SubVertex(const AData; AIndex: Integer);
begin
  FVBO.SubVertex(AData, AIndex);
end;

class procedure TVAO.Unbind;
begin
  glBindVertexArray(0);
  BoundVAO := nil;
end;

procedure TVAO.Unmap;
begin
  FVBO.Unmap;
end;

{ TVBO }

procedure TVBO.AddVertex(const AData);
begin
  if FMappedData = nil then
    raise EVBONotMapped.Create;
  if FSize >= FMaxSize then
    raise EVBOFull.Create;

  Move(AData, (FMappedData + FMappedOffset)^, FStride);
  Inc(FMappedOffset, FStride);
  Inc(FSize);
end;

procedure TVBO.AddVertices(const AData; ACount: Integer);
begin
  if FMappedData = nil then
    raise EVBONotMapped.Create;
  if FSize + ACount > FMaxSize then
    raise EVBONotEnoughSpace.Create(FSize + ACount - FMaxSize);

  Move(AData, (FMappedData + FMappedOffset)^, FStride * ACount);
  Inc(FMappedData, FStride * ACount);
  Inc(FSize, ACount);
end;

procedure TVBO.Bind;
begin
  if Self <> BoundVBO then
  begin
    if BindLock then
      raise EVBOBindLock.Create;
    glBindBuffer(Ord(btArrayBuffer), GLName);
    BoundVBO := Self;
  end;
end;

constructor TVBO.Create(AStride: Integer);
begin
  inherited Create;
  FStride := AStride;
end;

procedure TVBO.DeleteObject(var AGLName: Cardinal);
begin
  glDeleteBuffers(1, @AGLName);
end;

procedure TVBO.Generate(const AData; ADataSize: Integer; AUsage: TGLBufferUsage);
begin
  FMaxSize := ADataSize;
  FSize := ADataSize;
  if FMaxSize = 0 then
    Exit;
  Bind;
  glBufferData(Ord(btArrayBuffer), ADataSize * FStride, @AData, Ord(AUsage));
end;

procedure TVBO.Generate(AMaxSize: Integer; AUsage: TGLBufferUsage);
begin
  FMaxSize := AMaxSize;
  FSize := 0;
  if FMaxSize = 0 then
    Exit;
  Bind;
  glBufferData(Ord(btArrayBuffer), AMaxSize * FStride, nil, Ord(AUsage));
end;

procedure TVBO.GenObject(var AGLName: Cardinal);
begin
  glGenBuffers(1, @AGLName);
end;

function TVBO.GetObjectType: TGLObjectType;
begin
  Result := otBuffer;
end;

procedure TVBO.Map(AAccess: TGLBufferAccess);
begin
  if AAccess = baWriteOnly then
    FSize := 0;
  if FMaxSize = 0 then
    Exit;
  Bind;
  BindLock := True;
  FMappedData := glMapBuffer(Ord(btArrayBuffer), Ord(AAccess));
  if FMappedData = nil then
    raise EVBOUnmappable.Create;
  FMappedOffset := 0;
end;

procedure TVBO.SubVertex(const AData; AIndex: Integer);
begin
  Bind;
  glBufferSubData(Ord(btArrayBuffer), AIndex * FStride, FStride, @AData);
end;

class procedure TVBO.Unbind;
begin
  BoundVBO := nil;
  glBindBuffer(Ord(btArrayBuffer), 0);
end;

procedure TVBO.Unmap;
begin
  if FMaxSize = 0 then
    Exit;
  glUnmapBuffer(Ord(btArrayBuffer));
  FMappedData := nil;
  BindLock := False;
end;

{ EVBOBindLock }

constructor EVBOBindLock.Create;
begin
  inherited Create('A VBO must stay bound as long as it is mapped');
end;

{ TVAOProxy }

function TVAOProxy.Bounds: TBounds3;
begin
  Result := 0;
end;

constructor TVAOProxy.Create(ASourceVAO: TVAO);
begin
  FSourceVAO := ASourceVAO;
  FLocation := TLocation.Create;
end;

destructor TVAOProxy.Destroy;
begin
  FLocation.Free;
  inherited;
end;

function TVAOProxy.GetVisible: Boolean;
begin
  Result := True;
end;

function TVAOProxy.HasBounds: Boolean;
begin
  Result := False;
end;

function TVAOProxy.ModelMatrix: TMatrix4;
begin
  Result := Location.Matrix;
end;

procedure TVAOProxy.Render;
begin
  FSourceVAO.Render;
end;

end.
