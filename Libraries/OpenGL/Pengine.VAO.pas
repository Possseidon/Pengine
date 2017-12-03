unit Pengine.VAO;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,

  Pengine.Camera,
  Pengine.GLEnums,
  Pengine.GLObject,
  Pengine.Interfaces,
  Pengine.Collections,
  Pengine.CollectionInterfaces,
  Pengine.Matrix,
  Pengine.Shader,
  Pengine.Vector,
  Pengine.IntMaths;

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

    function GetLocation: TLocation;
    procedure SetLocation(const Value: TLocation);

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

    property Visible: Boolean read GetVisible write SetVisible;
    property Location: TLocation read GetLocation write SetLocation;
    function CullPoints: IIterable<TVector3>; virtual;
    function CullRadius: Single; virtual;
    function RenderableChildren: IIterable<IRenderable>; virtual;

    procedure Render; virtual;
    // procedure Render(const ABounds: TIntBounds1); virtual;

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

  TVAOProxy = class(TRenderable)
  private
    FVisible: Boolean;
    FSourceVAO: TVAO;
    FLocation: TLocation;

    procedure SetVisible(const Value: Boolean);

  protected
    function GetVisible: Boolean; override;

  public
    constructor Create(ASourceVAO: TVAO);
    destructor Destroy; override;

    property Visible: Boolean read GetVisible write SetVisible;

    procedure Render; override;

    property SourceVAO: TVAO read FSourceVAO write FSourceVAO;

  end;

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
    // procedure Render(const ABounds: TIntBounds1); override;

  end;

implementation

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

procedure TVAO.GenAttributes;
var
  I: Integer;
  Attribute: TShader.TAttribute;
begin
  Bind;
  FVBO.Bind;
  for I := 0 to Shader.AttributeCount - 1 do
  begin
    Attribute := Shader.Attributes[I];
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

function TVAO.GetLocation: TLocation;
begin
  Result := nil;
end;

function TVAO.GetMaxSize: Integer;
begin
  Result := FVBO.MaxSize;
end;

function TVAO.GetSize: Integer;
begin
  Result := FVBO.Size;
end;

function TVAO.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TVAO.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TVAO.BeforeRender;
begin
  FShader.Enable;
end;

procedure TVAO.AfterRender;
begin
  // do nothing after rendering at the moment by default
end;

procedure TVAO.GenObject(var AGLName: Cardinal);
begin
  glGenVertexArrays(1, @AGLName);
end;

procedure TVAO.DeleteObject(var AGLName: Cardinal);
begin
  glDeleteVertexArrays(1, @AGLName);
end;

function TVAO.GetObjectType: TGLObjectType;
begin
  Result := otVertexArray;
end;

procedure TVAO.SetGLLabel(const Value: AnsiString);
begin
  inherited;
  FVBO.GLLabel := Value + ' [VBO]';
end;

procedure TVAO.SetLocation(const Value: TLocation);
begin
  Location.Assign(Value);
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

destructor TVAO.Destroy;
begin
  FVBO.Free;
  inherited;
end;

procedure TVAO.Bind;
begin
  if BoundVAO <> Self then
  begin
    glBindVertexArray(GLName);
    BoundVAO := Self;
  end;
end;

class procedure TVAO.Unbind;
begin
  glBindVertexArray(0);
  BoundVAO := nil;
end;

function TVAO.CullPoints: IIterable<TVector3>;
begin
  Result := nil;
end;

function TVAO.CullRadius: Single;
begin
  Result := Infinity;
end;

function TVAO.RenderableChildren: IIterable<IRenderable>;
begin
  Result := nil;
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

procedure TVAO.Generate(AMaxSize: Integer; AUsage: TGLBufferUsage);
begin
  FVBO.Generate(AMaxSize, AUsage);
end;

procedure TVAO.Generate(const AData; ADataSize: Integer; AUsage: TGLBufferUsage);
begin
  FVBO.Generate(AData, ADataSize, AUsage);
end;

procedure TVAO.Map(AAccess: TGLBufferAccess);
begin
  FVBO.Map(AAccess);
end;

procedure TVAO.Unmap;
begin
  FVBO.Unmap;
end;

procedure TVAO.AddVertex(const AData);
begin
  FVBO.AddVertex(AData);
end;

procedure TVAO.AddVertices(const AData; ACount: Integer);
begin
  FVBO.AddVertices(AData, ACount);
end;

procedure TVAO.SubVertex(const AData; AIndex: Integer);
begin
  FVBO.SubVertex(AData, AIndex);
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

{ TAutoUpdateVAO }

procedure TAutoUpdateVAO.Build;
begin
  if not CheckForChanges then
    Exit;
  BuildVAO;
  FChanged := False;
end;

function TAutoUpdateVAO.CheckForChanges: Boolean;
begin
  Result := FChanged;
end;

procedure TAutoUpdateVAO.NotifyChanges;
begin
  FChanged := True;
end;

procedure TAutoUpdateVAO.Render;
begin
  Build;
  inherited;
end;

{ TVAOProxy }

constructor TVAOProxy.Create(ASourceVAO: TVAO);
begin
  FVisible := True;
  FSourceVAO := ASourceVAO;
  FLocation := TLocation.Create;
end;

destructor TVAOProxy.Destroy;
begin
  FLocation.Free;
  inherited;
end;

procedure TVAOProxy.SetVisible(const Value: Boolean);
begin
  if Value = FVisible then
    Exit;
  FVisible := Value;
end;

function TVAOProxy.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TVAOProxy.Render;
begin
  FSourceVAO.Render;
end;

end.
