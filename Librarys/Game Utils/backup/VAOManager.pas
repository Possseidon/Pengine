unit VAOManager;

interface

uses
  dglOpenGL, GLEnums, SysUtils, Shaders, Matrix, VectorGeometry;

type

  { EVBOFull }

  EVBOFull = class (Exception)
  public
    constructor Create;
  end;

  { EVBONotEnoughSpace }

  EVBONotEnoughSpace = class (Exception)
  public
    constructor Create(AMissing: Cardinal);
  end;

  { EVBOOutOfRange }

  EVBOOutOfRange = class (Exception)
  public
    constructor Create(AIndex, AMax: Cardinal);
  end;

  { EVBOAttribOutOfRange }

  EVBOAttribOutOfRange = class (Exception)
  public
    constructor Create(AIndex, AMax: Cardinal);
  end;

  { EVBONotMapped }

  EVBONotMapped = class (Exception)
  public
    constructor Create;
  end;

  { EVBOUnmappable }

  EVBOUnmappable = class (Exception)
  public
    constructor Create;
  end;

  { TVAO }

  TVAO = class
  private
    type

      { TAttribute }

      TAttribute = record
        Location: Integer;      // Shader Location
        DataCount: Cardinal;    // DataCount per Vertex
        DataType: TGLDataType;  // float, int, ...
        DataSize: Cardinal;     // DataCount * Size of DataType
        Offset: Cardinal;       // Offset in Data
      end;

  private
    FVAOID, FVBOID: Cardinal; // IDs for VAO and VBO

    FPVBO: PByte; // Start Pointer of Mapped Data
    FVBOPos: Cardinal; // Offset to Start Pointer for Vertex add Procedures
    FBeginMode: TGLBeginMode;

    FSize, FMaxSize: Cardinal; // size for rendering, full size

    FStride: Cardinal; // size per one vertex set

    FAttribs: array of TAttribute;

    FShader: TShader;

    FVisible: Boolean;

    class var
      BoundVAO: TVAO; // remember the last bound VAO for more performance

    procedure GenAttributes;
    function GetAttribCount: Cardinal;

  protected
    property Shader: TShader read FShader;

    function GetModelMatrix: TMatrix4; virtual;
    function GetBounds: TGBounds3; virtual;

    procedure BeforeRender; virtual;
    procedure AfterRender; virtual;
  public
    constructor Create(AShader: TShader; ABeginMode: TGLBeginMode = rmTriangles);
    destructor Destroy; override;

    // VBO Functions
    procedure Generate(AMaxSize: Cardinal; AUsage: TGLBufferUsage); overload;
    procedure Generate(const AData; ADataSize: Cardinal; AUsage: TGLBufferUsage = buStaticDraw); overload;

    procedure Map(AAccess: TGLBufferAccess);
    procedure Unmap;

    procedure Clear; // Resets Size
    procedure AddSize(ACount: Cardinal);
    procedure SetSize(ASize: Cardinal);
    function GetSize: Cardinal;

    procedure AddVertex(const AData);
    procedure AddVertices(const AData; Count: Cardinal);
    procedure SetVertex(const AData; AIndex: Cardinal);
    procedure SetAttribute(const AData; Index, AAttrib: Cardinal);

    procedure SubVertex(const AData; AIndex: Cardinal);
    procedure SubAttribute(const AData; AIndex, AAttrib: Cardinal);

    // VAO Functions
    property AttribCount: Cardinal read GetAttribCount;

    procedure Bind;
    procedure Unbind;

    procedure Render; virtual;
    procedure RenderTo(ACount: Cardinal); virtual;
    procedure RenderFrom(AFirst: Cardinal); virtual;
    procedure RenderFromTo(AFirst, ACount: Cardinal); virtual;

    property Visible: Boolean read FVisible write FVisible;
    property ModelMatrix: TMatrix4 read GetModelMatrix;
    property Bounds: TGBounds3 read GetBounds;
    function HasBounds: Boolean; virtual;

  end;

  { TAutoUpdateVAO }
  // Automatically calls BuildVAO if NotifyChanges got called or CheckForChanges returns true
  TAutoUpdateVAO = class abstract (TVAO)
  private
    FChanged: Boolean;
    procedure Build;
  protected
    procedure BuildVAO; virtual; abstract;
    function CheckForChanges: Boolean; virtual;
  public
    procedure NotifyChanges;

    procedure Render; override;
    procedure RenderTo(ACount: Cardinal); override;
    procedure RenderFrom(AFirst: Cardinal); override;
    procedure RenderFromTo(AFirst, ACount: Cardinal); override;
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

procedure TAutoUpdateVAO.RenderTo(ACount: Cardinal);
begin
  Build;
  inherited RenderTo(ACount);
end;

procedure TAutoUpdateVAO.RenderFrom(AFirst: Cardinal);
begin
  Build;
  inherited RenderFrom(AFirst);
end;

procedure TAutoUpdateVAO.RenderFromTo(AFirst, ACount: Cardinal);
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

procedure TVAO.AddVertex(const AData);
begin
  if FPVBO = nil then
    raise EVBONotMapped.Create;
  if FSize >= FMaxSize then
    raise EVBOFull.Create;

  Move(AData, (FPVBO + FVBOPos)^, FStride);
  Inc(FVBOPos, FStride);
  Inc(FSize);
end;

procedure TVAO.AddVertices(const AData; Count: Cardinal);
begin
  if FPVBO = nil then
    raise EVBONotMapped.Create;
  if FSize + Count > FMaxSize then
    raise EVBONotEnoughSpace.Create(FSize + Count - FMaxSize);

  Move(AData, (FPVBO + FVBOPos)^, FStride * Count);
  Inc(FVBOPos, FStride * Count);
  Inc(FSize, Count);
end;

procedure TVAO.Bind;
begin
  if Pointer(BoundVAO) <> Pointer(Self) then
  begin
    glBindVertexArray(FVAOID);
    BoundVAO := Self;
  end;
end;

procedure TVAO.BeforeRender;
begin
  FShader.Enable;
end;

procedure TVAO.AfterRender;
begin
  // do nothing after rendering at the moment by default
end;

constructor TVAO.Create(AShader: TShader; ABeginMode: TGLBeginMode);
var
  A: TShader.TAttribute;
  I: Integer;
begin
  FShader := AShader;
  glGenVertexArrays(1, @FVAOID);
  glGenBuffers(1, @FVBOID);
  glBindBuffer(Ord(btArrayBuffer), FVBOID);
  FBeginMode := ABeginMode;

  SetLength(FAttribs, FShader.AttributeCount);
  for I := 0 to FShader.AttributeCount - 1 do
  begin
    A := FShader.Attributes[I];
    with FAttribs[I] do
    begin
      Location := FShader.AttribLocation[PAnsiChar(A.Name)];
      DataCount := A.Count;
      DataType := A.DataType;
      DataSize := A.Count * GetDataSize(A.DataType);
      Offset := FStride;
      FStride := FStride + DataSize;
    end;
  end;
  GenAttributes;

  FVisible := True;
end;

destructor TVAO.Destroy;
begin
  glDeleteVertexArrays(1, @FVAOID);
  glDeleteBuffers(1, @FVBOID);
  inherited;
end;

procedure TVAO.Generate(AMaxSize: Cardinal; AUsage: TGLBufferUsage);
begin
  FMaxSize := AMaxSize;
  if FMaxSize = 0 then
    Exit;
  glBindBuffer(Ord(btArrayBuffer), FVBOID);
  glBufferData(Ord(btArrayBuffer), AMaxSize * FStride, nil, Ord(AUsage));
end;

procedure TVAO.Generate(const AData; ADataSize: Cardinal; AUsage: TGLBufferUsage);
begin
  FSize := ADataSize;
  if FMaxSize = 0 then
    Exit;
  glBindBuffer(Ord(btArrayBuffer), FVBOID);
  glBufferData(Ord(btArrayBuffer), ADataSize * FStride, @AData, Ord(AUsage));
end;

procedure TVAO.GenAttributes;
var
  I: Integer;
begin
  Bind;
  for I := 0 to AttribCount - 1 do
  begin
    if FAttribs[I].Location = -1 then
      Continue;
    glEnableVertexAttribArray(FAttribs[I].Location);
    glVertexAttribPointer(
      FAttribs[I].Location,
      FAttribs[I].DataCount,
      Ord(FAttribs[I].DataType),
      ByteBool(Ord(blTrue)),
      FStride,
 {%H-}Pointer(FAttribs[I].Offset));
  end;
  Unbind;
end;

function TVAO.GetAttribCount: Cardinal;
begin
  Result := FShader.AttributeCount;
end;

function TVAO.GetModelMatrix: TMatrix4;
begin
  Result.LoadIdentity;
end;

function TVAO.HasBounds: Boolean;
begin
  Result := False;
end;

function TVAO.GetBounds: TGBounds3;
begin
  Result := TGBounds3.Create(Origin, Origin);
end;

procedure TVAO.Map(AAccess: TGLBufferAccess);
begin
  if AAccess = baWriteOnly then
    FSize := 0;
  if FMaxSize = 0 then
    Exit;
  glBindBuffer(Ord(btArrayBuffer), FVBOID);
  FPVBO := glMapBuffer(Ord(btArrayBuffer), Ord(AAccess));
  if FPVBO = nil then
    raise EVBOUnmappable.Create;
  FVBOPos := 0;
end;

procedure TVAO.Render;
begin
  if (FSize = 0) or not Visible then
    Exit;
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), 0, FSize);
  AfterRender;
end;

procedure TVAO.RenderFrom(AFirst: Cardinal);
begin
  if (FSize = 0) or (Visible = False) then
    Exit;
  if AFirst >= FSize then
    raise EVBOOutOfRange.Create(AFirst, FMaxSize);
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), AFirst, FSize);
  AfterRender;
end;

procedure TVAO.RenderFromTo(AFirst, ACount: Cardinal);
begin
  if (FSize = 0) or (Visible = False) then
    Exit;
  if AFirst >= FMaxSize then
    raise EVBOOutOfRange.Create(AFirst, FMaxSize);
  if AFirst + ACount > FMaxSize then
    raise EVBOOutOfRange.Create(AFirst + ACount, FMaxSize);
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), AFirst, ACount);
  AfterRender;
end;

procedure TVAO.RenderTo(ACount: Cardinal);
begin
  if (FSize = 0) or (Visible = False) then
    Exit;
  if ACount >= FSize then
    raise EVBOOutOfRange.Create(ACount, FMaxSize);
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), 0, ACount);
  AfterRender;
end;

procedure TVAO.SetAttribute(const AData; Index, AAttrib: Cardinal);
begin
  if FPVBO = nil then
    raise EVBONotMapped.Create;
  if Index >= FMaxSize then
    raise EVBOOutOfRange.Create(Index, FMaxSize);
  if AAttrib >= AttribCount then
    raise EVBOAttribOutOfRange.Create(AAttrib, AttribCount);

  Move(AData, (FPVBO + Index * FStride + FAttribs[AAttrib].Offset)^, FAttribs[AAttrib].DataSize);
end;

procedure TVAO.SubVertex(const AData; AIndex: Cardinal);
begin
  glBufferSubData(Ord(btArrayBuffer), AIndex * FStride, FStride, @AData);
end;

procedure TVAO.SubAttribute(const AData; AIndex, AAttrib: Cardinal);
begin
  glBufferSubData(Ord(btArrayBuffer), AIndex * FStride + FAttribs[AAttrib].Offset, FAttribs[AAttrib].DataSize, @AData);
end;

procedure TVAO.SetVertex(const AData; AIndex: Cardinal);
begin
  if FPVBO = nil then
    raise EVBONotMapped.Create;
  if AIndex >= FMaxSize then
    raise EVBOOutOfRange.Create(AIndex, FMaxSize);
  Move(AData, (FPVBO + AIndex * FStride)^, FStride);
end;

procedure TVAO.Unbind;
begin
  glBindVertexArray(0);
  BoundVAO := nil;
end;

procedure TVAO.Unmap;
begin
  if FMaxSize = 0 then
    Exit;
  glUnmapBuffer(Ord(btArrayBuffer));
  glBindBuffer(Ord(btArrayBuffer), 0);
  FPVBO := nil;
end;

procedure TVAO.Clear;
begin
  FSize := 0;
end;

procedure TVAO.AddSize(ACount: Cardinal);
begin
  if FSize + ACount > FMaxSize then
    raise EVBONotEnoughSpace.Create(FSize + ACount - FMaxSize);
  Inc(FVBOPos, FStride * ACount);
  Inc(FSize, ACount);
end;

procedure TVAO.SetSize(ASize: Cardinal);
begin
  if ASize > FMaxSize then
    raise EVBONotEnoughSpace.Create(ASize - FMaxSize);
  FSize := ASize;
end;

function TVAO.GetSize: Cardinal;
begin
  Result := FSize;
end;

end.
