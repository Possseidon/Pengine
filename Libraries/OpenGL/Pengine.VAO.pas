unit Pengine.VAO;

interface

uses
  dglOpenGL,

  System.SysUtils,
  System.Math,

  Pengine.Camera,
  Pengine.GLEnums,
  Pengine.GLState,
  Pengine.Interfaces,
  Pengine.Collections,
  Pengine.CollectionInterfaces,
  Pengine.Matrix,
  Pengine.GLProgram,
  Pengine.Vector,
  Pengine.IntMaths;

type

  EVBOFull = class(Exception)
  public
    constructor Create;
  end;

  EVBONotEnoughSpace = class(Exception)
  public
    constructor Create(AMissing: Cardinal);
  end;

  EVBOOutOfRange = class(Exception)
  public
    constructor Create;
  end;

  EVBOAttribOutOfRange = class(Exception)
  public
    constructor Create;
  end;

  EVBOUnmappable = class(Exception)
  public
    constructor Create;
  end;

  EVBOBindLock = class(Exception)
  public
    constructor Create;
  end;

  /// <summary>An abstract base class for a Vertex Buffer Object. Stores data, which can get rendered via a GLProgram.</summary>
  TVBO = class abstract(TGLObject)
  public type

    TBinding = class(TGLObjectBinding<TVBO>)
    private
      FLockCount: Integer;

    protected
      procedure SetBoundObject(const Value: TVBO); override;

    public
      procedure Lock;
      procedure Unlock;

    end;

  protected
    FCount: Integer;

    procedure GenObject(out AGLName: Cardinal); override;
    procedure DeleteObject(const AGLName: Cardinal); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

    function Binding: TBinding;

    class function GetDataSize: Integer; virtual; abstract;

  public
    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    property Count: Integer read FCount;

  end;

  /// <summary>An abstract class for a specialized Vertex Buffer Object, that stores an array of the given record type.</summary>
  TVBO<TData: record > = class abstract(TVBO)
  public type

    PData = ^TData;

    TMapping = class abstract(TInterfacedObject)
    private
      FVBO: TVBO<TData>;
      FData: PData;

    protected
      function GetData(APos: Integer): TData;
      procedure SetData(APos: Integer; const Value: TData);

      class function GetBufferAccess: TGLBufferAccess; virtual; abstract;

    public
      constructor Create(AVBO: TVBO<TData>);
      destructor Destroy; override;

    end;

    IReader = interface
      function GetData(APos: Integer): TData;
      property Data[APos: Integer]: TData read GetData; default;

    end;

    TReader = class(TMapping, IReader)
    protected
      class function GetBufferAccess: TGLBufferAccess; override;

    public
      property Data[APos: Integer]: TData read GetData; default;

    end;

    IWriter = interface
      function GetPos: Integer;
      procedure SetPos(const Value: Integer);
      function GetBuffer(AOffset: Integer): PData;
      function GetCurrent: PData;

      property Buffer[AOffset: Integer]: PData read GetBuffer; default;
      property Current: PData read GetCurrent;

      property BufferPos: Integer read GetPos write SetPos;
      procedure AddToBuffer(AData: TData);
      procedure NextBufferPos;

    end;

    TWriter = class(TMapping, IWriter)
    public type

      TDataArray = array [0 .. 0] of TData;
      PDataArray = ^TDataArray;

    private
      FPos: Integer;
      FBufferData: PDataArray;

      function GetPos: Integer;
      procedure SetPos(const Value: Integer);
      function GetBuffer(AOffset: Integer): PData;
      function GetCurrent: PData;

    protected
      class function GetBufferAccess: TGLBufferAccess; override;

    public
      constructor Create(AVBO: TVBO<TData>);

      /// <summary>Grants easy and fast access to the buffer at <c>BufferPos</c>.
      /// <p>Default property.</p></summary>
      /// <remarks>/!\ Only allows write access.</remarks>
      property Buffer[AOffset: Integer]: PData read GetBuffer; default;
      property Current: PData read GetCurrent;

      /// <summary>The current position, where Buffer points to.</summary>
      property BufferPos: Integer read GetPos write SetPos;
      /// <summary>Adds a single data segment and advances the BufferPos.</summary>
      procedure AddToBuffer(AData: TData);
      /// <summary>Simply advances the bufferpos by one.</summary>
      procedure NextBufferPos;

    end;

    // TODO: New interface "IReadWriter"
    TReadWriter = class(TMapping)
    protected
      class function GetBufferAccess: TGLBufferAccess; override;

    public
      property Data[APos: Integer]: TData read GetData write SetData; default;

    end;

  private
    function GetSubData(APos: Integer): TData; overload;
    function GetSubData(ABounds: TIntBounds1): TArray<TData>; overload;
    function GetData: TArray<TData>;
    procedure SetSubData(APos: Integer; const Value: TData); overload;
    procedure SetSubData(ABounds: TIntBounds1; const Value: TArray<TData>); overload;
    procedure SetData(const Value: TArray<TData>);

  protected
    class function GetDataSize: Integer; override;

  public
    property SubData[APos: Integer]: TData read GetSubData write SetSubData; default;
    property SubData[ABounds: TIntBounds1]: TArray<TData> read GetSubData write SetSubData; default;
    property Data: TArray<TData> read GetData write SetData;

  end;

  /// <summary>A class for immutable Vertex Buffer Objects. The size is set on construction and immutable for the
  /// lifetime of the object.</summary>
  /// <remarks>Uses <c>glBufferStorage()</c>.</remarks>
  TVBOImmutable<TData: record > = class(TVBO<TData>)
  public
    /// <summary>Create a VBO with a specified buffer size and optional data, to fill it with.</summary>
    constructor Create(AGLState: TGLState; ACount: Integer; AFlags: TGLBufferFlags; AData: Pointer = nil); overload;
    /// <summary>Create a VBO and instantly fill it with the specified data.</summary>
    constructor Create(AGLState: TGLState; AData: TArray<TData>; AFlags: TGLBufferFlags); overload;

  end;

  /// <summary>A class for mutable Vertex Buffer Objects. The size is set using the <c>generate</c> method and can be
  /// changed at any time, if neccessary. It is not possible to read from the buffer.</summary>
  /// <remarks>Uses <c>glBufferData()</c>.</remarks>
  TVBOMutable<TData: record > = class(TVBO<TData>)
  private
    FUsageHint: TGLBufferUsage;

  public
    /// <summary>Generate the buffer store with a specific size and optionally intialize it with data.</summary>
    procedure Generate(ACount: Integer; AUsageHint: TGLBufferUsage; AData: Pointer = nil); overload;
    /// <summary>Gernerate the buffer store and instantly fill it with data from an array.</summary>
    procedure Generate(AData: TArray<TData>; AUsageHint: TGLBufferUsage); overload; inline;

    property UsageHint: TGLBufferUsage read FUsageHint;

    /// <summary>Map the buffer to make changes to the data. Recommended usage, using the <c>with</c> statement.</summary>
    function Map: TVBO<TData>.IWriter;

  end;

  TVAO = class(TGLObject, IRenderable)
  private
    FVBO: TVBO;
    FBeginMode: TGLBeginMode;
    FGLProgram: TGLProgram;
    FVisible: Boolean;

    procedure GenAttributes;

    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);

    function GetLocation: TLocation3;
    procedure SetLocation(const Value: TLocation3);

  protected
    procedure GenObject(out AGLName: GLuint); override;
    procedure DeleteObject(const AGLName: GLuint); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

    procedure SetGLLabel(const Value: AnsiString); override;

    procedure BeforeRender; virtual;
    procedure AfterRender; virtual;

  public
    constructor Create(AGLProgram: TGLProgram; AVBO: TVBO; ABeginMode: TGLBeginMode = rmTriangles);
    destructor Destroy; override;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    property Visible: Boolean read GetVisible write SetVisible;
    property Location: TLocation3 read GetLocation write SetLocation;
    function CullPoints: IIterable<TVector3>; virtual;
    function CullRadius: Single; virtual;
    function RenderableChildren: IIterable<IRenderable>; virtual;

    procedure Render; overload;
    procedure Render(const ABounds: TIntBounds1); overload;

    property GLProgram: TGLProgram read FGLProgram;

    property VBO: TVBO read FVBO;

  end;

  TVAO<T: record > = class(TVAO)
  public type

    TData = T;

    IWriter = TVBO<T>.IWriter;

  private
    function GetVBO: TVBO<T>; inline;

  public
    property VBO: TVBO<T> read GetVBO;

  end;

  TVAOImmutable<T: record > = class(TVAO<T>)
  private
    function GetVBO: TVBOImmutable<T>;

  public
    constructor Create(AGLProgram: TGLProgram; ACount: Integer; AFlags: TGLBufferFlags; AData: Pointer = nil;
      ABeginMode: TGLBeginMode = rmTriangles); overload;
    constructor Create(AGLProgram: TGLProgram; AData: TArray<T>; AFlags: TGLBufferFlags;
      ABeginMode: TGLBeginMode = rmTriangles); overload;

    property VBO: TVBOImmutable<T> read GetVBO;

  end;

  TVAOMutable<T: record > = class(TVAO<T>)
  private
    function GetVBO: TVBOMutable<T>;

  public
    constructor Create(AGLProgram: TGLProgram; ABeginMode: TGLBeginMode = rmTriangles);

    property VBO: TVBOMutable<T> read GetVBO;

  end;

  TVAOProxy = class(TRenderable)
  private
    FVisible: Boolean;
    FSourceVAO: TVAO;
    FLocation: TLocation3;

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
  {
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
    procedure Render(const ABounds: TIntBounds1); override;

    end;
  }
implementation

{ EVBOUnmappable }

constructor EVBOUnmappable.Create;
begin
  inherited Create('VBO mapping returned nil! Generated? Size 0? Not enough RAM? Attributes? Mapped twice?');
end;

{ EVBOAttribOutOfRange }

constructor EVBOAttribOutOfRange.Create;
begin
  inherited Create('VBO-Buffer attribute index out of range.');
end;

{ EVBOOutOfRange }

constructor EVBOOutOfRange.Create;
begin
  inherited Create('VBO-Buffer index out of range.');
end;

{ EVBONotEnoughSpace }

constructor EVBONotEnoughSpace.Create;
begin
  inherited Create('Not enough space in VBO-Buffer.');
end;

{ EVBOFull }

constructor EVBOFull.Create;
begin
  inherited Create('VBO-Buffer is full, can''t add more vertices!');
end;

{ TVAO }

procedure TVAO.GenAttributes;
var
  Attribute: TGLProgram.TAttribute;
begin
  Bind;
  FVBO.Bind;
  for Attribute in GLProgram.Attributes do
  begin
    if Attribute.Location <> -1 then
    begin
      glEnableVertexAttribArray(Attribute.Location);
      // TODO: Matrices (not sure about mat2) needs to be handles like this instead, but might still give some problems:
      {
        if Attribute.DataType = dtMat2x4 then
        begin
        glVertexAttribPointer(
        Attribute.Location,
        4,
        Ord(Attribute.BaseDataType),
        True,
        GLProgram.AttributeStride,
        Pointer(Attribute.Offset));
        glVertexAttribPointer(
        Attribute.Location + 1,
        4,
        Ord(Attribute.BaseDataType),
        True,
        GLProgram.AttributeStride,
        Pointer(Attribute.Offset + SizeOf(Single) * 4));
        end
      }
      glVertexAttribPointer(
        Attribute.Location,
        Attribute.BaseCount,
        Ord(Attribute.BaseDataType),
        True,
        GLProgram.AttributeStride,
        Pointer(Attribute.Offset));
    end;
  end;
end;

function TVAO.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TVAO.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

function TVAO.GetLocation: TLocation3;
begin
  Result := nil;
end;

procedure TVAO.SetLocation(const Value: TLocation3);
begin
  Location.Assign(Value);
end;

procedure TVAO.GenObject(out AGLName: GLuint);
begin
  glGenVertexArrays(1, @AGLName);
end;

procedure TVAO.DeleteObject(const AGLName: GLuint);
begin
  glDeleteVertexArrays(1, @AGLName);
end;

procedure TVAO.BindGLObject;
begin
  glBindVertexArray(GLName);
end;

procedure TVAO.UnbindGLObject;
begin
  glBindVertexArray(0);
end;

procedure TVAO.SetGLLabel(const Value: AnsiString);
begin
  inherited;
  FVBO.GLLabel := Value;
end;

procedure TVAO.BeforeRender;
begin
  GLProgram.Bind;
end;

procedure TVAO.AfterRender;
begin
  // do nothing after rendering at the moment by default
end;

constructor TVAO.Create(AGLProgram: TGLProgram; AVBO: TVBO; ABeginMode: TGLBeginMode = rmTriangles);
begin
  inherited Create(AGLProgram.GLState);
  FGLProgram := AGLProgram;
  FBeginMode := ABeginMode;
  FVBO := AVBO;
  GenAttributes;
  FVisible := True;
end;

destructor TVAO.Destroy;
begin
  FVBO.Free;
  inherited;
end;

class function TVAO.GetObjectType: TGLObjectType;
begin
  Result := otVertexArray;
end;

class function TVAO.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TGLObjectBinding<TVAO>;
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
  if (FVBO.Count = 0) or not Visible then
    Exit;
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), 0, FVBO.Count);
  AfterRender;
end;

procedure TVAO.Render(const ABounds: TIntBounds1);
begin
  if (FVBO.Count = 0) or not Visible then
    Exit;
  BeforeRender;
  Bind;
  glDrawArrays(Ord(FBeginMode), ABounds.C1, ABounds.Length);
  AfterRender;
end;

{ TVBO }

procedure TVBO.BindGLObject;
begin
  glBindBuffer(Ord(btArrayBuffer), GLName);
end;

function TVBO.Binding: TBinding;
begin
  Result := TBinding(inherited Binding);
end;

procedure TVBO.DeleteObject(const AGLName: Cardinal);
begin
  glDeleteBuffers(1, @AGLName);
end;

procedure TVBO.GenObject(out AGLName: Cardinal);
begin
  glGenBuffers(1, @AGLName);
end;

class function TVBO.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TBinding;
end;

class function TVBO.GetObjectType: TGLObjectType;
begin
  Result := otBuffer;
end;

procedure TVBO.UnbindGLObject;
begin
  glBindBuffer(Ord(btArrayBuffer), 0);
end;

{ EVBOBindLock }

constructor EVBOBindLock.Create;
begin
  inherited Create('A VBO must stay bound as long as it is mapped');
end;

{ TAutoUpdateVAO }
{
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
}
{ TVAOProxy }

constructor TVAOProxy.Create(ASourceVAO: TVAO);
begin
  FVisible := True;
  FSourceVAO := ASourceVAO;
  FLocation := TLocation3.Create;
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

{ TVBO.TBinding }

procedure TVBO.TBinding.Lock;
begin
  Inc(FLockCount);
end;

procedure TVBO.TBinding.SetBoundObject(const Value: TVBO);
begin
  if FLockCount > 0 then
    raise EVBOBindLock.Create;
  inherited;
end;

procedure TVBO.TBinding.Unlock;
begin
  Dec(FLockCount);
end;

{ TVBO<TData> }

function TVBO<TData>.GetSubData(APos: Integer): TData;
begin
  Bind;
  glGetBufferSubData(Ord(btArrayBuffer), APos * GetDataSize, GetDataSize, @Result);
end;

function TVBO<TData>.GetSubData(ABounds: TIntBounds1): TArray<TData>;
var
  Count: Integer;
begin
  Result := TArray<TData>.Create;
  Count := ABounds.Length;
  Result.Capacity := Count;
  glGetBufferSubData(Ord(btArrayBuffer), ABounds.C1 * GetDataSize, Count * GetDataSize, Result.DataPointer);
  Result.ForceCount(Count);
end;

function TVBO<TData>.GetData: TArray<TData>;
begin
  Result := SubData[IBounds1(Count)];
end;

class function TVBO<TData>.GetDataSize: Integer;
begin
  Result := SizeOf(TData);
end;

procedure TVBO<TData>.SetData(const Value: TArray<TData>);
begin
  SubData[IBounds1(Count)] := Value;
end;

procedure TVBO<TData>.SetSubData(ABounds: TIntBounds1; const Value: TArray<TData>);
begin
  glBufferSubData(Ord(btArrayBuffer), ABounds.C1 * GetDataSize, ABounds.Length * GetDataSize, Value.DataPointer);
end;

procedure TVBO<TData>.SetSubData(APos: Integer; const Value: TData);
begin
  glBufferSubData(Ord(btArrayBuffer), APos * GetDataSize, GetDataSize, @Value);
end;

{ TVBO<TData>.TReader }

class function TVBO<TData>.TReader.GetBufferAccess: TGLBufferAccess;
begin
  Result := baReadOnly;
end;

{ TVBO<TData>.TWriter }

procedure TVBO<TData>.TWriter.AddToBuffer(AData: TData);
begin
  FBufferData[BufferPos] := AData;
  NextBufferPos;
end;

constructor TVBO<TData>.TWriter.Create(AVBO: TVBO<TData>);
begin
  inherited;
  FBufferData := PDataArray(FData);
end;

function TVBO<TData>.TWriter.GetBuffer(AOffset: Integer): PData;
begin
  Result := @FBufferData[BufferPos + AOffset];
end;

class function TVBO<TData>.TWriter.GetBufferAccess: TGLBufferAccess;
begin
  Result := baWriteOnly;
end;

function TVBO<TData>.TWriter.GetCurrent: PData;
begin
  Result := @FBufferData[BufferPos];
end;

function TVBO<TData>.TWriter.GetPos: Integer;
begin
  Result := FPos;
end;

procedure TVBO<TData>.TWriter.NextBufferPos;
begin
  BufferPos := BufferPos + 1;
end;

procedure TVBO<TData>.TWriter.SetPos(const Value: Integer);
begin
  // one more, to allow the buffer to be one after the last value at the end
  if not(Value in IBounds1(FVBO.Count + 1)) then
    raise EVBOOutOfRange.Create;
  FPos := Value;
end;

{ TVBO<TData>.TMapping }

constructor TVBO<TData>.TMapping.Create(AVBO: TVBO<TData>);
begin
  FVBO := AVBO;
  FVBO.Bind;
  FVBO.Binding.Lock;
  FData := glMapBuffer(Ord(btArrayBuffer), Ord(GetBufferAccess));
end;

destructor TVBO<TData>.TMapping.Destroy;
begin
  glUnmapBuffer(Ord(btArrayBuffer));
  FVBO.Binding.Unlock;
  inherited;
end;

{$POINTERMATH ON}


function TVBO<TData>.TMapping.GetData(APos: Integer): TData;
begin
  Result := (FData + APos)^;
end;

procedure TVBO<TData>.TMapping.SetData(APos: Integer; const Value: TData);
begin
  (FData + APos)^ := Value;
end;

{$POINTERMATH OFF}

{ TVAO<T> }

function TVAO<T>.GetVBO: TVBO<T>;
begin
  Result := TVBO<T>(inherited VBO);
end;

{ TVBO<T>.TReadWriter }

class function TVBO<TData>.TReadWriter.GetBufferAccess: TGLBufferAccess;
begin
  Result := baReadWrite;
end;

{ TVBOImmutable<TData> }

constructor TVBOImmutable<TData>.Create(AGLState: TGLState; ACount: Integer; AFlags: TGLBufferFlags; AData: Pointer);
begin
  inherited Create(AGLState);
  FCount := ACount;
  glBufferStorage(Ord(btArrayBuffer), ACount * GetDataSize, AData, ToGLBitfield(AFlags));
end;

constructor TVBOImmutable<TData>.Create(AGLState: TGLState; AData: TArray<TData>; AFlags: TGLBufferFlags);
begin
  inherited Create(GLState);
  FCount := AData.Count;
  glBufferStorage(Ord(btArrayBuffer), AData.Count, AData.DataPointer, ToGLBitfield(AFlags));
end;

{ TVBOMutable<TData> }

procedure TVBOMutable<TData>.Generate(ACount: Integer; AUsageHint: TGLBufferUsage; AData: Pointer);
begin
  if (AData = nil) and (Count = ACount) and (UsageHint = AUsageHint) then
    Exit;
  FCount := ACount;
  FUsageHint := AUsageHint;
  Bind;
  glBufferData(Ord(btArrayBuffer), ACount * GetDataSize, AData, Ord(AUsageHint));
end;

procedure TVBOMutable<TData>.Generate(AData: TArray<TData>; AUsageHint: TGLBufferUsage);
begin
  FCount := AData.Count;
  FUsageHint := AUsageHint;
  Bind;
  glBufferData(Ord(btArrayBuffer), AData.Count * GetDataSize, AData.DataPointer, Ord(AUsageHint));
end;

function TVBOMutable<TData>.Map: TVBO<TData>.IWriter;
begin
  Result := TWriter.Create(Self);
end;

{ TVAOImmutable<T> }

constructor TVAOImmutable<T>.Create(AGLProgram: TGLProgram; ACount: Integer; AFlags: TGLBufferFlags; AData: Pointer;
  ABeginMode: TGLBeginMode);
begin
  inherited Create(AGLProgram, TVBOImmutable<T>.Create(AGLProgram.GLState, ACount, AFlags, AData), ABeginMode);
end;

constructor TVAOImmutable<T>.Create(AGLProgram: TGLProgram; AData: TArray<T>; AFlags: TGLBufferFlags;
  ABeginMode: TGLBeginMode);
begin
  inherited Create(AGLProgram, TVBOImmutable<T>.Create(AGLProgram.GLState, AData, AFlags), ABeginMode);
end;

function TVAOImmutable<T>.GetVBO: TVBOImmutable<T>;
begin
  Result := TVBOImmutable<T>(inherited VBO);
end;

{ TVAOMutable<T> }

constructor TVAOMutable<T>.Create(AGLProgram: TGLProgram; ABeginMode: TGLBeginMode);
begin
  inherited Create(AGLProgram, TVBOMutable<T>.Create(AGLProgram.GLState), ABeginMode);
end;

function TVAOMutable<T>.GetVBO: TVBOMutable<T>;
begin
  Result := TVBOMutable<T>(inherited VBO);
end;

end.
