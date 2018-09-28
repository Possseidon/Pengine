unit Pengine.Renderbuffer;

interface

uses
  dglOpenGL, 

  Pengine.IntMaths,
  Pengine.GLState,
  Pengine.GLEnums;

type

  TRenderbuffer = class (TGLObject)
  private
    FPixelFormat: TGLPixelFormat;
    FSize: TIntVector2;

  protected
    procedure GenObject(out AGLName: Cardinal); override;
    procedure DeleteObject(const AGLName: Cardinal); override;

    procedure BindGLObject; override;
    procedure UnbindGLObject; override;

    constructor Create(AGLState: TGLState); overload;

  public
    constructor Create(AGLState: TGLState; ASize: TIntVector2; APixelFormat: TGLPixelFormat = pfRGBA); overload;

    class function GetObjectType: TGLObjectType; override;
    class function GetBindingClass: TGLObjectBindingClass; override;

    property PixelFormat: TGLPixelFormat read FPixelFormat;
    property Size: TIntVector2 read FSize;
    property Width: Integer read FSize.X;
    property Height: Integer read FSize.Y;

  end;

  TRenderbufferMS = class (TRenderbuffer)
  private
    FSamples: Cardinal;

  public
    constructor Create(AGLState: TGLState; ASize: TIntVector2; ASamples: Cardinal; APixelFormat: TGLPixelFormat = pfRGBA);

    property Samples: Cardinal read FSamples;

  end;

implementation

{ TRenderbuffer }

procedure TRenderbuffer.GenObject(out AGLName: Cardinal);
begin
  glGenRenderbuffers(1, @AGLName);
end;

constructor TRenderbuffer.Create(AGLState: TGLState);
begin
  inherited;
end;

procedure TRenderbuffer.DeleteObject(const AGLName: Cardinal);
begin
  glDeleteRenderbuffers(1, @AGLName);
end;

procedure TRenderbuffer.BindGLObject;
begin
  glBindRenderbuffer(GL_RENDERBUFFER, GLName);
end;

procedure TRenderbuffer.UnbindGLObject;
begin
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
end;

constructor TRenderbuffer.Create(AGLState: TGLState; ASize: TIntVector2; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState);
  FSize := ASize;
  FPixelFormat := APixelFormat;
  Bind;
  glRenderbufferStorage(GL_RENDERBUFFER, Ord(PixelFormat), Width, Height);
end;

class function TRenderbuffer.GetObjectType: TGLObjectType;
begin
  Result := otRenderbuffer;
end;

class function TRenderbuffer.GetBindingClass: TGLObjectBindingClass;
begin
  Result := TGLObjectBinding<TRenderbuffer>;
end;

{ TRenderbufferMS }

constructor TRenderbufferMS.Create(AGLState: TGLState; ASize: TIntVector2; ASamples: Cardinal; APixelFormat: TGLPixelFormat);
begin
  inherited Create(AGLState);
  FSize := ASize;
  FPixelFormat := APixelFormat;
  FSamples := ASamples;
  Bind;
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, Samples, Ord(PixelFormat), Width, Height);
end;

end.
