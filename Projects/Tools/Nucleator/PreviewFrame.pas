unit PreviewFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,

  Vcl.Forms,

  Pengine.GLContext,
  Pengine.IntMaths,
  Vcl.Controls,
  Vcl.StdCtrls,
  Pengine.GLState;

type

  TfrmPreview = class(TFrame)
  private
    FContext: TGLContext;

    function GetSize: TIntVector2;

    procedure Render;

  protected
    procedure Resize; override;

    procedure WndProc(var Message: TMessage); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Size: TIntVector2 read GetSize;

  end;

implementation

{$R *.dfm}

{ TfrmPreview }

constructor TfrmPreview.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TfrmPreview.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TfrmPreview.GetSize: TIntVector2;
begin
  Result := IVec2(ClientWidth, ClientHeight);
end;

procedure TfrmPreview.Render;
begin

end;

procedure TfrmPreview.Resize;
begin
  inherited;
end;

procedure TfrmPreview.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_PAINT:
      begin
        if FContext = nil then
          FContext := TGLContext.Create(GetDC(Handle), Size, Render);
        FContext.GLState[stClearColor] := Random($1000000);
        FContext.Render;
        ValidateRect(Handle, ClientRect);
        Message.Result := 0;
      end
  else
    inherited;
  end;
end;

end.
