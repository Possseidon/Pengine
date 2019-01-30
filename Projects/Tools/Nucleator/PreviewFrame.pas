unit PreviewFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,

  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,

  dglOpenGL,

  Pengine.GLContext,
  Pengine.IntMaths,
  Pengine.GLState;

type

  TfrmPreview = class(TFrame)
  private
    FContext: TGLContext;

    function GetSize: TIntVector2;

    procedure Render;

  protected
    procedure WndProc(var Message: TMessage); override;

  public
    destructor Destroy; override;

    property Size: TIntVector2 read GetSize;

  end;

implementation

{$R *.dfm}

{ TfrmPreview }

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

procedure TfrmPreview.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_PAINT:
      begin
        if FContext = nil then
        begin
          FContext := TGLContext.Create(GetDC(Handle), Size, Render);
          FContext.AutoFinish := True;
        end;
        FContext.Render;
        ValidateRect(Handle, ClientRect);
        Message.Result := 0;
      end
  else
    inherited;
  end;
end;

end.
