unit CVViewerFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ToolsAPI;

type
  TfrmListVisualizer = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater)
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
  end;

implementation

{$R *.dfm}

{ TfrmListVisualizer }

procedure TfrmListVisualizer.CloseVisualizer;
begin

end;

procedure TfrmListVisualizer.MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
begin

end;

procedure TfrmListVisualizer.RefreshVisualizer(const Expression, TypeName, EvalResult: string);
begin

end;

procedure TfrmListVisualizer.SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
begin

end;

end.
