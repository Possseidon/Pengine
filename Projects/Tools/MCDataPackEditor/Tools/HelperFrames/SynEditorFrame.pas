unit SynEditorFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  SynEdit;

type

  TfrmSynEditor = class(TFrame)
    synEditor: TSynEdit;
  private

  public

  end;

implementation

{$R *.dfm}

end.
