unit LootTableFrameCondition;

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
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Pengine.MC.LootTable;

type

  TfrmCondition = class(TFrame)
    gbMain: TGroupBox;
    Panel1: TPanel;
    btnMoveLeft: TButton;
    btnMoveRight: TButton;
    btnRemove: TButton;
  public const

    BoxCaption = 'Condition #%d - %s';

  private
    FCondition: TLootTable.TCondition;

    procedure AddConditionFrame;

    procedure UpdateBoxCaption;

  public
    constructor Create(AOwner: TComponent; ACondition: TLootTable.TCondition); reintroduce;

    property Condition: TLootTable.TCondition read FCondition;

  end;

implementation

uses
  ConditionFrameEntityProperties;

const

  ConditionFrameClasses: array [TLootTable.TCondition.TType] of TCustomFrameClass = (
    TfrmConditionEntityProperties,
    nil,
    nil,
    nil,
    nil
  );

{$R *.dfm}

{ TFrame1 }

procedure TfrmCondition.AddConditionFrame;
var
  FrameClass: TCustomFrameClass;
  Frame: TCustomFrame;
begin
  FrameClass := ConditionFrameClasses[Condition.GetType];
  if FrameClass = nil then
    Exit;
  Frame := FrameClass.Create(Self);
  Frame.Parent := gbMain;
  Frame.Align := alClient;
  Frame.Name := '';
  Height := Height + Frame.Height;
end;

constructor TfrmCondition.Create(AOwner: TComponent; ACondition: TLootTable.TCondition);
begin
  inherited Create(AOwner);
  FCondition := ACondition;
  AddConditionFrame;
  UpdateBoxCaption;
end;

procedure TfrmCondition.UpdateBoxCaption;
begin
  gbMain.Caption := Format(BoxCaption, [Condition.Index + 1, Condition.GetDisplayName]);
end;

end.
