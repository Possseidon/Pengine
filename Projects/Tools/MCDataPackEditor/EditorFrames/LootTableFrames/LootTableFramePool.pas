unit LootTableFramePool;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  Pengine.MC.LootTable,

  HelperFrameIntBounds,
  HelperFrameBounds,
  LootTableFrameCondition;

type

  TfrmPool = class(TFrame)
    gbPool: TGroupBox;
    frmRolls: TfrmHelperIntBounds;
    frmBonusRolls: TfrmHelperBounds;
    Panel7: TPanel;
    PageControl1: TPageControl;
    Button2: TButton;
    cbEntries: TComboBox;
    Panel1: TPanel;
    Button1: TButton;
    Button3: TButton;
  public const

    BoxCaption = 'Pool #%d';

  private
    FPool: TLootTable.TPool;

    procedure UpdateBoxCaption;

    procedure RollsChange(Sender: TObject);
    procedure BonusRollsChange(Sender: TObject);

    procedure InitEntryBox;

    procedure AddCondition(ACondition: TLootTable.TCondition);
    procedure AddEntry(AEntry: TLootTable.TEntry);

  public
    constructor Create(AOwner: TComponent; APool: TLootTable.TPool); reintroduce;

    property Pool: TLootTable.TPool read FPool;

  end;

implementation

{$R *.dfm}

{ TfrmPool }

procedure TfrmPool.RollsChange(Sender: TObject);
begin
  Pool.Rolls := frmRolls.Bounds;
end;

procedure TfrmPool.AddCondition(ACondition: TLootTable.TCondition);
// var
//   ConditionFrame: TfrmCondition;
begin
  {
  ConditionFrame := TfrmCondition.Create(gbConditions, ACondition);
  ConditionFrame.Parent := gbConditions;
  ConditionFrame.Align := alTop;
  ConditionFrame.AlignWithMargins := True;
  ConditionFrame.Name := '';
  ConditionFrame.Top := Integer.MaxValue;
  gbConditions.Height := gbConditions.Height + ConditionFrame.Height;
  }
end;

procedure TfrmPool.AddEntry(AEntry: TLootTable.TEntry);
begin

end;

procedure TfrmPool.BonusRollsChange(Sender: TObject);
begin
  Pool.BonusRolls := frmBonusRolls.Bounds;
end;

constructor TfrmPool.Create(AOwner: TComponent; APool: TLootTable.TPool);
var
  Condition: TLootTable.TCondition;
  Entry: TLootTable.TEntry;
begin
  inherited Create(AOwner);
  FPool := APool;
  InitEntryBox;
  UpdateBoxCaption;
  frmRolls.Bounds := Pool.Rolls;
  frmRolls.OnChange := RollsChange;
  frmBonusRolls.Bounds := Pool.BonusRolls;
  frmBonusRolls.OnChange := BonusRollsChange;
  for Condition in Pool.Conditions do
    AddCondition(Condition);
  for Entry in Pool.Entries do
    AddEntry(Entry);      
end;

procedure TfrmPool.InitEntryBox;
var
  Entry: TLootTable.TEntry.TType;
begin
  cbEntries.Items.BeginUpdate;
  for Entry := Low(TLootTable.TEntry.TType) to High(TLootTable.TEntry.TType) do
    cbEntries.Items.Add(TLootTable.EntryDisplayNames[Entry]);
  cbEntries.Items.EndUpdate;
  cbEntries.ItemIndex := 0;
end;

procedure TfrmPool.UpdateBoxCaption;
begin
  gbPool.Caption := Format(BoxCaption, [Pool.Index + 1]);
end;

end.
