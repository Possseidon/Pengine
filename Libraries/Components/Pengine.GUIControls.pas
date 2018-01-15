unit Pengine.GUIControls;

interface

uses
  Pengine.EventHandling,
  Pengine.GUI,
  Pengine.Texture,
  Pengine.Collections,
  Pengine.SpriteSystem,
  Pengine.Utility,
  Pengine.Vector;

type

  TLabel = class(TControl)
  public type

    TCaptionChangeEventInfo = class(TControl.TEventInfo)
    private
      FOldCaption: string;

    public
      constructor Create(ASender: TLabel; AOldCaption: string);

      property OldCaption: string read FOldCaption;

    end;

    TCaptionChangeEvent = TEvent<TCaptionChangeEventInfo>;

    TCharSprites = TRefArray<TCharSprite>;

  private
    FCaption: string;
    FWidth: TOpt<Single>;
    FChars: TCharSprites;

    FOnCaptionChanged: TCaptionChangeEvent;

    procedure SetCaption(const Value: string);
    procedure GenCaption;
    function GetWidth: Single;

    function GetOnCaptionChanged: TCaptionChangeEvent.TAccess;

    procedure FontChanged(AInfo: TControl.TFontChangeEventInfo);
    procedure BoundsChanged(AInfo: TControl.TEventInfo);

  protected
    function GetAspect: Single; override;

  public
    constructor Create(AParent: TContainerControl); override;
    destructor Destroy; override;

    property Caption: string read FCaption write SetCaption;
    property Width: Single read GetWidth;

    property OnCaptionChanged: TCaptionChangeEvent.TAccess read GetOnCaptionChanged;

  end;

  TButton = class(TControl)
  private

  public

  end;

  TPanel = class(TContainerControl)
  private

  public

  end;

implementation

{ TLabel }

procedure TLabel.BoundsChanged(AInfo: TControl.TEventInfo);
begin
  GenCaption;
end;

constructor TLabel.Create(AParent: TContainerControl);
begin
  inherited;
  FChars := TCharSprites.Create;
  FWidth := TOpt<Single>.Create;
  OnFontChanged.Add(FontChanged);
  OnBoundsChanged.Add(BoundsChanged);
end;

destructor TLabel.Destroy;
begin
  FChars.Free;
  FWidth.Free;
  inherited;
end;

procedure TLabel.FontChanged(AInfo: TControl.TFontChangeEventInfo);
begin
  FWidth.Clear;
  GenCaption;
end;

procedure TLabel.GenCaption;
var
  I: Integer;
  Offset: TVector2;
begin
  while FChars.Count > Length(Caption) do
  begin
    FChars.Last.Remove;
    FChars.DelLast;
  end;
  while FChars.Count < Length(Caption) do
    FChars.Add(AddChar);

  Offset := BoundsCentered.S;
  Offset.X := Offset.X - Width / 2;
  for I := 0 to FChars.Count - 1 do
  begin
    FChars[I].BeginUpdate;
    FChars[I].Char := FCaption[I + 1];
    FChars[I].Location.Parent := Location;
    if I = 0 then
      Offset.X := Offset.X + FChars[I].Width / 2
    else
      Offset.X := Offset.X + FChars[I].WidthSpaced / 2;
    FChars[I].Location.Pos := Offset;
    Offset.X := Offset.X + FChars[I].WidthSpaced / 2;
    FChars[I].EndUpdate;
  end;
end;

function TLabel.GetAspect: Single;
begin
  Result := Width;
end;

function TLabel.GetOnCaptionChanged: TCaptionChangeEvent.TAccess;
begin
  Result := FOnCaptionChanged.Access;
end;

function TLabel.GetWidth: Single;
begin
  if not FWidth.HasValue then
    FWidth.Value := FontTile.TextWidth(Caption);
  Result := FWidth.Value;
end;

procedure TLabel.SetCaption(const Value: string);
var
  OldCaption: string;
begin
  if Caption = Value then
    Exit;
  OldCaption := Caption;
  FCaption := Value;
  FWidth.Clear;
  GenCaption;
  FOnCaptionChanged.Execute(TCaptionChangeEventInfo.Create(Self, OldCaption));
end;

{ TLabel.TCaptionChangeEventInfo }

constructor TLabel.TCaptionChangeEventInfo.Create(ASender: TLabel; AOldCaption: string);
begin
  inherited Create(ASender);
  FOldCaption := AOldCaption;
end;

end.
