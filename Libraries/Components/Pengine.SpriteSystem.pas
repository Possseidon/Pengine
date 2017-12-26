unit Pengine.SpriteSystem;

interface

uses
  Pengine.GLState,
  Pengine.Collections,
  Pengine.VAO,
  Pengine.Vector,
  Pengine.GLProgram,
  Pengine.Color,
  Pengine.Texture;

type

  TSpriteGLProgamBase = class(TGLProgramResource)
  public type

    TData = record
      Pos: TVector3;
      Color: TColorRGBA;
      TexCoord: TVector2;
      Border: TBounds2;
    end;

  protected
    class function GetAttributeOrder: TGLProgram.TAttributeOrder; override;
   
  end;

  TSprite = class;

  TSpriteSystem = class
  public type

    TSprites = TObjectArray<TSprite>;
    TVAO = TVAOMutable<TSpriteGLProgamBase.TData>;

  private
    FSprites: TSprites;
    FVAO: TVAO;

  public
    constructor Create(AGLProgram: TGLProgram);
    destructor Destroy; override;

    procedure Update;
    procedure Render;

    procedure Add(ASprite: TSprite);
    procedure Del(ASprite: TSprite);

  end;

  TAnimation = class
  public type
    TFrames = TArray<TTexturePage>;

  private
    FFrames: TFrames;


  public


  end;

  TSprite = class
  private
    FSpriteSystem: TSpriteSystem;

    FPos: TVector3;
    FBounds: TBounds2;
    FRotation: Single;
    FColor: TColorRGBA;

    procedure SetBounds(const Value: TBounds2);
    procedure SetColor(const Value: TColorRGBA);
    procedure SetPos(const Value: TVector3);
    procedure SetRotation(const Value: Single);

  public
    constructor Create(ASpriteSystem: TSpriteSystem);
    destructor Destroy; override;

    property SpriteSystem: TSpriteSystem read FSpriteSystem;

    property Pos: TVector3 read FPos write SetPos;
    property Bounds: TBounds2 read FBounds write SetBounds;
    property Rotation: Single read FRotation write SetRotation;
    property Color: TColorRGBA read FColor write SetColor;
    property Animation: TAnimation read FAnimation write SetAnimation;
    
    procedure Update;

    procedure AddToVAO(AVAO: TSpriteSystem.TVAO);

  end;

implementation

{ TSpriteGLProgamBase }

class function TSpriteGLProgamBase.GetAttributeOrder: TGLProgram.TAttributeOrder;
begin
  Result := ['vpos', 'vcolor', 'vtexcoord', 'vborderlow', 'vborderhigh'];
end;

{ TSpriteSystem }

procedure TSpriteSystem.Add(ASprite: TSprite);
begin
  FSprites.Add(ASprite);
end;

constructor TSpriteSystem.Create(AGLProgram: TGLProgram);
begin
  FSprites := TSprites.Create;
  FVAO := TVAO.Create(AGLProgram);
end;

procedure TSpriteSystem.Del(ASprite: TSprite);
begin
  FSprites.Del(ASprite);
end;

destructor TSpriteSystem.Destroy;
begin
  FVAO.Free;
  FSprites.Free;
  inherited;
end;

procedure TSpriteSystem.Update;
var
  Sprite: TSprite;
begin
  for Sprite in FSprites do
    Sprite.Update;
end;

procedure TSpriteSystem.Render;
begin
  FVAO.Render;
end;

{ TSprite }

procedure TSprite.SetBounds(const Value: TBounds2);
begin
  FBounds := Value;
end;

procedure TSprite.SetColor(const Value: TColorRGBA);
begin
  FColor := Value;
end;

procedure TSprite.SetPos(const Value: TVector3);
begin
  FPos := Value;
end;

procedure TSprite.SetRotation(const Value: Single);
begin
  FRotation := Value;
end;

constructor TSprite.Create(ASpriteSystem: TSpriteSystem);
begin
  FSpriteSystem := ASpriteSystem;
  SpriteSystem.Add(Self);
end;

destructor TSprite.Destroy;
begin
  SpriteSystem.Del(Self);
  inherited;
end;

procedure TSprite.Update;
begin

end;

procedure TSprite.AddToVAO(AVAO: TSpriteSystem.TVAO);
begin

end;

end.
