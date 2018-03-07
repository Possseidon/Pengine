unit Minecraft.BrigadierParser;

interface

uses
  System.SysUtils,
  System.JSON,
  System.Math,
  System.RegularExpressions,

  Pengine.IntMaths,
  Pengine.Vector,

  Minecraft.Brigadier;

type

  EBrigadierUnknownParserProperty = class(Exception)
  public
    constructor Create(AParserClass: TBrigadierParserClass; AProperty, AValue: string);
  end;

  TBrigadierBoolParser = class(TBrigadierParser)
  protected
    function Parse: Boolean;

  public
    class function GetParserString: string; override;
    
  end;

  TBrigadierBool = class(TBrigadierArgumentParameter)
  public const
    BoolStrings: array [Boolean] of string = ('false', 'true');

  private
    FValue: Boolean;

  public
    property Value: Boolean read FValue;

    function Format: string; override;

  end;

  TBrigadierInteger = class(TBrigadierArgumentParameter)
  private
    FValue: Integer;

  protected
    procedure Parse(ACommand: string); override;

  public
    function Parser: TParser; reintroduce;

    property Value: Integer read FValue;

    function Format: string; override;

  end;

  TBrigadierIntegerParser = class(TBrigadierParser)
  private
    FBounds: TIntBounds1;

  public
    constructor Create(AProperties: TJSONObject); override;

    class function GetParserString: string; override;
    class function GetParameterClass: TBrigadierParameterClass; override;

    property Bounds: TIntBounds1 read FBounds;

  end;

  TBrigadierFloat = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    private
      FBounds: TBounds1;

    public
      constructor Create(AProperties: TJSONObject); override;

      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;

      property Bounds: TBounds1 read FBounds;

    end;

    TDoubleParser = class(TParser)
    public
      class function GetParserString: string; override;
    end;

  private
    FValue: Single;

  protected
    procedure Parse(ACommand: string); override;

  public
    function Parser: TParser; reintroduce;

    property Value: Single read FValue;

    function Format: string; override;

  end;

  TBrigadierString = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public type

      TType = (
        bstWord,
        bstGreedy,
        bstPhrase
        );

    public const

      TypeStrings: array [TType] of string = (
        'word',
        'greedy',
        'phrase'
        );

    private
      FParseType: TType;

    public
      constructor Create(APropeties: TJSONObject); override;

      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;

      class function GetType(ATypeString: string): TType;

      property ParseType: TType read FParseType;

    end;

  private
    FValue: string;

  protected
    procedure Parse(ACommand: string); override;

  public
    function Parser: TParser; reintroduce;

    property Value: string read FValue;

    function Format: string; override;

  end;

  // TODO: Currently just a greedy brigadier:string... this can contain selectors though
  TMinecraftMessage = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private
    FMessage: string;

  protected
    procedure Parse(ACommand: string); override;

  public
    property Message: string read FMessage;

    function Format: string; override;

  end;

  TMinecraftBlockPredicate = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftObjectiveCriteria = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftColor = class(TBrigadierArgumentParameter)
  public type

    TColor = (
      mclBlack,
      mclDarkBlue,
      mclDarkGreen,
      mclDarkAqua,
      mclDarkRed,
      mclDarkPurple,
      mclGold,
      mclGray,
      mclDarkGray,
      mclBlue,
      mclGreen,
      mclAqua,
      mclRed,
      mclLightPurple,
      mclYellow,
      mclWhite,
      mclReset
    );

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  public const

    ColorStrings: array [TColor] of string = (
      'black',
      'dark_blue',
      'dark_green',
      'dark_aqua',
      'dark_red',
      'dark_purple',
      'gold',
      'gray',
      'dark_gray',
      'blue',
      'green',
      'aqua',
      'red',
      'light_purple',
      'yellow',
      'white',
      'reset'
    );

  private
    FColor: TColor;

  protected
    procedure Parse(ACommand: string); override;

  public
    property Color: TColor read FColor;

    function Format: string; override;

  end;

  TMinecraftComponent = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftBlockPos = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftObjective = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftFunction = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftNBTPath = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftItemSlot = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftScoreboardSlot = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftTeam = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private
    FTeam: string;
  
  protected
    procedure Parse(ACommand: string); override;

  public
    property Team: string read FTeam;
  
    function Format: string; override;

  end;

  TMinecraftSwizzle = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftMobEffect = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftParticle = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftNBT = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftItemStack = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftResourceLocation = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftItemPredicate = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftScoreHolder = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftOperation = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftRotation = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftVec3 = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftVec2 = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftBlockState = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftGameProfile = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftEntity = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftEntityAnchor = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftRange = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

  TMinecraftItemEnchantment = class(TBrigadierArgumentParameter)
  public type

    TParser = class(TBrigadierParser)
    public
      class function GetParserString: string; override;
      class function GetParameterClass: TBrigadierParameterClass; override;
    end;

  private

  protected
    // procedure Parse(ACommand: string); override;

  public
    // function Format: string; override;

  end;

implementation

{ TMinecraftMessageParameter.TParser }

class function TMinecraftMessage.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftMessage;
end;

class function TMinecraftMessage.TParser.GetParserString: string;
begin
  Result := 'minecraft:message';
end;

{ TBrigadierInteger.TParser }

constructor TBrigadierInteger.TParser.Create(AProperties: TJSONObject);
var
  Node: TJSONNumber;
begin
  inherited;

  if Assigned(AProperties) and AProperties.TryGetValue<TJSONNumber>('min', Node) then
    FBounds.Low := Node.AsInt
  else
    FBounds.Low := Integer.MinValue;

  if Assigned(AProperties) and AProperties.TryGetValue<TJSONNumber>('max', Node) then
    FBounds.High := Node.AsInt
  else
    FBounds.High := Integer.MaxValue;
end;

class function TBrigadierInteger.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TBrigadierInteger;
end;

class function TBrigadierInteger.TParser.GetParserString: string;
begin
  Result := 'brigadier:integer';
end;

{ TBrigadierFloat.TParser }

constructor TBrigadierFloat.TParser.Create(AProperties: TJSONObject);
var
  Node: TJSONNumber;
begin
  inherited;

  if Assigned(AProperties) and AProperties.TryGetValue<TJSONNumber>('min', Node) then
    FBounds.Low := Node.AsDouble
  else
    FBounds.Low := -Infinity;

  if Assigned(AProperties) and AProperties.TryGetValue<TJSONNumber>('max', Node) then
    FBounds.High := Node.AsDouble
  else
    FBounds.High := Infinity;
end;

class function TBrigadierFloat.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TBrigadierFloat;
end;

class function TBrigadierFloat.TParser.GetParserString: string;
begin
  Result := 'brigadier:float';
end;

{ TBrigadierString.TParser }

constructor TBrigadierString.TParser.Create(APropeties: TJSONObject);
begin
  inherited;
  FParseType := GetType(APropeties.GetValue<TJSONString>('type').Value);
end;

class function TBrigadierString.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TBrigadierString;
end;

class function TBrigadierString.TParser.GetParserString: string;
begin
  Result := 'brigadier:string';
end;

class function TBrigadierString.TParser.GetType(ATypeString: string): TType;
var
  T: TType;
begin
  for T := Low(TType) to High(TType) do
    if ATypeString = TypeStrings[T] then
      Exit(T);
  raise EBrigadierUnknownParserProperty.Create(Self, 'type', ATypeString);
end;

{ EBrigadierUnknownParserProperty }

constructor EBrigadierUnknownParserProperty.Create(AParserClass: TBrigadierParserClass; AProperty, AValue: string);
begin
  inherited CreateFmt('Unknown value "%s" for property "%s" in %s.', [AValue, AProperty, AParserClass.GetParserString]);
end;

{ TParserBrigadierDouble }

class function TBrigadierFloat.TDoubleParser.GetParserString: string;
begin
  Result := 'brigadier:double';
end;

{ TMinecraftMessageParameter }

function TMinecraftMessage.Format: string;
begin
  Result := FMessage;
end;

procedure TMinecraftMessage.Parse(ACommand: string);
begin
  FMessage := ACommand;
  ParseSuccess(ACommand.Length);
end;

{ TBrigadierBool.TParser }

class function TBrigadierBool.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TBrigadierBool;
end;

class function TBrigadierBool.TParser.GetParserString: string;
begin
  Result := 'brigadier:bool';
end;

{ TMinecraftBlockPredicate.TParser }

class function TMinecraftBlockPredicate.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftBlockPredicate;
end;

class function TMinecraftBlockPredicate.TParser.GetParserString: string;
begin
  Result := 'minecraft:block_predicate';
end;

{ TMinecraftObjectiveCriteria.TParser }

class function TMinecraftObjectiveCriteria.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftObjectiveCriteria;
end;

class function TMinecraftObjectiveCriteria.TParser.GetParserString: string;
begin
  Result := 'minecraft:objective_criteria';
end;

{ TMinecraftColor.TParser }

class function TMinecraftColor.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftColor;
end;

class function TMinecraftColor.TParser.GetParserString: string;
begin
  Result := 'minecraft:color';
end;

{ TMinecraftComponent.TParser }

class function TMinecraftComponent.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftComponent;
end;

class function TMinecraftComponent.TParser.GetParserString: string;
begin
  Result := 'minecraft:component';
end;

{ TMinecraftBlockPos.TParser }

class function TMinecraftBlockPos.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftBlockPos;
end;

class function TMinecraftBlockPos.TParser.GetParserString: string;
begin
  Result := 'minecraft:block_pos';
end;

{ TMinecraftObjective.TParser }

class function TMinecraftObjective.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftObjective;
end;

class function TMinecraftObjective.TParser.GetParserString: string;
begin
  Result := 'minecraft:objective';
end;

{ TMinecraftFunction.TParser }

class function TMinecraftFunction.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftFunction;
end;

class function TMinecraftFunction.TParser.GetParserString: string;
begin
  Result := 'minecraft:function';
end;

{ TMinecraftNBTPath.TParser }

class function TMinecraftNBTPath.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftNBTPath;
end;

class function TMinecraftNBTPath.TParser.GetParserString: string;
begin
  Result := 'minecraft:nbt_path';
end;

{ TMinecraftItemSlot.TParser }

class function TMinecraftItemSlot.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftItemSlot;
end;

class function TMinecraftItemSlot.TParser.GetParserString: string;
begin
  Result := 'minecraft:item_slot';
end;

{ TMinecraftScoreboardSlot.TParser }

class function TMinecraftScoreboardSlot.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftScoreboardSlot;
end;

class function TMinecraftScoreboardSlot.TParser.GetParserString: string;
begin
  Result := 'minecraft:scoreboard_slot';
end;

{ TMinecraftTeam.TParser }

class function TMinecraftTeam.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftTeam;
end;

class function TMinecraftTeam.TParser.GetParserString: string;
begin
  Result := 'minecraft:team';
end;

{ TMinecraftSwizzle.TParser }

class function TMinecraftSwizzle.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftSwizzle;
end;

class function TMinecraftSwizzle.TParser.GetParserString: string;
begin
  Result := 'minecraft:swizzle';
end;

{ TMinecraftMobEffect.TParser }

class function TMinecraftMobEffect.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftMobEffect;
end;

class function TMinecraftMobEffect.TParser.GetParserString: string;
begin
  Result := 'minecraft:mob_effect';
end;

{ TMinecraftParticle.TParser }

class function TMinecraftParticle.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftParticle;
end;

class function TMinecraftParticle.TParser.GetParserString: string;
begin
  Result := 'minecraft:particle';
end;

{ TMinecraftNBT.TParser }

class function TMinecraftNBT.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftNBT;
end;

class function TMinecraftNBT.TParser.GetParserString: string;
begin
  Result := 'minecraft:nbt';
end;

{ TMinecraftItemStack.TParser }

class function TMinecraftItemStack.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftItemStack;
end;

class function TMinecraftItemStack.TParser.GetParserString: string;
begin
  Result := 'minecraft:item_stack';
end;

{ TMinecraftResourceLocation.TParser }

class function TMinecraftResourceLocation.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftResourceLocation;
end;

class function TMinecraftResourceLocation.TParser.GetParserString: string;
begin
  Result := 'minecraft:resource_location';
end;

{ TMinecraftItemPredicate.TParser }

class function TMinecraftItemPredicate.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftItemPredicate;
end;

class function TMinecraftItemPredicate.TParser.GetParserString: string;
begin
  Result := 'minecraft:item_predicate';
end;

{ TMinecraftScoreHolder.TParser }

class function TMinecraftScoreHolder.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftScoreHolder;
end;

class function TMinecraftScoreHolder.TParser.GetParserString: string;
begin
  Result := 'minecraft:score_holder';
end;

{ TMinecraftOperation.TParser }

class function TMinecraftOperation.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftOperation;
end;

class function TMinecraftOperation.TParser.GetParserString: string;
begin
  Result := 'minecraft:operation';
end;

{ TMinecraftRotation.TParser }

class function TMinecraftRotation.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftRotation;
end;

class function TMinecraftRotation.TParser.GetParserString: string;
begin
  Result := 'minecraft:rotation';
end;

{ TMinecraftVec3.TParser }

class function TMinecraftVec3.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftVec3;
end;

class function TMinecraftVec3.TParser.GetParserString: string;
begin
  Result := 'minecraft:vec3';
end;

{ TMinecraftVec2.TParser }

class function TMinecraftVec2.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftVec2;
end;

class function TMinecraftVec2.TParser.GetParserString: string;
begin
  Result := 'minecraft:vec2';
end;

{ TMinecraftBlockState.TParser }

class function TMinecraftBlockState.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftBlockState;
end;

class function TMinecraftBlockState.TParser.GetParserString: string;
begin
  Result := 'minecraft:block_state';
end;

{ TMinecraftGameProfile.TParser }

class function TMinecraftGameProfile.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftGameProfile;
end;

class function TMinecraftGameProfile.TParser.GetParserString: string;
begin
  Result := 'minecraft:game_profile';
end;

{ TMinecraftEntity.TParser }

class function TMinecraftEntity.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftEntity;
end;

class function TMinecraftEntity.TParser.GetParserString: string;
begin
  Result := 'minecraft:entity';
end;

{ TBrigadierBool }

function TBrigadierBool.Format: string;
begin
  Result := BoolStrings[Value];
end;

procedure TBrigadierBool.Parse(ACommand: string);
var
  B: Boolean;
  Match: TMatch;
begin
  for B := False to True do
  begin
    Match := TRegex.Match(ACommand, '^' + TRegex.Escape(BoolStrings[B]) + '(?:$|\s+)');
    if Match.Success then
    begin
      FValue := B;
      ParseSuccess(Match.Length);
      Exit;
    end;
  end;
end;

{ TBrigadierInteger }

function TBrigadierInteger.Format: string;
begin
  Result := Value.ToString;
end;

procedure TBrigadierInteger.Parse(ACommand: string);
var
  Match: TMatch;
begin
  Match := TRegex.Match(ACommand, '^([+-]?\d+)(?:$|\s)');
  if Match.Success then
    if Integer.TryParse(Match.Groups[1].Value, FValue) then
      if Value in Parser.Bounds then
        ParseSuccess(Match.Groups[1].Value.Length);
end;

function TBrigadierInteger.Parser: TParser;
begin
  Result := TParser(inherited Parser);
end;

{ TBrigadierFloat }

function TBrigadierFloat.Format: string;
begin
  Result := Value.ToString(ffGeneral, 7, 0, TFormatSettings.Invariant);
end;

procedure TBrigadierFloat.Parse(ACommand: string);
var
  Match: TMatch;
begin
  Match := TRegex.Match(ACommand, '^(.+?)(?:$|\s)');
  if Match.Success then
    if Single.TryParse(Match.Groups[1].Value, FValue, TFormatSettings.Invariant) then
      if Value in Parser.Bounds then
        ParseSuccess(Match.Groups[1].Value.Length);
end;

function TBrigadierFloat.Parser: TParser;
begin
  Result := TParser(inherited Parser);
end;

{ TBrigadierString }

function TBrigadierString.Format: string;
var
  Match: TMatch;
begin
  case Parser.ParseType of
    bstWord, bstGreedy:
      Result := Value;
    bstPhrase:
      begin
        Match := TRegex.Match(Value, '^\w+$');
        if Match.Success then
          Result := Value
        else
        begin
          Result := Value.Replace('"', '\"');
          Result := Result.Replace('\', '\\');
          Result := '"' + Result + '"';
        end;
      end;
  end;
end;

procedure TBrigadierString.Parse(ACommand: string);
var
  Match: TMatch;
begin
  case Parser.ParseType of
    // take up to the next whitespace
    bstWord:
      begin
        Match := TRegex.Match(ACommand, '^(.+?)(?:$|\s)');
        if Match.Success then
        begin
          FValue := Match.Groups[1].Value;
          ParseSuccess(Value.Length);
        end;
      end;
    // take the whole rest command as string
    bstGreedy:
      begin
        FValue := ACommand;
        ParseSuccess(ACommand.Length);
      end;
    // surrounded by "" if there are special characters or whitespaces
    bstPhrase:
      begin
        Match := TRegex.Match(ACommand, '^("((?:[^\\"]|\\"|\\\\)*)")(?:$|\s)');
        if Match.Success then
        begin
          FValue := Match.Groups[2].Value.Replace('\"', '"');
          FValue := FValue.Replace('\\', '\');
          ParseSuccess(Match.Groups[1].Length);
          Exit;
        end;

        Match := TRegex.Match(ACommand, '^(\w+)(?:$|\s)');
        if Match.Success then
        begin
          FValue := Match.Groups[1].Value;
          ParseSuccess(Match.Groups[1].Length);
        end;
      end;
  end;
end;

function TBrigadierString.Parser: TParser;
begin
  Result := TParser(inherited Parser);
end;

{ TMinecraftColor }

function TMinecraftColor.Format: string;
begin
  Result := ColorStrings[Color];
end;

procedure TMinecraftColor.Parse(ACommand: string);
var
  Match: TMatch;
  C: TColor;
begin
  Match := TRegex.Match(ACommand, '^(.+?)(?:$|\s)');
  if Match.Success then
  begin
    for C := Low(TColor) to High(TColor) do
    begin
      if Match.Groups[1].Value = ColorStrings[C] then
      begin
        FColor := C;
        ParseSuccess(Match.Groups[1].Length);
        Exit;
      end;
    end;
  end;
end;

{ TMinecraftTeam }

function TMinecraftTeam.Format: string;
begin
  Result := FTeam;
end;

procedure TMinecraftTeam.Parse(ACommand: string);
var
  Match: TMatch;
begin
  Match := TRegex.Match(ACommand, '^(.+?)(?:$|\s)');
  if Match.Success then
  begin 
    FTeam := Match.Groups[1].Value;
    ParseSuccess(Match.Groups[1].Length);
  end;
end;

{ TMinecraftEntityAnchor.TParser }

class function TMinecraftEntityAnchor.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftEntityAnchor;
end;

class function TMinecraftEntityAnchor.TParser.GetParserString: string;
begin
  Result := 'minecraft:entity_anchor';
end;

{ TMinecraftRange.TParser }

class function TMinecraftRange.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftRange;
end;

class function TMinecraftRange.TParser.GetParserString: string;
begin
  Result := 'minecraft:range';
end;

{ TMinecraftItemEnchantment.TParser }

class function TMinecraftItemEnchantment.TParser.GetParameterClass: TBrigadierParameterClass;
begin
  Result := TMinecraftItemEnchantment;
end;

class function TMinecraftItemEnchantment.TParser.GetParserString: string;
begin
  Result := 'minecraft:item_enchantment';
end;

initialization

TBrigadierBool.TParser.RegisterClass;
TBrigadierParser.RegisterClass(TBrigadierInteger.TParser);
TBrigadierParser.RegisterClass(TBrigadierFloat.TParser);
TBrigadierParser.RegisterClass(TBrigadierFloat.TDoubleParser);
TBrigadierParser.RegisterClass(TBrigadierString.TParser);

TBrigadierParser.RegisterClass(TMinecraftMessage.TParser);
TBrigadierParser.RegisterClass(TMinecraftBlockPredicate.TParser);
TBrigadierParser.RegisterClass(TMinecraftObjectiveCriteria.TParser);
TBrigadierParser.RegisterClass(TMinecraftColor.TParser);
TBrigadierParser.RegisterClass(TMinecraftComponent.TParser);
TBrigadierParser.RegisterClass(TMinecraftBlockPos.TParser);
TBrigadierParser.RegisterClass(TMinecraftObjective.TParser);
TBrigadierParser.RegisterClass(TMinecraftFunction.TParser);
TBrigadierParser.RegisterClass(TMinecraftNBTPath.TParser);
TBrigadierParser.RegisterClass(TMinecraftItemSlot.TParser);
TBrigadierParser.RegisterClass(TMinecraftScoreboardSlot.TParser);
TBrigadierParser.RegisterClass(TMinecraftTeam.TParser);
TBrigadierParser.RegisterClass(TMinecraftSwizzle.TParser);
TBrigadierParser.RegisterClass(TMinecraftMobEffect.TParser);
TBrigadierParser.RegisterClass(TMinecraftParticle.TParser);
TBrigadierParser.RegisterClass(TMinecraftNBT.TParser);
TBrigadierParser.RegisterClass(TMinecraftItemStack.TParser);
TBrigadierParser.RegisterClass(TMinecraftResourceLocation.TParser);
TBrigadierParser.RegisterClass(TMinecraftItemPredicate.TParser);
TBrigadierParser.RegisterClass(TMinecraftScoreHolder.TParser);
TBrigadierParser.RegisterClass(TMinecraftOperation.TParser);
TBrigadierParser.RegisterClass(TMinecraftRotation.TParser);
TBrigadierParser.RegisterClass(TMinecraftVec3.TParser);
TBrigadierParser.RegisterClass(TMinecraftVec2.TParser);
TBrigadierParser.RegisterClass(TMinecraftBlockState.TParser);
TBrigadierParser.RegisterClass(TMinecraftGameProfile.TParser);
TBrigadierParser.RegisterClass(TMinecraftEntity.TParser);
TBrigadierParser.RegisterClass(TMinecraftEntityAnchor.TParser);
TBrigadierParser.RegisterClass(TMinecraftRange.TParser);
TBrigadierParser.RegisterClass(TMinecraftItemEnchantment.TParser);

end.
