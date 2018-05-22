unit BrigadierDefine;

interface

// Things to note:
// - redirect and children are mutually exclusive
// - keep literals and arguments in separate lists, to get string-hash boost for literals, but preserve order of
//   argument parsers. for parsing, first check for literals, and then check for arguments by trying to parse in order

uses
  System.Generics.Collections, Lists, JSON, Classes, SysUtils, Dialogs, IntfBase;

type

  TBrigadierSystem = class;
  TBrigadierChild = class;

  TBrigadierBasicParser = class
  
  end;
                    
  TBrigadierLiteralParser = class(TBrigadierBasicParser)
  private
    FLiteral: TBrigadierLiteral;
    
  public
    property Literal: TBrigadierLiteral read FLiteral;
  
  end;

  TBrigadierWhitespaceParser = class(TBrigadierBasicParser)
  private
    FText: string;

  public
    property Text: string read FText;
    
  end;
  
  { TBrigadierParser }

  TBrigadierParserClass = class of TBrigadierParser;

  TBrigadierParser = class(TBrigadierBasicParser)
  private type
    TParserMap = TStringMap<TBrigadierParserClass>;

  private class var
    FParserMap: TParserMap;
    
  protected 
    class function GetParserName: string; virtual; abstract;
    
  public         
    class constructor Create;
    class destructor Destroy;
  
    class procedure RegisterParser(AParser: TBrigadierParserClass); overload; static; 
    class procedure RegisterParser(AParsers: TArray<TBrigadierParserClass>); overload; static; 
        
    constructor Create(AProperties: TJSONObject); virtual;
    class function CreateTyped(AData: TJSONObject): TBrigadierParser;

  end;
               
  { TBrigadierParseResult }

  TBrigadierParseResult = class
  public type
    TParserList = TRefArray<TBrigadierBasicParser>;
  
  private
    FBrigadierSystem: TBrigadierSystem;
    FParserList: TParserList;

  public
    constructor Create(ABrigadierSystem: TBrigadierSystem; ACommand: string);
    destructor Destroy; override;
  
    function Format: string;
  
  end;

  { TBrigadierMinecraftResourceLocationParser }
  
  TBrigadierMinecraftResourceLocationParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierStringParser }
  
  TBrigadierStringParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftEntityParser }

  TBrigadierMinecraftEntityParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftMessageParser }
  
  TBrigadierMinecraftMessageParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftGameProfileParser }
  
  TBrigadierMinecraftGameProfileParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftNBTParser }
  
  TBrigadierMinecraftNBTParser = class(TBrigadierParser)  
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftBlockPosParser }
  
  TBrigadierMinecraftBlockPosParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierIntegerParser }
  
  TBrigadierIntegerParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftItemParser }
  
  TBrigadierMinecraftItemParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;
  
  { TBrigadierMinecraftBlockParser }
  
  TBrigadierMinecraftBlockParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierFloatParser }
  
  TBrigadierFloatParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftNBTPathParser }

  TBrigadierMinecraftNBTPathParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierBoolParser }
  
  TBrigadierBoolParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;
  
  { TBrigadierMinecraftSwizzleParser }
  
  TBrigadierMinecraftSwizzleParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftObjectiveParser }
  
  TBrigadierMinecraftObjectiveParser = class(TBrigadierParser)  
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftScoreHolderParser }
  
  TBrigadierMinecraftScoreHolderParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftVec3Parser }
  
  TBrigadierMinecraftVec3Parser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftParticleParser }
  
  TBrigadierMinecraftParticleParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftItemSlotParser }
  
  TBrigadierMinecraftItemSlotParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftObjectiveCriteriaParser }
  
  TBrigadierMinecraftObjectiveCriteriaParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftScoreboardSlotParser }
  
  TBrigadierMinecraftScoreboardSlotParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftOperationParser }
  
  TBrigadierMinecraftOperationParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftVec2Parser }
  
  TBrigadierMinecraftVec2Parser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftTeamParser }
  
  TBrigadierMinecraftTeamParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftColorParser }
  
  TBrigadierMinecraftColorParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftRotationParser }
  
  TBrigadierMinecraftRotationParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierMinecraftComponentParser }
  
  TBrigadierMinecraftComponentParser = class(TBrigadierParser)
  protected
    class function GetParserName: string; override;
  end;

  { TBrigadierChild }

  TBrigadierLiteral = class;
  TBrigadierArgument = class;
  
  TBrigadierChildClass = class of TBrigadierChild;

  TBrigadierChild = class abstract
  private type
    TTypeMap = TStringMap<TBrigadierChildClass>;
    TLiteralChildMap = TStringObjectMap<TBrigadierLiteral>;
    TArgumentChildMap = TStringObjectMap<TBrigadierArgument>;
    TRedirect = TGenericArray<string>;

  private class var
    FTypeMap: TTypeMap;

  private
    FRoot: TBrigadierSystem;
    FName: string;
    FExecutable: Boolean;
    FLiteralChildren: TLiteralChildMap;
    FArgumentChildren: TArgumentChildMap;
    FRedirect: TRedirect;
    
    function GetLiteralChild(AName: string): TBrigadierLiteral;
    function GetRedirect: TBrigadierChild;

    procedure GenerateChildMaps(ANode: TJSONObject);

  protected
    class function GetTypeString: string; virtual; abstract;

  public
    class constructor Create;
    class destructor Destroy;
  
    class procedure RegisterSubClass(ASubClass: TBrigadierChildClass); overload; static;
    class procedure RegisterSubClass(ASubClasses: TArray<TBrigadierChildClass>); overload; static;
    
    class function CreateTyped(AData: TJSONPair; ARoot: TBrigadierSystem): TBrigadierChild; static;

    constructor Create(AData: TJSONPair; ARoot: TBrigadierSystem); virtual;
    destructor Destroy; override;

    property Root: TBrigadierSystem read FRoot;
    property Name: string read FName;
    property Executable: Boolean read FExecutable;
    property LiteralChildren[AName: string]: TBrigadierLiteral read GetLiteralChild; default;
    property Redirect: TBrigadierChild read GetRedirect;
    
  end;
  
  { TBrigadierLiteral }

  TBrigadierLiteral = class(TBrigadierChild)
  protected
    class function GetTypeString: string; override;
  end;

  { TBrigadierArgument }

  TBrigadierArgument = class(TBrigadierChild)
  private
    FParser: TBrigadierParser;
    
  protected
    class function GetTypeString: string; override;

  public
    constructor Create(AData: TJSONPair; ARoot: TBrigadierSystem); override;
    destructor Destroy; override;
    
    property Parser: TBrigadierParser read FParser;
  
  end;
                  
  { TBrigadierSystem }

  TBrigadierSystem = class(TBrigadierChild)
  private type
    
  protected
    class function GetTypeString: string; override;
    
  public
    constructor Create(AData: TJSONObject); reintroduce; overload;
    constructor Create(AFileName: string); reintroduce; overload;

    function Parse(ACommand: string): TBrigadierParseResult;
    
  end;

implementation
    
{ TBrigadierSystem }

constructor TBrigadierSystem.Create(AData: TJSONObject);
var
  Pair: TJSONPair;
begin
  Pair := TJSONPair.Create;
  AData.Owned := False;
  Pair.JsonValue := AData;
  try
    Create(Pair, nil);
  finally
    Pair.Free;
  end;
end;

class function TBrigadierSystem.GetTypeString: string;
begin
  Result := 'root';
end;

function TBrigadierSystem.Parse(ACommand: string): TBrigadierParseResult;
begin
  Result := TBrigadierParseResult.Create(Self, ACommand);
end;

constructor TBrigadierSystem.Create(AFileName: string);
var                       
  FileStream: TFileStream;
  JSONData: TBytes;
  JSONObject: TJSONObject;
begin
  FileStream := nil;
  JSONObject := nil;
  try
    FileStream := TFileStream.Create(AFileName, fmOpenRead);
    SetLength(JSONData, FileStream.Size);
    FileStream.ReadBuffer(JSONData, FileStream.Size);

    JSONObject := TJSONObject.Create;
    JSONObject.Parse(JSONData, 0);

    Create(JSONObject);
  finally
    FileStream.Free;
    JSONObject.Free;
  end;
end;

{ TBrigadierChild }

constructor TBrigadierChild.Create(AData: TJSONPair; ARoot: TBrigadierSystem);
var
  JsonData: TJSONObject;
  Executable: TJSONValue;
  Redirect: TJSONArray;
  RedirectNode: TJSONValue;
begin        
  FRoot := ARoot;

  if AData.JsonString <> nil then
    FName := AData.JsonString.Value;

  JsonData := AData.JsonValue as TJSONObject;

  GenerateChildMaps(JsonData);
  
  Executable := JsonData.Values['executable'];
  if Executable = nil then
    FExecutable := False
  else if Executable is TJSONBool then
    FExecutable := TJSONBool(Executable).AsBoolean
  else
    raise Exception.Create('executable must be bool');

  Redirect := JsonData.Values['redirect'] as TJSONArray;
  if (Redirect <> nil) and (Redirect.Count > 0) then
  begin
    FRedirect := TGenericArray<string>.Create;
    for RedirectNode in Redirect do
      FRedirect.Add((RedirectNode as TJSONString).Value);
  end;
end;

class constructor TBrigadierChild.Create;
begin
  FTypeMap := TTypeMap.Create;
end;

class function TBrigadierChild.CreateTyped(AData: TJSONPair; ARoot: TBrigadierSystem): TBrigadierChild;
var
  Data: TJSONObject;
  ChildType: TJSONString;
  ChildClass: TBrigadierChildClass;
begin
  Data := AData.JsonValue as TJSONObject;
  ChildType := Data.Values['type'] as TJSONString;
  if ChildType = nil then
    raise Exception.Create('Child type missing') // TODO: Exception
  else if FTypeMap.Get(ChildType.Value, ChildClass) then
    Result := ChildClass.Create(AData, ARoot)
  else
    raise Exception.CreateFmt('Unknown child type: %s', [ChildType.Value]); // TODO: Exception
end;

destructor TBrigadierChild.Destroy;
begin            
  FLiteralChildren.Free;
  FArgumentChildren.Free;
  FRedirect.Free;
  inherited;
end;

procedure TBrigadierChild.GenerateChildMaps(ANode: TJSONObject);
var
  ChildrenNode: TJSONObject;
  ChildNode: TJSONPair;
  Children: TRefArray<TBrigadierChild>;
  LiteralCount, ArgumentCount: Integer;
  Child: TBrigadierChild;
begin
  ChildrenNode := ANode.Values['children'] as TJSONObject;

  LiteralCount := 0;
  ArgumentCount := 0;
  
  if ChildrenNode = nil then
    Exit;
    
  Children := TRefArray<TBrigadierChild>.Create;

  try
    for ChildNode in ChildrenNode do
    begin
      Child := TBrigadierChild.CreateTyped(ChildNode, Root);
      if Child is TBrigadierLiteral then
        Inc(LiteralCount)
      else if Child is TBrigadierArgument then
        Inc(ArgumentCount)
      else
        raise Exception.Create('Unknown Child-Type');
      Children.Add(Child);
    end;
  
    if LiteralCount > 0 then
      FLiteralChildren := TLiteralChildMap.Create;
    if ArgumentCount > 0 then
      FArgumentChildren := TArgumentChildMap.Create;
  
    for Child in Children do
    begin
      if Child is TBrigadierLiteral then
        FLiteralChildren[Child.Name] := TBrigadierLiteral(Child)
      else if Child is TBrigadierArgument then
        FArgumentChildren[Child.Name] := TBrigadierArgument(Child)
      else
        raise Exception.Create('Unknown Child-Type');   
    end;
  finally   
    Children.Free;
  end;
end;

function TBrigadierChild.GetLiteralChild(AName: string): TBrigadierLiteral;
begin
  if (FLiteralChildren = nil) or not FLiteralChildren.Get(AName, Result) then
    raise Exception.Create('Child not found'); // TODO: Exception
end;

function TBrigadierChild.GetRedirect: TBrigadierChild;
var
  Redirection: string;
begin
  if FRedirect = nil then
    Exit(nil);
  Result := Self;
  for Redirection in FRedirect do
    Result := Result[Redirection];
end;

class destructor TBrigadierChild.Destroy;
begin
  FTypeMap.Free;
end;

class procedure TBrigadierChild.RegisterSubClass(ASubClass: TBrigadierChildClass);
begin
  FTypeMap[ASubClass.GetTypeString] := ASubClass;
end;

class procedure TBrigadierChild.RegisterSubClass(ASubClasses: TArray<TBrigadierChildClass>);
var
  SubClass: TBrigadierChildClass;
begin
  for SubClass in ASubClasses do
    RegisterSubClass(SubClass);  
end;

{ TBrigadierLiteral }

class function TBrigadierLiteral.GetTypeString: string;
begin
  Result := 'literal';
end;

{ TBrigadierArgument }

constructor TBrigadierArgument.Create(AData: TJSONPair; ARoot: TBrigadierSystem);
begin
  inherited;
  FParser := TBrigadierParser.CreateTyped(AData.JsonValue as TJSONObject);
end;

destructor TBrigadierArgument.Destroy;
begin
  FParser.Free;
  inherited;
end;

class function TBrigadierArgument.GetTypeString: string;
begin
  Result := 'argument';
end;

{ TBrigadierParser }

class constructor TBrigadierParser.Create;
begin
  FParserMap := TParserMap.Create;
end;

constructor TBrigadierParser.Create(AProperties: TJSONObject);
begin
  // nothing by default  
end;

class function TBrigadierParser.CreateTyped(AData: TJSONObject): TBrigadierParser;
var
  ParserName: TJSONString;
  ParserClass: TBrigadierParserClass;
  Properties: TJSONObject;
begin
  ParserName := AData.Values['parser'] as TJSONString;
  if ParserName = nil then
    raise Exception.Create('Parser missing') // TODO: Exception
  else if FParserMap.Get(ParserName.Value, ParserClass) then
  begin
    Properties := AData.Values['properties'] as TJSONObject;
    if Properties = nil then
    begin
      Properties := TJSONObject.Create;
      Result := ParserClass.Create(Properties);
      Properties.Free;
    end
    else
      Result := ParserClass.Create(Properties);
  end
  else
    // raise Exception.CreateFmt('Unknown parser: %s', [ParserName.Value]); // TODO: Exception 
  begin
    ShowMessage(ParserName.Value);
    Result := nil;
  end;
end;

class destructor TBrigadierParser.Destroy;
begin
  FParserMap.Free;
end;

class procedure TBrigadierParser.RegisterParser(AParser: TBrigadierParserClass);
begin
  FParserMap[AParser.GetParserName] := AParser;
end;

class procedure TBrigadierParser.RegisterParser(AParsers: TArray<TBrigadierParserClass>);
var
  Parser: TBrigadierParserClass;
begin
  for Parser in AParsers do
    RegisterParser(Parser);
end;

{ TBrigadierMinecraftResourceLocationParser }

class function TBrigadierMinecraftResourceLocationParser.GetParserName: string;
begin
  Result := 'minecraft:resource_location';
end;

{ TBrigadierMinecraftStringParser }

class function TBrigadierStringParser.GetParserName: string;
begin
  Result := 'brigadier:string';
end;

{ TBrigadierEntityParser }

class function TBrigadierMinecraftEntityParser.GetParserName: string;
begin
  Result := 'minecraft:entity';
end;

{ TBrigadierMinecraftMessageParser }

class function TBrigadierMinecraftMessageParser.GetParserName: string;
begin
  Result := 'minecraft:message';
end;

{ TBrigadierMinecraftGameProfileParser }

class function TBrigadierMinecraftGameProfileParser.GetParserName: string;
begin
  Result := 'minecraft:game_profile';
end;

{ TBrigadierMinecraftNBTParser }

class function TBrigadierMinecraftNBTParser.GetParserName: string;
begin
  Result := 'minecraft:nbt';
end;

{ TBrigadierMinecraftBlockPosParser }

class function TBrigadierMinecraftBlockPosParser.GetParserName: string;
begin
  Result := 'minecraft:block_pos';
end;

{ TBrigadierIntegerParser }

class function TBrigadierIntegerParser.GetParserName: string;
begin
  Result := 'brigadier:integer';
end;

{ TBrigadierMinecraftItemParser }

class function TBrigadierMinecraftItemParser.GetParserName: string;
begin
  Result := 'minecraft:item';
end;

{ TBrigadierMinecraftBlockParser }

class function TBrigadierMinecraftBlockParser.GetParserName: string;
begin
  Result := 'minecraft:block';
end;

{ TBrigadierFloatParser }

class function TBrigadierFloatParser.GetParserName: string;
begin
  Result := 'brigadier:float';
end;

{ TBrigadierMinecraftNBTPathParser }

class function TBrigadierMinecraftNBTPathParser.GetParserName: string;
begin
  Result := 'minecraft:nbt_path';
end;

{ TBrigadierBoolParser }

class function TBrigadierBoolParser.GetParserName: string;
begin
  Result := 'brigadier:bool';
end;

{ TBrigadierMinecraftSwizzleParser }

class function TBrigadierMinecraftSwizzleParser.GetParserName: string;
begin
  Result := 'minecraft:swizzle';
end;

{ TBrigadierMinecraftObjectiveParser }

class function TBrigadierMinecraftObjectiveParser.GetParserName: string;
begin
  Result := 'minecraft:objective';
end;

{ TBrigadierMinecraftScoreHolderParser }

class function TBrigadierMinecraftScoreHolderParser.GetParserName: string;
begin
  Result := 'minecraft:score_holder';
end;

{ TBrigadierMinecraftVec3Parser }

class function TBrigadierMinecraftVec3Parser.GetParserName: string;
begin
  Result := 'minecraft:vec3';
end;

{ TBrigadierMinecraftParticleParser }

class function TBrigadierMinecraftParticleParser.GetParserName: string;
begin
  Result := 'minecraft:particle';
end;

{ TBrigadierMinecraftItemSlotParser }

class function TBrigadierMinecraftItemSlotParser.GetParserName: string;
begin
  Result := 'minecraft:item_slot';
end;

{ TBrigadierMinecraftObjectiveCriteriaParser }

class function TBrigadierMinecraftObjectiveCriteriaParser.GetParserName: string;
begin
  Result := 'minecraft:objective_criteria';
end;

{ TBrigadierMinecraftScoreboardSlotParser }

class function TBrigadierMinecraftScoreboardSlotParser.GetParserName: string;
begin
  Result := 'minecraft:scoreboard_slot';
end;

{ TBrigadierMinecraftOperationParser }

class function TBrigadierMinecraftOperationParser.GetParserName: string;
begin
  Result := 'minecraft:operation';
end;

{ TBrigadierMinecraftVec2Parser }

class function TBrigadierMinecraftVec2Parser.GetParserName: string;
begin
  Result := 'minecraft:vec2';
end;

{ TBrigadierMinecraftTeamParser }

class function TBrigadierMinecraftTeamParser.GetParserName: string;
begin
  Result := 'minecraft:team';
end;

{ TBrigadierMinecraftColorParser }

class function TBrigadierMinecraftColorParser.GetParserName: string;
begin
  Result := 'minecraft:color';
end;

{ TBrigadierMinecraftRotationParser }

class function TBrigadierMinecraftRotationParser.GetParserName: string;
begin
  Result := 'minecraft:rotation';
end;

{ TBrigadierMinecraftComponentParser }

class function TBrigadierMinecraftComponentParser.GetParserName: string;
begin
  Result := 'minecraft:component';
end;

{ TBrigadierParseResult }

constructor TBrigadierParseResult.Create(ABrigadierSystem: TBrigadierSystem; ACommand: string);
var
  Child: TBrigadierChild;
begin
  FBrigadierSystem := ABrigadierSystem;
  FSegments := TSegmentList.Create;

  Child := ABrigadierSystem;
  
  while ACommand <> '' do
  begin
    FSegments.Add();    
  end;
end;

destructor TBrigadierParseResult.Destroy;
begin
  FSegments.Free;
  inherited;
end;

function TBrigadierParseResult.Format: string;
var
  Child: TBrigadierChild;
begin
  Result := '';
  for Child in FChildren do
  begin
    Result := Result + Child.Name;  
  end;
end;

initialization

TBrigadierParser.RegisterParser([
  TBrigadierMinecraftResourceLocationParser,
  TBrigadierStringParser,
  TBrigadierMinecraftEntityParser,
  TBrigadierMinecraftMessageParser,
  TBrigadierMinecraftGameProfileParser,
  TBrigadierMinecraftNBTParser,
  TBrigadierMinecraftBlockPosParser,
  TBrigadierIntegerParser,
  TBrigadierMinecraftItemParser,
  TBrigadierMinecraftBlockParser,
  TBrigadierFloatParser,
  TBrigadierMinecraftNBTPathParser,
  TBrigadierBoolParser,
  TBrigadierMinecraftSwizzleParser,
  TBrigadierMinecraftObjectiveParser,
  TBrigadierMinecraftScoreHolderParser,
  TBrigadierMinecraftVec3Parser,
  TBrigadierMinecraftParticleParser,
  TBrigadierMinecraftItemSlotParser,
  TBrigadierMinecraftObjectiveCriteriaParser,
  TBrigadierMinecraftScoreboardSlotParser,
  TBrigadierMinecraftOperationParser,
  TBrigadierMinecraftVec2Parser,
  TBrigadierMinecraftTeamParser,
  TBrigadierMinecraftColorParser,
  TBrigadierMinecraftRotationParser,
  TBrigadierMinecraftComponentParser
  ]);

TBrigadierChild.RegisterSubClass([
  TBrigadierSystem,
  TBrigadierLiteral,
  TBrigadierArgument
  ]);
  
end.

