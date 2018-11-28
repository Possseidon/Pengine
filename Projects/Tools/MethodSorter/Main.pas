unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.RegularExpressions,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  Pengine.TimeManager,
  Pengine.Hasher,
  Pengine.HashCollections,
  Pengine.Collections,
  Pengine.IntMaths;

type

  TPatternType = (
    ptInterface,
    ptImplementation,
    ptStructure,
    ptEnd,
    ptMethod
    );

  TForm2 = class(TForm)
    btnOpen: TButton;
    pnlMain: TPanel;
    spltMain: TSplitter;
    gbInterface: TGroupBox;
    gbImplementation: TGroupBox;
    odOpenFile: TOpenDialog;
    lbInterface: TListBox;
    lbImplementation: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure pnlMainResize(Sender: TObject);
    procedure spltMainMoved(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    FSplitAspect: Single;
    FSourceFile: TStringList;
    FPatterns: array [TPatternType] of TRegex;

    function MainWidth: Integer;

    procedure LoadFile(AFileName: string);

  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

const

  Patterns: array [TPatternType] of string = (
    '^ *interface.*$',
    '^ *implementation.*$',
    '^ *(\w+) *= *(?:class|record).*$',
    '^ *end;.*$',
    '^ *(?:function|procedure|constructor|destructor) +([\w.]+).*$'
    );

implementation

{$R *.dfm}


function TForm2.MainWidth: Integer;
begin
  Result :=
    pnlMain.Width -
    gbInterface.Margins.Left -
    gbInterface.Margins.Right -
    gbImplementation.Margins.Left -
    gbImplementation.Margins.Right -
    spltMain.Width;
end;

procedure TForm2.LoadFile(AFileName: string);
type
  TClassPath = TArray<string>;
  TClassOrder = TArray<string>;
  TMethods = TToObjectMap<string, TArray<string>, TStringHasher>;
  TMethodBounds = TMap<string, TIntBounds1, TStringHasher>;
  
var
  Line, Method: string;
  Match: TMatch;
  ImplementationReached: Boolean;
  ClassPath: TClassPath;
  ClassOrder: TClassOrder;
  Methods: TMethods;
  MethodBounds: TMethodBounds;
  MethodSplit: Integer;
  LastMethod: string;
  I: Integer;
  Pair: TMethodBounds.TPair;

  function ClassPathToString: string;
  var
    I: Integer;
  begin
    if ClassPath.Empty then
      Exit('');
    Result := ClassPath[0];
    for I := 1 to ClassPath.MaxIndex do
      Result := Result + '.' + ClassPath[I];
  end;

begin
  // StartTimer;

  FSourceFile.LoadFromFile(AFileName);

  ClassPath := TClassPath.Create;
  ClassOrder := TClassOrder.Create;
  MethodBounds := TMethodBounds.Create;

  Methods := TMethods.Create;
  Methods[ClassPathToString] := TArray<string>.Create;
  ClassOrder.Add(ClassPathToString);

  ImplementationReached := False;
  for I := 0 to FSourceFile.Count - 1 do
  begin
    Line := FSourceFile[I];    
    if Line.Trim.IsEmpty then
      Continue;

    if not ImplementationReached then
    begin
      if FPatterns[ptImplementation].IsMatch(Line) then
      begin
        ImplementationReached := True;
        Continue;
      end;

      Match := FPatterns[ptMethod].Match(Line);
      if Match.Success then
      begin
        Methods[ClassPathToString].Add(Match.Groups[1].Value);
        Continue;
      end;

      Match := FPatterns[ptStructure].Match(Line);
      if Match.Success then
      begin
        ClassPath.Add(Match.Groups[1].Value);
        ClassOrder.Add(ClassPathToString);
        Methods[ClassPathToString] := TArray<string>.Create;
      end
      else if FPatterns[ptEnd].IsMatch(Line) then
        ClassPath.RemoveLast;
    end
    else
    begin
      Match := FPatterns[ptMethod].Match(Line);
      if Match.Success then
      begin
        LastMethod := Match.Groups[1].Value;
        MethodBounds[LastMethod] := IBounds1(I, I);
      end;

      if FPatterns[ptEnd].IsMatch(Line) then
        MethodBounds[LastMethod] := IBounds1(MethodBounds[LastMethod].C1, I);
    end;
  end;

  lbInterface.Items.BeginUpdate;
  lbImplementation.Items.BeginUpdate;

  lbInterface.Clear;
  lbImplementation.Clear;

  for Line in ClassOrder do
  begin
    lbInterface.Items.Add('type ' + Line);
    for Method in Methods[Line] do
    begin
      lbInterface.Items.Add('  ' + Method);
    end;
    lbInterface.Items.Add('end;');
    lbInterface.Items.Add('');
  end;

  for Pair in MethodBounds do
  begin
    lbImplementation.Items.Add(Pair.Key + Pair.Value.ToString);
  end;

  lbInterface.Items.EndUpdate;
  lbImplementation.Items.EndUpdate;

  ClassOrder.Free;
  ClassPath.Free;
  Methods.Free;
  MethodBounds.Free;

  // ShowMessage(StopTimerGetString);

end;

procedure TForm2.btnOpenClick(Sender: TObject);
begin
  if not odOpenFile.Execute then
    Exit;
  LoadFile(odOpenFile.FileName);
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  PatternType: TPatternType;
begin
  FSourceFile := TStringList.Create;
  FSplitAspect := 0.5;

  for PatternType := Low(TPatternType) to High(TPatternType) do
    FPatterns[PatternType] := TRegex.Create('^' + Patterns[PatternType] + '$');

  LoadFile('Data\TestFile.pas');
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FSourceFile.Free;
end;

procedure TForm2.pnlMainResize(Sender: TObject);
var
  TotalWidth: Integer;
begin
  TotalWidth := MainWidth;
  gbInterface.Width := Round(FSplitAspect * TotalWidth);
  gbImplementation.Width := Round((1 - FSplitAspect) * TotalWidth);
end;

procedure TForm2.spltMainMoved(Sender: TObject);
begin
  FSplitAspect := gbInterface.Width / MainWidth;
end;

end.
