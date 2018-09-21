unit TakeItEasy.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Pengine.GLForm,
  Pengine.GUI,
  Pengine.GLProgram,
  Pengine.SpriteSystem,
  Pengine.GLState,
  Pengine.Collections,
  Pengine.Color,
  Pengine.TimeManager,
  Pengine.GLEnums,
  Pengine.Texture,
  Pengine.GUIControls,

  TakeItEasy.Game,
  TakeItEasy.Control,
  TakeItEasy.NeuralNet;

type

  TSpriteGLProgram = class(TSpriteGLProgramBase)
  protected
    class procedure GetData(out AName: string; out AResource: Boolean); override;

  end;

  TfrmMain = class(TGLForm)
  public const

    LogName = 'progress.csv';

  private
    FSpriteGLProgram: TGLProgram;
    FGUI: TGUI;
    FGame: TTakeItEasy;
    FGameControlA: TTakeItEasyControl;
    FGameControlB: TTakeItEasyControl;
    FFitnessLabel: TLabel;
    FScoreA: TLabel;
    FScoreB: TLabel;
    FEvolver: TTakeItEasyEvolver;
    FAutoEvolve: Boolean;
    FGeneration: Integer;

    procedure UpdateFPS;
    procedure GameUpdate;

    procedure KeyDown; reintroduce;
    procedure Evolve;

  public
    procedure Init; override;
    procedure Finalize; override;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


procedure TfrmMain.Finalize;
begin
  FGame.Free;
  FEvolver.Free;
  FGUI.Free;
  TSpriteGLProgram.Release(GLState.ResParam);
end;

procedure TfrmMain.GameUpdate;
begin
  if FAutoEvolve then
    Evolve;

  if FGameControlA.Board <> nil then
    FScoreA.Caption := FGameControlA.Board.CalculateScore.ToString;
  if FGameControlB.Board <> nil then
    FScoreB.Caption := FGameControlB.Board.CalculateScore.ToString;

  if FGame.State = gsTurnEnd then
    FGame.Turn;
end;

procedure TfrmMain.Init;
begin
  Context.VSync := True;
  // Context.Samples := Context.MaxSamples;

  FSpriteGLProgram := TSpriteGLProgram.Make(GLState.ResParam);

  FGUI := TGUI.Create(Game, FSpriteGLProgram);
  FGUI.TextureAtlas.AddFromResource('tie_tile', 'TIE_TILE');
  FGUI.TextureAtlas.AddFromResource('tie_tile_background', 'TIE_TILE_BACKGROUND');
  FGUI.TextureAtlas.AddFromResource('tie_line', 'TIE_LINE');
  FGUI.TextureAtlas.AddFromResource('tie_values', 'TIE_VALUES');
  FGUI.TextureAtlas.AddFromResource('font', 'FONT');
  FGUI.TextureAtlas.Texture.MagFilter := magNearest;
  // FGUI.TextureAtlas.Texture.MagFilter := minLinear;

  FFitnessLabel := FGUI.Add<TLabel>;
  FFitnessLabel.Location.Scale := 0.25;
  FFitnessLabel.OriginX := oxCenter;
  FFitnessLabel.OriginY := oyTop;

  FScoreA := FGUI.Add<TLabel>;
  FScoreA.Location.Scale := 0.25;
  FScoreA.OriginX := oxLeft;
  FScoreA.OriginY := oyTop;

  FScoreB := FGUI.Add<TLabel>;
  FScoreB.Location.Scale := 0.25;
  FScoreB.OriginX := oxRight;
  FScoreB.OriginY := oyTop;

  FGameControlA := FGUI.Add<TTakeItEasyControl>;
  FGameControlA.Location.Scale := 1.7;
  FGameControlA.OriginX := oxLeft;

  FGameControlB := FGUI.Add<TTakeItEasyControl>;
  FGameControlB.Location.Scale := 1.5;
  FGameControlB.OriginX := oxRight;

  FGame := TTakeItEasy.Create;

  FEvolver := TTakeItEasyEvolver.Create(500);

  Game.Timer.OnFPSUpdate.Add(UpdateFPS);
  Game.OnUpdate.Add(GameUpdate);
  Input.OnKeyDown.Add(KeyDown);

  TFile.WriteAllText(LogName, '"Generation";"Top";"Average";"Worst";"Second Try"' + sLineBreak);
end;

procedure TfrmMain.KeyDown;
begin
  FAutoEvolve := not FAutoEvolve;
end;

procedure TfrmMain.Evolve;
var
  NetPlayer: TTakeItEasyNeuralNet.TPlayer;
  Net: TTakeItEasyNeuralNet;
  Avg: Single;
begin
  FGameControlA.Player := nil;
  FGameControlB.Board := nil;
  FGame.RemovePlayers;

  if FEvolver.Simulated then
    FEvolver.Evolve;
  FEvolver.Simulate;

  Avg := 0;
  for Net in FEvolver.Nets do
    Avg := Avg + Net.Fitness;
  Avg := Avg / FEvolver.Nets.Count;

  FFitnessLabel.Caption := Format('Top: %g | Avg: %.2g', [FEvolver.Best.Fitness, Avg], TFormatSettings.Invariant);

  // FGameControlA.Player := FGame.AddPlayer<TTakeItEasyControl.TPlayer>;
  NetPlayer := FGame.AddPlayer<TTakeItEasyNeuralNet.TPlayer>;
  NetPlayer.Net := FEvolver.Best;
  FGameControlB.Board := NetPlayer.Board;

  FGame.Start;
  FGame.PlayAll;

  Inc(FGeneration);
  // '"Generation" "Top" "Average" "Worst" "Second Try"');
  TFile.AppendAllText(LogName, Format('%d;%f;%f;%f;%d' + sLineBreak,
    [FGeneration, FEvolver.Best.Fitness, Avg, FEvolver.Worst.Fitness, NetPlayer.Board.CalculateScore]));
end;

procedure TfrmMain.UpdateFPS;
begin
  Caption := Format('Take it Easy AI - FPS: %.0f', [Context.FPS]);
end;

{ TSpriteGLProgram }

class procedure TSpriteGLProgram.GetData(out AName: string; out AResource: Boolean);
begin
  AResource := True;
  if AResource then
    AName := 'SPRITE'
  else
    AName := 'Data\Shaders\sprite';
end;

end.
