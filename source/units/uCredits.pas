unit uCredits;

interface

uses
  ElysionApplication,
  ElysionAudio,
  ElysionColor,
  ElysionTypes,
  ElysionScene,
  ElysionGraphics,
  ElysionInput,
  ElysionGUI,
  ElysionTrueTypeFont,

  uBasic,
  uGlobal,
  uConfig;

type
  TCredits = class(TelScene)
  private
    fButton: TelButton;
    fSmallFont: TelTrueTypeFont;
  public
    constructor Create; Override;
    destructor Destroy; Override;
	
    procedure Render; Override;
    procedure Update(dt: Double); Override;
    procedure HandleEvents; //Override;
  published
    property SmallFont: TelTrueTypeFont read fSmallFont write fSmallFont;
    property Button: TelButton read fButton write fButton;
  end;

implementation

constructor TCredits.Create;
begin
  inherited;

  SmallFont := TelTrueTypeFont.Create;
  SmallFont.LoadFromFile(GetStdFont, 10);
  SmallFont.Color := Color.clWhite;
  SmallFont.RenderStyle := rtBlended;

  Button := TelButton.Create;
  Button.LoadFromFile(GetResImgPath + 'button.png', GetStdFont);
  Button.Caption := 'Back';
  Button.Position := makeV3f(ActiveWindow.Width - 300 - 80 + 16 + 8, 450 + 16);
  
  Button.TextLabel.Color := makeCol(0, 0, 0);
end;

destructor TCredits.Destroy;
begin


  inherited;
end;

procedure TCredits.Render;
begin
  GUI.RoundedBox(makeRect(ActiveWindow.Width - 300 - 80 + 16, 60 + 16, 300, 450), makeCol(0, 0, 0, 128), 8);

  SmallFont.TextOut(makeV3f(ActiveWindow.Width - 300 - 80 + 16 + 8, 60 + 16 + 8), 'Credits stuff');

  Button.Draw();
end;

procedure TCredits.Update(dt: Double);
begin

end;

procedure TCredits.HandleEvents;
begin
  if Button.Click or Input.XBox360Controller.B() then
  begin
    GameState := gsMainMenu;
  end;
end;

end.
