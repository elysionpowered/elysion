unit uCredits;

interface

uses
  ElysionApplication,
  ElysionAudio,
  ElysionColor,
  ElysionTypes,
  ElysionScene,
  ElysionGraphics,
  ElysionGraphicsProvider,
  ElysionInput,
  ElysionButton,
  ElysionGUI,
  ElysionTrueTypeFont,

  uBasic,
  uGlobal,
  uConfig;

type
  TCredits = class(TelScene)
  private
    fButton: TelButton;
    fFont: TelTrueTypeFont;
  public
    constructor Create; Override;
    destructor Destroy; Override;
	
    procedure Render(Graphics: IGraphicsProvider); Override;
    procedure Update(dt: Double = 0.0); Override;
    procedure HandleEvents; Override;
  published
    property Font: TelTrueTypeFont read fFont write fFont;
    property Button: TelButton read fButton write fButton;
  end;

implementation

constructor TCredits.Create;
begin
  inherited Create;

  Font := TelTrueTypeFont.Create;
  Font.LoadFromFile(GetStdFont, 14);
  Font.Color := TelColor.clWhite;
  Font.RenderStyle := rtBlended;

  Button := TelButton.Create;
  Button.LoadFromFile(GetResImgPath + 'button.png', GetStdFont);
  Button.TextLabel.Size := 15;
  Button.Caption := 'Back';
  Button.Position := TelVector3f.Create(ActiveWindow.Width - 500 - 80 + 16 + 16, 450 - 4);
  
  Button.TextLabel.Color := TelColor.Create(0, 0, 0);
end;

destructor TCredits.Destroy;
begin


  inherited Destroy;
end;

procedure TCredits.Render(Graphics: IGraphicsProvider);
begin
  inherited Render(Graphics);

  GUI.RoundedBox(TelRect.Create(ActiveWindow.Width - 500 - 80 + 16, 60 + 16, 500, 450), TelColor.Create(0, 0, 0, 128), 8);

  Font.TextOut(TelVector3f.Create(ActiveWindow.Width - 500 - 80 + 16 + 8, 60 + 16 + 8), 'Credits stuff \n Social buttons by Elegant Themes (http://www.elegantthemes.com) \n XNA Button Pack by Jeff Jenkins (http://sinnix.net/downloads/?did=2)');

  Button.Draw(Graphics);
end;

procedure TCredits.Update(dt: Double = 0.0);
begin
  inherited Update(dt);
end;

procedure TCredits.HandleEvents;
begin
  if Button.Click or Input.XBox360Controller.B() then
  begin
    Self.Parent.SwitchTo('MainMenu');
  end;
end;

end.
