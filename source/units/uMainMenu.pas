unit uMainMenu;

interface

uses
  ElysionColor,
  ElysionTypes,
  ElysionScene,
  ElysionGraphics,
  ElysionAudio,
  ElysionApplication,
  ElysionMenu,
  ElysionLabel,
  ElysionGUI,
  ElysionInput,
  ElysionAnimator,
  ElysionAssets,

  SysUtils,
  uBasic,
  uGlobal,
  uConfig;

type
  TMainMenu = class(TelScene)
  private
    fMenu: TelMenu;
    fLabel: TelLabel;

    fNewGameClick: Boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;
	
    procedure Render; Override;
    procedure Update(dt: Double); Override;
    procedure HandleEvents; Override;
  published
    property Menu: TelMenu read fMenu write fMenu;

    property NewGameClick: Boolean read fNewGameClick write fNewGameClick;
  end;

implementation

constructor TMainMenu.Create;
var
  i: Integer;
begin
  inherited;


  Menu := TelMenu.Create;
  Menu.setButtons(GetResImgPath + 'button.png', GetStdFont, 15, ['New game', 'Credits', 'Options', 'Quit']);
  Menu.Spacing := 16;

  Menu.Position := makeV3f((ActiveWindow.Width - Menu.Width) - 32, (ActiveWindow.Height - Menu.Height) div 2);

  for i := 0 to Menu.Count - 1 do
    Menu.Items[i].TextLabel.Color := makeCol(0, 0, 0);


  fLabel := TelLabel.Create;
  fLabel.LoadFromFile(GetStdFont);
  fLabel.Caption := '(C) Your Name Here';
  fLabel.Size := 14;
  fLabel.Color := makeCol(255, 255, 255);
  fLabel.Position := makeV3f(8, ActiveWindow.Height - 25);


end;

destructor TMainMenu.Destroy;
begin
  Menu.Destroy;

  inherited;
end;

procedure TMainMenu.Render;
begin
  inherited;


  fLabel.Draw;

  GUI.RoundedBox(makeRect(Menu.Position.X - 8, Menu.Position.Y - 8, Menu.Width + 16, Menu.Height), makeCol(0, 0, 0, 128), 8);
  Menu.Draw;

end;

procedure TMainMenu.Update(dt: Double);
var
  i: Integer;
begin
  inherited;

  fLabel.Update();

  Menu.Update(dt);
end;

procedure TMainMenu.HandleEvents;
begin
  if Menu.ButtonClick('New game') or Input.XBox360Controller.Start() then
  begin
    //GameState := gsGame;
    SceneDirector.SwitchTo('game');
    //fNewGameClick := true;
  end;


  //if Menu.OnButtonClick('How to play') then GameState := gsInstructions;
  if Menu.ButtonClick('Credits') then
  begin
    //GameState := gsCredits;
    SceneDirector.SwitchTo('credits');
  end;

  if Menu.ButtonClick('Options') then
  begin
    //GameState := gsOptions;
    SceneDirector.SwitchTo('options');
  end;

  if Menu.ButtonClick('Quit') or Input.XBox360Controller.B() then
    Application.Quit;

end;

end.
