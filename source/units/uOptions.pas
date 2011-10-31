unit uOptions;

interface

uses
  ElysionApplication,
  ElysionTypes,
  ElysionScene,
  ElysionGraphics,
  ElysionInput,
  ElysionMenu,
  ElysionGUI,

  uBasic,
  uGlobal,
  uConfig;

type
  TOptions = class(TelScene)
  private
    fMenu: TelMenu;
  public
    constructor Create; Override;
    destructor Destroy; Override;
	
    procedure Render; Override;
    procedure Update(dt: Double); Override;
    procedure HandleEvents; Override;
  published
    property Menu: TelMenu read fMenu write fMenu;
  end;

implementation

constructor TOptions.Create;
var
  i: Integer;
begin
  inherited;

  Menu := TelMenu.Create;
  Menu.setButtons(GetResImgPath + 'button.png', GetStdFont, 14, ['Music: On', 'Sound: On', 'Back']);
  Menu.Spacing := 16;
  Menu.Position := makeV3f((ActiveWindow.Width - Menu.Width) div 2, (ActiveWindow.Height - Menu.Height) div 2);
  //Menu.HoverAnimation := true;

  for i := 0 to Menu.Count - 1 do Menu.Items[i].TextLabel.Color := makeCol(0, 0, 0);

end;

destructor TOptions.Destroy;
begin
  Menu.Destroy;

  inherited;
end;

procedure TOptions.Render;
begin
  inherited;

  GUI.RoundedBox(makeRect(Menu.Position.X - 8, Menu.Position.Y - 8, Menu.Width + 16, Menu.Height), makeCol(0, 0, 0, 128), 8);
  Menu.Draw;
end;

procedure TOptions.Update(dt: Double);
begin
  inherited;

  Menu.Update(dt);

  if Music then Menu.Items[0].Caption := 'Music: On' else Menu.Items[0].Caption := 'Music: Off';
  if Sound then Menu.Items[1].Caption := 'Sound: On' else Menu.Items[1].Caption := 'Sound: Off';
end;

procedure TOptions.HandleEvents;
begin
  if Menu.ButtonClick(0) then
  begin
    Music := not Music;
  end;

  if Menu.ButtonClick(1) then
  begin
    Sound := not Sound;
  end;

  if Menu.ButtonClick('Back') or Input.XBox360Controller.B() then
  begin
    //GameState := gsMainMenu;
    SceneDirector.SwitchTo('mainmenu');
  end;
end;

end.
