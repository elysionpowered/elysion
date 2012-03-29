unit uMainMenu;

interface

uses
  ElysionColor,
  ElysionTypes,
  ElysionGraphicsProvider,
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
  ElysionSprite,

  SysUtils,
  uBasic,
  uGlobal,
  uConfig;

type
  TMainMenu = class(TelScene)
  private
    fMenu: TelMenu;
    fLblCopyright: TelLabel;
    fBtnFacebook, fBtnTwitter: TelSprite;

    fNewGameClick: Boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;
	
    procedure Render(Graphics: IGraphicsProvider); Override;
    procedure Update(dt: Double); Override;
    procedure HandleEvents; Override;
  published
    property Menu: TelMenu read fMenu write fMenu;

    property BtnFacebook: TelSprite read fBtnFacebook write fBtnFacebook;
    property BtnTwitter: TelSprite read fBtnTwitter write fBtnTwitter;

    property LblCopyright: TelLabel read fLblCopyright write fLblCopyright;
  end;

implementation

constructor TMainMenu.Create;
var
  i: Integer;
  RoundRect: TelRoundedRectangle;
begin
  inherited;


  Menu := TelMenu.Create;
  Menu.setButtons(GetResImgPath + 'button.png', GetStdFont, 15, ['New game', 'Credits', 'Options', 'Quit']);
  Menu.Spacing := 16;

  Menu.Position := TelVector3f.Create((ActiveWindow.Width - Menu.Width) - 32, (ActiveWindow.Height - Menu.Height) / 2);

  for i := 0 to Menu.Count - 1 do
    Menu.Items[i].TextLabel.Color := TelColor.Create(0, 0, 0);


  RoundRect := TelRoundedRectangle.Create(Menu.Width + 16, Menu.Height);
  RoundRect.Position := TelVector3f.Create(Menu.Position.X - 8, Menu.Position.Y - 8);
  RoundRect.Color := TelColor.Create(0, 0, 0, 128);
  RoundRect.RoundedRadius := 8;
  Self.Add(RoundRect);

  Self.Add(Menu);


  LblCopyright := TelLabel.Create;
  LblCopyright.LoadFromFile(GetStdFont);
  LblCopyright.Caption := '(C) Your Name Here';
  LblCopyright.Size := 14;
  LblCopyright.Color := TelColor.Create(255, 255, 255);
  LblCopyright.Left := 8;
  LblCopyright.Bottom := 8;
  LblCopyright.HyperLink := 'http://thatsmyawesomewebsite.com';

  Self.Add(LblCopyright);


  BtnTwitter := TelSprite.Create;
  BtnTwitter.LoadFromFile(GetResImgPath + 'twitter.png');
  BtnTwitter.Right := 8;
  BtnTwitter.Bottom := 8;
  BtnTwitter.HyperLink := 'http://twitter.com';
  BtnTwitter.Color := TelColor.Create(192, 192, 192);
  Self.Add(BtnTwitter);


  BtnFacebook := TelSprite.Create;
  BtnFacebook.LoadFromFile(GetResImgPath + 'facebook.png');
  BtnFacebook.Bottom := 8;
  BtnFacebook.Right := BtnTwitter.Right + BtnTwitter.Width + 8;
  BtnFacebook.HyperLink := 'http://facebook.com';
  BtnFacebook.Color := TelColor.Create(192, 192, 192);
  Self.Add(BtnFacebook);
end;

destructor TMainMenu.Destroy;
begin
  Menu.Destroy;

  inherited;
end;

procedure TMainMenu.Render(Graphics: IGraphicsProvider);
begin
  inherited Render(Graphics);

end;

procedure TMainMenu.Update(dt: Double);
begin
  inherited Update(dt);

  if BtnTwitter.MouseOver then
    BtnTwitter.Color := TelColor.Create(255, 255, 255)
  else
    BtnTwitter.Color := TelColor.Create(192, 192, 192);

  if BtnFacebook.MouseOver then
    BtnFacebook.Color := TelColor.Create(255, 255, 255)
  else
    BtnFacebook.Color := TelColor.Create(192, 192, 192);
end;

procedure TMainMenu.HandleEvents;
begin
  if Menu.ButtonClick('New game') or Input.XBox360Controller.Start() then
  begin
    Self.Parent.SwitchTo('GameScreen');
  end;


  if Menu.ButtonClick('Credits') then
  begin
    Self.Parent.SwitchTo('Credits');
  end;

  if Menu.ButtonClick('Options') then
  begin
    Self.Parent.SwitchTo('Options');
  end;

  if Menu.ButtonClick('Quit') or Input.XBox360Controller.B() then
    Application.Quit;

end;

end.
