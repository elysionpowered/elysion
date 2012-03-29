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
<<<<<<< HEAD
  ElysionMenu,
  ElysionLabel,
  ElysionGUI,
=======
  //ElysionMenu,
  ElysionLabel,
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  ElysionInput,
  ElysionAnimator,
  ElysionAssets,
  ElysionSprite,
<<<<<<< HEAD
=======
  ElysionPrimitives,
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1

  SysUtils,
  uBasic,
  uGlobal,
  uConfig;

type
  TMainMenu = class(TelScene)
  private
<<<<<<< HEAD
    fMenu: TelMenu;
=======
    //fMenu: TelMenu;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
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
<<<<<<< HEAD
    property Menu: TelMenu read fMenu write fMenu;
=======
    //property Menu: TelMenu read fMenu write fMenu;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1

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
<<<<<<< HEAD
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
=======
  inherited Create;


  (*Menu := TelMenu.Create;
  Menu.setButtons(GetResImgPath + 'button.png', GetStdFont, 15, ['New game', 'Credits', 'Options', 'Quit']);
  Menu.Spacing := 16;

  Menu.Position := makeV3f((ActiveWindow.Width - Menu.Width) - 32, (ActiveWindow.Height - Menu.Height) / 2);

  for i := 0 to Menu.Count - 1 do
    Menu.Items[i].TextLabel.Color := makeCol(0, 0, 0);


  RoundRect := TelRoundedRectangle.Create(Menu.Width + 16, Menu.Height);
  RoundRect.Position := makeV3f(Menu.Position.X - 8, Menu.Position.Y - 8);
  RoundRect.Color := makeCol(0, 0, 0, 128);
  RoundRect.RoundedRadius := 8; *)
  //Self.Add(RoundRect);

  //Self.Add(Menu);


  (*LblCopyright := TelLabel.Create;
  LblCopyright.LoadFromFile(GetStdFont);
  LblCopyright.Caption := '(C) Your Name Here';
  LblCopyright.Size := 14;
  LblCopyright.Color := makeCol(255, 255, 255);
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  LblCopyright.Left := 8;
  LblCopyright.Bottom := 8;
  LblCopyright.HyperLink := 'http://thatsmyawesomewebsite.com';

<<<<<<< HEAD
  Self.Add(LblCopyright);
=======
  //Self.Add(LblCopyright);      *)
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1


  BtnTwitter := TelSprite.Create;
  BtnTwitter.LoadFromFile(GetResImgPath + 'twitter.png');
  BtnTwitter.Right := 8;
<<<<<<< HEAD
  BtnTwitter.Bottom := 8;
  BtnTwitter.HyperLink := 'http://twitter.com';
  BtnTwitter.Color := TelColor.Create(192, 192, 192);
  Self.Add(BtnTwitter);


  BtnFacebook := TelSprite.Create;
=======
  //BtnTwitter.Bottom := 8;
  BtnTwitter.HyperLink := 'http://twitter.com';
  BtnTwitter.Color := makeCol(192, 192, 192);
  Self.Add(BtnTwitter);


  (*BtnFacebook := TelSprite.Create;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  BtnFacebook.LoadFromFile(GetResImgPath + 'facebook.png');
  BtnFacebook.Bottom := 8;
  BtnFacebook.Right := BtnTwitter.Right + BtnTwitter.Width + 8;
  BtnFacebook.HyperLink := 'http://facebook.com';
<<<<<<< HEAD
  BtnFacebook.Color := TelColor.Create(192, 192, 192);
  Self.Add(BtnFacebook);
=======
  BtnFacebook.Color := makeCol(192, 192, 192);
  //Self.Add(BtnFacebook);  *)
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
end;

destructor TMainMenu.Destroy;
begin
<<<<<<< HEAD
  Menu.Destroy;

  inherited;
=======
  //Menu.Destroy;

  inherited Destroy;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
end;

procedure TMainMenu.Render(Graphics: IGraphicsProvider);
begin
  inherited Render(Graphics);

end;

procedure TMainMenu.Update(dt: Double);
begin
  inherited Update(dt);

<<<<<<< HEAD
  if BtnTwitter.MouseOver then
    BtnTwitter.Color := TelColor.Create(255, 255, 255)
  else
    BtnTwitter.Color := TelColor.Create(192, 192, 192);

  if BtnFacebook.MouseOver then
    BtnFacebook.Color := TelColor.Create(255, 255, 255)
  else
    BtnFacebook.Color := TelColor.Create(192, 192, 192);
=======
  (*if BtnTwitter.MouseOver then
    BtnTwitter.Color := makeCol(255, 255, 255)
  else
    BtnTwitter.Color := makeCol(192, 192, 192);

  if BtnFacebook.MouseOver then
    BtnFacebook.Color := makeCol(255, 255, 255)
  else
    BtnFacebook.Color := makeCol(192, 192, 192);*)
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
end;

procedure TMainMenu.HandleEvents;
begin
<<<<<<< HEAD
  if Menu.ButtonClick('New game') or Input.XBox360Controller.Start() then
  begin
    Self.Parent.SwitchTo('GameScreen');
=======
  (*if Menu.ButtonClick('New game') or Input.XBox360Controller.Start() then
  begin
    Self.Parent.SwitchTo('game');
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  end;


  if Menu.ButtonClick('Credits') then
  begin
<<<<<<< HEAD
    Self.Parent.SwitchTo('Credits');
=======
    Self.Parent.SwitchTo('credits');
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
  end;

  if Menu.ButtonClick('Options') then
  begin
<<<<<<< HEAD
    Self.Parent.SwitchTo('Options');
  end;

  if Menu.ButtonClick('Quit') or Input.XBox360Controller.B() then
    Application.Quit;
=======
    Self.Parent.SwitchTo('options');
  end;

  if Menu.ButtonClick('Quit') or Input.XBox360Controller.B() then
    Application.Quit;*)
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1

end;

end.
