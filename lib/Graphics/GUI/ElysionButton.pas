unit ElysionButton;

interface

{$I Elysion.inc}

uses
  Classes,
  SysUtils,
  OpenURLUtil,
  ElysionUtils,

  ElysionTypes,
  ElysionGraphicsProvider,

  ElysionTexture,
  ElysionApplication,
  ElysionInput,
  ElysionNode,
  ElysionSprite,
  ElysionLabel;

type
  TelButtonStyle = (bsNormal, bsGradient, bsRounded, bsImage);

  TelButton = class(TelNode)
  protected
    fEnabled, fFocus: Boolean;

    fSprite: TelSprite;
    fTextLabel: TelLabel;

    fStyle: TelButtonStyle;

    function GetCaption(): String; inline;
    procedure SetCaption(Value: String); inline;

    function GetHyperLink(): String; inline;
    procedure SetHyperLink(Value: String); inline;

    function GetWidth(): Single; Override;
    function GetHeight(): Single; Override;

    function GetMouseOver(): Boolean; Override;
    function GetClick(): Boolean; Override;
  public
    constructor Create; Override;
    destructor Destroy(); Override;

    procedure LoadFromFile(const ButtonFilename: String; const LabelFilename: String); inline;
    procedure LoadImageFromFile(const Filename: String); Overload; inline;
    procedure LoadImageFromFile(const Filename: String; aRect: TelRect); Overload; inline;
    procedure LoadImageFromTexture(Texture: TelTexture); inline;
    procedure LoadLabelFromFile(const Filename: String); inline;

    procedure ClipImage(Rect: TelRect); inline;

    procedure Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true); Override;
    procedure Update(dt: Double = 0.0); Override;

    function OnRightClick(): Boolean; inline;
  published
    property Caption: String read GetCaption write SetCaption;

    property Enabled: Boolean read fEnabled write fEnabled;
    property Focus: Boolean read fFocus write fFocus;

    property HyperLink: String read GetHyperLink write SetHyperLink;

    property Style: TelButtonStyle read fStyle write fStyle;

    property TextLabel: TelLabel read fTextLabel write fTextLabel;
    property Sprite: TelSprite read fSprite write fSprite;
  end;

implementation

constructor TelButton.Create;
begin
  inherited Create;

  fStyle := bsNormal;

  fSprite := TelSprite.Create;
  fTextLabel := TelLabel.Create;

  fFocus := false;
  fVisible := true;
end;

destructor TelButton.Destroy;
begin
  fSprite.Destroy;
  fTextLabel.Destroy;

  inherited;
end;

procedure TelButton.LoadFromFile(const ButtonFilename: String; const LabelFilename: String);
begin
  Self.LoadImageFromFile(ButtonFilename);
  Self.LoadLabelFromFile(LabelFilename);
end;

procedure TelButton.LoadImageFromFile(const Filename: String);
begin
  fSprite.LoadFromFile(Filename);
end;

procedure TelButton.LoadImageFromFile(const Filename: String; aRect: TelRect);
begin
  fSprite.LoadFromFile(Filename, aRect);
end;

procedure TelButton.LoadImageFromTexture(Texture: TelTexture);
begin
  fSprite.LoadFromTexture(Texture);
end;

procedure TelButton.LoadLabelFromFile(const Filename: String);
begin
  fTextLabel.LoadFromFile(Filename);
end;

procedure TelButton.ClipImage(Rect: TelRect);
begin
  if fSprite <> nil then fSprite.ClipImage(Rect);
end;

function TelButton.GetWidth(): Single;
begin
  if fSprite <> nil then Result := fSprite.Width;
end;

function TelButton.GetHeight(): Single;
begin
  if fSprite <> nil then Result := fSprite.Height;
end;

function TelButton.GetHyperLink(): String;
begin
  Result := fSprite.HyperLink;
end;

procedure TelButton.SetHyperLink(Value: String);
begin
  fSprite.HyperLink := Value;
end;

function TelButton.GetMouseOver: Boolean;
var
  fRect: TelRect;
begin
  inherited;

  fRect := makeRect(Position.X, Position.Y, GetWidth(), GetHeight());

  if (ActiveWindow.Cursor in fRect) then
    begin
      Result := true;
      fFocus := true;
    end else
    begin
      Result := false;
      fFocus := false;
    end;
end;

function TelButton.GetClick: Boolean;
begin
  inherited;

  Result := (MouseOver and Input.Mouse.LeftClick);
end;

procedure TelButton.SetCaption(Value: String);
begin
  Self.TextLabel.Caption := Value;
end;

function TelButton.GetCaption(): String;
begin
  Result := Self.TextLabel.Caption;
end;

function TelButton.OnRightClick: Boolean;
begin
  if MouseOver and Input.Mouse.RightClick then Result := true
  else Result := false;
end;

procedure TelButton.Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true);
var
  TextNew: String;
  TextList: TStringList;
  Lines: Integer;
(*begin
  if Visible then
  begin
    TextNew := StringReplace(fCaption, '\n', #13, [rfReplaceAll, rfIgnoreCase]);
    TextList := Split(TextNew, #13, true);
    Lines := TextList.Count;
    TextList.Free;

    if ((fSprite <> nil) and (fFont <> nil)) then
    begin
      fSprite.Position := makeV3f(Position.X, Position.Y, Position.Z);
      fSprite.Draw;
      fFont.TextOut(makeV3i(Trunc(fSprite.Position.X + ((fSprite.Width - (fFont.GetWidth_Text(fCaption) div Lines)) div 2)),
                          Trunc(fSprite.Position.Y + ((fSprite.Height - (fFont.GetHeight * Lines)) div 2)), 0),
                          fCaption);
    end;

  end;   *)

begin
  if Visible then
  begin
    TextNew := StringReplace(Caption, '\n', #13, [rfReplaceAll, rfIgnoreCase]);
    TextList := Split(TextNew, #13, true);
    Lines := TextList.Count;
    TextList.Free;

    if ((fSprite <> nil) and (fTextLabel <> nil)) then
    begin
      if (fSprite.Color <> Self.Color) then
        fSprite.Color := Self.Color;

      // Update positions
      fSprite.Position := Self.Position;
      fTextLabel.Position := makeV3f(Self.Position.X + ((fSprite.Width - (fTextLabel.FontContainer.GetWidth_Text(Caption) / Lines)) / 2),
                                     Self.Position.Y + ((fSprite.Height - (fTextLabel.Height * Lines)) / 2),
                                     Self.Position.Z);

      // Draw stuff
      fSprite.Draw(Graphics, DrawChildren);
      fTextLabel.Draw(Graphics, DrawChildren);
    end;
  end;

  inherited Draw(Graphics, DrawChildren);
end;

procedure TelButton.Update(dt: Double = 0.0);
begin
  inherited Update(dt);

  if (HyperLink <> '') and GetClick then OpenURL(HyperLink);
end;

end.
