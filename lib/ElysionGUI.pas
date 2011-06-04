unit ElysionGUI;

interface

{$I Elysion.inc}

{$IFDEF FPC}
  //{$mode delphi}
{$ENDIF}

uses
  ElysionNode,
  ElysionTypes,
  ElysionApplication,
  ElysionTexture,
  ElysionGraphics,
  ElysionInput,
  ElysionTrueTypeFont,
  ElysionUtils,
  ElysionAnimator,

  SDLUtils,
  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}

  OpenURLUtil,
  SysUtils,
  Classes;

type
  TelButtonStyle = (bsNormal, bsGradient, bsRounded, bsImage);

  // GUI elements
  TelLabel = class(TelNode)
  protected
    fCaption: String;
    fStrings: TStringList;

    fRect: TelRect;

    fHyperLink: String;

    fFontContainer: TelTrueTypeFont;

    function GetWidth(): Integer; Override;
    function GetHeight(): Integer; Override;

    function GetMouseOver(): Boolean; Override;
    function GetClick(): Boolean; Override;

    function GetSize(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetSize(Value: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetCaption(Value: String);
  public
    constructor Create(); Override;
    destructor Destroy(); Override;

    procedure LoadFromFile(const aFilename: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure LoadFromFontContainer(aFontContainer: TelTrueTypeFont); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Draw(); Override;
    procedure Update(dt: Double = 0.0); Override;
  published
    property Caption: String read fCaption write SetCaption;
    property FontContainer: TelTrueTypeFont read fFontContainer write fFontContainer;

    property HyperLink: String read fHyperLink write fHyperLink;

    property Size: Integer read GetSize write SetSize;
  end;

  TelButton = class(TelNode)
  protected
    fEnabled, fFocus: Boolean;

    fSprite: TelSprite;
    fTextLabel: TelLabel;

    fStyle: TelButtonStyle;

    function GetCaption(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetCaption(Value: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetHyperLink(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetHyperLink(Value: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetWidth(): Integer; Override;
    function GetHeight(): Integer; Override;

    function GetMouseOver(): Boolean; Override;
    function GetClick(): Boolean; Override;
  public
    constructor Create(); Override;
    destructor Destroy(); Override;

    procedure LoadFromFile(const ButtonFilename: String; const LabelFilename: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure LoadImageFromFile(const Filename: String); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure LoadImageFromFile(const Filename: String; aRect: TelRect); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure LoadImageFromTexture(Texture: TelTexture); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure LoadLabelFromFile(const Filename: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure ClipImage(Rect: TelRect); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure Draw(); Override;
    procedure Update(dt: Double = 0.0); Override;

    function OnRightClick(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property Caption: String read GetCaption write SetCaption;

    property Enabled: Boolean read fEnabled write fEnabled;
    property Focus: Boolean read fFocus write fFocus;

    property HyperLink: String read GetHyperLink write SetHyperLink;

    property Style: TelButtonStyle read fStyle write fStyle;

    property TextLabel: TelLabel read fTextLabel write fTextLabel;
    property Sprite: TelSprite read fSprite write fSprite;
  end;

(*TelButton = class(TelObject)
  private
	fSprite: TelSprite;
	fFont: TelTrueTypeFont;

	fCaption: String;
	FEnabled: Boolean;
        fVisible: Boolean;

	function GetWidth: Integer;
	function GetHeight: Integer;
  public
    Position: TelVector3i;

    constructor Create; Override;
    destructor Destroy; Override;

	//procedure VirtualClone(FromButton: TelButton);

	function GetImage: TelSprite;
	function GetFont: TelTrueTypeFont;

	procedure LoadFromFile(Filename: String); Overload;
	procedure LoadFromFile(Filename: String; Rect: TelRect); Overload;

	//procedure LoadFromSprite(Sprite: TelSprite); Overload;
	//procedure LoadFromSprite(Sprite: TelSprite; Rect: TelRect); Overload;

	procedure LoadFont(Filename: String; ptSize: Integer = 10);

	procedure Move(Point: TelVector2i);

    procedure ClipImage(Rect: TelRect);

    function OnMouseOver: Boolean;
    function OnClick: Boolean;
	function OnRightClick: Boolean;

	procedure Draw;
  published
    property Caption: String read fCaption write fCaption;

	property Enabled: Boolean read FEnabled write FEnabled;

    property Font: TelTrueTypeFont read fFont write fFont;

	property Sprite: TelSprite read FSprite write FSprite;

	property Width: Integer read GetWidth;
	property Height: Integer read GetHeight;
    property Visible: Boolean read fVisible write fVisible;
end;       *)

TelMenuLayout = (mlVertical, mlHorizontal);

 // Not completely intermediate GUI, but going in the right direction ;)
 TelMenu = class(TelNode)
 protected
   fAnimator: TelAnimator;

   fButtonList: TList;
   fNumButtons: Integer;

   fHoverSplit, fImageSplit: Boolean;
   fStdRect, fHoverRect: TelRect;

   fCurrentButton: TelButton;
   
   fSpacing: Integer;

   function GetCount: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
   function GetHeight: Integer; Override;
   function GetWidth: Integer; Override;
   //procedure AnimateButton(dt: Double; aID: Integer);

   function GetButton(aID: Integer): TelButton; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
   function GetButtonS(Caption: String): TelButton; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF} // Maybe improve => Runtime: O(n)

   function GetMouseOver(): Boolean; Override;
   function GetClick(): Boolean; Override;
 public
   constructor Create(); Override;
   destructor Destroy; Override;

   procedure setButtons(const ButtonImage, ButtonFont: String; FontSize: Integer; Buttons: array of String);

   function ButtonClick(aID: Integer): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
   function ButtonClick(Caption: String): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}  // Maybe improve => Runtime: O(n)

   function ButtonMouseOver(aID: Integer): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
   function ButtonMouseOver(Caption: String): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF} // Maybe improve => Runtime: O(n)

   procedure Draw; Override;
   procedure Update(dt: Double = 0.0); Override;

   procedure Reset(Exclusion: Integer = -1);
   
   property Items[Index: Integer]: TelButton read GetButton; default;
   property Find[Index: String]: TelButton read GetButtonS;
 published
   property Animator: TelAnimator read fAnimator write fAnimator;
 
   property Count: Integer read GetCount;

   property HoverSplit: Boolean read fHoverSplit write fHoverSplit;
   property ImageSplit: Boolean read fImageSplit write fImageSplit;

   //property AllButtons: TelButton write SetAllButtons;
   property CurrentButton: TelButton read fCurrentButton;
   
   property Spacing: Integer read fSpacing write fSpacing;
 end;

{TelCheckButton = class
  private

  public

  published
    property Checked: Boolean read FChecked
end;}

// Intermediate GUI (best GUI conecept ever!)
TelGUI = class
  private

  public
    constructor Create;
    destructor Destroy; Override;

    procedure Box(Rect: TelRect; Color: TelColor); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Box(Rect: TelRect; Color: TelGradient); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Box(Rect: TelRect; Vertices: TColorVertices); Overload;

    procedure RoundedBox(Rect: TelRect; Color: TelColor; RoundedRadius: Integer = 5); Overload;
    //procedure RoundedBox(Rect: TelRect; Color: TelGradient; RoundedRadius: Integer = 5); Overload;

    procedure Circle(Rect: TelRect; Color: TelColor); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Polygon(Vertices: array of TelVertex); Overload;
    procedure Polygon(Rect: TelRect; Color: TelColor; Polygons: Integer = 5; Rotation: Single = 0.0); Overload;

    function Button(Rect: TelRect; Caption: String): Boolean;
end;

{$IFDEF AUTO_INIT}
var
  GUI: TelGUI;
{$ENDIF}

implementation

//
// GUI elements
//


//
// TelLabel
//

constructor TelLabel.Create;
begin
  inherited;

  fRect.Clear();

  fFontContainer := TelTrueTypeFont.Create;
end;

destructor TelLabel.Destroy;
begin
  fFontContainer.Destroy;

  inherited;
end;

function TelLabel.GetWidth(): Integer;
begin
  Result := fFontContainer.GetWidth_Text(fCaption);
end;

function TelLabel.GetHeight(): Integer;
begin
  Result := fFontContainer.Height;
end;

function TelLabel.GetMouseOver(): Boolean;
begin
  inherited;

  fRect := makeRect(Position.X, Position.Y, GetWidth(), GetHeight());

  {$IFDEF CAN_METHODS}
    Result := fRect.ContainsVector(ActiveWindow.Cursor);
  {$ELSE}
    Result := RectContainsVector(fRect, ActiveWindow.Cursor);
  {$ENDIF}
end;

function TelLabel.GetClick(): Boolean;
begin
  inherited;

  Result := (Self.MouseOver and Input.Mouse.LeftClick);
end;

procedure TelLabel.SetSize(Value: Integer);
begin
  Self.FontContainer.Size := Value;
end;

function TelLabel.GetSize(): Integer;
begin
  Result := Self.FontContainer.Size;
end;

procedure TelLabel.SetCaption(Value: String);
begin
  fCaption := Value;

end;

procedure TelLabel.LoadFromFile(const aFilename: String);
begin
  if GetFilenameExtension(aFilename) = 'TTF' then
  begin

    fFontContainer.LoadFromFile(aFilename);
    fFontContainer.RenderStyle := rtBlended;
    fFontContainer.Size := 12;
  end;
end;

procedure TelLabel.LoadFromFontContainer(aFontContainer: TelTrueTypeFont);
begin
  if (aFontContainer <> nil) then
    fFontContainer := aFontContainer;
end;

procedure TelLabel.Draw();
begin
  if (fFontContainer <> nil) then
  begin
    fFontContainer.Color := Self.Color;
    fFontContainer.TextOut(Self.Position, Self.Caption);
  end;
end;

procedure TelLabel.Update(dt: Double = 0.0);
begin
  inherited;

  if (HyperLink <> '') and GetClick then OpenURL(HyperLink);
end;

//
// TelButton
//

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

function TelButton.GetWidth(): Integer;
begin
  if fSprite <> nil then Result := fSprite.Width;
end;

function TelButton.GetHeight(): Integer;
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

  {$IFDEF CAN_METHODS}
    if fRect.ContainsVector(ActiveWindow.Cursor) then
  {$ELSE}
    if RectContainsVector(fRect, ActiveWindow.Cursor) then
  {$ENDIF}
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

procedure TelButton.Draw;
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
      if not ColorEquals(fSprite.Color, Self.Color) then
        fSprite.Color := Self.Color;

      // Update positions
      fSprite.Position := Self.Position;
      fTextLabel.Position := makeV3f(Self.Position.X + ((fSprite.Width - (fTextLabel.FontContainer.GetWidth_Text(Caption) / Lines)) / 2),
                                     Self.Position.Y + ((fSprite.Height - (fTextLabel.Height * Lines)) / 2),
                                     Self.Position.Z);

      // Draw stuff
      fSprite.Draw();
      fTextLabel.Draw();
    end;
  end;
end;

procedure TelButton.Update(dt: Double = 0.0);
begin
  inherited;

  if (HyperLink <> '') and GetClick then OpenURL(HyperLink);
end;

constructor TelMenu.Create;
begin
  inherited Create;

  Animator := TelAnimator.Create;

  // Default animator, can be replaced though, but only if you really want it
  Animator.ColorEffect(makeCol(180, 180, 180), makeCol(255, 255, 255));
  Animator.Duration := 1500;

  Position.Clear;

  fButtonList := TList.Create;
end;

function TelMenu.GetMouseOver(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fButtonList.Count - 1 do
  begin
    if TelButton(fButtonList.Items[i]).MouseOver then
    begin
      fCurrentButton := TelButton(fButtonList.Items[i]);
      Result := true;
      Exit;
    end else
    begin
      fCurrentButton := nil;
      Result := false;
    end;
  end;
end;

function TelMenu.GetClick(): Boolean;
var
  i: Integer;
begin
  inherited;

  for i := 0 to fButtonList.Count - 1 do
  begin
    if TelButton(fButtonList.Items[i]).Click then
    begin
      fCurrentButton := TelButton(fButtonList.Items[i]);
      Result := true;
      Exit;
    end else
    begin
      fCurrentButton := nil;
      Result := false;
    end;
  end;
end;

procedure TelMenu.SetButtons(const ButtonImage, ButtonFont: String; FontSize: Integer; Buttons: array of String);
var
  tmpButton: TelButton;
  i: Integer;
begin


  if ((ButtonImage <> '') and (ButtonFont <> '')) then
  begin
    if Length(Buttons) >= 0 then
    begin


      (*
      // DEBUG
      TelLogger.getInstance.writeLog('Length of Array Buttons: ' + IntToStr(Length(Buttons)));
      TelLogger.getInstance.writeLog('Length of Array fNumButtons: ' + IntToStr(fNumButtons));
      TelLogger.getInstance.writeLog('Length of Array fButton:' + IntToStr(Length(fButton)));
      *)

      for i := 0 to Length(Buttons) - 1 do
      begin
        tmpButton := TelButton.Create;
        tmpButton.LoadFromFile(ButtonImage, ButtonFont);
        tmpButton.TextLabel.Size := FontSize;

        if ImageSplit then
        begin
          fStdRect := makeRect(0, 0, tmpButton.Sprite.TextureWidth div 2, tmpButton.Sprite.TextureHeight);
          fHoverRect := makeRect(tmpButton.Sprite.TextureWidth div 2, 0, tmpButton.Sprite.TextureWidth div 2, tmpButton.Sprite.TextureHeight);

          tmpButton.ClipImage(fStdRect);
        end;

        tmpButton.Caption := Buttons[i];

        if Animator <> nil then
        begin
          case Animator.AnimProperty.AnimType of
            atAlpha: tmpButton.Alpha := Animator.AnimProperty.StartAlpha;
            atPosition: tmpButton.Position := Animator.AnimProperty.StartPosition;
            atOrigin: tmpButton.Origin := Animator.AnimProperty.StartOrigin;
            atRotation: tmpButton.Rotation := Animator.AnimProperty.StartRotation;
            atColor: tmpButton.Color := Animator.AnimProperty.StartColor;
            atScale: tmpButton.Scale := Animator.AnimProperty.StartScale;
          end;

        end;

        fButtonList.Add(tmpButton);
      end;
    end;
  end;
end;

destructor TelMenu.Destroy;
begin
  fButtonList.Free;

  inherited;
end;

(*procedure TelMenu.AnimateButton(dt: Double; aID: Integer);
var
  tmpAnimFactor: Single;
begin
  if TelButton(fButtonList.Items[aID]).Color.R = HoverColor.R then fAnimUp := false;
  if TelButton(fButtonList.Items[aID]).Color.R = StandardColor.R then fAnimUp := true;

  if (AnimFactor * dt) < 1.0 then tmpAnimFactor := 1
    else tmpAnimFactor := (AnimFactor * dt);

  if fAnimUp then
  begin
    TelButton(fButtonList.Items[aID]).Color.R := Trunc(TelButton(fButtonList.Items[aID]).Color.R + tmpAnimFactor);
    TelButton(fButtonList.Items[aID]).Color.G := Trunc(TelButton(fButtonList.Items[aID]).Color.G + tmpAnimFactor);
    TelButton(fButtonList.Items[aID]).Color.B := Trunc(TelButton(fButtonList.Items[aID]).Color.B + tmpAnimFactor);
  end else
  begin
    TelButton(fButtonList.Items[aID]).Color.R := Trunc(TelButton(fButtonList.Items[aID]).Color.R - tmpAnimFactor);
    TelButton(fButtonList.Items[aID]).Color.G := Trunc(TelButton(fButtonList.Items[aID]).Color.G - tmpAnimFactor);
    TelButton(fButtonList.Items[aID]).Color.B := Trunc(TelButton(fButtonList.Items[aID]).Color.B - tmpAnimFactor);
  end;

end;*)

function TelMenu.GetCount: Integer;
begin
  Result := fButtonList.Count;
end;

function TelMenu.GetHeight: Integer;
begin
  Result := Self.Count * (TelButton(fButtonList.Items[0]).Height + Spacing);
end;

function TelMenu.getWidth: Integer;
begin
  Result := (TelButton(fButtonList.Items[0])).Width;
end;

function TelMenu.ButtonClick(aID: Integer): Boolean;
begin
  Result := (TelButton(fButtonList.Items[aID])).Click;
end;

function TelMenu.ButtonClick(Caption: String): Boolean;
var
  i: Integer;
begin
  for i := 0 to fButtonList.Count - 1 do
  begin
    if (TelButton(fButtonList.Items[i]).Caption = Caption) then
    begin
      Result := (TelButton(fButtonList.Items[i])).Click;
      Exit;
    end;
  end;
end;

function TelMenu.ButtonMouseOver(aID: Integer): Boolean;
begin
  Result := (TelButton(fButtonList.Items[aID])).MouseOver;
end;

function TelMenu.ButtonMouseOver(Caption: String): Boolean;
var
  i: Integer;
begin
  for i := 0 to fButtonList.Count - 1 do
  begin
    if (TelButton(fButtonList.Items[i]).Caption = Caption) then
    begin
      Result := (TelButton(fButtonList.Items[i])).MouseOver;
      Exit;
    end;
  end;
end;

function TelMenu.GetButton(aID: Integer): TelButton;
begin
  Result := (TelButton(fButtonList.Items[aID]));
end;

function TelMenu.GetButtonS(Caption: String): TelButton;
var
  i: Integer;
begin
  for i := 0 to fButtonList.Count - 1 do
  begin
    if ((TelButton(fButtonList.Items[i])).Caption = Caption) then
    begin
      Result := (TelButton(fButtonList.Items[i]));
      Exit;
    end;
  end;
end;

procedure TelMenu.Draw;
var
  i: Integer;
begin

  for i := 0 to fButtonList.Count - 1 do
  begin
    (TelButton(fButtonList.Items[i])).Position := makeV3f(Self.Position.X, Self.Position.Y + i * (TelButton(fButtonList.Items[i]).Height + Spacing), Self.Position.Z);
    (TelButton(fButtonList.Items[i])).Draw;
  end;
end;

procedure TelMenu.Update(dt: Double);

  function SaveFocus(): Integer;
  var
    j: Integer;
  begin
    Result := -1;
  
    for j := 0 to fButtonList.Count - 1 do
    begin
      if Self.Items[j].Focus then
        Result := j;
        
      Self.Items[j].Focus := false;
    end;
  end;

var
  i: Integer;
  tmpFocus: Integer;
begin
  inherited;

  tmpFocus := SaveFocus();

  if HoverSplit then
  begin
    for i := 0 to fButtonList.Count - 1 do
    begin
      if (TelButton(fButtonList.Items[i])).MouseOver then
        (TelButton(fButtonList.Items[i])).ClipImage(fHoverRect)
        else (TelButton(fButtonList.Items[i])).ClipImage(fStdRect);
    end;
  end else
  begin
    Reset(tmpFocus);

    (*if tmpFocus <> -1 then
      Animator.Target := Items[tmpFocus];

    Animator.Update(dt); *)

    if tmpFocus <> -1 then
    begin
      Animator.Target := Items[tmpFocus];

      //if not Animator.Finished then
      //begin
      if Animator.Active then
        Animator.Update(dt)
      else
        Animator.Start();

      //end;

    end else
    begin
      if Animator.Active then
        Animator.Stop();
    end;

    (*if ((StandardColor.R <> HoverColor.R) or
        (StandardColor.G <> HoverColor.G) or
        (StandardColor.B <> HoverColor.B)) then
    begin
      if tmpFocus <> -1 then
      begin
        if HoverAnimation then AnimateButton(dt, tmpFocus)
          else Self.Items[tmpFocus].Color := HoverColor;
      end else
      begin
        for i := 0 to Count - 1 do Self.Items[i].Color := StandardColor;
      end;
    
      {for i := 0 to fButtonList.Count - 1 do
      begin
          if (TelButton(fButtonList.Items[i])).Focus then
          begin
            if HoverAnimation then AnimateButton(dt, i)
              else (TelButton(fButtonList.Items[i])).Color := HoverColor;
          end else (TelButton(fButtonList.Items[i])).Color := StandardColor;
      end;}
    end;*)
  end;

end;

procedure TelMenu.Reset(Exclusion: Integer = -1);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Exclusion = -1 then
    begin
      Animator.Target := Items[i];
      Animator.Reset();
    end else
    begin
      if (Exclusion <> i) then
      begin
        Animator.Target := Items[i];
        Animator.Reset();
      end;
    end;
  end;
end;

//
// TelGUI
//
constructor TelGUI.Create;
begin

end;

destructor TelGUI.Destroy;
begin

end;


procedure TelGUI.Box(Rect: TelRect; Color: TelColor);
begin
  Box(Rect, makeGradient(Color, Color, gsVertical));
  (*glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Color.A / 255);
    DrawQuad(Rect.X * ActiveWindow.ResScale.X,
             Rect.Y * ActiveWindow.ResScale.Y,
             Rect.W * ActiveWindow.ResScale.X,
             Rect.H * ActiveWindow.ResScale.Y, 0);
    glDisable(GL_BLEND);
  glPopMatrix;       *)
end;

procedure TelGUI.Box(Rect: TelRect; Color: TelGradient);
var
  Vertices: TColorVertices;
begin
  case Color.GradientStyle of
    gsVertical:
      begin
        Vertices[0] := Color.StartColor;
        Vertices[1] := Color.StartColor;
        Vertices[2] := Color.EndColor;
        Vertices[3] := Color.EndColor;
      end;
    gsHorizontal:
      begin
        Vertices[0] := Color.EndColor;
        Vertices[1] := Color.StartColor;
        Vertices[2] := Color.StartColor;
        Vertices[3] := Color.EndColor;
      end;
  end;

  Self.Box(Rect, Vertices);

  (*glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    DrawQuad(Rect.X * ActiveWindow.ResScale.X,
             Rect.Y * ActiveWindow.ResScale.Y,
             Rect.W * ActiveWindow.ResScale.X,
             Rect.H * ActiveWindow.ResScale.Y, 0, Vertices);

    glDisable(GL_BLEND);
  glPopMatrix;*)
end;

procedure TelGUI.Box(Rect: TelRect; Vertices: TColorVertices);
begin
  glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glDisable(GL_TEXTURE_2D);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    DrawQuad(Rect.X * ActiveWindow.ResScale.X,
             Rect.Y * ActiveWindow.ResScale.Y,
             Rect.W * ActiveWindow.ResScale.X,
             Rect.H * ActiveWindow.ResScale.Y, 0, Vertices);

    glDisable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
  glPopMatrix;
end;

procedure TelGUI.RoundedBox(Rect: TelRect; Color: TelColor; RoundedRadius: Integer = 5);
var
  i: Single;
begin
  if (RoundedRadius > (Trunc(Rect.W) div 2)) then RoundedRadius := (Trunc(Rect.W) div 2);

  if (ActiveWindow.ResScale.X <> 0) then
  begin
    Rect.X := Rect.X * ActiveWindow.ResScale.X;
    Rect.W := Rect.W * ActiveWindow.ResScale.X;
  end;

  if (ActiveWindow.ResScale.Y <> 0) then
  begin
    Rect.Y := Rect.Y * ActiveWindow.ResScale.Y;
    Rect.H := Rect.H * ActiveWindow.ResScale.Y;
  end;

  glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Color.A / 255);

    glBegin(GL_POLYGON);

      glVertex2f(Rect.X + RoundedRadius, Rect.Y);
      glVertex2f(Rect.X + Rect.W - RoundedRadius, Rect.Y);

      i := Pi * 1.5;
      while i < (Pi * 2) do
      begin
        glVertex2f(Rect.X + Rect.W - RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

      glVertex2f(Rect.X + Rect.W , Rect.Y + RoundedRadius);
      glVertex2f(Rect.X + Rect.W , Rect.Y + Rect.H - RoundedRadius);

      i := 0.0;
      while i < (Pi * 0.5) do
      begin
        glVertex2f(Rect.X + Rect.W - RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + Rect.H - RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

      glVertex2f(Rect.X + Rect.W - RoundedRadius , Rect.Y + Rect.H);
      glVertex2f(Rect.X + RoundedRadius , Rect.Y + Rect.H);

      i := Pi * 0.5;
      while i < Pi do
      begin
        glVertex2f(Rect.X + RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + Rect.H - RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

      glVertex2f(Rect.X , Rect.Y + Rect.H - RoundedRadius);
      glVertex2f(Rect.X , Rect.Y + RoundedRadius);

      i := Pi;
      while i < (Pi * 1.5) do
      begin
        glVertex2f(Rect.X + RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

    glEnd();


    glDisable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
  glPopMatrix;
end;

procedure TelGUI.Circle(Rect: TelRect; Color: TelColor);
begin
  if Rect.W <> Rect.H then
  begin
    if Rect.W > Rect.H then Rect.H := Rect.W
      else Rect.W := Rect.H;
  end;

  Self.RoundedBox(Rect, Color, Trunc(Rect.W) div 2);
end;

procedure TelGUI.Polygon(Vertices: array of TelVertex);
var
  i: Integer;
begin
  glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;

    glBegin(GL_POLYGON);

      for i := 0 to High(Vertices) do
      begin
        glColor4f(Vertices[i].Color.R / 255, Vertices[i].Color.G / 255, Vertices[i].Color.B / 255, Vertices[i].Color.A / 255);
        glVertex3f(Vertices[i].Vector.X, Vertices[i].Vector.Y, Vertices[i].Vector.Z);
      end;

    glEnd();

  glPopMatrix;
end;

procedure TelGUI.Polygon(Rect: TelRect; Color: TelColor; Polygons: Integer = 5; Rotation: Single = 0.0);
begin
  if Polygons < 3 then Polygons := 3;


end;

function TelGUI.Button(Rect: TelRect; Caption: String): Boolean;
begin

end;

{$IFDEF AUTO_INIT}
initialization
  GUI := TelGUI.Create;

finalization
  GUI.Destroy;
{$ENDIF}

end.
