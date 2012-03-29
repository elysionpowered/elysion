unit ElysionMenu;

interface

{$I Elysion.inc}

uses
  Classes,

  ElysionTypes,
  ElysionColor,
  ElysionGraphicsProvider,

  ElysionNode,
  ElysionButton,

  ElysionAnimator,
  ElysionAnimTypes;

type

 TelMenuLayout = (mlVertical, mlHorizontal);

 TelMenu = class(TelNode)
 protected
   fAnimator: TelAnimator;

   fButtonList: TList;
   fNumButtons: Integer;

   fHoverSplit, fImageSplit: Boolean;
   fStdRect, fHoverRect: TelRect;

   fCurrentButton: TelButton;

   fSpacing: Integer;

   function GetCount: Integer; inline;
   function GetHeight: Single; Override;
   function GetWidth: Single; Override;
   //procedure AnimateButton(dt: Double; aID: Integer);

   function GetButton(aID: Integer): TelButton; Overload; inline;
   function GetButtonS(Caption: String): TelButton; Overload; inline; // Maybe improve => Runtime: O(n)

   function GetMouseOver(): Boolean; Override;
   function GetClick(): Boolean; Override;
 public
   constructor Create; Override;
   destructor Destroy; Override;

   procedure setButtons(const ButtonImage, ButtonFont: String; FontSize: Integer; Buttons: array of String);

   function ButtonClick(aID: Integer): Boolean; Overload; inline;
   function ButtonClick(Caption: String): Boolean; Overload; inline;  // Maybe improve => Runtime: O(n)

   function ButtonMouseOver(aID: Integer): Boolean; Overload; inline;
   function ButtonMouseOver(Caption: String): Boolean; Overload; inline; // Maybe improve => Runtime: O(n)

   procedure Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true); Override;
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

implementation

constructor TelMenu.Create;
begin
  inherited Create;

  ImageSplit := false;
  HoverSplit := false;

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

function TelMenu.GetHeight: Single;
begin
  Result := Self.Count * (TelButton(fButtonList.Items[0]).Height + Spacing);
end;

function TelMenu.GetWidth: Single;
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

procedure TelMenu.Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true);
var
  i: Integer;
begin

  for i := 0 to fButtonList.Count - 1 do
  begin
    (TelButton(fButtonList.Items[i])).Position := makeV3f(Self.Position.X, Self.Position.Y + i * (TelButton(fButtonList.Items[i]).Height + Spacing), Self.Position.Z);
    (TelButton(fButtonList.Items[i])).Draw(Graphics, DrawChildren);
  end;

  inherited Draw(Graphics, DrawChildren);
end;

procedure TelMenu.Update(dt: Double = 0.0);

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
  inherited Update(dt);

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
    //Reset(tmpFocus);

    (*if tmpFocus <> -1 then
      Animator.Target := Items[tmpFocus];

    Animator.Update(dt); *)

    for i := 0 to fButtonList.Count - 1 do
    begin
      (TelButton(fButtonList.Items[i])).Color := makeCol(180, 180, 180);

      if (TelButton(fButtonList.Items[i])).MouseOver then
      begin
        Animator.Target := TelButton(Items[i]);

        if Animator.Active then
          Animator.Update(dt)
        else
        begin
          if not Animator.Finished then Animator.Reset();
          Animator.Start();
        end;
      end;
    end;

    //if tmpFocus <> -1 then
    //begin
      //Animator.Target := Items[tmpFocus];

      //if not Animator.Finished then
      //begin
      //if Animator.Active then
        //Animator.Update(dt)
      //else
        //Animator.Start();

      //end;

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
  //end;

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

end.
