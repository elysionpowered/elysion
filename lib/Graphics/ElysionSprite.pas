unit ElysionSprite;

interface

{$I Elysion.inc}

uses
  Classes,
  SysUtils,
  OpenURLUtil,
  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}

  ElysionObject,
  ElysionContent,
  ElysionLogger,
  ElysionUtils,
  ElysionNode,
  ElysionTexture,
  ElysionTypes,
  ElysionAnimTypes,
  ElysionTimer,
  ElysionInput,
  ElysionApplication;

type
  TelSpriteList = class;

  TelSprite = class(TelNode)
    private
      fHyperLink: String;
      fTexture, fMask: TelTexture;
      fClipRect: TelRect;
      fBlendMode: TelBlendMode;
      fBoundingBox: TelBoundingBox;
      fCustomBBox: TelRect;

      function GetFilename(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetTransparent(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetTransparent(Value: Boolean); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetTextureWidth(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetTextureHeight(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetAspectRatio(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    protected
      function GetWidth(): Integer; Override;
      function GetHeight(): Integer; Override;

      function GetMouseDown(): Boolean; Override;
      function GetMouseUp(): Boolean; Override;
      function GetMouseMove(): Boolean; Override;
      function GetMouseOver(): Boolean; Override;
      function GetMouseOut(): Boolean; Override;
      function GetDragStart(): Boolean; Override;
      function GetDragging(): Boolean; Override;
      function GetDragEnd(): Boolean; Override;
      function GetClick(): Boolean; Override;
      function GetRightClick(): Boolean; Override;
      function GetDblClick(): Boolean; Override;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      function LoadFromFile(const aFilename: String): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadFromFile(const aFilename: String; aClipRect: TelRect): Boolean; Overload;

      procedure LoadFromTexture(aTexture: TelTexture); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure LoadFromTexture(aTexture: TelTexture; aClipRect: TelRect); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure LoadFromStream(aStream: TStream); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SaveToStream(aStream: TStream); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure ClipImage(aRect: TelRect);

      procedure SetColorKey(aColor: TelColor); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetColorKey(aPoint: TelVector2i); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function OnPoint(Coord: TelVector2f): Boolean;

      procedure Move(aPoint: TelVector2f); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(aPoint: TelVector2i); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(aPoint: TelVector3f); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function Collides(Other: TelSprite; AllowInvisibleObjects: Boolean = false): Boolean; Overload;
      function Collides(Others: array of TelSprite; AllowInvisibleObjects: Boolean = false): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function Collides(Others: TelSpriteList; AllowInvisibleObjects: Boolean = false): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Draw(DrawChildren: Boolean = true); Override;
      procedure Update(dt: Double = 0.0); Override;

      property ClipRect: TelRect read fClipRect; // Use ClipImage to set ClipRect
      // Custom Bounding Box
      property CustomBBox: TelRect read fCustomBBox write fCustomBBox;
    published
      property AspectRatio: Single read GetAspectRatio;

      property BlendMode: TelBlendMode read fBlendMode write fBlendMode;
      property BoundingBox: TelBoundingBox read fBoundingBox write fBoundingBox;

      property Filename: String read GetFilename;

      property HyperLink: String read fHyperLink write fHyperLink;

      property Texture: TelTexture read fTexture write fTexture;
      property Mask: TelTexture read fMask write fMask;

      property TextureWidth: Integer read GetTextureWidth;
      property TextureHeight: Integer read GetTextureHeight;

      property Transparent: Boolean read GetTransparent write SetTransparent;

      property Width: Integer read GetWidth;
      property Height: Integer read GetHeight;
  end;

TelSpriteList = class(TelObject)
  private
    FSpriteList: TList;
    Head: String[13];

    function Get(Index: Integer): TelSprite;
    function GetPos(Index: String): Integer;
    procedure Put(Index: Integer; const Item: TelSprite);
    procedure PutS(Index: String; const Item: TelSprite);
    function GetS(Index: String): TelSprite;
    function GetCount: integer;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Insert(Index: Integer; Sprite: TelSprite);
    function  Add(Sprite: TelSprite): Integer;
    procedure Delete(Index: Integer);
    procedure LoadFromStream(Stream : TFileStream);
    procedure SaveToStream(Stream : TFileStream);

    procedure LoadFromFile(Filename: String);
    procedure SaveToFile(Filename: String);

    property Items[Index: Integer]: TelSprite read Get write Put; default;
    property Find[Index: String]: TelSprite read GetS write PutS;
  published
    property Count: Integer read GetCount;
end;

implementation

uses
  ElysionGraphics;

constructor TelSprite.Create;
begin
  inherited;

  Texture := TelTexture.Create;
  Mask := TelTexture.Create;

  BlendMode := bmNormal;
  fBoundingBox := bbDefault;

end;

destructor TelSprite.Destroy;
begin
  Texture.Destroy;
  Mask.Destroy;

  inherited;
end;

function TelSprite.GetFilename(): String;
begin
  Result := Texture.Filename;
end;

function TelSprite.GetTransparent(): Boolean;
begin
  Result := Texture.Transparent;
end;

procedure TelSprite.SetTransparent(Value: Boolean);
begin
  Texture.Transparent := Value;
end;

function TelSprite.GetMouseDown(): Boolean;
begin
  inherited;

  Result := ((MouseOver) and (Input.Mouse.Down));
end;

function TelSprite.GetMouseUp(): Boolean;
begin
  inherited;

  Result := ((MouseOver) and (Input.Mouse.Up));
end;

function TelSprite.GetMouseMove(): Boolean;
begin
  inherited;

  Result := ((MouseOver) and (Input.Mouse.Motion));
end;

function TelSprite.GetMouseOver(): Boolean;
var
  tempRect: TelRect;
begin
  inherited;

  {$IFDEF CAN_METHODS}
    if ((Self.BoundingBox = bbCustom) and (Self.CustomBBox.IsEmpty())) then Self.BoundingBox := bbDefault;
  {$ELSE}
    if ((Self.BoundingBox = bbCustom) and (IsRectEmpty(Self.CustomBBox)) then Self.BoundingBox := bbDefault;
  {$ENDIF}

  case Self.BoundingBox of
    bbDefault:
    begin
      tempRect.X := Self.Position.X * ActiveWindow.ResScale.X;
      tempRect.Y := Self.Position.Y * ActiveWindow.ResScale.Y;
      tempRect.W := fClipRect.W * ActiveWindow.ResScale.X;
      tempRect.H := fClipRect.H * ActiveWindow.ResScale.Y;

      {$IFDEF CAN_METHODS}
        Result := tempRect.ContainsVector(ActiveWindow.Cursor);
      {$ELSE}
        Result := RectContainsVector(fClipRect, ActiveWindow.Cursor);
      {$ENDIF}
    end;

    bbCustom:
    begin
      tempRect.X := (Self.Position.X + Self.CustomBBox.X) * ActiveWindow.ResScale.X;
      tempRect.Y := (Self.Position.Y + Self.CustomBBox.Y) * ActiveWindow.ResScale.Y;
      tempRect.W := Self.CustomBBox.W * ActiveWindow.ResScale.X;
      tempRect.H := Self.CustomBBox.H * ActiveWindow.ResScale.Y;

      {$IFDEF CAN_METHODS}
        Result := tempRect.ContainsVector(ActiveWindow.Cursor);
      {$ELSE}
        Result := RectContainsVector(fClipRect, ActiveWindow.Cursor);
      {$ENDIF}
    end;

    bbPixel:
    begin
      Result := PixelTest(Self, makeRect(ActiveWindow.Cursor.X, ActiveWindow.Cursor.Y, 1, 1));
    end;
  end;

end;

function TelSprite.GetMouseOut(): Boolean;
begin
  inherited;

  Result := not GetMouseOver();
end;

function TelSprite.GetDragStart(): Boolean;
begin
  inherited GetDragStart;

  //if (MouseDown and MouseMove) then fDidDragStart := true;
  //if (MouseUp) then if fDidDragStart then fDidDragStart := false;

  Result := fDidDragStart;
end;

function TelSprite.GetDragging(): Boolean;
begin
  inherited;

  if fDidDragStart then
  begin
    fDidDragging := (MouseMove);
  end;

  fDidDragStart := not fDidDragging;

  Result := fDidDragging;
end;

function TelSprite.GetDragEnd(): Boolean;
begin
  inherited GetDragEnd;

  if (fDidDragging and MouseUp) then
  begin
    Result := true;

    fDidDragging := false;
    if fDidDragStart then fDidDragStart := false;
  end;
end;

function TelSprite.GetClick(): Boolean;
begin
  inherited;

  Result := ((MouseOver) and (Input.Mouse.LeftClick));
end;

function TelSprite.GetRightClick(): Boolean;
begin
  inherited;

  Result := ((MouseOver) and (Input.Mouse.RightClick()));
end;

function TelSprite.GetDblClick(): Boolean;
begin
  inherited;

  Result := ((MouseOver) and (Input.Mouse.DblClick));
end;

function TelSprite.GetTextureWidth(): Integer;
begin
  Result := Texture.Width;
end;

function TelSprite.GetTextureHeight(): Integer;
begin
  Result := Texture.Height;
end;

function TelSprite.GetAspectRatio(): Single;
begin
  Result := Texture.AspectRatio;
end;

function TelSprite.GetWidth(): Integer;
begin
  Result := Trunc(ClipRect.W);
end;

function TelSprite.GetHeight(): Integer;
begin
  Result := Trunc(ClipRect.H);
end;

function TelSprite.LoadFromFile(const aFilename: String): Boolean;
begin
  Result := Self.LoadFromFile(aFilename, makeRect(0, 0, -1, -1));
end;

function TelSprite.LoadFromFile(const aFilename: String; aClipRect: TelRect): Boolean;
var
  Directory: String;
  OptExtension: String; //< Optional extension
begin

  if ({(aFilename <> (Directory + Content.RootDirectory + Self.Filename)) and} (aFilename <> '')) then
  begin
    Directory := ExtractFilePath(ParamStr(0));
    OptExtension := '';

    if not FileExists(Directory + Content.RootDirectory + aFilename) then
    begin
      if GetFilenameExtension(aFilename) = '' then
      begin
        // Order: Least favorite image format to best texture format
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.bmp')) then OptExtension := '.bmp';

        {$IFDEF USE_VAMPYRE}
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.gif')) then OptExtension := '.gif';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.pcx')) then OptExtension := '.pcx';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.tif')) then OptExtension := '.tif';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.tiff')) then OptExtension := '.tiff';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.xpm')) then OptExtension := '.xpm';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.psd')) then OptExtension := '.psd';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.jpg')) then OptExtension := '.jpg';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.jpeg')) then OptExtension := '.jpeg';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.png')) then OptExtension := '.png';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.tga')) then OptExtension := '.tga';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.dds')) then OptExtension := '.dds';
        {$ENDIF}

        {$IFDEF USE_SDL_IMAGE}
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.gif')) then OptExtension := '.gif';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.pcx')) then OptExtension := '.pcx';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.tif')) then OptExtension := '.tif';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.tiff')) then OptExtension := '.tiff';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.xpm')) then OptExtension := '.xpm';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.jpg')) then OptExtension := '.jpg';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.jpeg')) then OptExtension := '.jpeg';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.png')) then OptExtension := '.png';
        if FileExists(LowerCase(Directory + Content.RootDirectory + aFilename + '.tga')) then OptExtension := '.tga';
        {$ENDIF}
      end;
    end;

    if FileExists(Directory + Content.RootDirectory + aFilename + OptExtension) then
    begin
      Self.Texture := TextureManager.CreateNewTexture(Directory + Content.RootDirectory + aFilename + OptExtension);

      if aClipRect.X < 0 then aClipRect.X := 0;
      if aClipRect.Y < 0 then aClipRect.Y := 0;
      if aClipRect.W <= 0 then aClipRect.W := TextureWidth;
      if aClipRect.H <= 0 then aClipRect.H := TextureHeight;


      ClipImage(aClipRect);

      // Sets origin to center
      Origin := Center(Self);

      //FAnim.W := GetSurfaceWidth div Trunc(FClipRect.W);
      //FAnim.H := GetSurfaceHeight div Trunc(FClipRect.H);

      //FMaxFrames := Trunc(FAnim.W) * Trunc(FAnim.H);

      Result := true;
    end else Self.Log('File not found: ' + Directory + Content.RootDirectory + aFilename + OptExtension);

  end;
end;

procedure TelSprite.LoadFromTexture(aTexture: TelTexture);
begin
  fTexture := aTexture;
end;

procedure TelSprite.LoadFromTexture(aTexture: TelTexture; aClipRect: TelRect);
begin
  LoadFromTexture(aTexture);
  ClipImage(aClipRect);
end;

procedure TelSprite.LoadFromStream(aStream: TStream);
begin
  Texture.LoadFromStream(aStream);
end;

procedure TelSprite.SaveToStream(aStream: TStream);
begin
  Texture.SaveToStream(aStream);
end;

procedure TelSprite.ClipImage(aRect: TelRect);
begin
  if (fClipRect.X <> aRect.X) then fClipRect.X := aRect.X;
  if (fClipRect.Y <> aRect.Y) then fClipRect.Y := aRect.Y;

  if (fClipRect.W <> aRect.W) then
  begin
    fClipRect.W := aRect.W;
  end;

  if (fClipRect.H <> aRect.H) then
  begin
    fClipRect.H := aRect.H;
  end;
end;

procedure TelSprite.SetColorKey(aColor: TelColor);
begin
  Texture.SetColorKey(aColor);
end;

procedure TelSprite.SetColorKey(aPoint: TelVector2i);
begin
  Texture.SetColorKey(aPoint);
end;

function TelSprite.OnPoint(Coord: TelVector2f): Boolean;
var
  marLeft, marTop, marRight, marBottom: Single;
  padLeft, padTop, padRight, padBottom: Single;
  borLeft, borTop, borRight, borBottom: Single;
begin

  if edMargin in Decorations then
  begin
    marLeft := Margin.Left;
    marTop := Margin.Top;
    marRight := Margin.Right;
    marBottom := Margin.Bottom;
  end else
  begin
    marLeft := 0;
    marTop := 0;
    marRight := 0;
    marBottom := 0;
  end;

  if edPadding in Decorations then
  begin
    padLeft := Padding.Left;
    padTop := Padding.Top;
    padRight := Padding.Right;
    padBottom := Padding.Bottom;
  end else
  begin
    padLeft := 0;
    padTop := 0;
    padRight := 0;
    padBottom := 0;
  end;

  if edBorder in Decorations then
  begin
    borLeft := Border.Left.Width;
    borTop := Border.Top.Width;
    borRight := Border.Right.Width;
    borBottom := Border.Bottom.Width;
  end else
  begin
    borLeft := 0;
    borTop := 0;
    borRight := 0;
    borBottom := 0;
  end;

  Result := ((Coord.X >= (AbsolutePosition.X - Origin.X - marLeft - borLeft - padLeft) * Scale.X * ActiveWindow.ResScale.X) and
             (Coord.Y >= (AbsolutePosition.Y - Origin.Y - marTop - borTop - padTop) * Scale.Y * ActiveWindow.ResScale.Y) and
             (Coord.X < (AbsolutePosition.X - Origin.X + ClipRect.W + marRight + borRight + padRight) * Scale.X * ActiveWindow.ResScale.X) and
             (Coord.Y < (AbsolutePosition.Y - Origin.Y + ClipRect.H + marBottom + borBottom + padBottom) * Scale.Y * ActiveWindow.ResScale.Y));
end;

procedure TelSprite.Move(aPoint: TelVector2f);
begin
  Position.Add(makeV3f(aPoint.X, aPoint.Y, 0.0));
end;

procedure TelSprite.Move(aPoint: TelVector2i);
begin
  Position.Add(makeV3f(aPoint.X, aPoint.Y, 0.0));
end;

procedure TelSprite.Move(aPoint: TelVector3f);
begin
  Position.Add(aPoint);
end;

function TelSprite.Collides(Other: TelSprite; AllowInvisibleObjects: Boolean = false): Boolean;
begin
  if Self.BoundingBox = Other.BoundingBox then
  begin
    case BoundingBox of
      bbDefault: Result := CollisionTest(Self, Other, AllowInvisibleObjects);
      bbCustom: Result := CollisionTest(Self.CustomBBox, Other.CustomBBox);
      bbPixel: Result := PixelTest(Self, Other, AllowInvisibleObjects);
    end;
  end else
  begin
    // Default bounding box <-> Custom bounding box
    if ((Self.BoundingBox = bbDefault) and (Other.BoundingBox = bbCustom)) then Result := CollisionTest(Self.ClipRect, Other.CustomBBox);
    if ((Self.BoundingBox = bbCustom) and (Other.BoundingBox = bbDefault)) then Result := CollisionTest(Other.CustomBBox, Self.ClipRect);

    // Default bounding box <-> Pixel
    if ((Self.BoundingBox = bbDefault) and (Other.BoundingBox = bbPixel)) then Result := PixelTest(Other, Self, AllowInvisibleObjects);
    if ((Self.BoundingBox = bbPixel) and (Other.BoundingBox = bbDefault)) then Result := PixelTest(Self, Other, AllowInvisibleObjects);

    // Custom bounding box <-> Pixel
    if ((Self.BoundingBox = bbCustom) and (Other.BoundingBox = bbPixel)) then Result := PixelTest(Other, Self.CustomBBox, AllowInvisibleObjects);
    if ((Self.BoundingBox = bbPixel) and (Other.BoundingBox = bbCustom)) then Result := PixelTest(Self, Other.CustomBBox, AllowInvisibleObjects);
  end;
end;

function TelSprite.Collides(Others: array of TelSprite; AllowInvisibleObjects: Boolean = false): Integer;
var
  i, Collided: Integer;
begin
  if Length(Others) = 0 then begin
    Result := 0;
    Exit;
  end;
  Collided := 0;

  for i := 0 to Length(Others) - 1 do
  begin
    if Self.Collides(Others[i], AllowInvisibleObjects) then Collided := Collided + 1;

    Result := Collided;
  end;
end;

function TelSprite.Collides(Others: TelSpriteList; AllowInvisibleObjects: Boolean = false): Integer;
var
  i, Collided: Integer;
begin
  if Others.Count = 0 then begin
    Result := 0;
    Exit;
  end;
  Collided := 0;

  for i := 0 to Others.Count - 1 do
  begin
    if Self.Collides(Others[i], AllowInvisibleObjects) then Collided := Collided + 1;

    Result := Collided;
  end;
end;

procedure TelSprite.Draw;
begin

  if ((Visible) and (not Texture.Empty)) then
  begin
    glColor4f(1.0, 1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);

    glPushMatrix;
      glColor4f(1.0, 1.0, 1.0, 1.0);
      glBindTexture(GL_TEXTURE_2D, Self.Texture.TextureID);
      if Transparent then
      begin
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GREATER, 0.1);
      end;

      glTranslatef((ParentPosition.X + Position.X - Margin.Left - Border.Left.Width - Padding.Left + Origin.X) * ActiveWindow.ResScale.X,
                   (ParentPosition.Y + Position.Y - Margin.Top - Border.Top.Width - Padding.Top + Origin.Y) * ActiveWindow.ResScale.Y, ParentPosition.Z);

      if Abs(Rotation.Angle) >= 360.0 then Rotation.Angle := 0.0;

      if Rotation.Angle <> 0.0 then glRotatef(Rotation.Angle, Rotation.Vector.X, Rotation.Vector.Y, Rotation.Vector.Z);

      glEnable(GL_BLEND);
      case BlendMode of
        bmAdd: glBlendFunc(GL_ONE, GL_ONE);
        bmNormal: glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        bmSub: glBlendFunc(GL_ZERO, GL_ONE);
      end;


      glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Color.A / 255);
      glScalef(Scale.X * ActiveWindow.ResScale.X, Scale.Y * ActiveWindow.ResScale.Y, 1);

      DrawQuad(Texture.Width, Texture.Height,
               fClipRect.X, fClipRect.Y, fClipRect.W, fClipRect.H,
               -Origin.X, -Origin.Y, Self.Width, Self.Height,
               Position.Z);
      //DrawQuad(TelVector2i.Create(TextureWidth, TextureHeight), FClipRect, TelRect.Create(-Offset.Rotation.X, -Offset.Rotation.Y, Width, Height), Position.Z);

      //DrawQuad(200, 200, 200, 200, 0);

      glDisable(GL_BLEND);
      if Transparent then glDisable(GL_ALPHA_TEST);
    glPopMatrix;

    glBindTexture(GL_TEXTURE_2D, 0);

    glDisable(GL_TEXTURE_2D);
  end;

  // Super is called here, because it should draw the object sprite before it draws its children
  inherited;
end;

procedure TelSprite.Update(dt: Double = 0.0);
begin
  inherited;

  if (HyperLink <> '') and GetClick then OpenURL(HyperLink);
end;


//
// TelSpriteList
//

constructor TelSpriteList.Create;
begin
  inherited;

  FSpriteList := TList.Create;
  Head := 'TelSpriteList';
end;

destructor TelSpriteList.Destroy;
var
  Counter : integer;
begin

  for Counter := 0 to FSpriteList.Count - 1 do
  begin
    TelSprite(FSpriteList[Counter]).Destroy;
  end;
  FSpriteList.Free;

  inherited Destroy;

end;

function TelSpriteList.GetCount: Integer;
begin
  Result := FSpriteList.Count;
end;

procedure TelSpriteList.Insert(Index: Integer; Sprite: TelSprite);
begin
  if ((Index >= 0) and (Index <= FSpriteList.Count - 1)) then FSpriteList.Insert(Index, Sprite)
  else begin
    if Index > FSpriteList.Count - 1 then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
  end;
end;

function TelSpriteList.Add(Sprite: TelSprite): Integer;
begin
  Result := FSpriteList.Add(Sprite);
end;

procedure TelSpriteList.Delete(Index: Integer);
var
  TmpSprite: TelSprite;
begin
  if ((Index >= 0) and (Index <= FSpriteList.Count - 1)) then
  begin
    TmpSprite := Get(Index);
    TmpSprite.Destroy;
    FSpriteList.Delete(Index);
  end
  else begin
    if Index > FSpriteList.Count - 1 then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
  end;

end;

function TelSpriteList.Get(Index: Integer): TelSprite;
begin
  if ((Index >= 0) and (Index <= FSpriteList.Count - 1)) then Result := TelSprite(FSpritelist[Index])
  else begin
    if Index > FSpriteList.Count - 1 then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
  end;

end;

function TelSpriteList.GetPos(Index: String): Integer;
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To FSpriteList.Count - 1 Do
    Begin
      if Items[a].Name <> Index then TMP := -1
      else begin
        TMP := a;
        Break;
      end;
    End;
  Finally
    Result := TMP;
  End;

end;

procedure TelSpriteList.Put(Index: Integer; const Item: TelSprite);
var
  TmpSprite: TelSprite;
begin
  if ((Index >= 0) and (Index <= FSpriteList.Count - 1)) then
  begin
    TmpSprite := Get(Index);
    TmpSprite.Destroy;
    Insert(Index, Item);
  end
  else begin
    if Index > FSpriteList.Count - 1 then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
  end;

end;

Function TelSpriteList.GetS(Index: String): TelSprite;
Var TMP: Integer;
Begin
  TMP := GetPos(Index);
  if TMP >= 0 then Result := TelSprite(FSpriteList[TMP])
			  else Result := nil;
End;

Procedure TelSpriteList.PutS(Index: String; const Item: TelSprite);
var
  TMP: Integer;
  TmpSprite: TelSprite;
Begin
  if (Index <> '') then
  begin
    TmpSprite := GetS(Index);
	if TmpSprite <> nil then
	begin
	  TMP := GetPos(Index);
      TmpSprite.Destroy;
      Insert(TMP, Item);
	end
    else TelLogger.GetInstance.WriteLog('SpriteList: Index does not exist');
  end
  else TelLogger.GetInstance.WriteLog('SpriteList: Index string is empty');
End;

procedure TelSpriteList.LoadFromStream(Stream: TFileStream);
var
  TmpHead: String[13];
  loop : integer;
  ImgBuf : TelSprite;
begin
  TmpHead := '';
  loop := 0;

  Stream.Read(TmpHead, SizeOf(TmpHead));

  if TmpHead <> Head then
  begin
    TelLogger.GetInstance.WriteLog('Could not load file: Wrong file');
  end else
  begin
    Stream.Read(loop, SizeOf(Integer));

    FSpriteList.Count := loop;

    for loop := 0 to FSpriteList.Count - 1 do
    begin
      ImgBuf := TelSprite.Create;
      ImgBuf.LoadFromStream(Stream);
      FSpriteList.Insert(0, ImgBuf);
    end;
  end;

end;


procedure TelSpriteList.SaveToStream( Stream : TFileStream );
var
  loop : integer;
begin
  Stream.Write(Head, SizeOf(Head));
  Stream.Write(FSpriteList.Count, SizeOf(Integer));

  for loop := 0 to FSpriteList.Count - 1 do
  begin
    TelSprite(FSpriteList[loop]).SaveToStream(Stream);
  end;

end;

procedure TelSpriteList.SaveToFile(Filename: String);
var
  FileHndl: TFileStream;
begin
  FileHndl := TFileStream.Create( Filename, fmCreate );

  SaveToStream(FileHndl);

  FileHndl.Free;
end;

procedure TelSpriteList.LoadFromFile(Filename: String);
var
  FileHndl: TFileStream;
  Counter: integer;
begin

  for Counter := 0 to FSpriteList.Count - 1 do
  begin
    TelSprite(FSpriteList[Counter]).Destroy;
  end;

  FileHndl := TFileStream.Create(FileName, fmOpenRead);

  loadFromStream(FileHndl);

  FileHndl.Free;
end;

end.
