unit ElysionGraphics;

interface

{$I Elysion.inc}

uses
  ElysionUtils,
  ElysionObject,
  ElysionApplication,
  ElysionTypes,
  ElysionColor,
  ElysionInput,
  ElysionTimer,
  ElysionNode,
  ElysionTexture,
  ElysionContent,
  ElysionLogger,
  ElysionMath,

  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}
  SDL,
  SDLUtils,
  //SDLTextures,

  SysUtils,
  OpenURLUtil,
  Classes;

type
  TelSprite = class(TelNode)
    private
      fHyperLink: String;
      fTexture: TelTexture;
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

      function LoadFromFile(aFilename: String): Boolean; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadFromFile(aFilename: String; aClipRect: TelRect): Boolean; Overload;

      procedure LoadFromTexture(aTexture: TelTexture); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure LoadFromStream(aStream: TStream); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SaveToStream(aStream: TStream); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure ClipImage(aRect: TelRect);

      procedure SetColorKey(aColor: TelColor); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetColorKey(aPoint: TelVector2i); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function OnPoint(Coord: TelVector2f): Boolean;

      procedure Move(aPoint: TelVector2f); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(aPoint: TelVector2i); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Move(aPoint: TelVector3f); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function Collides(Other: TelSprite): Boolean;

      procedure Draw; Override;
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
      property TextureWidth: Integer read GetTextureWidth;
      property TextureHeight: Integer read GetTextureHeight;

      property Transparent: Boolean read GetTransparent write SetTransparent;

      property Width: Integer read GetWidth;
      property Height: Integer read GetHeight;
  end;

  { TelParallaxSprite }

  TelParallaxDirection = (dtUp, dtDown, dtLeft, dtRight);

  TelParallaxSprite = class(TelSprite)
    private
      fSpeed: Single;
      fPaused: Boolean;
      fDirection: TelParallaxDirection;
      fInternalPosition: TelVector3f;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Start(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure UnPause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Draw(); Override;
      procedure Update(dt: Double = 0.0); Override;
    published
      property Direction: TelParallaxDirection read fDirection write fDirection;
      property Speed: Single read fSpeed write fSpeed;
  end;

  { TelSpriteSheet }

  TelSpriteSheet = class(TelSprite)
    private
      fMaxFrames, fFrame: Integer;
      fTimer: TelTimer;
      fFrameSize: TelSize;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Define(AnimName: String; aRect: TelRect; Length: Integer = 1000);

      procedure Play(AnimName: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure UnPause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Draw(); Override;
      procedure Update(dt: Double = 0.0); Override;

      property FrameSize: TelSize read fFrameSize write fFrameSize;
    published
      property Frame: Integer read fFrame write fFrame;

      property MaxFrames: Integer read fMaxFrames write fMaxFrames;
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

TelLight = class(TelSprite)
  public
    constructor Create; Override;
    destructor Destroy; Override;
end;

TelParticle = class(TelSprite)
  protected
    fTimer: TelTimer;
    fVelocity: Single;

    fStartPoint, fEndPoint: TelVector3f;

    fDead: Boolean;
    fLife, fMaxLife: Integer;

    procedure SetLife(Value: Integer);
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Draw; Override;
    procedure Update(dt: Double = 0.0); Override;

    procedure Start(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Pause(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Stop(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    property StartPoint: TelVector3f read fStartPoint write fStartPoint;
    property EndPoint: TelVector3f read fEndPoint write fEndPoint;

    property Velocity: Single read fVelocity write fVelocity;
  published
    property Dead: Boolean read fDead write fDead;

    property Life: Integer read fLife write SetLife;

end;

TelParticleEmitter = class
  protected
    fLoop, fIsActive, fIsPaused: Boolean;
  public

  published
    property Loop: Boolean read fLoop write fLoop;

    property IsActive: Boolean read fIsActive;
    property IsPaused: Boolean read fIsPaused;
end;


  function convCol(S: TelWindow; Color: TelColor): Cardinal; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function convCol(S: TelWindow; R, G, B: Byte): Cardinal; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function convCol(Color: TelColor): TSDL_Color; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  //function convCol(Color: TelColor): Cardinal; Overload;
  function convCol(R, G, B: Byte): Cardinal; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}



implementation

// deprecated, please use function convCol(Color: TelColor): Cardinal; instead
function convCol(S: TelWindow; Color: TelColor): Cardinal; Overload;
begin
  Result := SDL_MapRGB(S.SDL_Surface^.Format, Color.R, Color.G, Color.B);
end;

// deprecated, please use function convCol(R, G, B: Byte): Cardinal; instead
function convCol(S: TelWindow; R, G, B: Byte): Cardinal; Overload;
begin
  Result := SDL_MapRGB(S.SDL_Surface^.Format, R, G, B);
end;

//function convCol(Color: TelColor): Cardinal; Overload;
//begin
//  Result := SDL_MapRGB(Surface.SDL_Surface.Format, Color.R, Color.G, Color.B);
//end;

function convCol(R, G, B: Byte): Cardinal; Overload;
begin
  Result := SDL_MapRGB(ActiveWindow.SDL_Surface^.Format, R, G, B);
end;

function convCol(Color: TelColor): TSDL_Color; Overload;
begin
  Result.r := Color.R;
  Result.g := Color.G;
  Result.b := Color.B;
  Result.unused := 0;
end;

constructor TelSprite.Create;
begin
  inherited;

  Texture := TelTexture.Create;
  BlendMode := bmNormal;
  fBoundingBox := bbDefault;

  fDrawable := true;

end;

destructor TelSprite.Destroy;
begin
  Texture.Destroy;

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

function TelSprite.LoadFromFile(aFilename: String): Boolean; 
begin
  Result := Self.LoadFromFile(aFilename, makeRect(0, 0, -1, -1));
end;

function TelSprite.LoadFromFile(aFilename: String; aClipRect: TelRect): Boolean;
var
  Directory: String;
begin

  if ({(aFilename <> (Directory + Content.RootDirectory + Self.Filename)) and} (aFilename <> '')) then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Directory + Content.RootDirectory + aFilename) then
    begin

      Self.Texture := TextureManager.CreateNewTexture(Directory + Content.RootDirectory + aFilename);



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
    end else if isLoggerActive then Self.Log('File not found: ' + Directory + Content.RootDirectory + aFilename);

  end;
end;

procedure TelSprite.LoadFromTexture(aTexture: TelTexture); 
begin
  fTexture := aTexture;
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

  Result := ((Coord.X >= (Position.X - Origin.X - marLeft - borLeft - padLeft) * Scale.X * ActiveWindow.ResScale.X) and
             (Coord.Y >= (Position.Y - Origin.Y - marTop - borTop - padTop) * Scale.Y * ActiveWindow.ResScale.Y) and
             (Coord.X < (Position.X - Origin.X + ClipRect.W + marRight + borRight + padRight) * Scale.X * ActiveWindow.ResScale.X) and
             (Coord.Y < (Position.Y - Origin.Y + ClipRect.H + marBottom + borBottom + padBottom) * Scale.Y * ActiveWindow.ResScale.Y));
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

function TelSprite.Collides(Other: TelSprite): Boolean;
begin

end;

procedure TelSprite.Draw;
var
  IsTexture: Boolean;
begin

  end;
end;

procedure TelSprite.Update(dt: Double = 0.0);
begin
  inherited;

  if (HyperLink <> '') and GetClick then OpenURL(HyperLink);
end;


constructor TelSpriteSheet.Create;
begin
  inherited;

  fTimer := TelTimer.Create;
  fFrame := -1;
end;

destructor TelSpriteSheet.Destroy;
begin
  fTimer.Destroy;

  inherited;
end;

procedure TelSpriteSheet.Define(AnimName: String; aRect: TelRect;
  Length: Integer);
begin

end;

procedure TelSpriteSheet.Play(AnimName: String);
begin

end;

procedure TelSpriteSheet.Stop();
begin
  fTimer.Stop();
end;

procedure TelSpriteSheet.Pause();
begin
  fTimer.Pause();
end;

procedure TelSpriteSheet.UnPause();
begin
  fTimer.UnPause();
end;

procedure TelSpriteSheet.Draw();
begin
  inherited Draw();
end;

procedure TelSpriteSheet.Update(dt: Double);
begin
  inherited Update(dt);
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
    if Index > FSpriteList.Count - 1 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
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
    if Index > FSpriteList.Count - 1 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
  end;

end;

function TelSpriteList.Get(Index: Integer): TelSprite;
begin
  if ((Index >= 0) and (Index <= FSpriteList.Count - 1)) then Result := TelSprite(FSpritelist[Index])
  else begin
    if Index > FSpriteList.Count - 1 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
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
    if Index > FSpriteList.Count - 1 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList: Index > Count');
    if Index < 0 then if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList : Index < Count');
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
    else if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList: Index does not exist');
  end
  else if IsLoggerActive then TelLogger.GetInstance.WriteLog('SpriteList: Index string is empty');
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
    if IsLoggerActive then TelLogger.GetInstance.WriteLog('Could not load file: Wrong file');
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

{
  #############################################################################
  # TelLight                                                                  #
  #############################################################################

  Description:
    TelLight is essentially a sprite with additative blending

  Additional Notes: -

}

constructor TelLight.Create;
begin
  inherited;

  Self.BlendMode := bmAdd;
  Self.Transparent := true;
end;

destructor TelLight.Destroy;
begin
  inherited;
end;

constructor TelParticle.Create;
begin
  inherited;

  fDead := false;
  //fLoop := false;
  //fIsPaused := false;
  //fIsActive := true;

  fMaxLife := 1500;
  fLife := fMaxLife;

  fTimer := TelTimer.Create;
end;

destructor TelParticle.Destroy;
begin
  inherited;
end;

procedure TelParticle.SetLife(Value: Integer);
begin
  fMaxLife := Value;
  fLife := Value;
end;

procedure TelParticle.Start();
begin
  fTimer.Start();
end;

procedure TelParticle.Pause();
begin
  fTimer.Pause();
end;

procedure TelParticle.Stop();
begin
  fTimer.Stop();
end;

procedure TelParticle.Draw();
begin
  inherited;
end;

procedure TelParticle.Update(dt: Double = 0.0);
begin
  inherited;
end;


{ TelParallaxSprite }

constructor TelParallaxSprite.Create;
begin
  inherited Create;
end;

destructor TelParallaxSprite.Destroy;
begin
  inherited Destroy;
end;

procedure TelParallaxSprite.Start();
begin

end;

procedure TelParallaxSprite.Stop();
begin

end;

procedure TelParallaxSprite.Pause();
begin

end;

procedure TelParallaxSprite.UnPause();
begin

end;

procedure TelParallaxSprite.Draw();
begin
  inherited Draw();
end;

procedure TelParallaxSprite.Update(dt: Double);
begin
  inherited Update(dt);
end;

end.
