unit ElysionTexture;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}
{$I Elysion.inc}

interface

uses
  ElysionTypes,
  ElysionObject,

  {$IFDEF USE_VAMPYRE}   ImagingSDL, {$ENDIF}
  {$IFDEF USE_SDL_IMAGE} SDL_image,  {$ENDIF}
  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}
  SDL,
  SDLUtils,
  SDLTextures,

  SysUtils,
  Classes;

type
  TelTexture = class(TelObject)
    private
      fFilename, fImageType: String;
      fColorKey: TelColor;

      fTransparent: Boolean;

      fWidth, fHeight: Integer;

      procedure SetAutoColorKey(Value: Boolean); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      // Seperate function because of Delphi 7
      procedure SetColorKeyProperty(aColor: TelColor); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function GetColorKey: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetAspectRatio(): Single; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function IsEmpty(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    public
      TextureSurface: PSDL_Surface;
      TextureID: GLuInt;

      constructor Create; Override;
      destructor Destroy; Override;

      function LoadFromFile(const aFilename: String): Boolean;
      function LoadFromSDLSurface(aSurface: PSDL_Surface): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadFromStream(aStream: TStream): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SaveToStream(aStream: TStream); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure SetColorKey(aColor: TelColor); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure SetColorKey(aPoint: TelVector2i); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function Reload(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      property ColorKey: TelColor read GetColorKey write SetColorKeyProperty;
    published
      property AspectRatio: Single read GetAspectRatio;

      property Empty: Boolean read IsEmpty;

      property ImageType: String read fImageType;

      property Filename: String read fFilename;

      property Transparent: Boolean read fTransparent write SetAutoColorKey;

      property Width: Integer read fWidth;
      property Height: Integer read fHeight;
  end;

  TelTextureList = class(TelObject)
    private
      fTextureList: TList;
      Head: String[14];

      function Get(Index: Integer): TelTexture;
      function GetPos(Index: String): Integer;
      procedure Put(Index: Integer; const Item: TelTexture);
      procedure PutS(Index: String; const Item: TelTexture);
      function GetS(Index: String): TelTexture;

      function GetCount: Integer;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Insert(Index: Integer; Texture: TelTexture);
      function  Add(Texture: TelTexture): Integer;
      procedure Delete(Index: Integer);
      //procedure LoadFromStream(Stream : TFileStream);
      //procedure SaveToStream(Stream : TFileStream);

      //procedure LoadFromFile(Filename: String);
      //procedure SaveToFile(Filename: String);

      procedure ReloadAllTextures(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      property Items[Index: Integer]: TelTexture read Get write Put; default;
      property Find[Index: String]: TelTexture read GetS write PutS;
    published
      property Count: Integer read GetCount;
  end;

  TelTextureAtlas = class(TelObject)

  end;

  // Factory
  TelTextureManager = class(TelObject)
    private
      fTextureList: TelTextureList;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      function CreateNewTexture(aFilename: String): TelTexture; Overload;
      function CreateNewTexture(aSurface: PSDL_Surface): TelTexture; Overload;

      function ForceNewTexture(aFilename: String): TelTexture; Overload;
      function ForceNewTexture(aSurface: PSDL_Surface): TelTexture; Overload;

      procedure ReloadAllTextures(); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    published
      property TextureList: TelTextureList read fTextureList write fTextureList;
  end;

{$IFDEF AUTO_INIT}
var
  TextureManager: TelTextureManager;
{$ENDIF}

implementation

constructor TelTexture.Create;
begin
  inherited;

  TextureSurface := nil;

  fFilename := '';
  fTransparent := false;
end;

destructor TelTexture.Destroy;
begin
  if TextureSurface <> nil then
  begin
    SDL_FreeSurface(TextureSurface);
    glDeleteTextures(1, @TextureID);
  end;

  inherited;
end;

function TelTexture.LoadFromFile(const aFilename: String): Boolean;
begin
  fFilename := aFilename;

  fImageType := ExtractFileExt(aFilename);

  {$IFDEF USE_SDL_IMAGE}
    TextureSurface := IMG_Load(PChar(aFilename));
  {$ENDIF}
  {$IFDEF USE_VAMPYRE}
    TextureSurface := LoadSDLSurfaceFromFile(aFilename);
  {$ENDIF}

  fWidth := TextureSurface^.w;
  fHeight := TextureSurface^.h;

  Result := LoadTexture(fFilename, TextureID);
end;

function TelTexture.LoadFromSDLSurface(aSurface: PSDL_Surface): Boolean; 
begin
  TextureSurface := SDL_ConvertSurface(aSurface, aSurface^.format, aSurface^.flags);

  fWidth := TextureSurface^.w;
  fHeight := TextureSurface^.h;

  Result := LoadTexture(TextureSurface, TextureID);
end;

function TelTexture.LoadFromStream(aStream: TStream): Boolean; 
begin
  {$IFDEF USE_VAMPYRE}
    TextureSurface := LoadSDLSurfaceFromStream(aStream);
    if TextureSurface <> nil then
    begin
      Result := true;

      fWidth := TextureSurface^.w;
      fHeight := TextureSurface^.h;

      LoadTexture(TextureSurface, TextureID);
    end else Result := false;
  {$ENDIF}
end;

procedure TelTexture.SaveToStream(aStream: TStream); 
begin
  {$IFDEF USE_VAMPYRE}
    SaveSDLSurfaceToStream('png', aStream, TextureSurface);
  {$ENDIF}
end;

function TelTexture.GetAspectRatio(): Single;
begin
  Result := Self.Width / Self.Height;
end;

procedure TelTexture.SetAutoColorKey(Value: Boolean); 
begin
  fTransparent := Value;
  if fTransparent then SetColorKey(makeV2i(0, 0));
end;

function TelTexture.IsEmpty(): Boolean;
begin
  {$IFNDEF USE_DGL_HEADER}
    if glIsTexture(Self.TextureID) = GL_TRUE then Result := false
    else Result := true;
  {$ELSE}
    Result := (not glIsTexture(Self.TextureID));
  {$ENDIF}
end;

procedure TelTexture.SetColorKey(aColor: TelColor); 
begin
  if TextureSurface <> nil then
  begin
    fColorKey := aColor;

    SDL_SetColorKey(TextureSurface, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_MapRGB(TextureSurface^.Format, aColor.R, aColor.G, aColor.B));

    LoadTexture(SDL_ConvertSurface(TextureSurface, TextureSurface^.format, TextureSurface^.flags), TextureID);
  end;
end;

procedure TelTexture.SetColorKeyProperty(aColor: TelColor); 
begin
  SetColorKey(aColor);
end;

function TelTexture.GetColorKey: TelColor; 
begin
  Result := fColorKey;
end;

procedure TelTexture.SetColorKey(aPoint: TelVector2i); 
begin
  if TextureSurface <> nil then
  begin

    SDL_SetColorKey(TextureSurface, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_GetPixel(TextureSurface, aPoint.X, aPoint.Y));

    fColorKey.R := SDL_GetPixel(TextureSurface, aPoint.X, aPoint.Y) and $FF;
    fColorKey.G := (SDL_GetPixel(TextureSurface, aPoint.X, aPoint.Y) shr 8) and $FF;
    fColorKey.B := (SDL_GetPixel(TextureSurface, aPoint.X, aPoint.Y) shr 16) and $FF;

    // Need to check if that's correct - espacially when loading half-transparent PNGs
    if (fColorKey.A <> 255) then fColorKey.A := 255;

    LoadTexture(SDL_ConvertSurface(TextureSurface, TextureSurface^.format, TextureSurface^.flags), TextureID);
  end;
end;

function TelTexture.Reload(): Boolean;
begin
  if TextureSurface <> nil then Result := Self.LoadFromSDLSurface(Self.TextureSurface);
end;

constructor TelTextureList.Create;
begin
  inherited;

  fTextureList := TList.Create;
  Head := 'TelTextureList';
end;

destructor TelTextureList.Destroy;
var
  i: Integer;
begin
  for i := 0 to fTextureList.Count - 1 do
  begin
    TelTexture(fTextureList[i]).Destroy;
  end;
  fTextureList.Free;

  inherited;
end;

function TelTextureList.GetCount: Integer;
begin
  Result := fTextureList.Count;
end;

function TelTextureList.Get(Index: Integer): TelTexture;
begin
  if ((Index >= 0) and (Index <= fTextureList.Count - 1)) then Result := TelTexture(fTextureList[Index]);
end;

function TelTextureList.GetPos(Index: String): Integer;
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To fTextureList.Count - 1 Do
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

procedure TelTextureList.Put(Index: Integer; const Item: TelTexture);
var
  TmpTexture: TelTexture;
begin
  if ((Index >= 0) and (Index <= fTextureList.Count - 1)) then
  begin
    TmpTexture := Get(Index);
    TmpTexture.Destroy;
    Insert(Index, Item);
  end;

end;

procedure TelTextureList.PutS(Index: String; const Item: TelTexture);
var
  TMP: Integer;
  TmpTexture: TelTexture;
Begin
  if (Index <> '') then
  begin
    TmpTexture := GetS(Index);
	if TmpTexture <> nil then
	begin
	  TMP := GetPos(Index);
      TmpTexture.Destroy;
      Insert(TMP, Item);
	end;
   end;
end;

function TelTextureList.GetS(Index: String): TelTexture;
Var TMP: Integer;
Begin
  TMP := GetPos(Index);
  if TMP >= 0 then Result := TelTexture(fTextureList[TMP])
			  else Result := nil;
end;

procedure TelTextureList.Insert(Index: Integer; Texture: TelTexture);
begin
  if ((Index >= 0) and (Index <= fTextureList.Count - 1)) then fTextureList.Insert(Index, Texture);
end;

function TelTextureList.Add(Texture: TelTexture): Integer;
begin
  Result := fTextureList.Add(Texture);
end;

procedure TelTextureList.Delete(Index: Integer);
var
  TmpTexture: TelTexture;
begin
  if ((Index >= 0) and (Index <= fTextureList.Count - 1)) then
  begin
    TmpTexture := Get(Index);
    TmpTexture.Destroy;
    fTextureList.Delete(Index);
  end;

end;

procedure TelTextureList.ReloadAllTextures();
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    TelTexture(fTextureList[i]).Reload();
  end;
end;

constructor TelTextureManager.Create;
begin
  inherited;

  fTextureList := TelTextureList.Create;
end;

destructor TelTextureManager.Destroy;
begin
  fTextureList.Free;

  inherited;
end;

function TelTextureManager.CreateNewTexture(aFilename: String): TelTexture;
var
  i: Integer;
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  // Is Texture already in stack?
  if fTextureList.Count > 0 then
  begin
    for i := 0 to fTextureList.Count - 1 do
    begin
      if fTextureList.Items[i].Filename = aFilename then
      begin
        Result := fTextureList.Items[i];
        Exit;
      end;
    end;
  end;

  // If not, add it
  tmpTexture.LoadFromFile(aFilename);
  fTextureList.Add(tmpTexture);
  Result := tmpTexture;

end;

// Fix this! Is the same as ForceNewTexture
function TelTextureManager.CreateNewTexture(aSurface: PSDL_Surface): TelTexture;
var
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  tmpTexture.LoadFromSDLSurface(aSurface);
  fTextureList.Add(tmpTexture);
  Result := tmpTexture;

end;

function TelTextureManager.ForceNewTexture(aFilename: String): TelTexture;
var
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  tmpTexture.LoadFromFile(aFilename);
  fTextureList.Add(tmpTexture);
  Result := tmpTexture;
end;

function TelTextureManager.ForceNewTexture(aSurface: PSDL_Surface): TelTexture;
var
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  tmpTexture.LoadFromSDLSurface(aSurface);
  fTextureList.Add(tmpTexture);
  Result := tmpTexture;

end;

procedure TelTextureManager.ReloadAllTextures();
begin
  fTextureList.ReloadAllTextures();
end;

{$IFDEF AUTO_INIT}
initialization
  TextureManager := TelTextureManager.Create;

finalization
  TextureManager.Destroy;
{$ENDIF}

end.
