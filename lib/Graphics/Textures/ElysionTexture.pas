unit ElysionTexture;

{$I Elysion.inc}

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  ElysionColor,
  ElysionTypes,
  ElysionObject,
  ElysionList,

  {$IFDEF USE_VAMPYRE}
  ImagingSDL,
  {$ENDIF}

  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu,
  {$ENDIF}
  SDL,
  SDLUtilsLight,
  SDLTextures,

  SysUtils,
  Classes;

type

  { TelTexture }

  TelTexture = class(TelObject)
    private
      fFilename, fImageType: String;
      fColorKey: TelColor;

      fTransparent: Boolean;

      fWidth, fHeight: Integer;

      procedure SetAutoColorKey(Value: Boolean); inline;
      // Seperate function because of Delphi 7
      procedure SetColorKeyProperty(aColor: TelColor); inline;
      function GetColorKey: TelColor; inline;

      function GetAspectRatio(): Single; inline;

      function IsEmpty(): Boolean; inline;

      //function GetPixel(X, Y: Integer): TelColor;
      //procedure SetPixel(X, Y: Integer; AValue: TelColor);
    public
      TextureSurface: PSDL_Surface;
      TextureID: GLuInt;

      constructor Create; Overload; Override;
      constructor Create(aSize: TelSize); Overload;
      constructor Create(aWidth, aHeight: Integer); Overload;

      destructor Destroy; Override;

      function LoadFromFile(const aFilename: String): Boolean; inline;
      function LoadFromSDLSurface(aSurface: PSDL_Surface): Boolean; inline;
      function LoadFromStream(aStream: TStream): Boolean; inline;
      procedure SaveToStream(aStream: TStream); inline;

      procedure SetColorKey(aColor: TelColor); Overload; inline;
      procedure SetColorKey(aPoint: TelVector2i); Overload; inline;

      function Reload(): Boolean; inline;
    public
      property ColorKey: TelColor read GetColorKey write SetColorKeyProperty;

      //property Pixels[X, Y: Integer]: TelColor read GetPixel write SetPixel;
    published
      property AspectRatio: Single read GetAspectRatio;

      property Empty: Boolean read IsEmpty;

      property ImageType: String read fImageType;

      property Filename: String read fFilename;

      property Transparent: Boolean read fTransparent write SetAutoColorKey;

      property Width: Integer read fWidth;
      property Height: Integer read fHeight;
  end;

  TelTextureList = TelObjectList<TelTexture>;

  TelTextureListHelper = class helper for TelTextureList
  public
    procedure ReloadAllTextures;
  end;

implementation

constructor TelTexture.Create;
begin
  inherited;

  TextureSurface := nil;

  fFilename := '';
  fTransparent := false;
end;

constructor TelTexture.Create(aSize: TelSize);
begin

end;

constructor TelTexture.Create(aWidth, aHeight: Integer);
begin

end;

destructor TelTexture.Destroy;
begin
  if TextureSurface <> nil then
  begin
    SDL_FreeSurface(TextureSurface);
    glDeleteTextures(1, @TextureID);
  end;

  inherited Destroy;
end;

function TelTexture.LoadFromFile(const aFilename: String): Boolean;
begin
  fFilename := aFilename;
  fImageType := ExtractFileExt(aFilename);

  Result := LoadTexture(fFilename, TextureID, fWidth, fHeight);
end;

function TelTexture.LoadFromSDLSurface(aSurface: PSDL_Surface): Boolean; 
begin
  Result := LoadTexture(TextureSurface, TextureID, fWidth, fHeight);
end;

function TelTexture.LoadFromStream(aStream: TStream): Boolean; 
begin
  {$IFDEF USE_VAMPYRE}
    TextureSurface := LoadSDLSurfaceFromStream(aStream);
    if TextureSurface <> nil then
    begin
      Result := true;

      LoadTexture(TextureSurface, TextureID, fWidth, fHeight);
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

    LoadTexture(SDL_ConvertSurface(TextureSurface, TextureSurface^.format, TextureSurface^.flags), TextureID, fWidth, fHeight);
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

    LoadTexture(SDL_ConvertSurface(TextureSurface, TextureSurface^.format, TextureSurface^.flags), TextureID, fWidth, fHeight);
  end;
end;

function TelTexture.Reload(): Boolean;
begin
  if TextureSurface <> nil then
    Result := Self.LoadFromSDLSurface(Self.TextureSurface);
end;


procedure TelTextureListHelper.ReloadAllTextures();
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    TelTexture(Items[i]).Reload();
  end;
end;


end.
