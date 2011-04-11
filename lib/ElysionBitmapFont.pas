unit ElysionBitmapFonts;

interface

{$I Elysion.inc}

uses
  SDL,
  SFont;

type
  TelBitmapFont = class
    private
        fAlpha: Byte;

        fFontInfo: TSFont_FontInfo;
    public
        // Constructor
        constructor Create;

	// Destructor
        destructor Destroy;

	// Load BitmapFont from filename
        procedure LoadFromFile(Filename: String);

	// Set transparency from Color
        procedure SetTransparency(Color: TelColor); Overload;

	// Set transparency from Coordinate
        procedure SetTransparency(Coord: TelVector2i); Overload;


        procedure SetAlpha(Alpha: Byte);
        function GetAlpha: Byte;

        function GetWidth(Text: String): Integer;
        function GetHeight: Integer;

	// Draw text at designed position
        procedure TextOut(Point: TelVector2i; Text: String; AlignV: TAlignVertical = avNone; AlignH: TAlignHorizontal = ahNone);
    published
        property Alpha: Byte read GetAlpha write SetAlpha;

        property Height: Integer read GetHeight;

end;

implementation

constructor TelBitmapFont.Create;
begin
  inherited Create;

  FAlpha := 255;
end;

destructor TelBitmapFont.Destroy;
begin
  if fFontInfo.Surface <> nil then SDL_FreeSurface(fFontInfo.Surface);

  inherited Destroy;
end;

procedure TelBitmapFont.LoadFromFile(Filename: String);
var Directory: String;
begin
  if Filename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(Filename) then
    begin
      {$IFDEF USE_SDL_IMAGE}
	    FFontInfo.Surface := IMG_Load(PChar(Directory + Filename));
	  {$ENDIF}
	  {$IFDEF USE_VAMPYRE}
	    FFontInfo.Surface := LoadSDLSurfaceFromFile(Directory+Application.ContentPath+FileName);
	  {$ENDIF}
      SFont_InitFont2(@FFontInfo);
    {$IFNDEF USE_LOGGER}
	  end;
    {$ELSE}
    end else TelLogger.GetInstance.WriteLog('File not found: '+Directory+Application.ContentPath+FileName, ltError);
    {$ENDIF}
  {$IFNDEF USE_LOGGER}
  end;
  {$ELSE}
  end else TelLogger.GetInstance.WriteLog('No filename specifies.', ltError);
  {$ENDIF}

end;

procedure TelBitmapFont.SetTransparency(Color: TelColor);
begin
  if FFontInfo.Surface <> nil then
    SDL_SetColorKey(FFontInfo.Surface, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_MapRGB(FFontInfo.Surface.Format, Color.R, Color.G, Color.B));
end;

procedure TelBitmapFont.SetTransparency(Coord: TelVector2i);
begin
  if FFontInfo.Surface <> nil then
    SDL_SetColorKey(FFontInfo.Surface, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_GetPixel(FFontInfo.Surface, Coord.X, Coord.Y));
end;

procedure TelBitmapFont.SetAlpha(Alpha: Byte);
begin
  FAlpha := Alpha;
end;

function TelBitmapFont.GetAlpha: Byte;
begin
  Result := FAlpha;
end;

function TelBitmapFont.GetWidth(Text: String): Integer;
begin
  Result := SFont_TextWidth2(@FFontInfo, PChar(Text));
end;

function TelBitmapFont.GetHeight: Integer;
begin
  Result := FFontInfo.Surface.h;
end;

procedure TelBitmapFont.TextOut(Point: TelVector2i; Text: String;  AlignV: TAlignVertical = avNone; AlignH: TAlignHorizontal = ahNone);
begin
  if FAlpha <> 255 then SDL_SetAlpha(FFontInfo.Surface, SDL_SRCALPHA, FAlpha);

  case AlignV of
    avTop: Point.Y := 0;
	avCenter: Point.Y := (Surface.SDL_Surface.h - GetHeight) div 2;
	avButtom: Point.Y := Surface.SDL_Surface.h - GetHeight;
  end;
  case AlignH of
    ahLeft: Point.X := 0;
	ahCenter: Point.X := (Surface.SDL_Surface.w - GetWidth(Text)) div 2;
	ahRight: Point.X := Surface.SDL_Surface.w - GetWidth(Text);
  end;

  SFont_Write2(Surface.SDL_Surface, @FFontInfo, Point.X, Point.Y, PChar(Text));
end;

end.
