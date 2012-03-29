unit ElysionBitmapFont;

interface

{$I Elysion.inc}

uses
  Classes,
  SysUtils,
  SDL,
  SDLTextures,
  SFont,
  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}
  {$IFDEF USE_VAMPYRE}
  ImagingSDL,
  {$ENDIF}

  ElysionApplication,
  ElysionUtils,
  ElysionTypes,
  ElysionContent,
  ElysionLogger,
  ElysionObject,
  ElysionGraphics;

type
  TelBitmapFont = class(TelFontContainer)
    private
      fFontInfo: TSFont_FontInfo;
      fLines: Integer;

      Texture: array[0..1024] of GLuInt;
      fMargin: Integer;
      fTrim: Boolean;
    protected
      function GetHeight(): Integer; Override;
      function GetWidth(): Integer; Override;

      function GetSize(): Integer; Override;
      procedure SetSize(Value: Integer); Override;
    public
      // Constructor
      constructor Create; Override;

      // Destructor
      destructor Destroy; Override;

      // Load BitmapFont from filename
      procedure LoadFromFile(const aFilename: String); Override;

      // Set color key from Color
      //procedure SetColorKey(Color: TelColor); Overload;

      // Set color key from Coordinate
      //procedure SetColorKey(Coord: TelVector2i); Overload;

      function GetWidth_Text(aText: String): Integer;

      procedure TextOut(aPoint: TelVector3f; aText: String = ''; LineBreak: Boolean = true);
    published
      property Lines: Integer read fLines;

      property Trim: Boolean read fTrim write fTrim;
      property Margin: Integer read fMargin write fMargin;
    end;

implementation

constructor TelBitmapFont.Create;
begin
  inherited Create;

  fLines := 0;
  fTrim := true;
  fMargin := 2;
end;

destructor TelBitmapFont.Destroy;
begin
  if fFontInfo.Surface <> nil then SDL_FreeSurface(fFontInfo.Surface);

  inherited Destroy;
end;

procedure TelBitmapFont.LoadFromFile(const aFilename: String);
var Directory: String;
begin
  if aFilename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));

    if FileExists(aFilename) then
    begin
      {$IFDEF USE_SDL_IMAGE}
        fFontInfo.Surface := IMG_Load(PChar(Directory + Content.Root + aFilename));
      {$ENDIF}
      {$IFDEF USE_VAMPYRE}
        fFontInfo.Surface := LoadSDLSurfaceFromFile(Directory + Content.Root + aFileName);
      {$ENDIF}
      SFont_InitFont2(@fFontInfo);

      if isLoggerActive then TelLogger.GetInstance.WriteLog('<i>' + Self.UniqueID + '</i><br /> File loaded: ' + aFilename, ltNote, true);
    end else if isLoggerActive then TelLogger.GetInstance.WriteLog('File not found: '+Directory + Content.Root + aFileName, ltError);
  end else if isLoggerActive then TelLogger.GetInstance.WriteLog('No filename specifies.', ltError);
end;

(*procedure TelBitmapFont.SetColorKey(Color: TelColor);
begin
  if fFontInfo.Surface <> nil then
    SDL_SetColorKey(fFontInfo.Surface, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_MapRGB(fFontInfo.Surface.Format, Color.R, Color.G, Color.B));
end;

procedure TelBitmapFont.SetColorKey(Coord: TelVector2i);
begin
  if fFontInfo.Surface <> nil then
    SDL_SetColorKey(fFontInfo.Surface, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_GetPixel(fFontInfo.Surface, Coord.X, Coord.Y));
end;*)

function TelBitmapFont.GetWidth_Text(aText: String): Integer;
begin
  Result := SFont_TextWidth2(@fFontInfo, PChar(aText));
end;

function TelBitmapFont.GetWidth(): Integer;
begin
  Result := GetWidth_Text(fText);
end;

function TelBitmapFont.GetHeight(): Integer;
begin
  Result := fFontInfo.Surface.h;
end;

function TelBitmapFont.GetSize(): Integer;
begin
  Result := 0;
end;

procedure TelBitmapFont.SetSize(Value: Integer);
begin
  Value := 0;
end;

procedure TelBitmapFont.TextOut(aPoint: TelVector3f; aText: String = ''; LineBreak: Boolean = true);
var
  initial: PSDL_Surface;
  TextNew: String;
  TextList: TStringList;
  rmask, gmask, bmask, amask: Uint32;
  tmpTop: Single;
  i: Integer;
begin
  if aText <> fText then fText := aText;

  if (fText <> '') then
  begin
    if LineBreak then
    begin
      TextNew := StringReplace(fText, '\n', #13, [rfReplaceAll, rfIgnoreCase]);
      TextList := Split(TextNew, #13, fTrim);
      fLines := TextList.Count;
    end else
    begin
      TextList.Add(fText);
      fLines := 1;
    end;

    tmpTop := aPoint.Y;

    if SDL_BYTEORDER = SDL_BIG_ENDIAN then
    begin
      rmask:=$ff000000;
      gmask:=$00ff0000;
      bmask:=$0000ff00;
      //amask:=$000000ff;
      amask := 0;
    end else
    begin
      rmask:=$000000ff;
      gmask:=$0000ff00;
      bmask:=$00ff0000;
      //amask:=$ff000000;
      amask := 0;
    end;



    glEnable(GL_TEXTURE_2D);
    for i := 0 to TextList.Count - 1 do
    begin

      initial := SDL_CreateRGBSurface(SDL_SWSURFACE, Self.GetWidth_Text(TextList.Strings[i]), Self.Height, 24, rmask, gmask, bmask, amask);
      if not Assigned(initial) then Exit;


      SFont_Write2(initial, @fFontInfo, 0, 0, PChar(TextList.Strings[i]));

      LoadTexture(initial, Texture[i]);

      glColor3f(1.0, 1.0, 1.0);
      Bind(Texture[i]);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);

      //if LineBreak then
      //begin
        //tmpTop2 := tmpTop + (GetHeight + Margin) * i;
        //Point.Y := tmpTop2;
      //end;

        (*case AlignV of
            avNone:
              begin
                tmpTop2 := tmpTop + (GetHeight + Margin) * i;
                Point.Y := tmpTop2;
              end;
            avTop: Point.Y := 0 + (GetHeight + Margin) * i;
    	  avCenter: Point.Y := ((ActiveWindow.Height - GetHeight) div 2) + (GetHeight + Margin) * i;
    	  avBottom: Point.Y := ActiveWindow.Height - GetHeight - (GetHeight + Margin) * (TextList.Count - 1) + (GetHeight + Margin) * i;
        end;
    	case AlignH of
          ahLeft: Point.X := 0;
    	  ahCenter: Point.X := (ActiveWindow.Width - GetWidth_Text(TextList.Strings[i])) div 2;
    	  ahRight: Point.X := ActiveWindow.Width - GetWidth_Text(TextList.Strings[i]);
        end;*)

        //glScalef(ActiveWindow.ResScale.X, ActiveWindow.ResScale.Y, 1);
        DrawQuad(aPoint.X * ActiveWindow.ResScale.X ,
                 aPoint.Y * ActiveWindow.ResScale.Y + (GetHeight + Margin) * i,
                 GetWidth_Text(TextList.Strings[i]) * ActiveWindow.ResScale.X,
                 GetHeight * ActiveWindow.ResScale.Y,
                 aPoint.Z);

        //DrawQuad(aPoint.X, aPoint.Y, 100, GetHeight, 0);
        glDisable(GL_BLEND);

        glFinish(); // wait till operation is finished

  	glDeleteTextures(1, @Texture[i]); // don't forget this line, it muerto importanto! :) or else: Bad memory leak, it's bad, mkay!


    end;
    glDisable(GL_TEXTURE_2D);
    TextList.Free;


  end;

  //if FAlpha <> 255 then SDL_SetAlpha(fFontInfo.Surface, SDL_SRCALPHA, FAlpha);

  (*case AlignV of
    avTop: Point.Y := 0;
	avCenter: Point.Y := (Surface.SDL_Surface.h - GetHeight) div 2;
	avButtom: Point.Y := Surface.SDL_Surface.h - GetHeight;
  end;
  case AlignH of
    ahLeft: Point.X := 0;
	ahCenter: Point.X := (Surface.SDL_Surface.w - GetWidth(Text)) div 2;
	ahRight: Point.X := Surface.SDL_Surface.w - GetWidth(Text);
  end;*)
end;

end.
