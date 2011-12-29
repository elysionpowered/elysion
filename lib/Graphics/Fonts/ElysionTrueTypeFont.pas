unit ElysionTrueTypeFont;

interface

{$I Elysion.inc}

uses
  ElysionTypes,
  ElysionObject,
  ElysionApplication,
  ElysionGraphics,
  ElysionLogger,
  ElysionContent,
  ElysionUtils,

  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}
  SDL_ttf,
  SDLTextures,
  SDL,

  SysUtils,
  Classes;

type

TFontRender = (rtSolid, rtBlended); // Font Render: rtSolid = Simple with black background under Mac OS X and Linux; rtBlended = pretty, but slow
TFontStyle = (fsBold, fsItalic, fsUnderline); // Font Style: different font styles
TFontStyles = set of TFontStyle; // Font Style Set
TTextStyle = (tsUnicode, tsUTF8, tsNormal); // Text Style: which encoding to use

TelTrueTypeFont = class(TelFontContainer)
  private
    fSurface: PSDL_Surface;
    fFont: PTTF_Font;
    fFontRender: TFontRender;
    fFontStyles: TFontStyles;
    fTextStyle: TTextStyle;
    fColor: TelColor;

    fChange: Boolean;
    fTrim: Boolean;
    fMargin: Integer;
    fLines: Integer;

    fFilename: String;
    fSize: Integer;

    Texture: array[0..1024] of GLuInt;

    function RenderFont(Text: String; FontRender: TFontRender; TextStyle: TTextStyle): PSDL_Surface;

    procedure SetColor(aColor: TelColor); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetColor: TelColor; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetRenderType(aFontRender: TFontRender); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetRenderType: TFontRender; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetFontStyle(aFontStyles: TFontStyles); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetFontStyle: TFontStyles; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetTextStyle(aTextStyle: TTextStyle); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetTextStyle: TTextStyle; {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetFontPointer: PTTF_Font; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  protected
    function GetSize(): Integer; Override;
    procedure SetSize(Value: Integer); Override;

    function GetWidth(): Integer; Override;
    function GetHeight(): Integer; Override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure LoadFromFile(const aFilename: String); Overload;
    procedure LoadFromFile(const aFilename: String; PtSize: Integer); Overload;

    function GetWidth_Text(Text: String): Integer; Override;

    procedure TextOut(Point: TelVector3f; aText: String = ''; LineBreak: Boolean = true); Override;

    property Color: TelColor read GetColor write SetColor;
    property FontPointer: PTTF_Font read GetFontPointer;
  published
    property Filename: String read fFilename;

    property Margin: Integer read fMargin write fMargin;
    property Trim: Boolean read fTrim write fTrim;
    property Lines: Integer read fLines;

    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property TextStyle: TTextStyle read GetTextStyle write SetTextStyle;
    property RenderStyle: TFontRender read GetRenderType write SetRenderType;
end;

TSDLTTFFontContainer = class(TelModuleContainer)
public
  constructor Create; Override;
  destructor Destroy; Override;

  function Initialize(): Boolean; Override;
  procedure Finalize(); Override;
end;

{$IFDEF AUTO_INIT}
var
  Fonts: TSDLTTFFontContainer;
{$ENDIF}

implementation

constructor TSDLTTFFontContainer.Create;
begin
  inherited;

  fDriverName := 'SDL_ttf';

  if (not Application.Initialized) then Application.Initialize;
end;

destructor TSDLTTFFontContainer.Destroy;
begin
  inherited;
end;

function TSDLTTFFontContainer.Initialize(): Boolean;
begin
  if TTF_Init <> 0 then
  begin
    Result := false;
    TelLogger.GetInstance.WriteLog('Could not initialize SDL_TTF');
    Exit;
  end;

  Result := true;
end;

procedure TSDLTTFFontContainer.Finalize();
begin
  ;
end;

//
// TelTrueTypeFont
//
function TelTrueTypeFont.RenderFont(Text: String; FontRender: TFontRender; TextStyle: TTextStyle): PSDL_Surface;
var
  TMP_Surface: PSDL_Surface;
begin

  Result := nil;

  // Quickfix for mulitple line breaks
  if Text = '' then Text := ' ';

  case FontRender of
    rtSolid:
    begin
      case TextStyle of
	tsNormal: TMP_Surface := TTF_RenderText_Solid(fFont, PChar(Text), convCol(fColor));
	tsUnicode: TMP_Surface := TTF_RenderUNICODE_Solid(fFont, PUInt16(Text), convCol(fColor));
	tsUTF8: TMP_Surface := TTF_RenderUTF8_Solid(fFont, PChar(Text), convCol(fColor));
      end;
    end;

    rtBlended:
    begin
      case TextStyle of
	tsNormal: TMP_Surface := TTF_RenderText_Blended(fFont, PChar(Text), convCol(fColor));
	tsUnicode: TMP_Surface := TTF_RenderUNICODE_Blended(fFont, PUInt16(Text), convCol(fColor));
	tsUTF8: TMP_Surface := TTF_RenderUTF8_Blended(fFont, PChar(Text), convCol(fColor));
      end;
    end;

  end;

  Result := SDL_DisplayFormatAlpha(TMP_Surface);
  //Result := TMP_Surface;
  SDL_FreeSurface(TMP_Surface);

end;

constructor TelTrueTypeFont.Create();
begin
  inherited Create;
  fColor := makeCol(0, 0, 0, 255);

  fChange := true;
  fFont := nil;
  fText := '';

  fTrim := true;
  fMargin := 2;
end;

destructor TelTrueTypeFont.Destroy();
begin
  if fFont <> nil then TTF_CloseFont(fFont);
  if (fSurface <> nil) then SDL_FreeSurface(fSurface);

  inherited;
end;

function TelTrueTypeFont.GetFontPointer(): PTTF_Font; 
begin
  Result := fFont;
end;

procedure TelTrueTypeFont.LoadFromFile(const aFilename: String);
begin
  LoadFromFile(aFilename, 10);
end;

procedure TelTrueTypeFont.LoadFromFile(const aFilename: String; PtSize: Integer);
var
  Directory: String;
begin
  if aFilename <> '' then
  begin
    Directory := ExtractFilePath(ParamStr(0));
    fFilename := aFilename;

    if FileExists(Directory + Content.Root + aFilename) then
    begin
      fSize := PtSize;
      fFont := TTF_OpenFont(PChar(Directory + Content.Root + aFilename), PtSize);
      FontStyle := [];
      fTextStyle := tsNormal;
      fFontRender := rtSolid;

      TelLogger.GetInstance.WriteLog('<i>' + Self.UniqueID + '</i><br /> File loaded: ' + aFilename, ltNote, true);

    end else TelLogger.GetInstance.WriteLog('File not found: '+Directory+Content.RootDirectory+aFileName, ltError);
  end else TelLogger.GetInstance.WriteLog('No filename specifies.', ltError);
end;

procedure TelTrueTypeFont.SetFontStyle(aFontStyles: TFontStyles);
var FontFlag: Cardinal;
begin
    if aFontStyles <> fFontStyles then
    begin
      fFontStyles := aFontStyles;
      fChange := true;

      if fFontStyles = [] then TTF_SetFontStyle(fFont, TTF_STYLE_NORMAL)
      else begin
	if fsBold in fFontStyles then FontFlag := TTF_STYLE_BOLD;
	if fsItalic in fFontStyles then FontFlag := FontFlag or TTF_STYLE_ITALIC;
	if fsUnderline in fFontStyles then FontFlag := FontFlag or TTF_STYLE_UNDERLINE;

	TTF_SetFontStyle(fFont, FontFlag)
      end;
    end;
end;

function TelTrueTypeFont.GetFontStyle: TFontStyles; 
begin
  Result := fFontStyles;
end;

procedure TelTrueTypeFont.SetTextStyle(aTextStyle: TTextStyle);
begin
  if aTextStyle <> fTextStyle then
    fTextStyle := aTextStyle;
end;

function TelTrueTypeFont.GetTextStyle: TTextStyle; 
begin
  Result := fTextStyle;
end;

procedure TelTrueTypeFont.SetRenderType(aFontRender: TFontRender);
begin
  if aFontRender <> fFontRender then
    fFontRender := aFontRender;
end;

function TelTrueTypeFont.GetRenderType: TFontRender; 
begin
  Result := fFontRender;
end;

procedure TelTrueTypeFont.SetColor(aColor: TelColor);
begin
  if not fColor.Equals(aColor) then
    fColor := aColor;
end;

function TelTrueTypeFont.GetColor: TelColor; 
begin
  Result := fColor;
end;

function TelTrueTypeFont.GetHeight: Integer;
var
  tmp: Integer;
begin
  inherited;

  tmp := 0;

  case fTextStyle of
    tsNormal: TTF_SizeText(fFont, PChar(fText), tmp, Result);
    tsUTF8: TTF_SizeUTF8(fFont, PChar(fText), tmp, Result);
    tsUnicode: TTF_SizeUNICODE(fFont, PUInt16(fText), tmp, Result);
  end;
end;

function TelTrueTypeFont.GetWidth: Integer;
begin
  Result := GetWidth_Text(fText);
end;

procedure TelTrueTypeFont.SetSize(Value: Integer);
var
  Directory: String;
begin
  if Value <> fSize then
  begin
    fSize := Value;

    if fFont <> nil then
    begin
      TTF_CloseFont(fFont);

      Directory := ExtractFilePath(ParamStr(0));
      fFont := TTF_OpenFont(PChar(Directory + Content.RootDirectory + fFilename), fSize);
    end;
  end;
end;

function TelTrueTypeFont.GetSize(): Integer;
begin
  Result := fSize;
end;

function TelTrueTypeFont.GetWidth_Text(Text: String): Integer;
var
  tmp: Integer;
begin
  //inherited;

  tmp := 0;

  case fTextStyle of
    tsNormal: TTF_SizeText(fFont, PChar(Text), Result, tmp);
    tsUTF8: TTF_SizeUTF8(fFont, PChar(Text), Result, tmp);
    tsUnicode: TTF_SizeUNICODE(fFont, PUInt16(Text), Result, tmp);
  end;
end;

procedure TelTrueTypeFont.TextOut(Point: TelVector3f; aText: String = ''; LineBreak: Boolean = true);
var
  initial: PSDL_Surface;
  TextNew: String;
  TextList: TStringList;
  i: Integer;
  tmpTop, tmpTop2: Single;
  tmpWidth, tmpHeight: Longint;
begin
  if aText <> fText then fText := aText;

  if ((fFont <> nil) and (fText <> '')) then
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

    tmpTop := Point.Y;

    glEnable(GL_TEXTURE_2D);
    for i := 0 to TextList.Count - 1 do
    begin

      initial := RenderFont(TextList.Strings[i], fFontRender, fTextStyle);
      if initial = nil then Exit;

      LoadTexture(initial, Texture[i], tmpWidth, tmpHeight);

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
        DrawQuad(Point.X * ActiveWindow.ResScale.X ,
                 Point.Y * ActiveWindow.ResScale.Y + (GetHeight + Margin) * i,
                 GetWidth_Text(TextList.Strings[i]) * ActiveWindow.ResScale.X,
                 GetHeight * ActiveWindow.ResScale.Y,
                 Point.Z);
        glDisable(GL_BLEND);

         glFinish(); // wait till operation is finished

  	glDeleteTextures(1, @Texture[i]); // don't forget this line, it muerto importanto! :) or else: Bad memory leak, it's bad, mkay!


    end;
    glDisable(GL_TEXTURE_2D);
    TextList.Free;

  end;



end;

{$IFDEF AUTO_INIT}
initialization
  Fonts := TSDLTTFFontContainer.Create;
  Fonts.Initialize();

finalization
  Fonts.Finalize();
{$ENDIF}

end.
