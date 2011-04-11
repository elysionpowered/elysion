//----------------------------------------------------------------------------
//
// Author        : Johannes Stein
// Email         : johannesstein@freeze-dev.de
// Website       : http://www.freeze-dev.de
// Version 	     : 1.41
// Last modified : 10-10-17
// 
// Description   : Inspired by Jan Horn's Textures.pas I wrote a little
//				   texture loader for SDL + OpenGL to load images or 
//                 SDL surfaces and bind them to an OpenGL object
//
// License       : MIT license
//			       For more information see 
//				     http://www.opensource.org/licenses/mit-license.php
//
// What is needed before using SDLTextures.pas?
//   dglOpenGL (OpenGL header): http://www.delphigl.com
//     You can also use the old OpenGL headers from Delphi or similar
//     OpenGL headers. Just modify the uses clause.
//   SDL headers: http://jedi-sdl.pascalgamedevelopment.com
//     You only need these if you're using Delphi or FreePascal < 2.2.2
//
// Usage         : Load your texture
//				   from file: LoadTexture(Filename, Texture)
//					     e.g. LoadTexture('mytexture.png', MyTexture)
//				   from surface: LoadTexture(Surface, Texture)
//					  	    e.g. LoadTexture(MySurface, MyTexture)
//
//				   After your texture has been loaded you can
//				   bind it on your object with either Bind(Texture)
//				   or glBindTexture(GL_TEXTURE_2D, Texture)
//
//				   Sine qua non: You need the line
//                 glEnable(GL_TEXTURE_2D)
//                 somewhere in your code or the textures won't be displayed
//                 (Fun fact: Sine qua non is also the name of a Battlestar Galactica episode.
//                  Watch it if you haven't already....)
//
//----------------------------------------------------------------------------

unit SDLTextures;

{$I Elysion.inc}

interface

uses 
	SDL,
	SDLUtils,
	{$IFDEF USE_SDL_IMAGE}
	SDL_Image,
	{$ENDIF}
	{$IFDEF USE_VAMPYRE}
	ImagingSDL,
        ImagingOpenGL,
	{$ENDIF}
	{$IFDEF USE_DGL_HEADER}
    	dglOpenGL,
    {$ELSE}
    	gl, glu, glext,
    {$ENDIF}
	SysUtils;

type
  TTGAHEADER = packed record
    tfType  : Byte;
    tfColorMapType : Byte;
    tfImageType    : Byte;
    tfColorMapSpec : Array[0..4] of Byte;
    tfOrigX    : Word;
    tfOrigY    : Word;
    tfWidth    : Word;
    tfHeight   : Word;
    tfBpp      : Byte;
    tfImageDes : Byte;
  end;


// Returns the next power of 2
function PowerOfTwo(Input: Integer): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function SupportsNonPowerOfTwo: Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function SupportsARB_RectangleTexture(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function SupportsEXT_RectangleTexture(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
function SupportsRectangleTexture(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

function SupportsFramebufferObject(): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}
	
function LoadTexture(aFilename: String; var Texture: GLuint): Boolean; Overload;
function LoadTexture(Surface: PSDL_Surface; var Texture: GLuint): Boolean; Overload;

function SaveSDLTextureToTGA(aFilename: String; aSurface: PSDL_Surface): Boolean;

procedure SwapBGR(SDL_Surface: PSDL_Surface);

procedure Bind(var Texture: GLuint); {$IFDEF CAN_INLINE} inline; {$ENDIF}

var
  ErrorString: String;

implementation

function SupportsNonPowerOfTwo: Boolean;
begin
  if AnsiPos('GL_ARB_texture_non_power_of_two', glGetString(GL_EXTENSIONS)) <> 0 then Result := true else Result := false;
end;

function SupportsARB_RectangleTexture(): Boolean;
begin
  if AnsiPos('GL_ARB_texture_rectangle', glGetString(GL_EXTENSIONS)) <> 0 then Result := true else Result := false;
end;

function SupportsEXT_RectangleTexture(): Boolean;
begin
  if AnsiPos('GL_EXT_texture_rectangle', glGetString(GL_EXTENSIONS)) <> 0 then Result := true else Result := false;
end;

function SupportsRectangleTexture(): Boolean;
begin
  if (SupportsARB_RectangleTexture or SupportsEXT_RectangleTexture) then Result := true else Result := false;
end;

function SupportsFramebufferObject(): Boolean;
begin
  if AnsiPos('GL_EXT_framebuffer_object', glGetString(GL_EXTENSIONS)) <> 0 then Result := true else Result := false;
end;

(*
 * See for more information: http://www.gamedev.net/community/forums/topic.asp?topic_id=284259
 *)
function PowerOfTwo(Input: Integer): Integer;
var
  Value: Integer;
begin
  Value := 1;

  while ( Value < Input ) do Value := Value shl 1;
  Result := Value;
end;

// http://osdl.sourceforge.net/main/documentation/rendering/SDL-openGL-examples.html
function CreateTexture(SDL_Surface: PSDL_Surface; BGR_Order: Boolean = false): GLuInt;
var
  NewSurface: PSDL_Surface;
  Texture: GLuInt;
  area: TSDL_Rect;
  rmask, gmask, bmask, amask: UInt32;
  saved_flags: UInt32;
  saved_alpha: UInt8;

  BuildMipMap: Boolean;
begin
  Result := 0;

  if SupportsNonPowerOfTwo then BuildMipMap := false
     else BuildMipMap := true;

  //raise Exception.Create(glGetString(GL_VENDOR));

  if Assigned(SDL_Surface) then
  begin

    // Set up Power-of-two width and height for mipmaps
    //SrfWidth := PowerOfTwo(SDL_Surface^.w);
    //SrfHeight := PowerOfTwo(SDL_Surface^.h);

    // Set up mask for new surface
    if SDL_BYTEORDER = SDL_LIL_ENDIAN then
    begin
      rmask := $000000FF;
      gmask := $0000FF00;
      bmask := $00FF0000;
      amask := $FF000000;
    end else
    begin
      rmask := $FF000000;
      gmask := $00FF0000;
      bmask := $0000FF00;
      amask := $000000FF;
    end;

    // Create new surface, quit and show error if it fails
    NewSurface := SDL_CreateRGBSurface(SDL_SWSURFACE, SDL_Surface^.w, SDL_Surface^.h, 32, rmask, gmask, bmask, amask);

    if (NewSurface = nil) then
    begin
      ErrorString := 'SDLTextures.CreateTexture: Could not create RGB Surface';
      Exit;
    end;

    // Alpha stuff, see: http://twomix.devolution.com/pipermail/sdl/2002-September/049078.html
    saved_flags := SDL_Surface^.flags and (SDL_SRCALPHA or SDL_RLEACCELOK);
    saved_alpha := SDL_Surface^.format^.alpha;

    if ((saved_flags and SDL_SRCALPHA) = SDL_SRCALPHA) then SDL_SetAlpha(SDL_Surface, 0, 0);

    // Set are to be blitted
    area.x := 0;
    area.y := 0;
    area.w := SDL_Surface^.w;
    area.h := SDL_Surface^.h;

    // Blit old surface to new texture surface
    SDL_BlitSurface( SDL_Surface, @area, NewSurface, @area ) ;

    // Apply saved alpha
    if((saved_flags and SDL_SRCALPHA) = SDL_SRCALPHA) then SDL_SetAlpha(SDL_Surface, saved_flags, saved_alpha);

    // Create OpenGL texture
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);

    glGenTextures(1, @Texture);
    glBindTexture(GL_TEXTURE_2D, Texture);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

    // Mipmap Building deluxe
    {$IFDEF USE_DGL_HEADER}
    if (GL_VERSION_1_4) then
    {$ELSE}
    if (Load_GL_version_1_4) then
    {$ENDIF}
    begin
      if BuildMipMap then
      begin
        if SupportsFramebufferObject then
        begin
          glEnable(GL_TEXTURE_2D); // Counters ATI driver bug
          glGenerateMipmapEXT(GL_TEXTURE_2D);
        end else glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, 1);
      end;
    end else
    begin
      if NewSurface^.format^.BytesPerPixel > 3 then
      begin
        if BGR_Order then
          gluBuild2DMipmaps(GL_TEXTURE_2D, GL_BGRA, NewSurface^.w, NewSurface^.h, GL_BGRA, GL_UNSIGNED_BYTE, NewSurface^.pixels)
        else
          gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, NewSurface^.w, NewSurface^.h, GL_RGBA, GL_UNSIGNED_BYTE, NewSurface^.pixels);
      end else
      begin
        if BGR_Order then
          gluBuild2DMipmaps(GL_TEXTURE_2D, GL_BGR, NewSurface^.w, NewSurface^.h, GL_BGR, GL_UNSIGNED_BYTE, NewSurface^.pixels)
        else
          gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB, NewSurface^.w, NewSurface^.h, GL_RGB, GL_UNSIGNED_BYTE, NewSurface^.pixels);
      end;
    end;

    if NewSurface^.format^.BytesPerPixel > 3 then
    begin
      if BGR_Order then
        glTexImage2D(GL_TEXTURE_2D, 0, GL_BGRA, NewSurface^.w, NewSurface^.h, 0, GL_BGRA, GL_UNSIGNED_BYTE, NewSurface^.pixels)
      else
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, NewSurface^.w, NewSurface^.h, 0, GL_RGBA, GL_UNSIGNED_BYTE, NewSurface^.pixels);
    end else
    begin
      if BGR_Order then
        glTexImage2D(GL_TEXTURE_2D, 0, GL_BGR, NewSurface^.w, NewSurface^.h, 0, GL_BGR, GL_UNSIGNED_BYTE, NewSurface^.pixels)
      else
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, NewSurface^.w, NewSurface^.h, 0, GL_RGB, GL_UNSIGNED_BYTE, NewSurface^.pixels);
    end;

    // Free no longer needed surfaces
    SDL_FreeSurface(NewSurface);
    SDL_FreeSurface(SDL_Surface);
    Result := Texture;
  end else ErrorString := 'SDLTextures.CreateTexture: Input surface not assigned';
end;

function LoadTexture(aFilename: String; var Texture: GLuint): Boolean;
var
  TargaFile: Boolean;
begin
  Result := false;
  if FileExists(aFilename) then
  begin
    if UpperCase(ExtractFileExt(aFilename)) = '.TGA' then TargaFile := true
       else TargaFile := false;

    {$IFDEF USE_SDL_IMAGE}
	Texture := CreateTexture(IMG_Load(PChar(aFilename)));
    {$ENDIF}
	{$IFDEF USE_VAMPYRE}
	Texture := CreateTexture(LoadSDLSurfaceFromFile(aFilename), TargaFile);
        if (Texture = 0) then Texture := LoadGLTextureFromFile(aFilename);
        {$ENDIF}
	Result := true;
  end;
end;

function LoadTexture(Surface: PSDL_Surface; var Texture: GLuint): Boolean;
begin
  Result := false;
  if Surface <> nil then
  begin
    Texture := CreateTexture(Surface);
    Result := true;
  end;
end;

// http://wiki.delphigl.com/index.php/Screenshot
function SaveSDLTextureToTGA(aFilename: String; aSurface: PSDL_Surface): Boolean;
var
  TGAHEADER : TTGAHEADER;

  rwop: PSDL_RWops;
  ImageSize: Integer;
  i : Integer;
begin
  Result:=False;

  if not Assigned(aSurface) then
  begin
    ErrorString := 'No surface has been assigned.';
    exit;
  end;

  if aSurface.format.BytesPerPixel < 3 then
  begin
    ErrorString := 'Color depth not supported.';
    exit;
  end;

  if SDL_BYTEORDER <> SDL_BIG_ENDIAN then
  begin
    SwapBGR(aSurface);
  end;

  with TGAHEADER do
  begin
    tfType := 0;
    tfColorMapType := 0;
    tfImageType := 2;
    for i := 0 to 4 do
      tfColorMapSpec[i] := 0;
    tfOrigX := 0;
    tfOrigY := 0;
    tfWidth := aSurface.w;
    tfHeight := aSurface.h;
    tfBpp := aSurface.format.BitsPerPixel;
    tfImageDes := 0;
  end;

  ImageSize := aSurface.w * aSurface.h * aSurface.format.BytesPerPixel;

  rwop := SDL_RWfromFile(PChar(aFilename),'w+b');
  if rwop = nil then
  begin
    ErrorString := 'Error while creating Rwop';
    exit;
  end;
  try
    if SDL_RWWrite(rwop, @TGAHEADER, SizeOf(TGAHEADER), 1) <> 1 then
    begin
      ErrorString := 'Error while writing header';
      exit;
    end;

    if SDL_RWWrite(rwop, aSurface.pixels, ImageSize, 1) <> 1 then
    begin
      ErrorString := 'Error while writing data.';
      exit;
    end;

    Result := True;
  finally
    SDL_RWClose(rwop);

    SDL_FreeSurface(aSurface);
  end;
end;

procedure SwapBGR(SDL_Surface: PSDL_Surface);
var
  x,y: Integer;
  pixel: UInt32;
  pred, pgreen, pblue, palpha: PUInt8;
  red, green, blue, alpha: UInt8;
begin
  for y := 0 to (SDL_Surface^.h - 1) do
    for x := 0 to (SDL_Surface^.w - 1) do
    begin
      //reads the pixels them
      pixel := SDL_GetPixel(SDL_Surface, x, y);

      pred := @red;
      pgreen := @green;
      pblue := @blue;
      palpha := @alpha;

      //read and swap color
      SDL_GetRGBA(pixel, SDL_Surface^.format, pred, pgreen, pblue, palpha);
      pixel := SDL_MapRGBA(SDL_Surface^.format, blue, green, red, alpha);

      SDL_PutPixel(SDL_Surface, x, y, pixel);
    end;
end;

procedure Bind(var Texture: GLuint);
begin
  glBindTexture(GL_TEXTURE_2D, Texture);
end;

end.
