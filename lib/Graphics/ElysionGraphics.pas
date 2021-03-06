unit ElysionGraphics;

interface

{$I Elysion.inc}

uses
  ElysionApplication,
  ElysionTypes,
  ElysionColor,
  ElysionInput,
  ElysionTexture,
  ElysionContent,
  ElysionSprite,

  {$IFDEF USE_VAMPYRE}
  ImagingSDL,
  {$ENDIF}
  {$IFDEF USE_SDL_IMAGE}
  SDL_image,
  {$ENDIF}
  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu,
  {$ENDIF}
  SDL,
  SDLUtilsLight,
  //SDLTextures,

  SysUtils,
  Classes;



  function convCol(S: TelWindow; Color: TelColor): Cardinal; Overload; inline;
  function convCol(S: TelWindow; R, G, B: Byte): Cardinal; Overload; inline;
  function convCol(Color: TelColor): TSDL_Color; Overload; inline;
  //function convCol(Color: TelColor): Cardinal; Overload;
  function convCol(R, G, B: Byte): Cardinal; Overload; inline;

  procedure DrawQuad(pX, pY, pW, pH, pZ: Single); Overload;
  procedure DrawQuad(pX, pY, pW, pH, pZ: Single; Vertices: TelColorVertices); Overload;
  procedure DrawQuad(OrgX, OrgY, ClipX, ClipY, ClipW, ClipH, DrawX, DrawY, DrawW, DrawH, Z: Single); Overload;

  procedure DrawRoundedRect(X, Y, W, H, Z: Single; aRoundedRadius: Single);


  procedure DrawLine(Src, Dst: TelVector3f; Color: TelColor; LineWidth: Single = 1.0); Overload;
  procedure DrawLine(Points: array of TelVector3f; Color: TelColor; LineWidth: Single = 1.0); Overload;

  procedure DrawPolygon(Vertices: array of TelVertex);


  function CollisionTest(RectOne, RectTwo: TelRect): Boolean; Overload;
  function CollisionTest(SpriteOne, SpriteTwo: TelSprite; AllowInvisibleObjects: Boolean = false): Boolean; Overload;
  function PixelTest(SpriteOne, SpriteTwo: TelSprite; AllowInvisibleObjects: Boolean = false): Boolean; Overload;
  function PixelTest(Sprite: TelSprite; Rect: TelRect; AllowInvisibleObjects: Boolean = false): Boolean; Overload;



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

procedure DrawQuad(pX, pY, pW, pH, pZ: Single);
begin
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, 0); glVertex3f(pX, pY, -pZ);
    glTexCoord2f(0, 1); glVertex3f(pX, pY + pH, -pZ);
    glTexCoord2f(1, 0); glVertex3f(pX + pW, pY, -pZ);
    glTexCoord2f(1, 1); glVertex3f(pX + pW, pY + pH, -pZ);
  glEnd();
end;

procedure DrawQuad(pX, pY, pW, pH, pZ: Single; Vertices: TelColorVertices);
begin
  glBegin(GL_TRIANGLE_STRIP);
    glColor4f(Vertices[0].R / 255, Vertices[0].G / 255, Vertices[0].B / 255, Vertices[0].A / 255); glTexCoord2f(0, 0); glVertex3f(pX, pY, pZ);
    glColor4f(Vertices[1].R / 255, Vertices[1].G / 255, Vertices[1].B / 255, Vertices[1].A / 255); glTexCoord2f(0, 1); glVertex3f(pX, pY + pH, pZ);
    glColor4f(Vertices[2].R / 255, Vertices[2].G / 255, Vertices[2].B / 255, Vertices[2].A / 255); glTexCoord2f(1, 0); glVertex3f(pX + pW, pY, pZ);
    glColor4f(Vertices[3].R / 255, Vertices[3].G / 255, Vertices[3].B / 255, Vertices[3].A / 255); glTexCoord2f(1, 1); glVertex3f(pX + pW, pY + pH, pZ);
  glEnd();
end;

procedure DrawQuad(OrgX, OrgY, ClipX, ClipY, ClipW, ClipH, DrawX, DrawY, DrawW, DrawH, Z: Single);
//var tposx, tposy, row, col: single;
var tposx, tposy, tposw, tposh: Single;
begin
  if (OrgX > 0) and (OrgY > 0) then
  begin
    if ClipX > 0 then tposx := ClipX / OrgX else tposx := 0;
    if ClipY > 0 then tposy := ClipY / OrgY else tposy := 0;

    tposw := ClipW / OrgX;
    tposh := ClipH / OrgY;
  end;

  {row := OriginalSize.X / ClipRect.W;
  col := OriginalSize.Y / ClipRect.H;

  tposx := OriginalSize.X / ClipRect.X;
  tposy := OriginalSize.Y / ClipRect.Y;}

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0 + tposx		  , 0 + tposy); 		glVertex3f(DrawX, DrawY, Z);
    glTexCoord2f(0 + tposx		  , 0 + tposy + tposh); glVertex3f(DrawX, DrawY + DrawH, Z);
    glTexCoord2f(0 + tposx + tposw, 0 + tposy); glVertex3f(DrawX + DrawW, DrawY, Z);
    glTexCoord2f(0 + tposx + tposw, 0 + tposy + tposh); 		glVertex3f(DrawX + DrawW, DrawY + DrawH, Z);
  glEnd;

  {glBegin(GL_QUADS);
    glTexCoord2f(0+1/tposx+1/row, 1-1/tposy);       glTexCoord2f(1, 0); glVertex3f(DrawRect.X + DrawRect.W, DrawRect.Y, -Z);
    glTexCoord2f(0+1/tposx      , 1-1/tposy);       glTexCoord2f(0 + tposx, 0 + tposy); glVertex3f(DrawRect.X, DrawRect.Y, -Z);
    glTexCoord2f(0+1/tposx      , 1-1/tposy-1/col); glTexCoord2f(0, 1); glVertex3f(DrawRect.X, DrawRect.Y + DrawRect.H, -Z);
    glTexCoord2f(0+1/tposx+1/row, 1-1/tposy-1/col); glTexCoord2f(1, 1); glVertex3f(DrawRect.X + DrawRect.W, DrawRect.Y + DrawRect.H, -Z);
  glEnd;}
end;

procedure DrawRoundedRect(X, Y, W, H, Z: Single; aRoundedRadius: Single);
var
  i: Single;
begin
  glDisable(GL_CULL_FACE);

  glBegin(GL_TRIANGLE_FAN);

    glVertex2f(X + aRoundedRadius, Y);
    glVertex2f(X + W - aRoundedRadius, Y);

    i := Pi * 1.5;
    while i < (Pi * 2) do
    begin
      glVertex2f(X + W - aRoundedRadius + Cos(i)* aRoundedRadius, Y + aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glVertex2f(X + W , Y + aRoundedRadius);
    glVertex2f(X + W , Y + H - aRoundedRadius);

    i := 0.0;
    while i < (Pi * 0.5) do
    begin
      glVertex2f(X + W - aRoundedRadius + Cos(i)* aRoundedRadius, Y + H - aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glVertex2f(X + W - aRoundedRadius , Y + H);
    glVertex2f(X + aRoundedRadius , Y + H);

    i := Pi * 0.5;
    while i < Pi do
    begin
      glVertex2f(X + aRoundedRadius + Cos(i)* aRoundedRadius, Y + H - aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glVertex2f(X , Y + H - aRoundedRadius);
    glVertex2f(X , Y + aRoundedRadius);

    i := Pi;
    while i < (Pi * 1.5) do
    begin
      glVertex2f(X + aRoundedRadius + Cos(i)* aRoundedRadius, Y + aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glEnd();

  glEnable(GL_CULL_FACE);
end;

procedure DrawLine(Src, Dst: TelVector3f; Color: TelColor; LineWidth: Single = 1.0);
begin
  DrawLine([Src, Dst], Color, LineWidth);
end;

procedure DrawLine(Points: array of TelVector3f; Color: TelColor; LineWidth: Single = 1.0);
var
  i: Integer;
begin
  glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Color.A / 255);

  if LineWidth <> 1.0 then glLineWidth(LineWidth);

  // GL_LINE_STRIP instead of GL_LINES -> http://wiki.delphigl.com/index.php/glBegin
  glBegin(GL_LINE_STRIP);
    for i := 0 to High(Points) - 1 do
    begin
      glVertex3f(Points[i].X, Points[i].Y, Points[i].Z);
      glVertex3f(Points[i + 1].X, Points[i + 1].Y, Points[i + 1].Z);
    end;
  glEnd;

  if LineWidth <> 1.0 then glLineWidth(1.0);
end;

procedure DrawPolygon(Vertices: array of TelVertex);
var
  i: Integer;
begin
  glBegin(GL_TRIANGLE_FAN);

    for i := 0 to High(Vertices) do
    begin
      glColor4f(Vertices[i].Color.R / 255, Vertices[i].Color.G / 255, Vertices[i].Color.B / 255, Vertices[i].Color.A / 255);
      glVertex3f(Vertices[i].Vector.X, Vertices[i].Vector.Y, Vertices[i].Vector.Z);
    end;

    glEnd();
end;

function CollisionTest(RectOne, RectTwo: TelRect): Boolean;

  function OnPoint(Rect: TelRect; Coord: TelVector2f): Boolean;
  begin
    if (Coord.X >= Rect.X) and
       (Coord.Y >= Rect.Y) and
       (Coord.X < (Rect.X + Rect.W)) and
       (Coord.Y < (Rect.Y + Rect.H)) then Result := true else Result := false;
  end;

begin
  Result := False;
  if OnPoint(RectOne, TelVector2f.Create(RectTwo.X, RectTwo.Y)) Or
     OnPoint(RectTwo, TelVector2f.Create(RectOne.X, RectOne.Y)) Or
     OnPoint(RectOne, TelVector2f.Create(RectTwo.X + RectTwo.W, RectTwo.Y)) Or
     OnPoint(RectTwo, TelVector2f.Create(RectOne.X + RectOne.W, RectOne.Y)) Or
     OnPoint(RectOne, TelVector2f.Create(RectTwo.X, RectTwo.Y + RectTwo.H)) Or
     OnPoint(RectTwo, TelVector2f.Create(RectOne.X, RectOne.Y + RectOne.H)) Or
     OnPoint(RectOne, TelVector2f.Create(RectTwo.X + RectTwo.W, RectTwo.Y + RectTwo.H)) Or
     OnPoint(RectTwo, TelVector2f.Create(RectOne.X + RectOne.W, RectOne.Y + RectOne.H)) Then Result := True;
end;

function CollisionTest(SpriteOne, SpriteTwo: TelSprite; AllowInvisibleObjects: Boolean = false): Boolean;

  function HitTest: Boolean;
  begin
    Result := false;

    if SpriteOne.OnPoint(TelVector2f.Create(SpriteTwo.AbsolutePosition.X, SpriteTwo.AbsolutePosition.Y)) Or
       SpriteTwo.OnPoint(TelVector2f.Create(SpriteOne.AbsolutePosition.X, SpriteOne.AbsolutePosition.Y)) Or
       SpriteOne.OnPoint(TelVector2f.Create(SpriteTwo.AbsolutePosition.X + SpriteTwo.Width, SpriteTwo.AbsolutePosition.Y)) Or
       SpriteTwo.OnPoint(TelVector2f.Create(SpriteOne.AbsolutePosition.X + SpriteOne.Width, SpriteOne.AbsolutePosition.Y)) Or
       SpriteOne.OnPoint(TelVector2f.Create(SpriteTwo.AbsolutePosition.X,                   SpriteTwo.AbsolutePosition.Y + SpriteTwo.Height)) Or
       SpriteTwo.OnPoint(TelVector2f.Create(SpriteOne.AbsolutePosition.X,                   SpriteOne.AbsolutePosition.Y + SpriteOne.Height)) Or
       SpriteOne.OnPoint(TelVector2f.Create(SpriteTwo.AbsolutePosition.X + SpriteTwo.Width, SpriteTwo.AbsolutePosition.Y + SpriteTwo.Height)) Or
       SpriteTwo.OnPoint(TelVector2f.Create(SpriteOne.AbsolutePosition.X + SpriteOne.Width, SpriteOne.AbsolutePosition.Y + SpriteOne.Height)) Then Result := true;
  end;

begin
  Result := false;

  if AllowInvisibleObjects then Result := HitTest
  else Result := HitTest and ((SpriteOne.Visible) and (SpriteTwo.Visible));
end;

// TODO: Fix PixelTest for scaled sprites

function PixelTest(SpriteOne, SpriteTwo: TelSprite; AllowInvisibleObjects: Boolean = false): Boolean;

  function HitTest: Boolean;
  var
    SpriteOneRect, SpriteTwoRect: TSDL_Rect;
    SurfaceOne, SurfaceTwo: PSDL_Surface;
  begin
    Result := false;

    if (not SpriteOne.Transparent) then SpriteOne.Transparent := true;
    if (not SpriteTwo.Transparent) then SpriteTwo.Transparent := true;

    SpriteOneRect.x := Trunc(SpriteOne.ClipRect.X * SpriteOne.Scale.X);
    SpriteOneRect.y := Trunc(SpriteOne.ClipRect.Y * SpriteOne.Scale.Y);
    SpriteOneRect.w := Trunc(SpriteOne.ClipRect.W * SpriteOne.Scale.X);
    SpriteOneRect.h := Trunc(SpriteOne.ClipRect.H * SpriteOne.Scale.Y);

    SpriteTwoRect.x := Trunc(SpriteTwo.ClipRect.X * SpriteTwo.Scale.X);
    SpriteTwoRect.y := Trunc(SpriteTwo.ClipRect.Y * SpriteOne.Scale.Y);
    SpriteTwoRect.w := Trunc(SpriteTwo.ClipRect.W * SpriteTwo.Scale.X);
    SpriteTwoRect.h := Trunc(SpriteTwo.ClipRect.H * SpriteOne.Scale.Y);

    // Loads pixeldata on-the-fly if not available
    if not SpriteOne.Mask.Empty then
    begin
      if SpriteOne.Mask.TextureSurface = nil then
      begin
        {$IFDEF USE_VAMPYRE}
          SpriteOne.Mask.TextureSurface := LoadSDLSurfaceFromFile(SpriteOne.Mask.Filename);
        {$ENDIF}
        {$IFDEF USE_SDL_IMAGE}
          SpriteOne.Mask.TextureSurface := IMG_Load(PChar(SpriteOne.Mask.Filename));
        {$ENDIF}
      end;

      if SpriteOne.Mask.TextureSurface = nil then Exit;

      SurfaceOne := SpriteOne.Mask.TextureSurface;
    end else
    begin
      if SpriteOne.Texture.TextureSurface = nil then
      begin
        {$IFDEF USE_VAMPYRE}
          SpriteOne.Texture.TextureSurface := LoadSDLSurfaceFromFile(SpriteOne.Texture.Filename);
        {$ENDIF}
        {$IFDEF USE_SDL_IMAGE}
          SpriteOne.Texture.TextureSurface := IMG_Load(PChar(SpriteOne.Texture.Filename));
        {$ENDIF}
      end;

      if SpriteOne.Texture.TextureSurface = nil then Exit;

      SurfaceOne := SpriteOne.Texture.TextureSurface;
    end;

    if not SpriteTwo.Mask.Empty then
    begin
      if SpriteTwo.Mask.TextureSurface = nil then
      begin
        {$IFDEF USE_VAMPYRE}
          SpriteTwo.Mask.TextureSurface := LoadSDLSurfaceFromFile(SpriteTwo.Mask.Filename);
        {$ENDIF}
        {$IFDEF USE_SDL_IMAGE}
          SpriteTwo.Mask.TextureSurface := IMG_Load(PChar(SpriteTwo.Mask.Filename));
        {$ENDIF}
      end;

      if SpriteTwo.Mask.TextureSurface = nil then Exit;

      SurfaceTwo := SpriteTwo.Mask.TextureSurface;
    end else
    begin
      if SpriteTwo.Texture.TextureSurface = nil then
      begin
        {$IFDEF USE_VAMPYRE}
          SpriteTwo.Texture.TextureSurface := LoadSDLSurfaceFromFile(SpriteTwo.Texture.Filename);
        {$ENDIF}
        {$IFDEF USE_SDL_IMAGE}
          SpriteTwo.Texture.TextureSurface := IMG_Load(PChar(SpriteTwo.Texture.Filename));
        {$ENDIF}
      end;

      if SpriteTwo.Texture.TextureSurface = nil then Exit;

      SurfaceTwo := SpriteTwo.Texture.TextureSurface;
    end;

    if SDL_PixelTest(SurfaceOne, @SpriteOneRect,
                     SurfaceTwo, @SpriteTwoRect,
		     Trunc(SpriteOne.AbsolutePosition.X * SpriteOne.Scale.X),
                     Trunc(SpriteOne.AbsolutePosition.Y * SpriteOne.Scale.Y),
                     Trunc(SpriteTwo.AbsolutePosition.X * SpriteOne.Scale.X),
                     Trunc(SpriteTwo.AbsolutePosition.Y * SpriteOne.Scale.Y)) then Result := true;
  end;

begin
  Result := false;

  if AllowInvisibleObjects then Result := HitTest
  else Result := HitTest and ((SpriteOne.Visible) and (SpriteTwo.Visible));
end;

function PixelTest(Sprite: TelSprite; Rect: TelRect; AllowInvisibleObjects: Boolean = false): Boolean;

  function HitTest: Boolean;
  var
    SpriteRect, CRect: TSDL_Rect;
    Surface: PSDL_Surface;
  begin
    Result := false;

    if (not Sprite.Transparent) then Sprite.Transparent := true;

    SpriteRect.x := Trunc(Sprite.ClipRect.X);
    SpriteRect.y := Trunc(Sprite.ClipRect.Y);
    SpriteRect.w := Trunc(Sprite.ClipRect.W);
    SpriteRect.h := Trunc(Sprite.ClipRect.H);

    CRect.x := Trunc(Rect.X);
    CRect.y := Trunc(Rect.Y);
    CRect.w := Trunc(Rect.W);
    CRect.h := Trunc(Rect.H);

    // Loads pixeldata on-the-fly if not available
    if not Sprite.Mask.Empty then
    begin
      if Sprite.Mask.TextureSurface = nil then
      begin
        {$IFDEF USE_VAMPYRE}
          Sprite.Mask.TextureSurface := LoadSDLSurfaceFromFile(Sprite.Mask.Filename);
        {$ENDIF}
        {$IFDEF USE_SDL_IMAGE}
          Sprite.Mask.TextureSurface := IMG_Load(PChar(Sprite.Mask.Filename));
        {$ENDIF}
      end;

      if Sprite.Mask.TextureSurface = nil then Exit;

      Surface := Sprite.Mask.TextureSurface;
    end else
    begin
      if Sprite.Texture.TextureSurface = nil then
      begin
        {$IFDEF USE_VAMPYRE}
          Sprite.Texture.TextureSurface := LoadSDLSurfaceFromFile(Sprite.Texture.Filename);
        {$ENDIF}
        {$IFDEF USE_SDL_IMAGE}
          Sprite.Texture.TextureSurface := IMG_Load(PChar(Sprite.Texture.Filename));
        {$ENDIF}
      end;

      if Sprite.Texture.TextureSurface = nil then Exit;

      Surface := Sprite.Texture.TextureSurface;
    end;

    if SDL_PixelTestSurfaceVsRect(Surface,
                                  @SpriteRect, @CRect,
                                  Trunc(Sprite.AbsolutePosition.X * Sprite.Scale.X),
                                  Trunc(Sprite.AbsolutePosition.Y * Sprite.Scale.Y),
                                  Trunc(Rect.X),
                                  Trunc(Rect.Y)) then Result := true;
  end;

begin
  Result := false;

  if AllowInvisibleObjects then Result := HitTest
  else Result := HitTest and Sprite.Visible;
end;



end.
