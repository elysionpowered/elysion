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
  ElysionSprite,

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



  function convCol(S: TelWindow; Color: TelColor): Cardinal; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function convCol(S: TelWindow; R, G, B: Byte): Cardinal; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function convCol(Color: TelColor): TSDL_Color; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  //function convCol(Color: TelColor): Cardinal; Overload;
  function convCol(R, G, B: Byte): Cardinal; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  procedure DrawQuad(pX, pY, pW, pH, pZ: Single); Overload;
  procedure DrawQuad(pX, pY, pW, pH, pZ: Single; Vertices: TColorVertices); Overload;
  procedure DrawQuad(OrgX, OrgY, ClipX, ClipY, ClipW, ClipH, DrawX, DrawY, DrawW, DrawH, Z: Single); Overload;

  procedure DrawLine(Src, Dst: TelVector2f; Color: TelColor); Overload;
  procedure DrawLine(Points: array of TelVector2f; Color: TelColor); Overload;

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

// TODO: Use VBOs instead of glBegin/glEnd calls
procedure DrawQuad(pX, pY, pW, pH, pZ: Single);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(1, 0); glVertex3f(pX + pW, pY, -pZ);
    glTexCoord2f(0, 0); glVertex3f(pX	  , pY, -pZ);
    glTexCoord2f(0, 1); glVertex3f(pX	  , pY + pH, -pZ);
    glTexCoord2f(1, 1); glVertex3f(pX + pW, pY + pH, -pZ);
  glEnd();
end;

procedure DrawQuad(pX, pY, pW, pH, pZ: Single; Vertices: TColorVertices);
begin
  glBegin(GL_QUADS);
    glColor4f(Vertices[0].R / 255, Vertices[0].G / 255, Vertices[0].B / 255, Vertices[0].A / 255); glTexCoord2f(1, 0); glVertex3f(pX + pW, pY, -pZ);
    glColor4f(Vertices[1].R / 255, Vertices[1].G / 255, Vertices[1].B / 255, Vertices[1].A / 255); glTexCoord2f(0, 0); glVertex3f(pX	  , pY, -pZ);
    glColor4f(Vertices[2].R / 255, Vertices[2].G / 255, Vertices[2].B / 255, Vertices[2].A / 255); glTexCoord2f(0, 1); glVertex3f(pX	  , pY + pH, -pZ);
    glColor4f(Vertices[3].R / 255, Vertices[3].G / 255, Vertices[3].B / 255, Vertices[3].A / 255); glTexCoord2f(1, 1); glVertex3f(pX + pW, pY + pH, -pZ);
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

  // Use VBOs if OpenGL >= 1.5, else glBegin/glEnd

  {$IFDEF USE_DGL_HEADER}
  if (GL_VERSION_1_5) then
  {$ELSE}
  if (Load_GL_version_2_0) then
  {$ENDIF}
  begin

  end else
  begin

  end;

  glBegin(GL_QUADS);
    glTexCoord2f(0 + tposx		  , 0 + tposy); 		glVertex3f(DrawX, DrawY, -Z);
    glTexCoord2f(0 + tposx		  , 0 + tposy + tposh); glVertex3f(DrawX, DrawY + DrawH, -Z);
    glTexCoord2f(0 + tposx + tposw, 0 + tposy + tposh); glVertex3f(DrawX + DrawW, DrawY + DrawH, -Z);
    glTexCoord2f(0 + tposx + tposw, 0 + tposy); 		glVertex3f(DrawX + DrawW, DrawY, -Z);
  glEnd;

  {glBegin(GL_QUADS);
    glTexCoord2f(0+1/tposx+1/row, 1-1/tposy);       glTexCoord2f(1, 0); glVertex3f(DrawRect.X + DrawRect.W, DrawRect.Y, -Z);
    glTexCoord2f(0+1/tposx      , 1-1/tposy);       glTexCoord2f(0 + tposx, 0 + tposy); glVertex3f(DrawRect.X, DrawRect.Y, -Z);
    glTexCoord2f(0+1/tposx      , 1-1/tposy-1/col); glTexCoord2f(0, 1); glVertex3f(DrawRect.X, DrawRect.Y + DrawRect.H, -Z);
    glTexCoord2f(0+1/tposx+1/row, 1-1/tposy-1/col); glTexCoord2f(1, 1); glVertex3f(DrawRect.X + DrawRect.W, DrawRect.Y + DrawRect.H, -Z);
  glEnd;}
end;



procedure DrawLine(Src, Dst: TelVector2f; Color: TelColor);
begin
  glColor3f(Color.R / 255, Color.G / 255, Color.B / 255);

  // GL_LINE_STRIP instead of GL_LINES -> http://wiki.delphigl.com/index.php/glBegin
  glBegin(GL_LINE_STRIP);
    glVertex3f(Src.X * ActiveWindow.ResScale.X, Src.Y * ActiveWindow.ResScale.Y, 0);
    glVertex3f(Dst.X * ActiveWindow.ResScale.X, Dst.Y * ActiveWindow.ResScale.Y, 0);
  glEnd;
end;

procedure DrawLine(Points: array of TelVector2f; Color: TelColor);
var
  i: Integer;
begin
  glColor3f(Color.R / 255, Color.G / 255, Color.B / 255);

  // GL_LINE_STRIP instead of GL_LINES -> http://wiki.delphigl.com/index.php/glBegin
  glBegin(GL_LINE_STRIP);
    for i := 0 to High(Points) - 1 do
    begin
      glVertex3f(Points[i].X * ActiveWindow.ResScale.X, Points[i].Y * ActiveWindow.ResScale.Y, 0);
      glVertex3f(Points[i+1].X * ActiveWindow.ResScale.X, Points[i+1].Y * ActiveWindow.ResScale.Y, 0);
    end;
  glEnd;
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
  if OnPoint(RectOne, makeV2f(RectTwo.X, RectTwo.Y)) Or
     OnPoint(RectTwo, makeV2f(RectOne.X, RectOne.Y)) Or
     OnPoint(RectOne, makeV2f(RectTwo.X + RectTwo.W, RectTwo.Y)) Or
     OnPoint(RectTwo, makeV2f(RectOne.X + RectOne.W, RectOne.Y)) Or
     OnPoint(RectOne, makeV2f(RectTwo.X, RectTwo.Y + RectTwo.H)) Or
     OnPoint(RectTwo, makeV2f(RectOne.X, RectOne.Y + RectOne.H)) Or
     OnPoint(RectOne, makeV2f(RectTwo.X + RectTwo.W, RectTwo.Y + RectTwo.H)) Or
     OnPoint(RectTwo, makeV2f(RectOne.X + RectOne.W, RectOne.Y + RectOne.H)) Then Result := True;
end;

function CollisionTest(SpriteOne, SpriteTwo: TelSprite; AllowInvisibleObjects: Boolean = false): Boolean;

  function HitTest: Boolean;
  begin
    Result := false;

    if SpriteOne.OnPoint(makeV2f(SpriteTwo.AbsolutePosition.X, SpriteTwo.AbsolutePosition.Y)) Or
       SpriteTwo.OnPoint(makeV2f(SpriteOne.AbsolutePosition.X, SpriteOne.AbsolutePosition.Y)) Or
       SpriteOne.OnPoint(makeV2f(SpriteTwo.AbsolutePosition.X + SpriteTwo.Width, SpriteTwo.AbsolutePosition.Y)) Or
       SpriteTwo.OnPoint(makeV2f(SpriteOne.AbsolutePosition.X + SpriteOne.Width, SpriteOne.AbsolutePosition.Y)) Or
       SpriteOne.OnPoint(makeV2f(SpriteTwo.AbsolutePosition.X,                   SpriteTwo.AbsolutePosition.Y + SpriteTwo.Height)) Or
       SpriteTwo.OnPoint(makeV2f(SpriteOne.AbsolutePosition.X,                   SpriteOne.AbsolutePosition.Y + SpriteOne.Height)) Or
       SpriteOne.OnPoint(makeV2f(SpriteTwo.AbsolutePosition.X + SpriteTwo.Width, SpriteTwo.AbsolutePosition.Y + SpriteTwo.Height)) Or
       SpriteTwo.OnPoint(makeV2f(SpriteOne.AbsolutePosition.X + SpriteOne.Width, SpriteOne.AbsolutePosition.Y + SpriteOne.Height)) Then Result := true;
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

    if SpriteOne.Mask.Empty then SurfaceOne := SpriteOne.Mask.TextureSurface
      else SurfaceOne := SpriteOne.Texture.TextureSurface;

    if SpriteTwo.Mask.Empty then SurfaceTwo := SpriteTwo.Mask.TextureSurface
      else SurfaceTwo := SpriteTwo.Texture.TextureSurface;

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

    if Sprite.Mask.Empty then Surface := Sprite.Mask.TextureSurface
      else Surface := Sprite.Texture.TextureSurface;

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
