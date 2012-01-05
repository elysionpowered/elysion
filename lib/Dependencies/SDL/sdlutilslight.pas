(*
  A unit with just the sdlutils functions needed in Elysion
  
  Same license as sdlutils
*)

unit sdlutilslight;

interface

uses
  sdl;

function SDL_GetPixel( SrcSurface : PSDL_Surface; x : integer; y : integer ) : Uint32;
  
procedure SDL_FlipRectV( DstSurface : PSDL_Surface; Rect : PSDL_Rect );

function SDL_PixelTestSurfaceVsRect( SrcSurface1 : PSDL_Surface; SrcRect1 :
  PSDL_Rect; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) :
  boolean;
  
function SDL_PixelTest( SrcSurface1 : PSDL_Surface; SrcRect1 : PSDL_Rect; SrcSurface2 :
  PSDL_Surface; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) : boolean;

implementation

function SDL_GetPixel( SrcSurface : PSDL_Surface; x : integer; y : integer ) : Uint32;
var
  bpp          : UInt32;
  p            : PInteger;
begin
  bpp := SrcSurface.format.BytesPerPixel;
  // Here p is the address to the pixel we want to retrieve
  p := Pointer( PtrUInt( SrcSurface.pixels ) + UInt32( y ) * UInt32(SrcSurface.pitch) + UInt32( x ) *
    bpp );
  case bpp of
    1 : result := PUint8( p )^;
    2 : result := PUint16( p )^;
    3 :
      if ( SDL_BYTEORDER = SDL_BIG_ENDIAN ) then
        result := PUInt8Array( p )[ 0 ] shl 16 or PUInt8Array( p )[ 1 ] shl 8 or
          PUInt8Array( p )[ 2 ]
      else
        result := PUInt8Array( p )[ 0 ] or PUInt8Array( p )[ 1 ] shl 8 or
          PUInt8Array( p )[ 2 ] shl 16;
    4 : result := PUint32( p )^;
  else
    result := 0; // shouldn't happen, but avoids warnings
  end;
end;

// flips a rectangle vertically on given surface

procedure SDL_FlipRectV( DstSurface : PSDL_Surface; Rect : PSDL_Rect );
var
  TmpRect      : TSDL_Rect;
  Locked       : boolean;
  y, FlipLength, RowLength : integer;
  Row1, Row2   : Pointer;
  OneRow       : TByteArray; // Optimize it if you wish
begin
  if DstSurface <> nil then
  begin
    if Rect = nil then
    begin // if Rect=nil then we flip the whole surface
      TmpRect.x := 0;
      TmpRect.y := 0;
      TmpRect.w := DstSurface.w;
      TmpRect.h := DstSurface.h;
      Rect := @TmpRect;
    end;
    FlipLength := Rect^.h shr 1 - 1;
    RowLength := Rect^.w * DstSurface^.format.BytesPerPixel;
    if SDL_MustLock( DstSurface ) then
    begin
      Locked := true;
      SDL_LockSurface( DstSurface );
    end
    else
      Locked := false;
    Row1 := pointer( PtrUInt( DstSurface^.Pixels ) + UInt32( Rect^.y ) *
      DstSurface^.Pitch );
    Row2 := pointer( PtrUInt( DstSurface^.Pixels ) + ( UInt32( Rect^.y ) + Rect^.h - 1 )
      * DstSurface^.Pitch );
    for y := 0 to FlipLength do
    begin
      Move( Row1^, OneRow, RowLength );
      Move( Row2^, Row1^, RowLength );
      Move( OneRow, Row2^, RowLength );
      inc( PtrUInt( Row1 ), DstSurface^.Pitch );
      dec( PtrUInt( Row2 ), DstSurface^.Pitch );
    end;
    if Locked then
      SDL_UnlockSurface( DstSurface );
  end;
end;

function SDL_PixelTestSurfaceVsRect( SrcSurface1 : PSDL_Surface; SrcRect1 :
  PSDL_Rect; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) :
  boolean;
var
  Src_Rect1, Src_Rect2 : TSDL_Rect;
  right1, bottom1 : integer;
  right2, bottom2 : integer;
  Scan1Start, {Scan2Start,} ScanWidth, ScanHeight : PtrUInt;
  Mod1         : PtrUInt;
  Addr1        : PtrUInt;
  BPP          : PtrUInt;
  Pitch1       : PtrUInt;
  TransparentColor1 : PtrUInt;
  tx, ty       : PtrUInt;
  StartTick    : PtrUInt;
  Color1       : PtrUInt;
begin
  Result := false;
  if SrcRect1 = nil then
  begin
    with Src_Rect1 do
    begin
      x := 0;
      y := 0;
      w := SrcSurface1.w;
      h := SrcSurface1.h;
    end;
  end
  else
    Src_Rect1 := SrcRect1^;

  Src_Rect2 := SrcRect2^;
  with Src_Rect1 do
  begin
    Right1 := Left1 + w;
    Bottom1 := Top1 + h;
  end;
  with Src_Rect2 do
  begin
    Right2 := Left2 + w;
    Bottom2 := Top2 + h;
  end;
  if ( Left1 >= Right2 ) or ( Right1 <= Left2 ) or ( Top1 >= Bottom2 ) or ( Bottom1 <= Top2 ) then
    exit;
  if Left1 <= Left2 then
  begin
    // 1. left, 2. right
    Scan1Start := Src_Rect1.x + Left2 - Left1;
    //Scan2Start := Src_Rect2.x;
    ScanWidth := Right1 - Left2;
    with Src_Rect2 do
      if ScanWidth > w then
        ScanWidth := w;
  end
  else
  begin
    // 1. right, 2. left
    Scan1Start := Src_Rect1.x;
    //Scan2Start := Src_Rect2.x + Left1 - Left2;
    ScanWidth := Right2 - Left1;
    with Src_Rect1 do
      if ScanWidth > w then
        ScanWidth := w;
  end;
  with SrcSurface1^ do
  begin
    Pitch1 := Pitch;
    Addr1 := PtrUInt( Pixels );
    inc( Addr1, Pitch1 * UInt32( Src_Rect1.y ) );
    with format^ do
    begin
      BPP := BytesPerPixel;
      TransparentColor1 := colorkey;
    end;
  end;

  Mod1 := Pitch1 - ( Int64(ScanWidth) * Int64(BPP) );

  inc( Addr1, BPP * Scan1Start );

  if Top1 <= Top2 then
  begin
    // 1. up, 2. down
    ScanHeight := Bottom1 - Top2;
    if ScanHeight > Src_Rect2.h then
      ScanHeight := Src_Rect2.h;
    inc( Addr1, Pitch1 * UInt32( Top2 - Top1 ) );
  end
  else
  begin
    // 1. down, 2. up
    ScanHeight := Bottom2 - Top1;
    if ScanHeight > Src_Rect1.h then
      ScanHeight := Src_Rect1.h;

  end;
  case BPP of
    1 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PByte( Addr1 )^ <> TransparentColor1 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1 );

        end;
        inc( Addr1, Mod1 );

      end;
    2 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PWord( Addr1 )^ <> TransparentColor1 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 2 );

        end;
        inc( Addr1, Mod1 );

      end;
    3 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          Color1 := PLongWord( Addr1 )^ and $00FFFFFF;

          if ( Color1 <> TransparentColor1 )
            then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 3 );

        end;
        inc( Addr1, Mod1 );

      end;
    4 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PLongWord( Addr1 )^ <> TransparentColor1 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 4 );

        end;
        inc( Addr1, Mod1 );

      end;
  end;
end;

function SDL_PixelTest( SrcSurface1 : PSDL_Surface; SrcRect1 : PSDL_Rect; SrcSurface2 :
  PSDL_Surface; SrcRect2 : PSDL_Rect; Left1, Top1, Left2, Top2 : integer ) : boolean;
var
  Src_Rect1, Src_Rect2 : TSDL_Rect;
  right1, bottom1 : integer;
  right2, bottom2 : integer;
  Scan1Start, Scan2Start, ScanWidth, ScanHeight : PtrUInt;
  Mod1, Mod2   : PtrUInt;
  Addr1, Addr2 : Pointer;
  BPP          : PtrUInt;
  Pitch1, Pitch2 : PtrUInt;
  TransparentColor1, TransparentColor2 : PtrUInt;
  tx, ty       : PtrUInt;
  StartTick    : PtrUInt;
  Color1, Color2 : PtrUInt;
begin
  Result := false;
  if SrcRect1 = nil then
  begin
    with Src_Rect1 do
    begin
      x := 0;
      y := 0;
      w := SrcSurface1.w;
      h := SrcSurface1.h;
    end;
  end
  else
    Src_Rect1 := SrcRect1^;
  if SrcRect2 = nil then
  begin
    with Src_Rect2 do
    begin
      x := 0;
      y := 0;
      w := SrcSurface2.w;
      h := SrcSurface2.h;
    end;
  end
  else
    Src_Rect2 := SrcRect2^;
  with Src_Rect1 do
  begin
    Right1 := Left1 + w;
    Bottom1 := Top1 + h;
  end;
  with Src_Rect2 do
  begin
    Right2 := Left2 + w;
    Bottom2 := Top2 + h;
  end;
  if ( Left1 >= Right2 ) or ( Right1 <= Left2 ) or ( Top1 >= Bottom2 ) or ( Bottom1 <=
    Top2 ) then
    exit;
  if Left1 <= Left2 then
  begin
    // 1. left, 2. right
    Scan1Start := Src_Rect1.x + Left2 - Left1;
    Scan2Start := Src_Rect2.x;
    ScanWidth := Right1 - Left2;
    with Src_Rect2 do
      if ScanWidth > w then
        ScanWidth := w;
  end
  else
  begin
    // 1. right, 2. left
    Scan1Start := Src_Rect1.x;
    Scan2Start := Src_Rect2.x + Left1 - Left2;
    ScanWidth := Right2 - Left1;
    with Src_Rect1 do
      if ScanWidth > w then
        ScanWidth := w;
  end;
  with SrcSurface1^ do
  begin
    Pitch1 := Pitch;
    Addr1 := @Pixels;
    inc( Addr1, Pitch1 * UInt32( Src_Rect1.y ) );
    with format^ do
    begin
      BPP := BytesPerPixel;
      TransparentColor1 := colorkey;
    end;
  end;
  with SrcSurface2^ do
  begin
    TransparentColor2 := format.colorkey;
    Pitch2 := Pitch;
    Addr2 := @Pixels;
    inc( Addr2, Pitch2 * UInt32( Src_Rect2.y ) );
  end;
  Mod1 := Pitch1 - ( Int64(ScanWidth) * Int64(BPP) );
  Mod2 := Pitch2 - ( Int64(ScanWidth) * Int64(BPP) );
  inc( Addr1, BPP * Scan1Start );
  inc( Addr2, BPP * Scan2Start );
  if Top1 <= Top2 then
  begin
    // 1. up, 2. down
    ScanHeight := Bottom1 - Top2;
    if ScanHeight > Src_Rect2.h then
      ScanHeight := Src_Rect2.h;
    inc( Addr1, Pitch1 * UInt32( Top2 - Top1 ) );
  end
  else
  begin
    // 1. down, 2. up
    ScanHeight := Bottom2 - Top1;
    if ScanHeight > Src_Rect1.h then
      ScanHeight := Src_Rect1.h;
    inc( Addr2, Pitch2 * UInt32( Top1 - Top2 ) );
  end;
  case BPP of
    1 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PByte( Addr1 )^ <> TransparentColor1 ) and ( PByte( Addr2 )^ <>
            TransparentColor2 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1 );
          inc( Addr2 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
    2 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PWord( Addr1 )^ <> TransparentColor1 ) and ( PWord( Addr2 )^ <>
            TransparentColor2 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 2 );
          inc( Addr2, 2 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
    3 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          Color1 := PLongWord( Addr1 )^ and $00FFFFFF;
          Color2 := PLongWord( Addr2 )^ and $00FFFFFF;
          if ( Color1 <> TransparentColor1 ) and ( Color2 <> TransparentColor2 )
            then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 3 );
          inc( Addr2, 3 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
    4 :
      for ty := 1 to ScanHeight do
      begin
        for tx := 1 to ScanWidth do
        begin
          if ( PLongWord( Addr1 )^ <> TransparentColor1 ) and ( PLongWord( Addr2 )^ <>
            TransparentColor2 ) then
          begin
            Result := true;
            exit;
          end;
          inc( Addr1, 4 );
          inc( Addr2, 4 );
        end;
        inc( Addr1, Mod1 );
        inc( Addr2, Mod2 );
      end;
  end;
end;

end.
