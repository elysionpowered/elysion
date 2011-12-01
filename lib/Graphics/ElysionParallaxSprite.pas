unit ElysionParallaxSprite;

{$I Elysion.inc}

interface

uses
  ElysionTypes,
  ElysionApplication,
  ElysionSprite;

type
  { TelParallaxSprite }

  TelParallaxDirection = (dtUp, dtDown, dtLeft, dtRight);

  TelParallaxSprite = class(TelSprite)
    private
      fSpeed: Single;
      fPaused: Boolean;
      fDirection: TelParallaxDirection;
      fInternalPosition: TelVector3f;
      // Used by draw-function. Do not change it elsewhere!
      fRecursion : Boolean;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      procedure Start(); inline;
      procedure Stop(); inline;
      procedure Pause(); inline;
      procedure UnPause(); inline;

      procedure Draw(DrawChildren: Boolean = true); override;
      procedure Update(dt: Double = 0.0); Override;
    published
      property Direction: TelParallaxDirection read fDirection write fDirection;
      property Speed: Single read fSpeed write fSpeed;
  end;

implementation

{ TelParallaxSprite }

constructor TelParallaxSprite.Create;
begin
  inherited Create;
  fRecursion := true;
end;

destructor TelParallaxSprite.Destroy;
begin
  inherited Destroy;
end;

procedure TelParallaxSprite.Start();
begin
  fPaused := false;
end;

procedure TelParallaxSprite.Stop();
begin
  fPaused := true;
end;

procedure TelParallaxSprite.Pause();
begin
  fPaused := true;
end;

procedure TelParallaxSprite.UnPause();
begin
  fPaused := false;
end;

procedure TelParallaxSprite.Draw(DrawChildren: Boolean = true);
var i,j,leftMissing, rightMissing, upMissing, downMissing : Integer; formerX, formerY : Single; rotateMe : boolean;
begin
  inherited Draw(DrawChildren);
  // Immediately exit after draw: This is not the recursive call.
  if(not fRecursion) then Exit;
  fRecursion := false;
  // Ensure that the texture is bigger than the screen's width and height.
  // +2 because one picture can be partially visible, the second additional picture is guaranteed not to be seen.
  formerX := Position.X;
  formerY := Position.Y;

  //TODO: Ermitteln, wie viel nach links / rechts; oben / unten fehlt. Wenn das Bild zu weit außen ist, nach (0,0) verschieben (mindert Speicherbedarf).
  leftMissing := Trunc(Position.X / Width)+1;
  rightMissing := (ActiveWindow.Width - Trunc((Position.X+ Width)/Width)) +1;
  upMissing := Trunc(Position.Y / Height) +1;
  downMissing := (ActiveWindow.Height - Trunc((Position.Y + Height) / Height)) +1;

  if(leftMissing > 2) then
    formerX := Abs(ActiveWindow.Width-Position.X);
//  Position := MakeV3f(0,0,0);
  if(upMissing > 2)  then
    formerY := Abs(ActiveWindow.Height-Position.Y);
//  Position := MakeV3f(0,0,0);

  //  Draw shapes missing to completely fill the scene. Reset the sprite to its former coordinates after each run.

  for i := 0 to leftMissing do
  begin
    for j := 0 to upMissing do
    begin
    Position  := MakeV3f(formerX-(Width*i),formerY-j*Height);
       // if(j mod 2 = 1)then
       // Rotate(180);
    Draw;
    end;
  end;
  Position := MakeV3f(formerX, formerY);
  // Position 0 has already been taken by loop above (for i := 0 to leftMissing)
  for i := 1 to rightMissing do
  begin
    for j := 0 to upMissing do
    begin
      Position  := MakeV3f(i*Width+formerX,formerY-j*Height);
      Draw;
    end;
  end;
  Rotate(0);

  Position := MakeV3f(formerX, formerY);
  {for i := 0 to upMissing do
  begin
    Position := MakeV3f(formerX,formerY-i*Height);
    Draw;
  end;}

  Position := MakeV3f(formerX, formerY);
  for i := 0 to downMissing do
  begin
    Position := MakeV3f(formerX,formerY+i*Height);
    Draw;
  end;

  Position := MakeV3f(formerX, formerY);

  // Only the recursive call gets to here.
  fRecursion := true;
end;


procedure TelParallaxSprite.Update(dt: Double);
begin
  inherited Update(dt);
  case fDirection of
   dtUp: Position.Y := Position.Y -fSpeed*dt;
   dtDown: Position.Y := Position.Y +fSpeed*dt;
   dtLeft: Position.X := Position.X -fSpeed*dt;
   dtRight: Position.X := Position.X +fSpeed*dt;
  end;
end;

end.
