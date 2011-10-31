unit ElysionGUI;

interface

{$I Elysion.inc}

uses
  ElysionNode,
  ElysionTypes,
  ElysionApplication,
  ElysionTexture,
  ElysionGraphics,
  ElysionInput,
  ElysionTrueTypeFont,
  ElysionUtils,
  ElysionAnimTypes,
  ElysionAnimator,

  SDLUtils,
  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}

  OpenURLUtil,
  SysUtils,
  Classes;

type

{TelCheckButton = class
  private

  public

  published
    property Checked: Boolean read FChecked
end;}

{ TelRectangle }

TelRectangle = class(TelNode)
protected
  fVertices: TColorVertices;

  function GetWidth(): Integer; Override;
  function GetHeight(): Integer; Override;

  procedure SetColor(AValue: TelColor); Override;


  function GetMouseOver(): Boolean; Override;
  function GetMouseOut(): Boolean; Override;
  function GetClick(): Boolean; Override;
public
  constructor Create; Overload; Override;
  constructor Create(aWidth, aHeight: Integer); Overload;
  constructor Create(aSize: TelSize); Overload;

  destructor Destroy; Override;

  procedure Draw(DrawChildren: Boolean = true); Override;
public
  property Vertices: TColorVertices read fVertices write fVertices;
end;

{ TelGradientRectangle }

TelGradientRectangle = class(TelRectangle)
  protected
    fGradient: TelGradient;

    function GetGradient(): TelGradient;
    procedure SetGradient(AValue: TelGradient);
  public
    property Gradient: TelGradient read GetGradient write SetGradient;
end;

{ TelRoundedRect }

TelRoundedRectangle = class(TelRectangle)
  protected
    fRoundedRadius: Integer;
  public
    procedure Draw(DrawChildren: Boolean = true); Override;
  published
    property RoundedRadius: Integer read fRoundedRadius write fRoundedRadius;
end;

// Intermediate GUI (best GUI conecept ever!)
TelGUI = class
  private

  public
    constructor Create;
    destructor Destroy; Override;

    procedure Box(Rect: TelRect; Color: TelColor); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Box(Rect: TelRect; Color: TelGradient); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Box(Rect: TelRect; Vertices: TColorVertices); Overload;

    procedure RoundedBox(Rect: TelRect; Color: TelColor; RoundedRadius: Integer = 5); Overload;
    //procedure RoundedBox(Rect: TelRect; Color: TelGradient; RoundedRadius: Integer = 5); Overload;

    procedure Circle(Rect: TelRect; Color: TelColor); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Polygon(Vertices: array of TelVertex); Overload;
    procedure Polygon(Rect: TelRect; Color: TelColor; Polygons: Integer = 5; Rotation: Single = 0.0); Overload;

    function Button(Rect: TelRect; Caption: String): Boolean;
end;

{$IFDEF AUTO_INIT}
var
  GUI: TelGUI;
{$ENDIF}

implementation

{ TelRoundedRectangle }

procedure TelRoundedRectangle.Draw;

  procedure DrawRoundedRect(Rect: TelRect; aRoundedRadius: Integer);
  var
    i: Single;
  begin
    glBegin(GL_POLYGON);

    glVertex2f(Rect.X + aRoundedRadius, Rect.Y);
    glVertex2f(Rect.X + Rect.W - aRoundedRadius, Rect.Y);

    i := Pi * 1.5;
    while i < (Pi * 2) do
    begin
      glVertex2f(Rect.X + Rect.W - aRoundedRadius + Cos(i)* aRoundedRadius, Rect.Y + aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glVertex2f(Rect.X + Rect.W , Rect.Y + aRoundedRadius);
    glVertex2f(Rect.X + Rect.W , Rect.Y + Rect.H - aRoundedRadius);

    i := 0.0;
    while i < (Pi * 0.5) do
    begin
      glVertex2f(Rect.X + Rect.W - aRoundedRadius + Cos(i)* aRoundedRadius, Rect.Y + Rect.H - aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glVertex2f(Rect.X + Rect.W - aRoundedRadius , Rect.Y + Rect.H);
    glVertex2f(Rect.X + aRoundedRadius , Rect.Y + Rect.H);

    i := Pi * 0.5;
    while i < Pi do
    begin
      glVertex2f(Rect.X + aRoundedRadius + Cos(i)* aRoundedRadius, Rect.Y + Rect.H - aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glVertex2f(Rect.X , Rect.Y + Rect.H - aRoundedRadius);
    glVertex2f(Rect.X , Rect.Y + aRoundedRadius);

    i := Pi;
    while i < (Pi * 1.5) do
    begin
      glVertex2f(Rect.X + aRoundedRadius + Cos(i)* aRoundedRadius, Rect.Y + aRoundedRadius + Sin(i) * aRoundedRadius);
      i := i + 0.1;
    end;

    glEnd();
  end;

begin
  if Visible then
  begin

    if (RoundedRadius > (Trunc(fWidth) div 2)) then RoundedRadius := (Trunc(fWidth) div 2);

    glColor4f(1.0, 1.0, 1.0, 1.0);

    glPushMatrix;
      glDisable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);


      glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Color.A / 255);

      DrawRoundedRect(makeRect((ParentPosition.X + Position.X) * ActiveWindow.ResScale.X,
                               (ParentPosition.Y + Position.Y) * ActiveWindow.ResScale.Y,
                               fWidth * ActiveWindow.ResScale.X,
                               fHeight * ActiveWindow.ResScale.Y), RoundedRadius);


      glDisable(GL_BLEND);
      glEnable(GL_TEXTURE_2D);
    glPopMatrix;
  end;

end;

{ TelGradientRectangle }

function TelGradientRectangle.GetGradient: TelGradient;
begin
  Result := fGradient;
end;

procedure TelGradientRectangle.SetGradient(AValue: TelGradient);
begin
  fGradient := AValue;

  case fGradient.GradientStyle of
    gsVertical:
      begin
        Vertices[0] := fGradient.StartColor;
        Vertices[1] := fGradient.StartColor;
        Vertices[2] := fGradient.EndColor;
        Vertices[3] := fGradient.EndColor;
      end;
    gsHorizontal:
      begin
        Vertices[0] := fGradient.EndColor;
        Vertices[1] := fGradient.StartColor;
        Vertices[2] := fGradient.StartColor;
        Vertices[3] := fGradient.EndColor;
      end;
  end;
end;

{ TelRectangle }

function TelRectangle.GetWidth(): Integer;
begin
  Result := fWidth;
end;

function TelRectangle.GetHeight(): Integer;
begin
  Result := fHeight;
end;

procedure TelRectangle.SetColor(AValue: TelColor);
begin
  fColor := AValue;

  Vertices[0] := fColor;
  Vertices[1] := fColor;
  Vertices[2] := fColor;
  Vertices[3] := fColor;
end;

function TelRectangle.GetMouseOver: Boolean;
var
  tempRect: TelRect;
begin
  inherited;

  tempRect.X := Self.Position.X * ActiveWindow.ResScale.X;
  tempRect.Y := Self.Position.Y * ActiveWindow.ResScale.Y;
  tempRect.W := fWidth * ActiveWindow.ResScale.X;
  tempRect.H := fHeight * ActiveWindow.ResScale.Y;

  {$IFDEF CAN_METHODS}
    Result := tempRect.ContainsVector(ActiveWindow.Cursor);
  {$ELSE}
    Result := RectContainsVector(fClipRect, ActiveWindow.Cursor);
  {$ENDIF}
end;

function TelRectangle.GetMouseOut: Boolean;
begin
  inherited;

  Result := not GetMouseOver;
end;

function TelRectangle.GetClick: Boolean;
begin
  inherited;

  Result := ((Input.Mouse.LeftClick()) and (GetMouseOver));
end;

constructor TelRectangle.Create;
begin
  inherited Create;
end;

constructor TelRectangle.Create(aWidth, aHeight: Integer);
begin
  Create;

  fWidth := aWidth;
  fHeight := aHeight;

  Origin := Center(Self);
end;

constructor TelRectangle.Create(aSize: TelSize);
begin
  Create(aSize.Width, aSize.Height);
end;

destructor TelRectangle.Destroy;
begin
  inherited Destroy;
end;

procedure TelRectangle.Draw();
var
  i: Integer;
begin
  inherited Draw;

  if Visible then
  begin
    glColor4f(1.0, 1.0, 1.0, 1.0);
    glDisable(GL_TEXTURE_2D);

    glPushMatrix;
      glColor3f(1, 1, 1);

  	glTranslatef((ParentPosition.X + Position.X - Margin.Left - Border.Left.Width - Padding.Left + Origin.X) * ActiveWindow.ResScale.X,
                     (ParentPosition.Y + Position.Y - Margin.Top - Border.Top.Width - Padding.Top + Origin.Y) * ActiveWindow.ResScale.Y, ParentPosition.Z);

  	if Abs(Rotation.Angle) >= 360.0 then Rotation.Angle := 0.0;

  	if Rotation.Angle <> 0.0 then glRotatef(Rotation.Angle, Rotation.Vector.X, Rotation.Vector.Y, Rotation.Vector.Z);

      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);

        if Shadow.Visible then
        begin
          for i := 0 to Shadow.Blur do
          begin
            glColor4f(Shadow.Color.R / 255, Shadow.Color.G / 255, Shadow.Color.B / 255, (Abs(i - (Shadow.Blur div 2)) / Shadow.Blur) * (Shadow.Color.A / 255));
  	    glScalef(Scale.X * ActiveWindow.ResScale.X, Scale.Y * ActiveWindow.ResScale.Y, 1);

            DrawQuad(-Origin.X + Shadow.Position.X - i, -Origin.Y + Shadow.Position.Y - i, fWidth + 2 * i, fHeight + 2 * i, Position.Z);
          end;
        end;

        glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Alpha / 255);
  	glScalef(Scale.X * ActiveWindow.ResScale.X, Scale.Y * ActiveWindow.ResScale.Y, 1);

        DrawQuad(-Origin.X, -Origin.Y, fWidth, fHeight, Position.Z, Vertices);

      glDisable(GL_BLEND);
      glEnable(GL_TEXTURE_2D);
    glPopMatrix;
  end;
end;

//
// GUI elements
//








//
// TelGUI
//
constructor TelGUI.Create;
begin

end;

destructor TelGUI.Destroy;
begin

end;


procedure TelGUI.Box(Rect: TelRect; Color: TelColor);
begin
  Box(Rect, makeGradient(Color, Color, gsVertical));
  (*glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Color.A / 255);
    DrawQuad(Rect.X * ActiveWindow.ResScale.X,
             Rect.Y * ActiveWindow.ResScale.Y,
             Rect.W * ActiveWindow.ResScale.X,
             Rect.H * ActiveWindow.ResScale.Y, 0);
    glDisable(GL_BLEND);
  glPopMatrix;       *)
end;

procedure TelGUI.Box(Rect: TelRect; Color: TelGradient);
var
  Vertices: TColorVertices;
begin
  case Color.GradientStyle of
    gsVertical:
      begin
        Vertices[0] := Color.StartColor;
        Vertices[1] := Color.StartColor;
        Vertices[2] := Color.EndColor;
        Vertices[3] := Color.EndColor;
      end;
    gsHorizontal:
      begin
        Vertices[0] := Color.EndColor;
        Vertices[1] := Color.StartColor;
        Vertices[2] := Color.StartColor;
        Vertices[3] := Color.EndColor;
      end;
  end;

  Self.Box(Rect, Vertices);

  (*glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    DrawQuad(Rect.X * ActiveWindow.ResScale.X,
             Rect.Y * ActiveWindow.ResScale.Y,
             Rect.W * ActiveWindow.ResScale.X,
             Rect.H * ActiveWindow.ResScale.Y, 0, Vertices);

    glDisable(GL_BLEND);
  glPopMatrix;*)
end;

procedure TelGUI.Box(Rect: TelRect; Vertices: TColorVertices);
begin
  glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glDisable(GL_TEXTURE_2D);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    DrawQuad(Rect.X * ActiveWindow.ResScale.X,
             Rect.Y * ActiveWindow.ResScale.Y,
             Rect.W * ActiveWindow.ResScale.X,
             Rect.H * ActiveWindow.ResScale.Y, 0, Vertices);

    glDisable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
  glPopMatrix;
end;

procedure TelGUI.RoundedBox(Rect: TelRect; Color: TelColor; RoundedRadius: Integer = 5);
var
  i: Single;
begin
  if (RoundedRadius > (Trunc(Rect.W) div 2)) then RoundedRadius := (Trunc(Rect.W) div 2);

  if (ActiveWindow.ResScale.X <> 0) then
  begin
    Rect.X := Rect.X * ActiveWindow.ResScale.X;
    Rect.W := Rect.W * ActiveWindow.ResScale.X;
  end;

  if (ActiveWindow.ResScale.Y <> 0) then
  begin
    Rect.Y := Rect.Y * ActiveWindow.ResScale.Y;
    Rect.H := Rect.H * ActiveWindow.ResScale.Y;
  end;

  glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Color.A / 255);

    glBegin(GL_POLYGON);

      glVertex2f(Rect.X + RoundedRadius, Rect.Y);
      glVertex2f(Rect.X + Rect.W - RoundedRadius, Rect.Y);

      i := Pi * 1.5;
      while i < (Pi * 2) do
      begin
        glVertex2f(Rect.X + Rect.W - RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

      glVertex2f(Rect.X + Rect.W , Rect.Y + RoundedRadius);
      glVertex2f(Rect.X + Rect.W , Rect.Y + Rect.H - RoundedRadius);

      i := 0.0;
      while i < (Pi * 0.5) do
      begin
        glVertex2f(Rect.X + Rect.W - RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + Rect.H - RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

      glVertex2f(Rect.X + Rect.W - RoundedRadius , Rect.Y + Rect.H);
      glVertex2f(Rect.X + RoundedRadius , Rect.Y + Rect.H);

      i := Pi * 0.5;
      while i < Pi do
      begin
        glVertex2f(Rect.X + RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + Rect.H - RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

      glVertex2f(Rect.X , Rect.Y + Rect.H - RoundedRadius);
      glVertex2f(Rect.X , Rect.Y + RoundedRadius);

      i := Pi;
      while i < (Pi * 1.5) do
      begin
        glVertex2f(Rect.X + RoundedRadius + Cos(i)* RoundedRadius, Rect.Y + RoundedRadius + Sin(i) * RoundedRadius);
        i := i + 0.1;
      end;

    glEnd();


    glDisable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
  glPopMatrix;
end;

procedure TelGUI.Circle(Rect: TelRect; Color: TelColor);
begin
  if Rect.W <> Rect.H then
  begin
    if Rect.W > Rect.H then Rect.H := Rect.W
      else Rect.W := Rect.H;
  end;

  Self.RoundedBox(Rect, Color, Trunc(Rect.W) div 2);
end;

procedure TelGUI.Polygon(Vertices: array of TelVertex);
var
  i: Integer;
begin
  glColor4f(1.0, 1.0, 1.0, 1.0);

  glPushMatrix;

    glBegin(GL_POLYGON);

      for i := 0 to High(Vertices) do
      begin
        glColor4f(Vertices[i].Color.R / 255, Vertices[i].Color.G / 255, Vertices[i].Color.B / 255, Vertices[i].Color.A / 255);
        glVertex3f(Vertices[i].Vector.X, Vertices[i].Vector.Y, Vertices[i].Vector.Z);
      end;

    glEnd();

  glPopMatrix;
end;

procedure TelGUI.Polygon(Rect: TelRect; Color: TelColor; Polygons: Integer = 5; Rotation: Single = 0.0);
begin
  if Polygons < 3 then Polygons := 3;


end;

function TelGUI.Button(Rect: TelRect; Caption: String): Boolean;
begin

end;

{$IFDEF AUTO_INIT}
initialization
  GUI := TelGUI.Create;

finalization
  GUI.Destroy;
{$ENDIF}

end.
