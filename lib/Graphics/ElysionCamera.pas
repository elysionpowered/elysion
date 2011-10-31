unit ElysionCamera;

{$I Elysion.inc}

interface

uses
    Classes,
    SysUtils,

    {$IFDEF USE_DGL_HEADER}
    dglOpenGL,
    {$ELSE}
    gl, glu, glext,
    {$ENDIF}

    ElysionTypes,
    ElysionObject,
    ElysionLayer,
    ElysionApplication,
    ElysionNode;


type

{ TelCamera }

TelCamera = class(TelLayer)
  private
    fOrgPos: array of TelVector3f;
    fEnablePixelCheck: Boolean;
  protected
    function GetWidth(): Integer; Override;
    function GetHeight(): Integer; Override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Draw(); //Override;
    procedure Update(dt: Double = 0.0); Override;
  public
    Viewport: TelRect;
    Velocity: TelVector3f;
    FixedSize: TelSize;
  published
    property EnablePixelCheck: Boolean read fEnablePixelCheck write fEnablePixelCheck; // Enables bounding box viewport detection for TelSprite and derivates
end;

implementation

{ TelCamera }

function TelCamera.GetWidth(): Integer;
begin
  Result := Trunc(Viewport.W);
end;

function TelCamera.GetHeight(): Integer;
begin
  Result := Trunc(Viewport.H);
end;

constructor TelCamera.Create;
begin
  inherited Create;

  fEnablePixelCheck := true;

  Viewport := makeRect(0, 0, ActiveWindow.Width, ActiveWindow.Height);

  FixedSize.Width := -1;
  FixedSize.Height := -1;
end;

destructor TelCamera.Destroy;
begin

  inherited;
end;

procedure TelCamera.Draw();
var
  i: Integer;
begin
  // This right here is how it shouldn't be done
  // If you have a fix or a suggestion on how to make it better by not using
  // any GL code at all, please submit a patch through Github

  glPushMatrix;

    glTranslatef((Position.X - Margin.Left - Border.Left.Width - Padding.Left - Viewport.X + Origin.X) * ActiveWindow.ResScale.X * Scale.X,
                 (Position.Y - Margin.Top - Border.Top.Width - Padding.Top - Viewport.Y + Origin.Y) * ActiveWindow.ResScale.Y * Scale.Y, 0);

    if Abs(Rotation.Angle) >= 360.0 then Rotation.Angle := 0.0;

    if Rotation.Angle <> 0.0 then glRotatef(Rotation.Angle, Rotation.Vector.X, Rotation.Vector.Y, Rotation.Vector.Z);

    glScalef(Scale.X * ActiveWindow.ResScale.X, Scale.Y * ActiveWindow.ResScale.Y, 1);

    for i := 0 to fNodeList.Count - 1 do
    begin
      if (fNodeList.Items[i] <> nil) then
      begin
        fNodeList.Items[i].Draw;
      end;
    end;

  glPopMatrix;

end;

procedure TelCamera.Update(dt: Double = 0.0);
var
  i: Integer;
begin
  inherited;

  if ((Velocity.X <> 0.0) or (Velocity.Y <> 0.0)) then
  begin
    Self.Viewport.X := Self.Viewport.X + Velocity.X;
    Self.Viewport.Y := Self.Viewport.Y + Velocity.Y;
  end;
end;

end.
