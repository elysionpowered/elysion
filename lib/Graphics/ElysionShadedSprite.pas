unit ElysionShadedSprite;

{$I Elysion.inc}

interface

uses
  ElysionSprite,
  ElysionRendering;

type

  { TelShadedSprite }

  TelShadedSprite = class(TelSprite)
  private
    fShader : TelShader;
  public
    function LoadShaders(vshader: AnsiString; pshader: AnsiString): Boolean;
    procedure Draw(DrawChildren: Boolean = true); Override;
  end;

implementation

{ TelShadedSprite }

function TelShadedSprite.LoadShaders(vshader: AnsiString; pshader: AnsiString
  ): Boolean;
var
  plist, vlist : TStringList;
begin
  plist := TStringList.Create();
  vlist := TStringList.Create();

  plist.LoadFromFile( pshader );
  vlist.LoadFromFile( vshader );

  shader := TelShader.Create( vlist.Text, plist.Text );

  result := shader.Compile();

  shader.Map( Texture, 'texture');
  shader.Update;

  if not Result then Self.Log('Error while compiling shaders');
end;

procedure TelShadedSprite.Draw(DrawChildren: Boolean);
var
  loc : TelVector3i;
begin
  if ((Visible) and (not Texture.Empty)) then
  begin

  	loc.X := Width;
  	loc.Y := Height;

  	fWidth := fWidth *  Integer(Round(Scale.X) + 1);
  	fHeight := fHeight * Integer(Round(Scale.Y) + 1);

    shader.BindShader;
    glColor4f(1.0, 1.0, 1.0, 1.0);
    glEnable(GL_TEXTURE_2D);

    glPushMatrix;
      glColor3f(1, 1, 1);
      glBindTexture(GL_TEXTURE_2D, Self.Texture.TextureID);
      if Transparent then
      begin
        glEnable(GL_ALPHA_TEST);
        glAlphaFunc(GL_GREATER, 0.1);
      end;

  	glTranslatef((ParentPosition.X + Position.X - Margin.Left - Border.Left.Width - Padding.Left + Origin.X) * ActiveWindow.ResScale.X,
                     (ParentPosition.Y + Position.Y - Margin.Top - Border.Top.Width - Padding.Top + Origin.Y) * ActiveWindow.ResScale.Y, ParentPosition.Z);

  	if Abs(Rotation.Angle) >= 360.0 then Rotation.Angle := 0.0;

  	if Rotation.Angle <> 0.0 then glRotatef(Rotation.Angle, Rotation.Vector.X, Rotation.Vector.Y, Rotation.Vector.Z);

      case BlendMode of
        bmAdd: glBlendFunc(GL_ONE, GL_ONE);
        bmNormal: glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        bmSub: glBlendFunc(GL_ZERO, GL_ONE);
      end;
      glEnable(GL_BLEND);

  	glColor4f(Color.R / 255, Color.G / 255, Color.B / 255, Alpha / 255);
  //	glScalef(Scale.X * ActiveWindow.ResScale.X, Scale.Y * ActiveWindow.ResScale.Y, 1);

      glBegin(GL_QUADS);

        glTexCoord2f(0, 1);
        glVertex3f(Position.X / ActiveWindow.Width - 1  , Position.Y / ActiveWindow.Height, -Position.Z);

        glTexCoord2f(0, 0);
        glVertex3f( Position.X / ActiveWindow.Width- 1 , (Position.Y + fHeight) /  ActiveWindow.Height,  -Position.Z);

        glTexCoord2f(1, 0);
        glVertex3f((Position.X + fWidth) / ActiveWindow.Width - 1 , (Position.Y + fHeight) /  ActiveWindow.Height, -Position.Z);

        glTexCoord2f(1,1);
        glVertex3f((Position.X + fWidth) / ActiveWindow.Width - 1 , Position.Y / ActiveWindow.Height, -Position.Z);

      glEnd;


    glPopMatrix;

    glBindTexture(GL_TEXTURE_2D, 0);
    glDisable(GL_TEXTURE_2D);

    shader.UnbindShader;

    fWidth := loc.X;
    fHeight := loc.Y;
  end;

  inherited Draw(DrawChildren);
end;

end.
