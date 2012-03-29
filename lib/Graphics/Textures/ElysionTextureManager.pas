unit ElysionTextureManager;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}
{$I Elysion.inc}

interface

uses
  ElysionTypes,
  ElysionObject,
  ElysionContainer,
  ElysionTexture,

  {$IFDEF USE_VAMPYRE}
  ImagingSDL,
  {$ENDIF}

  SDL;


type

  // Factory
  TelTextureManager = class sealed(TelStaticObject)
    private
      class var fTextureList: TelTextureList;
    public
      class constructor Create;
      class destructor Destroy;

      class function CreateNewTexture(aFilename: String): TelTexture; Overload;
      class function CreateNewTexture(aSurface: PSDL_Surface): TelTexture; Overload;

      class function ForceNewTexture(aFilename: String): TelTexture; Overload;
      class function ForceNewTexture(aSurface: PSDL_Surface): TelTexture; Overload;

      class procedure ReloadAllTextures(); inline;
    public
      class property TextureList: TelTextureList read fTextureList write fTextureList;
  end;

implementation

class constructor TelTextureManager.Create;
begin
  inherited;

  fTextureList := TelTextureList.Create;
end;

class destructor TelTextureManager.Destroy;
begin
  fTextureList.Free;

  inherited;
end;

class function TelTextureManager.CreateNewTexture(aFilename: String): TelTexture;
var
  i: Integer;
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  // Is Texture already in stack?
  if fTextureList.Count > 0 then
  begin
    for i := 0 to fTextureList.Count - 1 do
    begin
      if fTextureList.Items[i].Filename = aFilename then
      begin
        Result := fTextureList.Items[i];
        Exit;
      end;
    end;
  end;


  // If not, add it
  tmpTexture.LoadFromFile(aFilename);

  fTextureList.Add(tmpTexture);
  Result := tmpTexture;

end;

// Fix this! Is the same as ForceNewTexture
class function TelTextureManager.CreateNewTexture(aSurface: PSDL_Surface): TelTexture;
var
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  tmpTexture.LoadFromSDLSurface(aSurface);
  fTextureList.Add(tmpTexture);
  Result := tmpTexture;

end;

class function TelTextureManager.ForceNewTexture(aFilename: String): TelTexture;
var
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  tmpTexture.LoadFromFile(aFilename);
  fTextureList.Add(tmpTexture);
  Result := tmpTexture;
end;

class function TelTextureManager.ForceNewTexture(aSurface: PSDL_Surface): TelTexture;
var
  tmpTexture: TelTexture;
begin
  tmpTexture := TelTexture.Create;

  tmpTexture.LoadFromSDLSurface(aSurface);
  fTextureList.Add(tmpTexture);
  Result := tmpTexture;

end;

class procedure TelTextureManager.ReloadAllTextures();
begin
  fTextureList.ReloadAllTextures();
end;

end.
