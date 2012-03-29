unit ElysionAssets;

{$I Elysion.inc}

interface

uses
  ElysionObject,
  ElysionTexture,
  ElysionSprite,
  ElysionParallaxSprite,
<<<<<<< HEAD
  ElysionSpriteSheet,
  ElysionGUI;
=======
  ElysionSpriteSheet;
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1

type

  { TelAssets }

  TelAssets = class(TelObject)
    private
      fCount: Integer;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      function LoadTexture(Filename: String): TelTexture;
<<<<<<< HEAD
      function LoadSprite(Filename: String): TelSprite; Overload; inline;
      function LoadSprite(Texture: TelTexture): TelSprite; Overload; inline;
      function LoadParallaxSprite(Filename: String): TelParallaxSprite; Overload; inline;
      function LoadParallaxSprite(Texture: TelTexture): TelParallaxSprite; Overload; inline;
      function LoadSpriteSheet(Filename: String): TelSpriteSheet; Overload; inline;
      function LoadSpriteSheet(Texture: TelTexture): TelSpriteSheet; Overload; inline;
=======
      function LoadSprite(Filename: String): TelSprite; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadSprite(Texture: TelTexture): TelSprite; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadParallaxSprite(Filename: String): TelParallaxSprite; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadParallaxSprite(Texture: TelTexture): TelParallaxSprite; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadSpriteSheet(Filename: String): TelSpriteSheet; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function LoadSpriteSheet(Texture: TelTexture): TelSpriteSheet; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
>>>>>>> b3f438658ffe9c95146f9fbe4504ce33a0f939d1
    published
      property Count: Integer read fCount;
  end;

{$IFDEF AUTO_INIT}
var
  Assets: TelAssets;
{$ENDIF}

implementation

{ TelAssets }

constructor TelAssets.Create;
begin
  inherited;

  fCount := 0;
end;

destructor TelAssets.Destroy;
begin
  inherited;
end;

function TelAssets.LoadTexture(Filename: String): TelTexture;
var
  tmpObject: TelTexture;
begin
  fCount := fCount + 1;

  tmpObject := TelTexture.Create;
  tmpObject.LoadFromFile(Filename);

  Result := tmpObject;
end;

function TelAssets.LoadSprite(Filename: String): TelSprite;
var
  tmpObject: TelSprite;
begin
  fCount := fCount + 1;

  tmpObject := TelSprite.Create;
  tmpObject.LoadFromFile(Filename);

  Result := tmpObject;
end;

function TelAssets.LoadSprite(Texture: TelTexture): TelSprite;
var
  tmpObject: TelSprite;
begin
  fCount := fCount + 1;

  tmpObject := TelSprite.Create;
  tmpObject.LoadFromTexture(Texture);

  Result := tmpObject;
end;

function TelAssets.LoadParallaxSprite(Filename: String): TelParallaxSprite;
var
  tmpObject: TelParallaxSprite;
begin
  fCount := fCount + 1;

  tmpObject := TelParallaxSprite.Create;
  tmpObject.LoadFromFile(Filename);

  Result := tmpObject;
end;

function TelAssets.LoadParallaxSprite(Texture: TelTexture): TelParallaxSprite;
var
  tmpObject: TelParallaxSprite;
begin
  fCount := fCount + 1;

  tmpObject := TelParallaxSprite.Create;
  tmpObject.LoadFromTexture(Texture);

  Result := tmpObject;
end;

function TelAssets.LoadSpriteSheet(Filename: String): TelSpriteSheet;
var
  tmpObject: TelSpriteSheet;
begin
  fCount := fCount + 1;

  tmpObject := TelSpriteSheet.Create;
  tmpObject.LoadFromFile(Filename);

  Result := tmpObject;
end;

function TelAssets.LoadSpriteSheet(Texture: TelTexture): TelSpriteSheet;
var
  tmpObject: TelSpriteSheet;
begin
  fCount := fCount + 1;

  tmpObject := TelSpriteSheet.Create;
  tmpObject.LoadFromTexture(Texture);

  Result := tmpObject;
end;

{$IFDEF AUTO_INIT}
initialization
  Assets := TelAssets.Create;

finalization
  Assets.Destroy;
{$ENDIF}

end.
