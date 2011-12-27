unit ElysionCollision;

interface

{$I Elysion.inc}

uses
  ElysionTypes,
  ElysionSprite;

type
  TPixelCollision = class
  public
    class function Check(): Boolean; Overload;
  end;

implementation

end.
