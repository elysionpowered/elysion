unit ElysionCollision;

interface

{$I Elysion.inc}

uses
  ElysionTypes,
  ElysionNode;

type
  TelCollision = class
  public
   class function Intersection(A, B: TelNode): TelRect;

   class function BoundingBox(A, B: TelNode): Boolean;
   class function Circle(A, B: TelNode): Boolean;

   class function PixelTest(A, B: TelTexturedNode): Boolean;
   class function PixelTest(aNode: TelTexturedNode; aRect: TelRect): Boolean;
  end;

implementation

end.
