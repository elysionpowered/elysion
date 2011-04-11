(*
  This unit provides classes for Horde3D (something we always wanted, right? ;) )
  which intertwines with Elysion classes. So you can use Elysion action animators
  with Horde 3D nodes.
*)
unit ElysionHorde3D;
// Random nerd comment:
// If you we ever visited GamesCom or Games Convention you might have noticed
// that Blizzard is always playing their Murloc song... So every time I hear
// the word "Horde" I'm always reminded of that song

interface

uses
  ElysionNode,
  ElysionContent,
  Horde3D;

type
  TelHorde3DNode = class(TelNode)

  end;

  TelHorde3DLightNode = class(TelHorde3DNode)

  end;

implementation

end.
