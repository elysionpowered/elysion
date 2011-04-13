unit ElysionInterfaces;

{$I Elysion.inc}

interface

uses
  ElysionTypes,

  Classes;

type
  IObject = interface
    function DebugInfo(): String;
  end;

  // General module container
  IModuleContainer = interface(IObject)
    function Initialize(): Boolean;
    procedure Finalize();
  end;
  
  IEventListener = interface(IObject)
    function AddEventListener(anEventName: String; anEvent: TelEvent): Boolean;
	function RemoveEventListener(anEvent: TelEvent): Boolean;
	function HasEventListener(anEventName: String): Boolean;
	procedure DispatchEvent(anEvent: TelEvent);
  end;
  
  IEntity = interface(IObject)
    procedure Update(dt: Double = 0.0);
  end;

  INode = interface(IEntity)

    procedure Move(Delta: TelVector3f); Overload;
    procedure Move(Delta: TelVector2i); Overload;

    procedure Rotate(DeltaAngle: Single);

    procedure Animate(AnimProperty: TelAnimationProperty; Duration: Integer = 1000; Delay: Integer = 0; Transition: TelAnimationTransition = atLinear);
    function WriteToXML(): TStringList;

    procedure Draw;
  end;

  ITimer = interface(IObject)
    procedure Start();

    procedure Pause();
    procedure UnPause();

    procedure Stop();

    procedure Update();
  end;

implementation

end.
