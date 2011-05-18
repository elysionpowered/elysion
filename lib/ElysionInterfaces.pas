unit ElysionInterfaces;

{$I Elysion.inc}

interface

uses
  ElysionTypes,

  Classes;

type
  IObject = interface
    procedure DebugInfo();
  end;
  
  IReadableData = interface
    function LoadFromXML(): Boolean;
	function LoadFromJSON(): Boolean;
  end;
  
  IWritableData = interface
    function WriteToXML(): TStringList;
	function WriteToJSON(): TStringList;
  end;

  // General module container
  IContainer = interface(IObject)
    function Initialize(): Boolean;
    procedure Finalize();
  end;
  
  {IStylable = interface(IObject, IReadableData, IWritableData)
    
  end;
  
  IGraphicsProvider = interface(IContainer)
    procedure DrawTexture(Texture: ITexture);
	procedure DrawShape(Rect: TelRect; Style: TShapeStyle);
	procedure DrawLine(StartPoint, EndPoint: TelVector3f);
  end;
  
  IComponent = interface(IContainer, IReadableData, IWritableData)
    procedure Update(dt: Double = 0.0);

	procedure SendMessage(Message: String); Overload;
	procedure SendMessage(Message: String; Receiver: IComponent); Overload;
  end;}
  
  //IEventListener = interface(IComponent)
  IEventListener = interface(IObject)
    function AddEventListener(anEventName: String; anEvent: TelEvent): Boolean;
	function RemoveEventListener(anEvent: TelEvent): Boolean;
	function HasEventListener(anEventName: String): Boolean;
	procedure DispatchEvent(anEvent: TelEvent);
  end;
  
  IEntity = interface(IObject) 
    (*procedure Attach(Component: IComponent); Overload;
	procedure Attach(Components: array of IComponent); Overload;
	
	procedure Detach(Component: IComponent); Overload;
	procedure Detach(Components: array of IComponent); Overload;
	
	procedure OnEnterFrame();
	
	procedure UpdateComponent(Components: array of IComponent);*)
	
	procedure Update(dt: Double = 0.0);
  end;

  INode = interface(IEntity)
	{procedure Attach(Node: INode); Overload;
	procedure Attach(Nodes: array of INode); Overload;
	
	procedure Detach(Node: INode); Overload;
	procedure Detach(Nodes: array of INode); Overload;}
  
    procedure Move(Delta: TelVector3f); Overload;
    procedure Move(Delta: TelVector2i); Overload;

    procedure Rotate(DeltaAngle: Single);

    procedure Animate(AnimProperty: TelAnimationProperty; Duration: Integer = 1000; Delay: Integer = 0; Transition: TelAnimationTransition = atLinear);

    procedure Draw;
  end;

  //ITimer = interface(IComponent)
  ITimer = interface(IObject)
    procedure Start();

    procedure Pause();
    procedure UnPause();

    procedure Stop();

    procedure Update(dt: Double = 0.0);
  end;

implementation

end.
