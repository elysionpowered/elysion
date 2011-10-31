unit ElysionInterfaces;

{$I Elysion.inc}

interface

uses
  ElysionTypes,
  ElysionEvents,
  ElysionAnimTypes,

  Classes;

type
  IObject = interface
    procedure DebugInfo();
  end;

  IDataContainer = interface

  end;
  
  IReadableData = interface
    function LoadFromXML(aData: TStringList): Boolean;
    function LoadFromJSON(aData: TStringList): Boolean;
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
  end;}
  
  IComponent = interface(IContainer)
    procedure Update(dt: Double = 0.0);

    procedure SendMessage(Message: String); Overload;
    procedure SendMessage(Message: String; Receiver: IComponent); Overload;

    function DidReceiveMessage(Message: String): Boolean;
    function ReceivedMessages: TStringList;
  end;


  IEventListener = interface(IObject)
    function AddEventListener(anEventName: String; anEvent: TelEvent): Boolean;
    function RemoveEventListener(anEvent: TelEvent): Boolean;
    function HasEventListener(anEventName: String): Boolean;
    procedure DispatchEvent(anEvent: TelEvent);

    procedure RegisterEvent();
  end;
  
  IEntity = interface(IObject) 
    {procedure Add(Component: IComponent); Overload;
    procedure Add(Components: array of IComponent); Overload;

    procedure Remove(Component: IComponent); Overload;
    procedure Remove(Components: array of IComponent); Overload;}

    procedure Update(dt: Double = 0.0);
  end;

  INode = interface(IObject)
    procedure Move(aPoint: TelVector3f; dt: Double = 0.0); Overload;
    procedure Move(aPoint: TelVector2i; dt: Double = 0.0); Overload;
    procedure Move(aPoint: TelVector2f; dt: Double = 0.0); Overload;

    procedure Rotate(DeltaAngle: Single; dt: Double = 0.0);

    procedure Animate(AnimProperty: TelAnimationProperty; Duration: Integer = 1000; Delay: Integer = 0; Transition: TelAnimationTransition = atLinear);

    procedure Draw(DrawChildren: Boolean = true);
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
