unit ElysionInterfaces;

{$I Elysion.inc}

interface

uses
  ElysionTypes,
  ElysionEvents,
  ElysionAnimTypes,

  ElysionGraphicsProvider,

  Classes;

type
  IObject = interface
    procedure DebugInfo();
  end;

  IDataContainer = interface

  end;

  IDataReader = interface
    function LoadFromFile(aFilename: AnsiString): Boolean;

    function GetNodeValue(aNodeName: AnsiString): AnsiString;

    function GetItemValue(anIndex: Integer): AnsiString;
    function GetItemName(anIndex: Integer): AnsiString;

    function GetItemAttributeName(anIndex, anAttribute: Integer): AnsiString;
    function GetItemAttributeValue(anIndex, anAttribute: Integer): AnsiString;
  end;


  IDataWriter = interface
    function SaveToFile(aFilename: AnsiString): Boolean;

    procedure AddElement(aNodeName, aNodeValue: AnsiString); Overload;
    procedure AddElement(aNodeName, aNodeValue: AnsiString; Attributes: array of AnsiString); Overload;
  end;
  
  IReadableData = interface
    function LoadFromPlain(aData: TStringList): Boolean;
    function LoadFromXML(aData: TStringList): Boolean;
    function LoadFromJSON(aData: TStringList): Boolean;
  end;
  
  IWritableData = interface
    function WriteToPlain(): TStringList;
    function WriteToXML(): TStringList;
    function WriteToJSON(): TStringList;
  end;

  // General module container
  IContainer = interface(IObject) ['{131D9EBF-615D-49C8-9C02-61AE1C50E5E2}']
    function Initialize(): Boolean;

    //function IsInitialized(): Boolean;
    //function IsFinalized(): Boolean;

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

    procedure SendMessage(Message: AnsiString);

    function DidReceiveMessage(Message: AnsiString): Boolean;
  end;


  IEventListener = interface(IObject)
    function AddEventListener(anEventName: AnsiString; anEvent: TelEvent): Boolean;
    function RemoveEventListener(anEvent: TelEvent): Boolean;
    function HasEventListener(anEventName: AnsiString): Boolean;
    procedure DispatchEvent(anEvent: TelEvent);

    procedure RegisterEvent();
  end;
  
  IEntity = interface(IObject) 

    procedure Update(dt: Double = 0.0);
  end;

  INode = interface(IObject)
    procedure Move(aPoint: TelVector3f; dt: Double = 0.0); Overload;
    procedure Move(aPoint: TelVector2i; dt: Double = 0.0); Overload;
    procedure Move(aPoint: TelVector2f; dt: Double = 0.0); Overload;

    procedure Rotate(DeltaAngle: Single; dt: Double = 0.0);

    procedure Animate(AnimProperty: String; TargetValue: Single; Duration: Integer = 1000; Delay: Integer = 0; Transition: TelAnimationTransition = atLinear);

    procedure Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true);
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
