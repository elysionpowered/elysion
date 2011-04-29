{%region '--- Unit description ---'}
(**
  *  Unit: ElysionObject
  *  Description: Contains basic class (TelObject)
  *
  *  Part of Elysion Frameworks
  *  Elysion Frameworks is licensed under the MIT/MPL license
  *  See LICENSE.TXT for more information
  *
  *)
{%endregion}

unit ElysionObject;

interface

{$I Elysion.inc}

uses
  ElysionInterfaces,

  ElysionUtils,
  ElysionTypes,
  ElysionLogger,
  SysUtils;

type

(**
  * Class: TelObject @br
  * Group: Essential @br
  * Description:
  *   Basic object
  *)
TelObject = class(TInterfacedObject)
private
  FObjectCount: Integer; //< Private stored: Object creation id -> Part of Unique ID

  (**
    * Access (read-only) through published property (TelObject.UniqueID)
    * @param None
    * @return Unique ID of a TelObject class or an ancestor of an TelObject class
    *
    *)
  function GetUniqueID(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
protected
  FName: String;      //< Privileged: Name of an object -> Access (read/write) through published properties (TelObject.Name)
  FNote: String;      //< Privileged: Note of an object -> Access (read/write) through published properties (TelObject.Note)
  FDrawable: Boolean; //< Privileged: Draw flag of an object -> Read-only
  FID: Integer;       //< Privileged: ID of an object -> Access (read/write) through published properties (TelObject.ID)
  FTag: Cardinal;     //< Privileged: Tag of an object -> Access (read/write) through published properties (TelObject.Tag)
public
  (**
    * constructor TelObject.Create @br
    * Type: Constructor @br
    * @param None @br
    * Description: Creates a basic object
    *)
  constructor Create(); virtual;

  (**
    * destructor TelObject.Destroy @br
    * Type: Destructor @br
    * @param None @br
    * Description: Destroy the basic object
    *)
  destructor Destroy; Override;

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: String @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: String); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: Integer @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: Integer); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: Single @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: Single); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
published
  property Drawable: Boolean read FDrawable;             //< Determines if object can be drawn on the screen
  property Name: String read FName write FName;          //< Name of an object
  property ID: Integer read FID write FID default 0;     //< ID of an object
  property UniqueID: String read GetUniqueID;            //< Unique ID of an object; can't be changed
  property Note: String read FNote write FNote;          //< Note of an object: For example to be used for description, tooltip, etc.
  property Tag: Cardinal read FTag write FTag default 0; //< Tag of an object
end;
	
//TelObjectList = class(TList)

//end;

TelModuleContainer = class(TelObject, IContainer)
protected
  fDriverName: String;
public
  function Initialize(): Boolean; virtual; abstract;
  procedure Finalize(); virtual; abstract;
published
  property DriverName: String read fDriverName;
end;

TelFontContainer = class(TelObject)
protected
  function GetHeight(): Integer; virtual; abstract;
  function GetWidth(): Integer; virtual; abstract;

  function GetSize(): Integer; virtual; abstract;
  procedure SetSize(Value: Integer); virtual; abstract; //< May be empty depending on the implementation
public
  procedure LoadFromFile(const aFilename: String); virtual; abstract;

  public function GetWidth_Text(Text: String): Integer; virtual; abstract;

  procedure TextOut(aPoint: TelVector3f; aText: String = ''; LineBreak: Boolean = true); virtual; abstract;
published
  property Height: Integer read GetHeight;
  property Width: Integer read GetWidth;

  property Size: Integer read GetSize write SetSize;
end;
	


implementation

var
  ObjectCount: Integer = 1;

//
// TelObject
//

constructor TelObject.Create();
begin
  inherited;
	
  fDrawable := false;
  fObjectCount := ObjectCount;
  
  if isLoggerActive then
  begin
    if ltNote in TelLogger.GetInstance.Priorities then
      TelLogger.GetInstance.WriteLog('<i>Object created:</i> ' + Self.UniqueID, 'Initialization', ltNote, true);
  end;

  ObjectCount := ObjectCount + 1;
end;

destructor TelObject.Destroy;
begin
  if isLoggerActive then
  begin
    if ltNote in TelLogger.GetInstance.Priorities then
      TelLogger.GetInstance.WriteLog('<i>Object destroyed:</i> ' + Self.UniqueID, 'Finalization', ltNote, true);
  end;
	
  inherited;
end;

function TelObject.GetUniqueID(): String;
begin
  Result := Self.ClassName + IntToString(fObjectCount, true, 4);
end;

procedure TelObject.Log(Msg: String);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + Msg);
end;

procedure TelObject.Log(Msg: Integer);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + IntToStr(Msg));
end;

procedure TelObject.Log(Msg: Single);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + FloatToStr(Msg));
end;

end.
