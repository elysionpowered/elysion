{%region '--- Unit description ---'}
(**
  *  Unit: ElysionObject
  *  Description: Contains basic class (TelObject)
  *
  * @author(Johannes Stein and contributors <http://elysionpowered.org>)
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

{ TelObject }

TelObject = class(TInterfacedObject, IObject)
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
  fName: String;          //< Privileged: Name of an object -> Access (read/write) through published properties (TelObject.Name)
  fNote: String;          //< Privileged: Note of an object -> Access (read/write) through published properties (TelObject.Note)
  fID: Integer;           //< Privileged: ID of an object -> Access (read/write) through published properties (TelObject.ID)
  fTag: Cardinal;         //< Privileged: Tag of an object -> Access (read/write) through published properties (TelObject.Tag)
  fDebug: Boolean;        //< Privileged: Debug flag -> Access (read/write) though published properties (TelObject.Debug)
  fIgnoreLogger: Boolean; //< Privileged: Ignore logging -> Won't log when object is created or destroyed (TelObject.IgnoreLogger)
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
    * procedure TelObject.DebugInfo @br
    * Type: Method @br
    * @param None @br
    * Description: Writes a debug message of the current object into the logger
    *)
  procedure DebugInfo(); virtual;

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: String @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: String; LogMessageType: TLogMessageType = ltError); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: Integer @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: Integer; LogMessageType: TLogMessageType = ltError); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: Single @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: Single; LogMessageType: TLogMessageType = ltError); Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
published
  property Debug: Boolean read fDebug write fDebug default false; //< Debug flag

  property Name: String read fName write fName;          //< Name of an object
  property ID: Integer read fID write fID default 0;     //< ID of an object
  property UniqueID: String read GetUniqueID;            //< Unique ID of an object; can't be changed
  property Note: String read fNote write fNote;          //< Note of an object: For example to be used for description, tooltip, etc.
  property Tag: Cardinal read fTag write fTag default 0; //< Tag of an object

  property IgnoreLogger: Boolean read fIgnoreLogger write fIgnoreLogger default false; //< Ignores sending passive messages
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
  fText: String;

  function GetHeight: Integer; virtual;
  function GetWidth: Integer; virtual;

  function GetSize: Integer; virtual;
  procedure SetSize(Value: Integer); virtual; //< May be empty depending on the implementation

  procedure SetText(aText: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function GetText: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
public
  procedure LoadFromFile(const aFilename: String); virtual; abstract;

  function GetWidth_Text(Text: String): Integer; virtual;

  procedure TextOut(aPoint: TelVector3f; aText: String = ''; LineBreak: Boolean = true); virtual; abstract;
published
  property Height: Integer read GetHeight;
  property Width: Integer read GetWidth;

  property Size: Integer read GetSize write SetSize;

  property Text: String read GetText write SetText;
end;
	


implementation

var
  ObjectCount: Integer = 1;

//
// TelObject
//

constructor TelObject.Create;
begin
  inherited Create;

  fObjectCount := ObjectCount;
  fIgnoreLogger := false;


  if ltNote in TelLogger.GetInstance.Priorities then
    TelLogger.GetInstance.WriteLog('<i>Object created:</i> ' + Self.UniqueID, 'Initialization', ltNote, true);

  ObjectCount := ObjectCount + 1;
end;

destructor TelObject.Destroy;
begin
  if ltNote in TelLogger.GetInstance.Priorities then
    TelLogger.GetInstance.WriteLog('<i>Object destroyed:</i> ' + Self.UniqueID, 'Finalization', ltNote, true);
	
  inherited;
end;

procedure TelObject.DebugInfo();
begin
  ;
end;

function TelObject.GetUniqueID(): String;
begin
  Result := Self.ClassName + IntToString(fObjectCount, true, 4);
end;

procedure TelObject.Log(Msg: String; LogMessageType: TLogMessageType = ltError);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + Msg, LogMessageType);
end;

procedure TelObject.Log(Msg: Integer; LogMessageType: TLogMessageType = ltError);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + IntToStr(Msg), LogMessageType);
end;

procedure TelObject.Log(Msg: Single; LogMessageType: TLogMessageType = ltError);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + FloatToStr(Msg), LogMessageType);
end;

procedure TelFontContainer.SetText(aText: String);
begin
  if aText <> fText then
    fText := aText;
end;

function TelFontContainer.GetText: String;
begin
  Result := fText;
end;

function TelFontContainer.GetSize: Integer;
begin
  Result := 0;
end;

procedure TelFontContainer.SetSize(Value: Integer);
begin
  ;
end;

function TelFontContainer.GetHeight: Integer;
begin
  Result := 0;
end;

function TelFontContainer.GetWidth: Integer;
begin
  Result := 0;
end;

function TelFontContainer.GetWidth_Text(Text: String): Integer;
begin
  Result := 0;
end;

end.
