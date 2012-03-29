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
  ElysionStrings,
  ElysionInterfaces,

  ElysionUtils,
  ElysionTypes,
  ElysionLogger,

  Classes,
  SysUtils,
  TypInfo;

type

(**
  * Class: TelObject @br
  * Group: Essential @br
  * Description:
  *   Basic object
  *)

{ TelObject }

TelObject = class abstract(TInterfacedPersistent, IObject)
strict private
  class var ObjectCount: Integer;
private
  fObjectCount: Integer; //< Private stored: Object creation id -> Part of Unique ID
  fNumProperties: Integer;

  fTypeData: PTypeData;
  fTypeInfo: PTypeInfo;

  (**
    * Access (read-only) through published property (TelObject.UniqueID)
    * @param None
    * @return Unique ID of a TelObject class or an ancestor of an TelObject class
    *
    *)
  function GetHash: AnsiString;
  function GetUniqueID: AnsiString; inline;
protected
  class var fContentPath: AnsiString;
protected
  fName: AnsiString;          //< Privileged: Name of an object -> Access (read/write) through published properties (TelObject.Name)
  fNote: AnsiString;          //< Privileged: Note of an object -> Access (read/write) through published properties (TelObject.Note)
  fID: Integer;           //< Privileged: ID of an object -> Access (read/write) through published properties (TelObject.ID)
  fTag: Cardinal;         //< Privileged: Tag of an object -> Access (read/write) through published properties (TelObject.Tag)
  fDebug: Boolean;        //< Privileged: Debug flag -> Access (read/write) though published properties (TelObject.Debug)

  // Standard property setters
  procedure SetPropertyStr(const aName: String; aValue: AnsiString); inline;

  // Standard property getters
  function GetPropertyStr(const aName: String): AnsiString; inline;

  procedure SetPropertyInt(Index: Integer; aValue: AnsiString); inline;

  function GetPropertyInt(Index: Integer): AnsiString; inline;


  // Debug Info
  function GetDebugInfoString(): TStringList; virtual;

  function GetAbsolutePath: AnsiString;
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
  procedure DebugInfo();

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: AnsiString @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: AnsiString; LogMessageType: TelLogger.TLogMessageType = ltError); Overload; inline;

  procedure Log(Msg: AnsiString; Args: array of const; LogMessageType: TelLogger.TLogMessageType = ltError); Overload;

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: Integer @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: Integer; LogMessageType: TelLogger.TLogMessageType = ltError); Overload; inline;

  (**
    * procedure TelObject.Msg @br
    * Type: Method @br
    * @param Msg: Single @br
    * Description: Logs Msg as an error message
    *)
  procedure Log(Msg: Single; LogMessageType: TelLogger.TLogMessageType = ltError); Overload; inline;

  procedure Log(Msg: TStringList; LogMessageType: TelLogger.TLogMessageType = ltError); Overload; inline;

  // RTTI public functions
  function GetProperty(const aName: String): TelObject; inline;

  procedure SetProperty(const aName: String; aValue: Integer); Overload; inline;
  procedure SetProperty(const aName: String; aValue: Boolean); Overload; inline;
  procedure SetProperty(const aName: String; aValue: Single); Overload; inline;
  procedure SetProperty(const aName: String; aValue: TelObject); Overload; inline;

  function PropertyExists(const aName: String; LogError: Boolean = true): Boolean; inline;
  function GetPropertyType(const aName: String): AnsiString; inline;
public
  property Properties[Index: Integer]: AnsiString read GetPropertyInt write SetPropertyInt;
  property Properties[Index: String]: AnsiString read GetPropertyStr write SetPropertyStr;
public
  class property ContentPath: AnsiString read fContentPath write fContentPath;
published
  property Debug: Boolean read fDebug write fDebug default false; //< Debug flag

  property Hash: AnsiString read GetHash;

  property Name: AnsiString read fName write fName;      //< Name of an object
  property ID: Integer read fID write fID default 0;     //< ID of an object
  property UniqueID: AnsiString read GetUniqueID;        //< Unique ID of an object; can't be changed
  property Note: AnsiString read fNote write fNote;      //< Note of an object: For example to be used for description, tooltip, etc.
  property Tag: Cardinal read fTag write fTag default 0; //< Tag of an object

  property NumProperties: Integer read fNumProperties;
end;


TelStaticObject = class abstract
private
  class var fDebug: Boolean;
  class var fID: Integer;
  class var fNote: AnsiString;
  class var fName: AnsiString;
  class var fTag: Cardinal;
public
  class property Debug: Boolean read fDebug write fDebug default false;

  class property Name: AnsiString read fName write fName;      //< Name of an object
  class property ID: Integer read fID write fID default 0;     //< ID of an object
  class property Note: AnsiString read fNote write fNote;      //< Note of an object: For example to be used for description, tooltip, etc.
  class property Tag: Cardinal read fTag write fTag default 0; //< Tag of an object
end;


TelFontContainer = class(TelObject)
protected
  fText: AnsiString;

  function GetHeight: Integer; virtual;
  function GetWidth: Integer; virtual;

  function GetSize: Integer; virtual;
  procedure SetSize(Value: Integer); virtual; //< May be empty depending on the implementation

  procedure SetText(aText: AnsiString); inline;
  function GetText: AnsiString; inline;
public
  procedure LoadFromFile(const aFilename: AnsiString); virtual; abstract;

  function GetWidth_Text(Text: AnsiString): Integer; virtual;

  procedure TextOut(aPoint: TelVector3f; aText: AnsiString = ''; LineBreak: Boolean = true); virtual; abstract;
published
  property Height: Integer read GetHeight;
  property Width: Integer read GetWidth;

  property Size: Integer read GetSize write SetSize;

  property Text: AnsiString read GetText write SetText;
end;
	


implementation

uses
  ElysionContainer,
  ElysionContent;

//
// TelObject
//

constructor TelObject.Create;
begin
  inherited Create;

  fDebug := false;
  fID := 0;
  fName := StringWithoutPrefix(Self.ClassName, 'T');


  fTag := 0;


  if ltNote in TelLogger.GetInstance.Priorities then
    TelLogger.GetInstance.WriteLog('<i>' + rsObjectCreated + ':</i> ' + Self.UniqueID, 'Initialization', ltNote, true);

  fObjectCount := fObjectCount + 1;
  fContentPath := '';


  fTypeInfo := Self.ClassInfo;
  fTypeData := GetTypeData(fTypeInfo);
  fNumProperties := fTypeData^.PropCount;
end;

destructor TelObject.Destroy;
begin

  // Only log destructors if this object is not a module container
  if not Self.InheritsFrom(TelContainer) then
  begin
    if ltNote in TelLogger.GetInstance.Priorities then
      TelLogger.GetInstance.WriteLog('<i>' + rsObjectDestroyed + ':</i> ' + Self.UniqueID, 'Finalization', ltNote, true);
  end;

  inherited Destroy;
end;

procedure TelObject.DebugInfo();
var
  tmpStringList: TStringList;
begin
  tmpStringList := Self.GetDebugInfoString();

  TelLogger.GetInstance.WriteLog(tmpStringList, 'Debug Info', ltNote);

  tmpStringList.Free;
end;

function TelObject.GetUniqueID: AnsiString;
begin
  Result := Self.ClassName + IntToString(fObjectCount, true, 4);
end;

function TelObject.GetHash: AnsiString;
begin

end;

procedure TelObject.SetPropertyStr(const aName: String; aValue: AnsiString);
var
  tmpName: String;
begin
  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  case PropType(Self, tmpName) of
    tkInteger, tkInt64, tkQWord: SetOrdProp(Self, tmpName, StrToInt(aValue));
    tkFloat: SetFloatProp(Self, tmpName, StrToFloat(aValue));
    tkSString, tkLString, tkAString, tkWString, tkUString: SetStrProp(Self, tmpName, aValue);
    tkVariant: SetVariantProp(Self, tmpName, aValue);
    tkBool: SetOrdProp(Self, tmpName, Ord(LowerCase(aValue) = 'true'));
  end;
end;

procedure TelObject.SetProperty(const aName: String; aValue: Integer);
var
  tmpName: String;
begin
  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  case PropType(Self, tmpName) of
    tkInteger, tkInt64, tkQWord: SetOrdProp(Self, tmpName, aValue);
    tkFloat   : SetFloatProp(Self, tmpName, aValue * 1.0);
    tkSString, tkLString, tkAString, tkWString, tkUString: SetStrProp(Self, tmpName, IntToStr(aValue));
    tkVariant : SetVariantProp(Self, tmpName, aValue);
    tkBool    : SetOrdProp(Self, tmpName, Ord(aValue = 1));
  end;
end;

procedure TelObject.SetProperty(const aName: String; aValue: Boolean);
var
  tmpName: String;
begin
  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  case PropType(Self, tmpName) of
    tkInteger, tkInt64, tkQWord : SetOrdProp(Self, tmpName, BoolToInt(aValue));
    tkFloat   : SetFloatProp(Self, tmpName, BoolToFloat(aValue));
    tkSString, tkLString, tkAString, tkWString, tkUString: SetStrProp(Self, tmpName, BoolToString(aValue));
    tkVariant : SetVariantProp(Self, tmpName, aValue);
    tkBool    : SetOrdProp(Self, tmpName, Ord(aValue));
  end;
end;

procedure TelObject.SetProperty(const aName: String; aValue: Single);
var
  tmpName: String;
begin
  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  case PropType(Self, tmpName) of
    tkInteger, tkInt64, tkQWord : SetOrdProp(Self, tmpName, Trunc(aValue));
    tkFloat   : SetFloatProp(Self, tmpName, aValue);
    tkSString, tkLString, tkAString, tkWString, tkUString: SetStrProp(Self, tmpName, FloatToStr(aValue));
    tkVariant : SetVariantProp(Self, tmpName, aValue);
    tkBool    : SetOrdProp(Self, tmpName, Ord(aValue = 1.0));
  end;
end;

procedure TelObject.SetProperty(const aName: String; aValue: TelObject);
var
  tmpName: String;
begin
  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  case PropType(Self, tmpName) of
    tkClass : SetObjectProp(Self, tmpName, aValue);
  end;
end;

function TelObject.GetPropertyStr(const aName: String): AnsiString;
var
  tmpName: String;
begin
  Result := '';

  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  case PropType(Self, aName) of
    tkInteger, tkInt64, tkQWord: Result := IntToStr(GetOrdProp(Self, tmpName));
    tkFloat   : Result := FloatToStr(GetFloatProp(Self, tmpName));
    tkSString, tkLString, tkAString, tkWString, tkUString : Result := GetStrProp(Self, tmpName);
    tkVariant : Result := GetVariantProp(Self, tmpName);
    tkBool    : if Boolean(GetOrdProp(Self, tmpName)) then
                  Result := 'true'
		else
		  Result := 'false';
  end;
end;

procedure TelObject.SetPropertyInt(Index: Integer; aValue: AnsiString);
begin

end;

function TelObject.GetPropertyInt(Index: Integer): AnsiString;
begin

end;

function TelObject.GetPropertyType(const aName: String): AnsiString;
var
  typeInfo: PTypeInfo;
  tmpName: String;
begin
  Result := '';

  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  typeInfo := GetPropInfo(Self, tmpName)^.PropType;

  Result := GetEnumName(typeInfo, Ord(PropType(Self, tmpName)));
end;

function TelObject.PropertyExists(const aName: String; LogError: Boolean = true): Boolean;
begin
  Result := IsPublishedProp(Self, CamelCase(aName));
  if not Result then if LogError then Self.Log('Property ' + CamelCase(aName) + ' does not exist.');
end;

function TelObject.GetDebugInfoString: TStringList;
var
  tmpStringList: TStringList;
begin
  tmpStringList := TStringList.Create;

  tmpStringList.Add('Properties: ' + IntToStr(Self.NumProperties));

  Result := tmpStringList;
end;

function TelObject.GetAbsolutePath: AnsiString;
var
  Directory: AnsiString;
begin
  Directory := ExtractFilePath(ParamStr(0));

  Result := PathString([Directory, Content.Root, ContentPath]);
end;

procedure TelObject.Log(Msg: AnsiString; LogMessageType: TelLogger.TLogMessageType = ltError);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + Msg, LogMessageType);
end;

procedure TelObject.Log(Msg: AnsiString; Args: array of const;
  LogMessageType: TelLogger.TLogMessageType = ltError);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + Msg, Args, LogMessageType);
end;

procedure TelObject.Log(Msg: Integer; LogMessageType: TelLogger.TLogMessageType = ltError);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + IntToStr(Msg), LogMessageType);
end;

procedure TelObject.Log(Msg: Single; LogMessageType: TelLogger.TLogMessageType = ltError);
begin
  TelLogger.GetInstance.WriteLog(Self.UniqueID + ' : ' + FloatToStr(Msg), LogMessageType);
end;

procedure TelObject.Log(Msg: TStringList; LogMessageType: TelLogger.TLogMessageType);
begin
  TelLogger.GetInstance.WriteLog(Msg, Self.UniqueID, ltError);
end;

function TelObject.GetProperty(const aName: String): TelObject;
var
  tmpName: String;
begin
  if not PropertyExists(aName) then Exit;

  tmpName := CamelCase(aName);

  case PropType(Self, tmpName) of
    tkClass : Result := TelObject(GetObjectProp(Self, tmpName, TelObject));
  end;
end;



procedure TelFontContainer.SetText(aText: AnsiString);
begin
  if aText <> fText then
    fText := aText;
end;

function TelFontContainer.GetText: AnsiString;
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

function TelFontContainer.GetWidth_Text(Text: AnsiString): Integer;
begin
  Result := 0;
end;

end.
