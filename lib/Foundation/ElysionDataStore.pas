unit ElysionDataStore;

interface

{$I Elysion.inc}

uses
  ElysionTypes,
  ElysionObject;

type
  TelDataItem = class(TelObject)
  public
    constructor Create; Override;
    destructor Destroy; Override;
  public
    function AsRect: TelRect;
    function AsVector2i: TelVector2i;
    function AsVector2f: TelVector2f;
    function AsVector3i: TelVector3i;
    function AsVector3f: TelVector3f;
    function AsColor: TelColor;
  end;

  TelDataStore = class(TelObject)
  protected
    fStringList: TStringList;
  public
    constructor Create; Override;
    destructor Destroy; Override;
  public

  end;

implemenation

end.
