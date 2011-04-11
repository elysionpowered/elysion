{%region '--- Unit description ---'}
(**
  *
  * ElysionStorage
  *   Provides a simple LocalStorage-like XML-based system for storage
  *
  *
  * (C) Johannes Stein, 2005 - 2011
  *   For more information about the license take a look at LICENSE.TXT
  *
  * http://elysion.freeze-dev.com
  *
  *)
{%endregion}
unit ElysionStorage;

interface

uses
  ElysionObject,
  ElysionUtils,
  ElysionApplication,
  ElysionLogger,
  SysUtils;

type

  { TelStorage }

  TelStorage = class(TelObject)
    private
      fFilename, fDirectory: String;
      fModified: Boolean;
    public
      constructor Create;
      destructor Destroy;

      procedure SetKey(KeyName, KeyValue: String);

      function GetKey(KeyName): String;
      function GetKeyAsInt(KeyName): Integer;
      function GetKeyAsBool(KeyName): Boolean;
      function GetKeyAsFloat(KeyName): Single;

      function KeyExists(KeyString: String): Boolean;

      procedure Push(); //< Pushes changes to default file
      procedure Pop(); //< Pops default file from storage

      procedure Clear(); //< Clears complete storage

      function LoadFileFromStorage(Filename: String): TStringList; //< Load custom file
      procedure SaveFileToStorage(Filename: String; Content: TStringList); //< Save custom file
    published
      property Filename: String read fFilename write fFilename; //< Default filename
      property Directory: String read fDirectory write fDirectory;
      property Modified: Boolean read fModified;
  end;

implementation

{ TelStorage }

constructor TelStorage.Create;
var
  AppName: String;
begin
  if Application.Name <> '' then AppName := Application.Name
    else AppName := GetFilenameWithoutExt(ParamStr(0));

  fFilename := GetFilenameWithoutExt(ParamStr(0) + '.storage';
  fDirectory := GetAppUserDirectory(AppName);
  fModified := false;

  if isLoggerActive then TelLogger.GetInstance.WriteLog('Default storage directory: ' + fDirectory, ltNote);
  if isLoggerActive then TelLogger.GetInstance.WriteLog('Default storage filename: ' + fFilename, ltNote)
end;

destructor TelStorage.Destroy;
begin

end;

procedure TelStorage.SetKey(KeyName, KeyValue: String);
begin

end;

function TelStorage.GetKey(KeyName): String;
begin

end;

function TelStorage.GetKeyAsInt(KeyName): Integer;
begin

end;

function TelStorage.GetKeyAsBool(KeyName): Boolean;
begin

end;

function TelStorage.GetKeyAsFloat(KeyName): Single;
begin

end;

function TelStorage.KeyExists(KeyString: String): Boolean;
begin

end;

procedure TelStorage.Push();
begin

end;

procedure TelStorage.Pop();
begin

end;

procedure TelStorage.Clear();
begin

end;

function TelStorage.LoadFileFromStorage(Filename: String): TStringList;
begin

end;

procedure TelStorage.SaveFileToStorage(Filename: String; Content: TStringList);
begin

end;

end.
