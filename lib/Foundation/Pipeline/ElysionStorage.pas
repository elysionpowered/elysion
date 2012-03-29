{%region '--- Unit description ---'}
(**
  *
  * ElysionStorage
  *   Provides a simple HTML5-LocalStorage-like XML-based system for storage
  *
  *
  * @author(Johannes Stein and contributors <http://elysionpowered.org>)
  *
  *)
{%endregion}
unit ElysionStorage;

interface

{$I Elysion.inc}

uses
  ElysionObject,
  ElysionUtils,
  ElysionApplication,
  ElysionLogger,
  ElysionTypes,

  Classes,
  SysUtils;

type

  // Storage File formats (only ffPlain working right now)
  TStorageFileFormat = (ffPlain, ffXML, ffPList, ffJSON);

  { TelStorage }

  // TODO: Reflect if that should be a singleton -> Should be a static class!
  TelStorage = class(TelObject)
    private
      fFileFormat: TStorageFileFormat;

      fFilename, fDirectory: String;
      fModified, fAutoSave: Boolean;

      fKeyList: TStringList;

      procedure SetDirectory(Value: String);
    public
      constructor Create; Override;
      destructor Destroy; Override;

      //procedure DebugInfo(); Override;

      procedure SetKey(KeyName, KeyValue: String);
      //procedure SetKey(aKey: TKeyIdent); Overload;

      //function GetKey(KeyName: String): TKeyIdent;
      function GetKeyValue(KeyName: String): String;
      function GetKeyAsInt(KeyName: String): Integer;
      function GetKeyAsBool(KeyName: String): Boolean;
      function GetKeyAsFloat(KeyName: String): Single;

      function KeyExists(KeyName: String): Boolean;

      procedure Push(); //< Pushes changes to default file
      procedure Pull(); //< Pull default file from storage

      procedure Clear(); //< Clears complete storage
    published
      property Filename: String read fFilename write fFilename; //< Default filename
      property FileFormat: TStorageFileFormat read fFileFormat write fFileFormat;

      property Directory: String read fDirectory write SetDirectory;
      property Modified: Boolean read fModified;

      property AutoSave: Boolean read fAutoSave write fAutoSave; //< Automatically saves if a key has been changed
  end;

{$IFDEF AUTO_INIT}
var
  Storage: TelStorage;
{$ENDIF}

implementation

{ TelStorage }

constructor TelStorage.Create;
var
  AppName: String;
begin
  inherited Create;

  fAutoSave := true;
  fFileFormat := ffPlain;

  fKeyList := TStringList.Create;

  //if Application.Name <> '' then AppName := Application.Name else
  AppName := GetFilenameWithoutExt(ParamStr(0));

  fFilename := GetFilenameWithoutExt(ParamStr(0)) + '.storage';
  Directory := GetAppUserDirectory(AppName);
  fModified := false;

  if FileExists(fDirectory + fFilename) then Pull;
end;

destructor TelStorage.Destroy;
begin
  fKeyList.Free;

  inherited Destroy;
end;

procedure TelStorage.SetDirectory(Value: String);
begin
  if not DirectoryExists(Value) then CreateDir(Value);
  fDirectory := Value;
end;

(*procedure TelStorage.DebugInfo();
begin
  TelLogger.GetInstance.WriteLog('Default storage directory: ' + fDirectory, ltNote);
  TelLogger.GetInstance.WriteLog('Default storage filename: ' + fFilename, ltNote);
end;*)

procedure TelStorage.SetKey(KeyName, KeyValue: String);
begin
  //SetKey(makeKey(KeyName, KeyValue));
end;

(*procedure TelStorage.SetKey(aKey: TKeyIdent);
var
  tmpPos: Integer;
begin
  tmpPos := fKeyList.GetPos(aKey.Name);

  fModified := true;
  if (tmpPos = -1) then fKeyList.Add(@aKey)
  else begin
    if (fKeyList.Items[tmpPos].Value <> aKey.Value) then
      fKeyList.Items[tmpPos].Value := aKey.Value
    else
      fModified := false;
  end;

  if AutoSave and Modified then Push;
end;

function TelStorage.GetKey(KeyName: String): TKeyIdent;
var
  tmpPos: Integer;
begin
  tmpPos := fKeyList.GetPos(KeyName);

  if (tmpPos <> -1) then Result := fKeyList.Items[tmpPos]
  else Self.Log('Key not found: ' + KeyName, ltWarning);
end;*)

function TelStorage.GetKeyValue(KeyName: String): String;
begin
  //Result := GetKey(KeyName).Value;
end;

function TelStorage.GetKeyAsInt(KeyName: String): Integer;
begin
  //Result := GetKey(KeyName).ToInt();
end;

function TelStorage.GetKeyAsBool(KeyName: String): Boolean;
begin
  //Result := GetKey(KeyName).ToBool();
end;

function TelStorage.GetKeyAsFloat(KeyName: String): Single;
begin
  //Result := GetKey(KeyName).ToFloat();
end;

function TelStorage.KeyExists(KeyName: String): Boolean;
begin
  //Result := fKeyList.Exists(KeyName);
end;

procedure TelStorage.Push();
var
  tmpStringList: TStringList;
  i: Integer;
begin
  (*tmpStringList := TStringList.Create;

  case FileFormat of
    ffPlain:
    begin
      for i := 0 to fKeyList.Count - 1 do tmpStringList.Add(fKeyList.Items[i].ToString());

      tmpStringList.SaveToFile(fDirectory + fFilename);

    end;
    ffXML:
    begin
      tmpStringList.Add('<?xml version=''1.0'' encoding=''UTF-8''?>');
      tmpStringList.Add('<storage>');
      for i := 0 to fKeyList.Count - 1 do tmpStringList.Add(fKeyList.Items[i].ToXML());
      tmpStringList.Add('</storage>');

      tmpStringList.SaveToFile(fDirectory + fFilename);
    end;
    ffPList:
    begin
      tmpStringList.Add('<?xml version=''1.0'' encoding=''UTF-8''?>');
      tmpStringList.Add('<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
      tmpStringList.Add('<plist version="1.0">');
      tmpStringList.Add('<dict>');
      for i := 0 to fKeyList.Count - 1 do
      begin
        tmpStringList.Add('<key>' + fKeyList.Items[i].Name + '</key>');
        tmpStringList.Add('<string>' + fKeyList.Items[i].Value + '</string>');
      end;
      tmpStringList.Add('</dict>');
      tmpStringList.Add('</plist>');
    end;
    ffJSON:
    begin
      tmpStringList.Add('{');
      for i := 0 to fKeyList.Count - 1 do
      begin
        if (i = fKeyList.Count - 1) then tmpStringList.Add(fKeyList.Items[i].ToJSON())
        else tmpStringList.Add(fKeyList.Items[i].ToJSON() + ',');
      end;
      tmpStringList.Add('}');

      tmpStringList.SaveToFile(fDirectory + fFilename);
    end;
  end;

  TelLogger.GetInstance.WriteLog('Saved storage to: ' + fDirectory + fFilename, ltNote);

  tmpStringList.Free;*)
end;

procedure TelStorage.Pull();
var
  tmpStringList: TStringList;
  i, j: Integer;
  tmpKeyName, tmpKeyValue: String;
  //tmpKey: TKeyIdent;
begin
  (*tmpStringList := TStringList.Create;

  tmpStringList.LoadFromFile(fDirectory + fFilename);
  
  // So, here is a guessing game which format the file has been saved in
  if Pos('{', tmpStringList.Strings[0]) = 1 then FileFormat := ffJSON
    else if Pos('<?xml version=''1.0'' encoding=''UTF-8''?>', tmpStringList.Strings[0]) = 1 then
    begin
      if Pos('<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">', tmpStringList.Strings[1]) = 1 then FileFormat := ffPList
      else FileFormat := ffXML;
    end
      else FileFormat := ffPlain;

  TelLogger.GetInstance.WriteLog('Storage file loaded: ' + fDirectory + fFilename, ltNote);

  case FileFormat of
    ffPlain:
    begin
      for i := 0 to tmpStringList.Count - 1 do
      begin
        //
        if Pos('Key', tmpStringList.Strings[i]) = 0 then Exit;

        tmpKeyName := Copy(tmpStringList.Strings[i], 6, Pos(':', tmpStringList.Strings[i]) - 6 - 1);
        tmpKeyValue := Copy(tmpStringList.Strings[i], Pos(':', tmpStringList.Strings[i]) + 1, Length(tmpStringList.Strings[i]) - Pos(':', tmpStringList.Strings[i]));

        // Trim 'em

        tmpKeyName := Trim(tmpKeyName);
        tmpKeyValue := Trim(tmpKeyValue);

        fKeyList.Add(@tmpKey);
      end;
    end;
    ffXML:
    begin

    end;
    ffJSON:
    begin

    end;
  end;

  tmpStringList.Free; *)
end;

procedure TelStorage.Clear();
begin
  fKeyList.Clear();
end;

{$IFDEF AUTO_INIT}
initialization
  Storage := TelStorage.Create;

finalization
  Storage.Free;
{$ENDIF}

end.
