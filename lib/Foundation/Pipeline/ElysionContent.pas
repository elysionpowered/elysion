unit ElysionContent;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}
{$I Elysion.inc}

interface

uses
  ElysionObject,
  Classes,
  SysUtils;

type


  { TelContent }

  TelContent = class(TelObject)
  private
    fDirList: TStringList;

    function Get(Index: Integer): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetAudioPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetDataPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetFontPath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetImagePath: String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    function GetS(Index: String): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Put(Index: Integer; Value: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetRoot(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure PutS(Index: String; AValue: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetAudioPath(AValue: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetDataPath(AValue: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetFontPath(AValue: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetImagePath(AValue: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    procedure SetRoot(Value: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetCount(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function AddPath(const aDirectory: String; const aName: String = ''): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}

  published
    property RootDirectory: String read GetRoot write SetRoot; //deprecated
    property Root: String read GetRoot write SetRoot;

    property ImagePath: String read GetImagePath write SetImagePath;
    property DataPath: String read GetDataPath write SetDataPath;
    property AudioPath: String read GetAudioPath write SetAudioPath;
    property FontPath: String read GetFontPath write SetFontPath;

    property Items[Index: Integer]: String read Get write Put;
    property Find[Index: String]: String read GetS write PutS; default;
    property Count: Integer read GetCount;
  end;

{$IFDEF AUTO_INIT}
var
  Content: TelContent;
{$ENDIF}

implementation

constructor TelContent.Create;
begin
  inherited;

  fDirList := TStringList.Create;
  fDirList.Delimiter := ':';

  fDirList.Add('');
  fDirList.Add('Root:resources,$EMPTY');
  fDirList.Add('Root[MacOS]:Resources');
  fDirList.Add('Data:data');
  fDirList.Add('Image:images');
  fDirList.Add('Audio:sounds');
  fDirList.Add('Font:fonts');
end;

destructor TelContent.Destroy;
begin
  fDirList.Free;

  inherited;
end;

function TelContent.GetCount(): Integer;
begin
  Result := fDirList.Count;
end;

function TelContent.Get(Index: Integer): String;
begin
  Result := fDirList.Strings[Index];
end;

function TelContent.GetAudioPath: String;
begin
  Result := Self.Find['Audio'];
end;

function TelContent.GetDataPath: String;
begin
  Result := Self.Find['Data'];
end;

function TelContent.GetFontPath: String;
begin
  Result := Self.Find['Font'];
end;

function TelContent.GetImagePath: String;
begin

end;

function TelContent.GetS(Index: String): String;
begin

end;

procedure TelContent.Put(Index: Integer; Value: String);
begin
  fDirList.Strings[Index] := Value;
end;

function TelContent.GetRoot(): String;
begin
  Result := fDirList.Strings[0];
end;

procedure TelContent.PutS(Index: String; AValue: String);
begin

end;

procedure TelContent.SetAudioPath(AValue: String);
begin

end;

procedure TelContent.SetDataPath(AValue: String);
begin

end;

procedure TelContent.SetFontPath(AValue: String);
begin

end;

procedure TelContent.SetImagePath(AValue: String);
begin

end;

procedure TelContent.SetRoot(Value: String);
begin
  fDirList.Strings[0] := Value;
end;

function TelContent.AddPath(const aDirectory: String; const aName: String = ''): Integer;
var
  tmpDirectory: String;
begin
  {$IFNDEF WINDOWS}
  tmpDirectory := StringReplace(aDirectory, '\', ' /', [rfReplaceAll, rfIgnoreCase]);;
  {$ENDIF}

  if aName = '' then Result := fDirList.Add(aDirectory + ':' + aDirectory)
  else Result := fDirList.Add(aName + ':' + aDirectory);
end;

{$IFDEF AUTO_INIT}
initialization
  Content := TelContent.Create;

finalization
  Content.Destroy;
{$ENDIF}

end.
