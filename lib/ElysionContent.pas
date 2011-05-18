unit ElysionContent;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}
{$I Elysion.inc}

interface

uses
  ElysionObject,
  //ElysionTexture,
  Classes;

type


  { TelContent }

  TelContent = class(TelObject)
  private
    fDirList: TStringList;

    function Get(Index: Integer): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure Put(Index: Integer; Value: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetRoot(): String; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    procedure SetRoot(Value: String); {$IFDEF CAN_INLINE} inline; {$ENDIF}

    function GetCount(): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function AddPath(const aName: String): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  published
    property RootDirectory: String read GetRoot write SetRoot; //deprecated
    property Root: String read GetRoot write SetRoot;

    property Items[Index: Integer]: String read Get write Put; default;
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
  fDirList.Add('');
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

procedure TelContent.Put(Index: Integer; Value: String);
begin
  fDirList.Strings[Index] := Value;
end;

function TelContent.GetRoot(): String;
begin
  Result := fDirList.Strings[0];
end;

procedure TelContent.SetRoot(Value: String);
begin
  fDirList.Strings[0] := Value;
end;

function TelContent.AddPath(const aName: String): Integer;
begin
  Result := fDirList.Add(aName);
end;

{$IFDEF AUTO_INIT}
initialization
  Content := TelContent.Create;

finalization
  Content.Destroy;
{$ENDIF}

end.

