unit ElysionList;

{$I Elysion.inc}
{$mode delphi}

interface

uses
  ElysionObject,

  Contnrs;

type

  { TelList }

  TelList<T> = class(TelObject)
  private
    function GetCount: Integer;
    function GetItemF(Index: String): T; inline;
    function GetItems(Index: Integer): T; inline;
    procedure SetItemF(Index: String; AValue: T); inline;
    procedure SetItems(Index: Integer; AValue: T); inline;
  protected
    fList: TObjectList;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Add(AValue: T): Integer; inline;

    function Remove(AValue: T): Integer; inline;
    function IndexOf(AValue: T): Integer; inline;
    procedure Insert(Index: Integer; AValue: T); inline;
    procedure Delete(Index: Integer); inline;

    function First: T; inline;
    function Last: T; inline;
  public
    property Items[Index: Integer]: T read GetItems write SetItems; default;
    property Find[Index: String]: T read GetItemF write SetItemF;
  published
    property Count: Integer read GetCount;
  end;


implementation

{ TelList<T> }

function TelList<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TelList<T>.GetItemF(Index: String): T;
var
  tmpObject: T;
begin

  // TODO: Change this to MD 5 for faster comparisons
  for tmpObject in fList do
  begin
    if TelObject(tmpObject).Name = Index then
    begin
      Result := tmpObject;
      Exit;
    end;
  end;
end;

function TelList<T>.GetItems(Index: Integer): T;
begin
  if ((Index >= 0) and (Index < GetCount)) then Result := T(fList.Items[Index]);
end;

procedure TelList<T>.SetItemF(Index: String; AValue: T);
var
  tmpObject: T;
begin
  for tmpObject in fList do
    if TelObject(tmpObject).Name = Index then tmpObject := AValue;
end;

procedure TelList<T>.SetItems(Index: Integer; AValue: T);
begin
  fList.Items[Index] := AValue;
end;

constructor TelList<T>.Create;
begin
  inherited Create;

  fList := TObjectList.Create;
end;

destructor TelList<T>.Destroy;
begin
  fList.Destroy;

  inherited Destroy;
end;

function TelList<T>.Add(AValue: T): Integer;
begin
  Result := fList.Add(AValue);
end;

function TelList<T>.Remove(AValue: T): Integer;
begin
  Result := fList.Remove(AValue);
end;

function TelList<T>.IndexOf(AValue: T): Integer;
begin
  Result := fList.IndexOf(AValue);
end;

procedure TelList<T>.Insert(Index: Integer; AValue: T);
begin
  if ((Index >= 0) and (Index < GetCount)) then fList.Insert(Index, AValue);
end;

procedure TelList<T>.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

function TelList<T>.First: T;
begin
  Result := T(fList.First);
end;

function TelList<T>.Last: T;
begin
  Result := T(fList.Last);
end;

end.
