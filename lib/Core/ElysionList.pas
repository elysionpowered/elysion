unit ElysionList;

{$I Elysion.inc}

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  ElysionObject,

  Classes;


const
  MaxListSize = MaxInt div 16;

type

  { TelList }

  TelList<T> = class(TelObject)
  private
    fItems: array of T;

    fCount: Integer;
    fCapacity: Integer;
  protected
    function GetItems(Index: Integer): T;
    procedure SetItems(Index: Integer; const AValue: T);
    function GetItemAdress(Index: Integer): Pointer;

    procedure SetCount(AValue: Integer);
    procedure SetCapacity(AValue: Integer);

    procedure Grow;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Clear;

    function Add(AValue: T): Integer;

    function Remove(AValue: T): Integer;
    function IndexOf(AValue: T): Integer;
    procedure Insert(Index: Integer; AValue: T);
    procedure Delete(Index: Integer);
    procedure Extract(AValue: T);
    procedure Exchange(Index1, Index2: Integer);

    function First: T;
    function Last: T;
  public
    property Items[Index: Integer]: T read GetItems write SetItems; default;
    property ItemAdress[Index: Integer]: Pointer read GetItemAdress;
    //property Find[Index: String]: T read GetItemF write SetItemF;
  published
    property Capacity: Integer read fCapacity write SetCapacity;
    property Count: Integer read fCount write SetCount;
  end;


  { TelObjectList }

  TelObjectList<T> = class(TelList<T>)
  protected
    function GetItemF(Index: String): T;
    procedure SetItemF(Index: String; const AValue: T);
  public
    property Find[Index: String]: T read GetItemF write SetItemF;
  end;


implementation




{ TelList<T> }

function TelList<T>.GetItems(Index: Integer): T;
begin
  if (Index < 0) or (Index > fCount) then
    Exit;

  Result := fItems[Index];
end;

procedure TelList<T>.SetItems(Index: Integer; const AValue: T);
begin
  if (Index < 0) or (Index > fCount) then
    Exit;

  fItems[Index] := AValue;
end;

function TelList<T>.GetItemAdress(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > fCount) then
    Exit;

  Result := @fItems[Index];
end;

procedure TelList<T>.SetCount(AValue: Integer);
var
  i: Integer;
begin
  if (AValue < 0) or (AValue > MaxListSize) then
    Exit;

  if AValue > fCapacity then
    SetCapacity(AValue);

  if AValue <= fCount then
    for i := fCount - 1 downto AValue do
      Delete(i);

  fCount := AValue;
end;

procedure TelList<T>.SetCapacity(AValue: Integer);
begin
  if (AValue < fCount) or (AValue > MaxListSize) then
    Exit;

  if AValue <> fCapacity then
  begin
    SetLength(fItems, AValue);
    fCapacity := AValue;
  end;
end;

procedure TelList<T>.Grow;
var
  Delta: Integer;
begin
  if fCapacity > 64 then
    Delta := fCapacity div 4
  else
    if fCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(fCapacity + Delta);
end;

constructor TelList<T>.Create;
begin
  inherited Create;
end;

destructor TelList<T>.Destroy;
begin
  Clear;

  inherited Destroy;
end;

procedure TelList<T>.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TelList<T>.Add(AValue: T): Integer;
begin
  Result := fCount;
  if Result = fCapacity then
   Grow;
  fItems[Result] := AValue;
  fCount := fCount + 1;
end;

function TelList<T>.Remove(AValue: T): Integer;
begin
  Result := IndexOf(AValue);

  if Result >= 0 then Delete(Result);
end;

function TelList<T>.IndexOf(AValue: T): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to fCount - 1 do
  begin
    if (fItems[i] = AValue) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TelList<T>.Insert(Index: Integer; AValue: T);
begin
  if (Index < 0) or (Index > fCount) then
    Exit;

  if fCount = fCapacity then Grow;

  if Index < fCount then
    Move(fItems[Index], fItems[Index + 1], (fCount - Index) * SizeOf(T));
  fItems[Index] := AValue;
  fCount := fCount + 1;
end;

procedure TelList<T>.Delete(Index: Integer);
var
  tmpObject: T;
begin
  if (Index < 0) or (Index > fCount) then
    Exit;

  tmpObject := fItems[Index];
  fCount := fCount - 1;
  if Index < fCount then
    Move(fItems[Index + 1], fItems[Index], (fCount - Index) * SizeOf(T));
end;

procedure TelList<T>.Extract(AValue: T);
var
  tmpIndex: Integer;
begin
  tmpIndex := IndexOf(AValue);

  if tmpIndex >= 0 then
    Delete(tmpIndex);
end;

procedure TelList<T>.Exchange(Index1, Index2: Integer);
var
  tmpObject: T;
begin
  if (Index1 < 0) or (Index1 > fCount) then
    Exit;
  if (Index2 < 0) or (Index2 > fCount) then
    Exit;

  tmpObject := fItems[Index1];
  fItems[Index1] := fItems[Index2];
  fItems[Index2] := tmpObject;
end;

function TelList<T>.First: T;
begin
  Result := GetItems(0);
end;

function TelList<T>.Last: T;
begin
  if fCount > 0 then
    Result := GetItems(fCount - 1);
end;


{ TelObjectList<T> }

function TelObjectList<T>.GetItemF(Index: String): T;
var
  tmpObject: T;
begin

  // TODO: Change this to MD5 for faster comparisons
  for tmpObject in fItems do
  begin
    if TelObject(tmpObject).Name = Index then
    begin
      Result := tmpObject;
      Exit;
    end;
  end;
end;

procedure TelObjectList<T>.SetItemF(Index: String; const AValue: T);
var
  tmpObject: T;
begin
  for tmpObject in fItems do
    if TelObject(tmpObject).Name = Index then tmpObject := AValue;
end;

end.