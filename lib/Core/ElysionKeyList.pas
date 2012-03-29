unit ElysionKeyList;

interface

uses
  Classes,

  ElysionTypes;

type

  { TKeyList }

  TKeyList = class
     private
      fList: TList;

      function Get(Index: Integer): TKeyIdent; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Put(Index: Integer; const Item: TKeyIdent); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetCount: Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
    public
      constructor Create;
      destructor Destroy; Override;

      procedure Insert(Index: Integer; Key: TKeyIdent); {$IFDEF CAN_INLINE} inline; {$ENDIF}
      //function Add(Key: TKeyIdent): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function Add(Key: PKeyIdent): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      //function Add(KeyArray: TKeyArray): Integer; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      procedure Delete(Index: Integer); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      function GetPos(KeyName: String): Integer; {$IFDEF CAN_INLINE} inline; {$ENDIF}
      function Exists(KeyName: String): Boolean; {$IFDEF CAN_INLINE} inline; {$ENDIF}

      procedure Clear(); {$IFDEF CAN_INLINE} inline; {$ENDIF}

      property Items[Index: Integer]: TKeyIdent read Get write Put; default;
    published
      property Count: Integer read GetCount;
  end;

implementation

{ TKeyList }

function TKeyList.Get(Index: Integer): TKeyIdent;
begin
  if ((Index >= 0) and (Index <= fList.Count - 1)) then Result := PKeyIdent(fList.Items[Index])^;
end;

procedure TKeyList.Put(Index: Integer; const Item: TKeyIdent);
begin
  if ((Index >= 0) and (Index <= fList.Count - 1)) then fList.Items[Index] := PKeyIdent(@Item);
end;

function TKeyList.GetCount: Integer;
begin
  Result := fList.Count;
end;

constructor TKeyList.Create;
begin
  inherited Create;

  fList := TList.Create;
end;

destructor TKeyList.Destroy;
begin
  fList.Free;

  inherited Destroy;
end;

procedure TKeyList.Insert(Index: Integer; Key: TKeyIdent);
begin
  if ((Index >= 0) and (Index <= fList.Count - 1)) then
  begin
    if (GetPos(Key.Name) = -1) then fList.Insert(Index, @Key);
  end;
end;

(*function TKeyList.Add(Key: TKeyIdent): Integer;
begin
  Result := Self.Add(PKeyIdent(@Key));
end;*)

function TKeyList.Add(Key: PKeyIdent): Integer;
begin
  if (GetPos(Key^.Name) = -1) then Result := fList.Add(Key);
end;

(*function TKeyList.Add(KeyArray: TKeyArray): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(KeyArray) - 1 do Result := Self.Add(PKeyIdent(@KeyArray[i]));
end;*)

procedure TKeyList.Delete(Index: Integer);
begin
  if ((Index >= 0) and (Index <= fList.Count - 1)) then fList.Delete(Index);
end;

function TKeyList.GetPos(KeyName: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to GetCount - 1 do
  begin
    if Items[i].Name = KeyName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TKeyList.Exists(KeyName: String): Boolean;
var
  i: Integer;
begin
  for i := 0 to GetCount - 1 do
  begin
    if Items[i].Name = KeyName then Result := true
    else Result := false;
  end;
end;

procedure TKeyList.Clear();
var
  i: Integer;
begin
  for i := 0 to fList.Count - 1 do Delete(i);
end;

end.
