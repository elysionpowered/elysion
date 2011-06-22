unit ElysionData;

{$I Elysion.inc}

interface

uses
  Classes;

type
  // Forward declarations
  PelColor = Pointer;
  PelVector2f = Pointer;
  PelVector2i = Pointer;
  PelVector3f = Pointer;
  PelVector3i = Pointer;
  PelSize = Pointer;
  PelRect = Pointer;

  PKeyIdent = ^TKeyIdent;

  { TKeyIdent }

  {$IFDEF FPC}
  TKeyIdent = object
  {$ELSE}
  TKeyIdent = record
  {$ENDIF}
    Name: AnsiString;
    Value: WideString;

    {$IFDEF CAN_METHODS}
      procedure Clear();

      procedure SetValue(aValue: Integer); Overload;
      procedure SetValue(aValue: Single); Overload;
      procedure SetValue(aValue: Boolean); Overload;

      function ToInt(): Integer;
      function ToFloat(): Single;
      function ToBool(): Boolean;

      function ToColor(): PelColor;
      function ToVector2f(): PelVector2f;
      function ToVector2i(): PelVector2i;
      function ToVector3f(): PelVector3f;
      function ToVector3i(): PelVector3i;
      function ToSize(): PelSize;
      function ToRect(): PelRect;

      function ToString(): AnsiString; Overload;
      function ToString(): WideString; Overload;

      function ToXML(): AnsiString; Overload;
      function ToXML(): WideString; Overload;

      function ToJSON(): AnsiString; Overload;
      function ToJSON(): WideString; Overload;
    {$ENDIF}
  end;

  TKeyArray = array of TKeyIdent;

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

  function makeKey(KeyName, KeyValue: String): TKeyIdent; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeKey(KeyName: String; KeyValue: Integer): TKeyIdent; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}
  function makeKey(KeyName: String; KeyValue: Single): TKeyIdent; Overload; {$IFDEF CAN_INLINE} inline; {$ENDIF}

implementation

uses
  ElysionTypes,
  ElysionColor,
  ElysionMath;

function makeKey(KeyName, KeyValue: String): TKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := KeyValue;

  Result := tmpKey;
end;

function makeKey(KeyName: String; KeyValue: Integer): TKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := IntToStr(KeyValue);

  Result := tmpKey;
end;

function makeKey(KeyName: String; KeyValue: Single): TKeyIdent;
var
  tmpKey: TKeyIdent;
begin
  tmpKey.Name := KeyName;
  tmpKey.Value := FloatToStr(KeyValue);

  Result := tmpKey;
end;

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

function TKeyList.Add(Key: PKeyIdent): Integer;
begin
  if (GetPos(Key^.Name) = -1) then Result := fList.Add(Key);
end;

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

{$IFDEF CAN_METHODS}

procedure TKeyIdent.Clear();
begin
  Value := '';
end;

procedure TKeyIdent.SetValue(aValue: Integer);
begin
  Value := IntToStr(aValue);
end;

procedure TKeyIdent.SetValue(aValue: Single);
begin
  Value := FloatToStr(aValue);
end;

procedure TKeyIdent.SetValue(aValue: Boolean);
begin
  if aValue then Value := 'true'
  else Value := 'false';
end;

function TKeyIdent.ToInt(): Integer;
begin
  try
    Result := StrToInt(Value);
  except
    on Exception: EConvertError do Exit;
  end;
end;

function TKeyIdent.ToFloat(): Single;
begin
  try
    Result := StrToFloat(Value);
  except
    on Exception: EConvertError do Exit;
  end;
end;

function TKeyIdent.ToBool(): Boolean;
begin
  if ((Value = 'true') or (Value = '1')) then Result := true
  else Result := false;
end;

function TKeyIdent.ToColor(): PelColor;
var
  tmpString: String;
  posR, posG, posB, posA: Integer;
  tmpR, tmpG, tmpB, tmpA: Byte;
  tmpColor: TelColor;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posR := Pos('R', tmpString);
  posG := Pos('G', tmpString);
  posB := Pos('B', tmpString);
  posA := Pos('A', tmpString);

  tmpR := StrToInt(Copy(tmpString, posR + 1, posG - (posR + 1)));
  tmpG := StrToInt(Copy(tmpString, posG + 1, posB - (posG + 1)));
  tmpB := StrToInt(Copy(tmpString, posB + 1, posA - (posB + 1)));
  tmpA := StrToInt(Copy(tmpString, posA + 1, Length(tmpString)));

  tmpColor := makeCol(tmpR, tmpG, tmpB, tmpA);

  Result := @tmpColor;
end;

function TKeyIdent.ToVector2f(): PelVector2f;
var
  tmpString: String;
  posX, posY: Integer;
  tmpX, tmpY: Single;
  tmpVec: TelVector2f;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, Length(tmpString)));

  tmpVec := makeV2f(tmpX, tmpY);

  Result := @tmpVec;
end;

function TKeyIdent.ToVector2i(): PelVector2i;
var
  tmpString: String;
  posX, posY: Integer;
  tmpX, tmpY: Integer;
  tmpVec: TelVector2i;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, Length(tmpString)));

  tmpVec := makeV2i(tmpX, tmpY);

  Result := @tmpVec;
end;

function TKeyIdent.ToVector3f(): PelVector3f;
var
  tmpString: String;
  posX, posY, posZ: Integer;
  tmpX, tmpY, tmpZ: Single;
  tmpVec: TelVector3f;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);
  posZ := Pos('Z', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, posZ - (posY + 1)));
  tmpZ := StrToInt(Copy(tmpString, posZ + 1, Length(tmpString)));

  tmpVec := makeV3f(tmpX, tmpY, tmpZ);

  Result := @tmpVec;
end;

function TKeyIdent.ToVector3i(): PelVector3i;
var
  tmpString: String;
  posX, posY, posZ: Integer;
  tmpX, tmpY, tmpZ: Integer;
  tmpVec: TelVector3i;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);
  posZ := Pos('Z', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, posZ - (posY + 1)));
  tmpZ := StrToInt(Copy(tmpString, posZ + 1, Length(tmpString)));

  tmpVec := makeV3i(tmpX, tmpY, tmpZ);

  Result := @tmpVec;
end;

function TKeyIdent.ToSize(): PelSize;
var
  tmpString: String;
  posW, posH: Integer;
  tmpW, tmpH: Integer;
  tmpSize: TelSize;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posW := Pos('Width', tmpString);
  posH := Pos('Height', tmpString);

  tmpW := StrToInt(Copy(tmpString, posW + 1, posH - (posW + 1)));
  tmpH := StrToInt(Copy(tmpString, posH + 1, Length(tmpString)));

  tmpSize := makeSize(tmpW, tmpH);

  Result := @tmpSize;
end;

function TKeyIdent.ToRect(): PelRect;
var
  tmpString: String;
  posX, posY, posW, posH: Integer;
  tmpX, tmpY, tmpW, tmpH: Single;
  tmpRect: TelRect;
begin
  // Remove spaces and double colons
  tmpString := StringReplace(Value, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  tmpString := StringReplace(Value, ':', '', [rfReplaceAll, rfIgnoreCase]);

  posX := Pos('X', tmpString);
  posY := Pos('Y', tmpString);
  posW := Pos('W', tmpString);
  posH := Pos('H', tmpString);

  tmpX := StrToInt(Copy(tmpString, posX + 1, posY - (posX + 1)));
  tmpY := StrToInt(Copy(tmpString, posY + 1, posW - (posY + 1)));
  tmpW := StrToInt(Copy(tmpString, posW + 1, posH - (posW + 1)));
  tmpH := StrToInt(Copy(tmpString, posH + 1, Length(tmpString)));

  tmpRect := makeRect(tmpX, tmpY, tmpW, tmpH);

  Result := @tmpRect;
end;

function TKeyIdent.ToString(): AnsiString;
begin
  Result := Format('Key "%s": %s', [Name, Value]);
end;

function TKeyIdent.ToString(): WideString;
begin
  Result := Format('Key "%s": %s', [Name, Value]);
end;


function TKeyIdent.ToXML(): AnsiString;
begin
  Result := Format('<key name="%s">%s</key>', [Name, Value]);
end;

function TKeyIdent.ToXML(): WideString;
begin
  Result := Format('<key name="%s">%s</key>', [Name, Value]);
end;


function TKeyIdent.ToJSON(): AnsiString;
begin
  Result := Format('"%s": {"%s"}', [Name, Value]);
end;

function TKeyIdent.ToJSON(): WideString;
begin
  Result := Format('"%s": {"%s"}', [Name, Value]);
end;


{$ENDIF}


end.

end.
