unit ElysionEntity;

interface

{$I Elysion.inc}

uses
  Classes,

  ElysionObject,
  ElysionInterfaces,
  ElysionNode;

type

  // Forward decleration
  TelComponent = class;
  TelComponentList = class;

  { TelEntity }

  // TODO: Reflect if an entity should inherit from TelNode instead
  TelEntity = class(TelObject, IEntity)
  protected
    fNode: TelNode;
    fReceivedMessages: TStringList;
    fComponents: TelComponentList;
  public
    constructor Create; Override; Overload;
    constructor Create(aNode: TelNode); Overload;

    procedure Draw;
    procedure Update(dt: Double = 0.0);

    procedure SendMessage(Message: AnsiString); Overload;
    procedure SendMessage(Message: AnsiString; Receiver: TelComponent); Overload;
    procedure SendMessage(Message: AnsiString; ComponentName: AnsiString); Overload;

    function DidReceiveMessage(Message: AnsiString): Boolean;
  published
    property Components: TelComponentList read fComponents write fComponents;

    property ReceivedMessages: TStringList read fReceivedMessages write fReceivedMessages;

    property Node: TelNode read fNode write fNode;
  end;

  TelEntityArray = array of TelEntity;

  TelEntityList = class(TelObject)
  private
    fEntityList: TList;

    function Get(Index: Integer): TelEntity; inline;
    function GetPos(Index: String): Integer; inline;
    procedure Put(Index: Integer; const Item: TelEntity); inline;
    procedure PutS(Index: String; const Item: TelEntity); inline;
    function GetS(Index: String): TelEntity; inline;

    function GetCount: Integer; inline;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Insert(Index: Integer; Entity: TelEntity); inline;
    function Add(Entity: TelEntity): Integer; Overload; inline;
    function Add(EntityArray: TelEntityArray): Integer; Overload; inline;
    procedure Delete(Index: Integer); inline;

    // Draws all drawable nodes in the list
    procedure Draw(); inline;

    // Updates all nodes in the list
    procedure Update(dt: Double = 0.0); inline;

    property Items[Index: Integer]: TelEntity read Get write Put; default;
    property Find[Index: String]: TelEntity read GetS write PutS;
  published
    property Count: Integer read GetCount;
  end;

  { TelComponent }

  TelComponent = class abstract(TelContainer, IComponent, IReadableData, IWritableData)
  protected
    fFinalized: Boolean;
    fOwner: TelEntity;

    fReceivedMessages: TStringList;
  public
    constructor Create; Override; Overload;
    constructor Create(anOwner: TelEntity); Overload;

    destructor Destroy; Override;

    function Initialize(): Boolean; Override;
    procedure Finalize(); Override;

    procedure Update(dt: Double = 0.0); virtual;

    procedure SendMessage(Message: AnsiString); Overload;
    procedure SendMessage(Message: AnsiString; Component: TelComponent); Overload;
    procedure SendMessage(Message: AnsiString; ComponentName: AnsiString); Overload;

    function DidReceiveMessage(Message: AnsiString): Boolean;

    function LoadFromPlain(aData: TStringList): Boolean;
    function LoadFromXML(aData: TStringList): Boolean;
    function LoadFromJSON(aData: TStringList): Boolean;

    function WriteToPlain: TStringList;
    function WriteToXML: TStringList;
    function WriteToJSON: TStringList;
  published
    //property Initialized: Boolean read IsInitialized;

    property ReceivedMessages: TStringList read fReceivedMessages write fReceivedMessages;

    property Owner: TelEntity read fOwner write fOwner;
  end;

  TelComponentArray = array of TelComponent;

  TelComponentList = class(TelObject)
      private
    fEntityList: TList;

    function Get(Index: Integer): TelComponent; inline;
    function GetPos(Index: String): Integer; inline;
    procedure Put(Index: Integer; const Item: TelComponent); inline;
    procedure PutS(Index: String; const Item: TelComponent); inline;
    function GetS(Index: String): TelComponent; inline;

    function GetCount: Integer; inline;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Insert(Index: Integer; Component: TelComponent); inline;
    function Add(Component: TelComponent): Integer; Overload; inline;
    function Add(ComponentArray: TelComponentArray): Integer; Overload; inline;
    procedure Delete(Index: Integer); inline;

    // Updates all nodes in the list
    procedure Update(dt: Double = 0.0); inline;
  public
    property Items[Index: Integer]: TelComponent read Get write Put; default;
    property Find[Index: String]: TelComponent read GetS write PutS;
  published
    property Count: Integer read GetCount;
  end;

implementation

{ TelEntity }

constructor TelEntity.Create;
begin
  inherited Create();

  fNode := TelNode.Create;
end;

constructor TelEntity.Create(aNode: TelNode);
begin
  inherited Create();

  fNode := aNode;
end;

procedure TelEntity.Draw;
begin
  if (fNode <> nil) then fNode.Draw;
end;

procedure TelEntity.Update(dt: Double);
begin
  fNode.Update(dt);
end;

procedure TelEntity.SendMessage(Message: AnsiString);
var
  i: Integer;
begin
  for i := 0 to fComponents.Count do
    Components[i].ReceivedMessages.Add(Message);
end;

procedure TelEntity.SendMessage(Message: AnsiString; Receiver: TelComponent);
var
  i: Integer;
begin
  for i := 0 to fComponents.Count do
   if Components[i] = Receiver then Components[i].ReceivedMessages.Add(Message);
end;

procedure TelEntity.SendMessage(Message: AnsiString; ComponentName: AnsiString
  );
var
  i: Integer;
begin
  for i := 0 to fComponents.Count do
   if Components[i].Name = ComponentName then Components[i].ReceivedMessages.Add(Message);
end;

function TelEntity.DidReceiveMessage(Message: AnsiString): Boolean;
var
  i: Integer;
begin
  for i := 0 to ReceivedMessages.Count do
   Result := (Message = ReceivedMessages[i]);
end;


constructor TelEntityList.Create;
begin
  inherited;

  fEntityList := TList.Create;
end;

destructor TelEntityList.Destroy;
var
  i: Integer;
begin
  for i := 0 to fEntityList.Count - 1 do
  begin
    TelEntity(fEntityList[i]).Destroy;
  end;
  fEntityList.Free;

  inherited;
end;

function TelEntityList.GetCount: Integer;
begin
  Result := fEntityList.Count;
end;

function TelEntityList.Get(Index: Integer): TelEntity;
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then Result := TelEntity(fEntityList[Index]);
end;

function TelEntityList.GetPos(Index: String): Integer;
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To fEntityList.Count - 1 Do
    Begin
      if Items[a].Name <> Index then TMP := -1
      else begin
        TMP := a;
        Break;
      end;
    End;
  Finally
    Result := TMP;
  End;

end;

procedure TelEntityList.Put(Index: Integer; const Item: TelEntity);
var
  TmpNode: TelEntity;
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then
  begin
    TmpNode := Get(Index);
    TmpNode.Destroy;
    Insert(Index, Item);
  end;

end;

procedure TelEntityList.PutS(Index: String; const Item: TelEntity);
var
  TMP: Integer;
  TmpNode: TelEntity;
Begin
  if (Index <> '') then
  begin
    TmpNode := GetS(Index);
	if TmpNode <> nil then
	begin
	  TMP := GetPos(Index);
      TmpNode.Destroy;
      Insert(TMP, Item);
	end;
   end;
end;

function TelEntityList.GetS(Index: String): TelEntity;
Var TMP: Integer;
Begin
  TMP := GetPos(Index);
  if TMP >= 0 then Result := TelEntity(fEntityList[TMP])
			  else Result := nil;
end;

procedure TelEntityList.Insert(Index: Integer; Entity: TelEntity);
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then fEntityList.Insert(Index, Entity);
end;

function TelEntityList.Add(Entity: TelEntity): Integer;
begin
  Result := fEntityList.Add(Entity);
end;

function TelEntityList.Add(EntityArray: TelEntityArray): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(EntityArray) - 1 do
    Result := fEntityList.Add(EntityArray[i]);
end;

procedure TelEntityList.Delete(Index: Integer);
var
  TmpNode: TelEntity;
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then
  begin
    TmpNode := Get(Index);
    TmpNode.Destroy;
    fEntityList.Delete(Index);
  end;

end;

procedure TelEntityList.Draw();
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
      if (Items[i] <> nil) then Items[i].Draw;
  end;
end;

procedure TelEntityList.Update(dt: Double = 0.0);
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do Items[i].Update(dt);
  end;
end;

{ TelComponent }

constructor TelComponent.Create;
begin
  inherited Create;
end;

constructor TelComponent.Create(anOwner: TelEntity);
begin
  Create;

  fOwner := anOwner;
end;

destructor TelComponent.Destroy;
begin
  if not fFinalized then Finalize;

  inherited Destroy;
end;

function TelComponent.Initialize(): Boolean;
begin
  fFinalized := false;
end;

procedure TelComponent.Finalize();
begin
  fFinalized := true;
end;

procedure TelComponent.Update(dt: Double);
begin

end;

procedure TelComponent.SendMessage(Message: AnsiString);
begin
  Owner.ReceivedMessages.Add(Message);
  Owner.SendMessage(Message);
end;

procedure TelComponent.SendMessage(Message: AnsiString; Component: TelComponent);
begin
  Owner.SendMessage(Message, Component);
end;

procedure TelComponent.SendMessage(Message: AnsiString; ComponentName: AnsiString);
begin
  Owner.SendMessage(Message, ComponentName);
end;

function TelComponent.DidReceiveMessage(Message: AnsiString): Boolean;
var
  i: Integer;
begin
  for i := 0 to ReceivedMessages.Count do
   Result := (Message = ReceivedMessages[i]);
end;

function TelComponent.LoadFromPlain(aData: TStringList): Boolean;
begin

end;

function TelComponent.LoadFromXML(aData: TStringList): Boolean;
begin
  if aData = nil then
  begin
    Result := false;
    Exit;
  end;

end;

function TelComponent.LoadFromJSON(aData: TStringList): Boolean;
begin
  if aData = nil then
  begin
    Result := false;
    Exit;
  end;

end;

function TelComponent.WriteToPlain: TStringList;
begin

end;

function TelComponent.WriteToXML: TStringList;
begin

end;

function TelComponent.WriteToJSON: TStringList;
begin

end;

constructor TelComponentList.Create;
begin
  inherited;

  fEntityList := TList.Create;
end;

destructor TelComponentList.Destroy;
var
  i: Integer;
begin
  for i := 0 to fEntityList.Count - 1 do
  begin
    TelComponent(fEntityList[i]).Destroy;
  end;
  fEntityList.Free;

  inherited;
end;

function TelComponentList.GetCount: Integer;
begin
  Result := fEntityList.Count;
end;

function TelComponentList.Get(Index: Integer): TelComponent;
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then Result := TelComponent(fEntityList[Index]);
end;

function TelComponentList.GetPos(Index: String): Integer;
Var a, TMP: Integer;
Begin
  Try
    For a := 0 To fEntityList.Count - 1 Do
    Begin
      if Items[a].Name <> Index then TMP := -1
      else begin
        TMP := a;
        Break;
      end;
    End;
  Finally
    Result := TMP;
  End;

end;

procedure TelComponentList.Put(Index: Integer; const Item: TelComponent);
var
  TmpNode: TelComponent;
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then
  begin
    TmpNode := Get(Index);
    TmpNode.Destroy;
    Insert(Index, Item);
  end;

end;

procedure TelComponentList.PutS(Index: String; const Item: TelComponent);
var
  TMP: Integer;
  TmpNode: TelComponent;
Begin
  if (Index <> '') then
  begin
    TmpNode := GetS(Index);
	if TmpNode <> nil then
	begin
	  TMP := GetPos(Index);
      TmpNode.Destroy;
      Insert(TMP, Item);
	end;
   end;
end;

function TelComponentList.GetS(Index: String): TelComponent;
Var TMP: Integer;
Begin
  TMP := GetPos(Index);
  if TMP >= 0 then Result := TelComponent(fEntityList[TMP])
			  else Result := nil;
end;

procedure TelComponentList.Insert(Index: Integer; Component: TelComponent);
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then fEntityList.Insert(Index, Component);
end;

function TelComponentList.Add(Component: TelComponent): Integer;
begin
  Result := fEntityList.Add(Component);
end;

function TelComponentList.Add(ComponentArray: TelComponentArray): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(ComponentArray) - 1 do
    Result := fEntityList.Add(ComponentArray[i]);
end;

procedure TelComponentList.Delete(Index: Integer);
var
  TmpNode: TelComponent;
begin
  if ((Index >= 0) and (Index <= fEntityList.Count - 1)) then
  begin
    TmpNode := Get(Index);
    TmpNode.Destroy;
    fEntityList.Delete(Index);
  end;

end;

procedure TelComponentList.Update(dt: Double = 0.0);
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do Items[i].Update(dt);
  end;
end;

end.
