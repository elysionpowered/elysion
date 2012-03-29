unit ElysionEntity;

interface

{$I Elysion.inc}

uses
  Classes,

  ElysionObject,
  ElysionContainer,
  ElysionList,
  ElysionInterfaces,
  ElysionGraphicsProvider,
  ElysionNode;

type

  // Forward decleration
  TelComponent = class;
  TelComponentList = TelList<TelComponent>;

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

    procedure Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true);
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

  TelEntityList = TelList<TelEntity>;

  TelEntityListHelper = class helper for TelEntityList
  public
    // Draws all drawable nodes in the list
    procedure Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true); inline;

    // Updates all nodes in the list
    procedure Update(dt: Double = 0.0); inline;
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
    property Initialized: Boolean read IsInitialized;

    property ReceivedMessages: TStringList read fReceivedMessages write fReceivedMessages;

    property Owner: TelEntity read fOwner write fOwner;
  end;

  TelComponentArray = array of TelComponent;

  TelComponentListHelper = class helper for TelComponentList
  public
    // Updates all nodes in the list
    procedure Update(dt: Double = 0.0); inline;
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

procedure TelEntity.Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true);
begin
  if (fNode <> nil) then fNode.Draw(Graphics, DrawChildren);
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



procedure TelEntityListHelper.Draw(Graphics: IGraphicsProvider; DrawChildren: Boolean = true);
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
      if (Items[i] <> nil) then Items[i].Draw(Graphics, DrawChildren);
  end;
end;

procedure TelEntityListHelper.Update(dt: Double = 0.0);
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

procedure TelComponent.Update(dt: Double = 0.0);
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


procedure TelComponentListHelper.Update(dt: Double = 0.0);
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do Items[i].Update(dt);
  end;
end;

end.
