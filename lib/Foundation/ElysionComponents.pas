unit ElysionComponents;

interface

{$I Elysion.inc}

uses
  Classes,

  ElysionObject,
  ElysionInterfaces;

type

  { TelComponent }

  TelComponent = class(TelObject, IComponent, IReadableData, IWritableData)
  private
    fFinalized: Boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Initialize(): Boolean;
    procedure Finalize();

    procedure Update(dt: Double = 0.0);

    procedure SendMessage(Message: AnsiString); Overload;
    procedure SendMessage(Message: AnsiString; Component: IComponent);

    function DidReceiveMessage(Message: AnsiString): Boolean;
    function ReceivedMessages: TStringList;


    function LoadFromXML(aData: TStringList): Boolean;
    function LoadFromJSON(aData: TStringList): Boolean;

    function WriteToXML: TStringList;
    function WriteToJSON: TStringList;
  end;

  TelComponentList = class(TelObject)

  end;

implementation

{ TelComponent }

constructor TelComponent.Create;
begin
  inherited Create;
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

end;

procedure TelComponent.SendMessage(Message: AnsiString; Component: IComponent);
begin

end;

function TelComponent.DidReceiveMessage(Message: AnsiString): Boolean;
begin

end;

function TelComponent.ReceivedMessages: TStringList;
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

function TelComponent.WriteToXML: TStringList;
begin

end;

function TelComponent.WriteToJSON: TStringList;
begin

end;

end.
