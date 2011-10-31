unit ElysionEntity;

interface

{$I Elysion.inc}

uses
  ElysionObject,
  ElysionInterfaces,
  ElysionComponents,
  ElysionNode;

type

  { TelEntity }

  TelEntity = class(TelObject, IEntity)
  private
    fNode: TelNode;
  public
    constructor Create(aNode: TelNode);

    procedure Draw;
    procedure Update(dt: Double = 0.0);

    function Add(aComponent: TelComponent): Integer;
  published
    property Node: TelNode read fNode write fNode;
  end;

  {TelEntityList = class(TelObject)

  end;}

implementation

{ TelEntity }

constructor TelEntity.Create(aNode: TelNode);
begin
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

function TelEntity.Add(aComponent: TelComponent): Integer;
begin

end;

end.
