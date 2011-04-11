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
  TelContent = class(TelObject)
  private
    fRootDirectory: String;

    fSubDirList: TStringList;

    //function GetSubDirectory(aName: String): String;
    //procedure SetSubDirectory(aName: String; Value: String);
  public
    constructor Create; Override;
    destructor Destroy; Override;

    //function Load(Filename: String);
    //function LoadTexture(Filename: String): TelTexture;


    procedure AddSubDirectory(aName: String);

  published
    property RootDirectory: String read fRootDirectory write fRootDirectory;
    //property SubDirectory[Index: String]: String read GetSubDirectory write SetSubDirectory;

  end;

{$IFDEF AUTO_INIT}
var
  Content: TelContent;
{$ENDIF}

implementation

constructor TelContent.Create;
begin
  fRootDirectory := '';

  fSubDirList := TStringList.Create;
end;

destructor TelContent.Destroy;
begin
  fSubDirList.Free;
end;

(*function TelContent.GetSubDirectory(aName: String): String;
begin

end;

procedure TelContent.SetSubDirectory(aName: String; Value: String);
begin

end;  *)

procedure TelContent.AddSubDirectory(aName: String);
begin

end;

{$IFDEF AUTO_INIT}
initialization
  Content := TelContent.Create;

finalization
  Content.Destroy;
{$ENDIF}

end.

