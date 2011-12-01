unit ElysionTextureAtlas;

{$I Elysion.inc}

{$IFNDEF FPC}
  {$FATAL 'TelTextureAtlas currently works in FreePascal only. Sorry, Delphi.'}
{$ENDIF}

interface

uses
  ElysionTypes,
  ElysionObject,

  ElysionTexture,
  ElysionSprite,
  ElysionContent,

  DOM,

  SysUtils,
  Classes;

type
  // Factory

  { TelTextureAtlas }

  TelTextureAtlas = class(TelObject)
  protected
    fTexture: TelTexture;
    fTextureNameList: TStringList;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure LoadFromFile(aFilename: AnsiString);

    function CreateSprite(aTextureName: AnsiString): TelSprite;
    procedure Apply(Target: TelSprite; aTexureName: AnsiString);
  end;

implementation



{ TelTextureAtlas }

constructor TelTextureAtlas.Create;
begin
  inherited;

  fTexture := TelTexture.Create;
  fTextureNameList := TStringList.Create;
end;

destructor TelTextureAtlas.Destroy;
begin
  fTexture.Destroy;
  fTextureNameList.Destroy;

  inherited;
end;

procedure TelTextureAtlas.LoadFromFile(aFilename: AnsiString);
var
  Doc: TXMLDocument;

  Directory: AnsiString;
begin
  Directory := ExtractFilePath(ParamStr(0));

  if FileExists(Directory + Content.Root + aFilename) then
  begin
    ReadXMLFile(Doc, Directory + Content.Root + aFilename);


  end;
end;

function TelTextureAtlas.CreateSprite(aTextureName: AnsiString): TelSprite;
begin

end;

procedure TelTextureAtlas.Apply(Target: TelSprite; aTexureName: AnsiString);
begin

end;

end.
