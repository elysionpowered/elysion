unit uConfig;

interface

uses
  SysUtils,
  uBasic;

const
  CONFIG_FILE = 'config.xml';

type
  TConfig = class
  private
    fWidth, fHeight, fBits: Integer;
  public
    constructor Create(const Filename: String);
    destructor Destroy; Override;
  published
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Bits: Integer read fBits;
  end;

var
  AppConfig: TConfig;

implementation

constructor TConfig.Create(const Filename: String);
begin
  if FileExists(GetResDataPath + Filename) then
  begin
    // TODO: Load and parse XML header for settings
  end else
  begin
    fWidth := uBasic.WIDTH;
    fHeight := uBasic.HEIGHT;
    fBits := uBasic.BITS;
  end;
end;

destructor TConfig.Destroy;
begin

end;

initialization
  AppConfig := TConfig.Create(CONFIG_FILE);

finalization
  AppConfig.Destroy;

end.