unit ElysionEnvironment;

{$I Elysion.inc}

interface

uses
  SDL,

  SysUtils,

  ElysionTypes,
  ElysionObject;

type

  { TelEnvironment }

  TelEnvironment = class sealed(TelObject)
    private
      class var fWidth: Integer;
      class var fHeight: Integer;
      class var fColorDepth: Byte;
      class var fMobile: Boolean;
      class var fValidResolutions: TelSizeArray;

      class function GetAspectRatio(): Single; static; inline;

      class function GetBasename: AnsiString; static; inline;
      class function GetWorkingPath: AnsiString; static; inline;
    public
      class constructor Create;
      class destructor Destroy;

    public
      class property AspectRatio: Single read GetAspectRatio;

      class property Width: Integer read fWidth;
      class property Height: Integer read fHeight;
      class property ColorDepth: Byte read fColorDepth;

      class property Mobile: Boolean read fMobile;

      class property ValidResolutions: TelSizeArray read fValidResolutions;

      class property Basename: AnsiString read GetBasename;
      class property WorkingPath: AnsiString read GetWorkingPath;
  end;

implementation


class constructor TelEnvironment.Create;
var
  VideoInfo: PSDL_VideoInfo;
begin
  inherited;


  VideoInfo := SDL_GetVideoInfo;

  if VideoInfo = nil then
  begin
    Exit;
  end;

  fWidth := VideoInfo^.current_w;
  fHeight := VideoInfo^.current_h;
  fColorDepth := VideoInfo^.vfmt^.BitsPerPixel;

  fMobile := false;
  {$IFDEF FPC}
    {$IFDEF IOS}
      fMobile := true;
    {$ENDIF}
    {$IFDEF ARM}
      // In most cases mobile; TODO: add more precise conditions
      fMobile := true;
    {$ENDIF}
  {$ENDIF}
end;

class destructor TelEnvironment.Destroy;
begin
  inherited;
end;

class function TelEnvironment.GetAspectRatio(): Single;
begin
  Result := (Width / Height);
end;

class function TelEnvironment.GetBasename: AnsiString;
begin
  Result := ParamStr(0);
end;

class function TelEnvironment.GetWorkingPath: AnsiString;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

end.
