unit ElysionGameUtils;

{$I Elysion.inc}

interface

uses
  SysUtils,
  Classes,

  ElysionObject,
  ElysionGraphics,
  ElysionGUI;

type
  TelAchievement = record
    Active: Boolean;
    Caption: String;
    Description: String;
    Icon: TelSprite;
    Points: Integer;
  end;

  { TAchievementManager }

  TAchievementManager = class(TelObject)
    private


      fAchievementList: TList;

      function GetCount(): Integer;
      function GetCount: Integer;
      function GetUnlocked(): Integer;
      function GetUnlocked: Integer;
    public
      constructor Create;
      destructor Destroy;

      procedure Add(Achievement: TelAchievement);

      procedure Show(Achievement: TelAchievement); Overload;
      procedure Show(Index: Integer); Overload;
      procedure Show(Caption: String); Overload;
    published
      property Count: Integer read GetCount;
      property Unlocked: Integer read GetUnlocked;
  end;
  
  TelProfile = class(TelObject)
  private

  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;

  TelProfileManagement = class(TelObject)
  private

  public
    constructor Create; Override;
    destructor Destroy; Override;

    procedure Add(aProfile: TelProfile);
    procedure Delete(anIndex: Integer);
  published

  end;
  
  TelSaveGame = class(TelObject)
    private
      fNodeArray: TelNodeArray;
      fCheckForUniqueID: Boolean;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      procedure LoadFromFile(const aFilename: String);
      procedure SaveToFile(const aFilename: String);

    published
      property NodeArray: TelNodeArray read fNodeArray write fNodeArray;
      property CheckForUniqueID: Boolean read fCheckForUniqueID write fCheckForUniqueID; //< If you're not 21, you're not going to be served ;)
  end;

  TelQuest = record
    Active, Completed: Boolean;
    Caption: String;
    Description: TStringList;
  end;

  TelQuestManager = class
    private
      fList: TList;

      function GetCompleted(): Integer;
      function GetCount(): Integer;
    public
      constructor Create;
      destructor Destroy;

      procedure Add(Quest: TelQuest);
    published
      property Completed: Integer read GetCompleted;
      property Count: Integer read GetCount;
  end;

  TelHighscoreEntry = record
    Caption: String;
    Points: Integer;
  end;

  TelHighscoreList = class
    private
      fList: TList;
    public
      constructor Create;
      destructor Destroy;

      procedure Show(Entries: Integer);
  end;

{$IFDEF AUTO_INIT}
var
  Achievements: TAchievementManager;
  ProfileManager: TelProfileManagement;
{$ENDIF}

implementation

constructor TelSaveGame.Create;
begin
  inherited Create;

  NodeArray := nil;
  fCheckForUniqueID := false;
end;

destructor TelSaveGame.Destroy;
begin
  inherited Destroy;
end;

procedure TelSaveGame.LoadFromFile(const aFilename: String);
var
  i: Integer;
begin
  if NodeArray = nil then Exit;

  if CheckForUniqueID then
  begin
    for i := 0 to High(NodeArray) do
    begin

    end;
  end else
  begin
    for i := 0 to High(NodeArray) do
    begin

    end;
  end;
end;

procedure TelSaveGame.SaveToFile(const aFilename: String);
begin
  if NodeArray = nil then Exit;


end;



{ TAchievementManager }

function TAchievementManager.GetCount(): Integer;
begin

end;

function TAchievementManager.GetUnlocked(): Integer;
begin

end;

constructor TAchievementManager.Create;
begin

end;

destructor TAchievementManager.Destroy;
begin

end;

procedure TAchievementManager.Add(Achievement: TelAchievement);
begin

end;

procedure TAchievementManager.Show(Achievement: TelAchievement);
begin

end;

procedure TAchievementManager.Show(Index: Integer);
begin

end;

procedure TAchievementManager.Show(Caption: String);
begin

end;

{$IFDEF AUTO_INIT}
initialization
  Achievements := TAchievementManager.Create;
  ProfileManager := TelProfileManager.Create();

finalization
  Achievements.Destroy;
  ProfileManager.Destroy();
{$ENDIF}
