(*
	LangResource Class
	to be used for localization (where you don't need VCL or LCL)
	similar to Netbeans ResourceMap from their Java Desktop Application
	
	(C) 2010 Johannes Stein
	Many thanks to Cézar Wagenheimer for the idea
	
	How to use:
		
		First, load your languages from file:
		TLanguage.getInstance.LoadFromFile('mylanguages.index');
		
		(The contents are just some some strings in a text file, no seperator,
		 every new language name in a new line)
		 
	    or:
	    
	    Add your languages in your source code:
	    TLanguage.getInstance.Add('English');
	    TLanguage.getInstance.Add('German');
	    etc.
	    
	    Load your resourcetable:
	    
	    var
	      MyResourceTable: TResourceTable;
	      
	    ...
	      
	    MyResourceTable := TResourceTable.Create('myresource.properties');
	    
	    or
	    
	    MyResourceTable := TResourceTable.Create;
	    
	    Add the resource in your source code:
	    MyResourceTable.Add('Text', 'This is a sample text');
	    MyResourceTable.Add('Text2', 'Another sample text');
	    
	    
	    Combine your resourcetable with the required language:
	    
	    
	    TLanguageResource.getInstance.Add(TLanguage.getInstance.CurrentLanguage, MyResourceTable);
	    
	    
	    Accessing a resource:
	    
	    Either use the GNU Gettext - like function _(String): String
	    or
	    
	
*)
unit ResourceMap;

interface

uses
  Classes,
  SysUtils;

type
  (*
    TLanguage
    
  *)
  TLanguage = class
  private
    // List of languages is stored internally as a StringList
    fLanguageList: TStringList;
    fCurLang: String;
    
    function getCurLang: String;
    procedure setCurLang(AName: String);
    function getCount;
  protected  
    {$WARNINGS OFF}
    constructor Create;
    {$WARNINGS ON}
  public
    class function getInstance: TLanguage;
    destructor Destroy;
    
    // Load from *.index (or *.txt) files
    procedure LoadFromFile(Filename: String);
    
    // Save to *.index (or *.txt) file
    procedure SaveToFile(Filename: String);
    
    // Add a language string manually
    procedure Add(AName: String);
    
    // Delete a language string manually
    procedure Del(AName: String);
    
    // Clear whole language list
    procedure Clear;
    
    // Returns the complete StringList
    function getContent: TStringList;
  published
    property Count: Integer read getCount;
    property CurrentLanguage: String read getCurLang write setCurLang;
  end;
  
  TResourceTable = class
  private
    fIndexTable, fItemTable: TStringList;
    
    function isWString(AName: String): Boolean;
    function isString(AName: String): Boolean;
    function isInteger(AName: String): Boolean;
    function isFloat(AName: String): Boolean;
  public
    constructor Create;
    constructor Create(Filename: String);
    destructor Destroy;
    
    procedure LoadFromFile(Filename: String);
    procedure SaveToFile(Filename: String);
    
    procedure Add(AnEntry: WideString);
    procedure Add(AName: String; AText: WideString);
    procedure Del(AnEntry: WideString);
    
    function getWString(AName: String): WideString;
    function getString(AName: String): AnsiString;
    function getInteger(AName: String): Integer;
    function getFloat(AName: String): Single;
  end;
  
  //
  TLangResource = class
  private
    fResourceTableList: TList;
 
  protected
    {$WARNINGS OFF}
    constructor Create;
    {$WARNINGS ON}
  public
    class function getInstance: TLanguage;
    destructor Destroy;
    
   
    
  end;
  
  function _(AName: String): WideString; Overload;
  function _(AName: String): String; Overload;

implementation

var
  Language: TLanguage;
  
  
function _(AName: String): Widestring;
begin
  
end;

function _(AName: String): String;
begin

end;


constructor TLanguage.Create;
begin
  fLanguageList := TStringList.Create;
end;

class function TLanguage.getInstance: TLanguage;
begin
  
end;

destructor TLanguage.Destroy;
begin
  fLanguageList.Destroy;
end;

function TLanguage.getCurLang: String;
begin
  if fCurLang <> '' then Result := fCurLang;
end;

procedure TLanguage.setCurLang(AName: String);
begin
  if fCurLang <> AName then
  begin
    if fLanguageList.IndexOf(AName) <> -1 then fCurLang := AName;
  end;
end;

function TLanguage.getCount: Integer;
begin
  Result := fLanguageList.Count;
end;
    
procedure TLanguage.LoadFromFile(Filename: String);
begin
  if ExtractFileExt('.txt') or ExtractFileExt('.index') then
    fLanguageList.LoadFromFile(Filename)
  else
    raise Exception.Create('Language file is neither a text or an index file.');
end;

procedure TLanguage.SaveToFile(Filename: String);
begin
  fLanguageList.SaveToFile(Filename);
end;

procedure TLanguage.Add(AName: String);
begin
  fLanguageList.Add(AName);
end;

procedure TLanguage.Del(AName: String);
begin
  fLanguageList.Del(fLanguageList.IndexOf(AName));
end;

procedure TLanguage.Clear;
begin
  fLanguageList.Clear;
end;

end.