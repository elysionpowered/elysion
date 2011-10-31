{
  uVersionHandler: Load current version from a specific XML file
  
  TODO: Re-evaluate this unit, it depends on FreePascal XML
}

unit uVersionHandler;

interface

uses SysUtils,
     XMLRead,
     DOM;

type

// Version control handler
// Singleton pattern
TVersionHandler = class
  private
    fVMajor, fVMinor, fVBuild, fVRevision: String;
    
    function GetVersion: String;
  protected
    {$WARNINGS OFF} 
    constructor Create; 
    {$WARNINGS ON}
  public
    class function GetInstance: TVersionHandler;
  
    procedure LoadFromFile(Filename: String);
  published
    property Version: String read GetVersion;
    property VersionMajor: String read fVMajor;
    property VersionMinor: String read fVMinor;
    property VersionBuild: String read fVBuild;
    property VersionRevision: String read fVRevision;
end;

var VersionHandler: TVersionHandler;

implementation

constructor TVersionHandler.Create;
begin
  // Assume version 1.0.0 if no xml has been loaded
  fVMajor := 1;
  fVMinor := 0;
  fVBuild := 0;
  fVRevision := 0
end;

class function TVersionHandler.GetInstance: TVersionHandler;
begin
  if (VersionHandler = nil) then VersionHandler := TVersionHandler.Create;
  Result := VersionHandler;
end;

function TVersionHandler.GetVersion: String;
begin
  Result := fVMajor + '.' + fVMinor + '.' + fVBuild + '.' + fVRevision;
end;

procedure TVersionHandler.LoadFromFile(Filename: String);
var Document: TXMLDocument;
    PassNode: TDOMNode;
    Directory: String;
begin
  
  Directory := ExtractFilePath(ParamStr(0));
    
    if FileExists(Directory + Filename) then
    begin
      ReadXMLFile(Document, Directory + Filename);      
            
      PassNode := Document.DocumentElement.FindNode('Major');
      fVMajor := PassNode.FirstChild.NodeValue;
      
      PassNode := Document.DocumentElement.FindNode('Minor');
      fVMinor := PassNode.FirstChild.NodeValue;
      
      PassNode := Document.DocumentElement.FindNode('Build');
      fVBuild := PassNode.FirstChild.NodeValue;
      
      PassNode := Document.DocumentElement.FindNode('Revision');
      fVRevision := PassNode.FirstChild.NodeValue;
      
      Document.Free;
    end;

end;


end.
