{%region '--- Unit description ---'}
(**
  *
  *
  *
  *
  *
  *
  *
  *)
{%endregion}

unit ElysionLogger;

interface

{$I Elysion.inc}

uses
  ElysionUtils,
  ElysionConst,
  ElysionTypes,

  Classes,
  SysUtils;

type

{%region 'Logger priority types'}
  // Logger message type

  {%endregion}

{%region 'Logger class (Prototype)'}
{
  Class: TelLogger @br
  Group: Optional @br
  Description: Very simple logger class @br
  Singleton pattern -> See http://en.wikipedia.org/wiki/Singleton_pattern for more information 
}
TelLogger = class sealed
// Type & Variable definitions
public type
  TLogMessageType =
    (ltError,   //< Displays message as an error
     ltWarning, //< Displays message as a warning
     ltNote);   //< Displays message as a note

  TLogMessagePriorities = set of TLogMessageType;
strict private
  class var Instance: TelLogger;
private
  fFilename: AnsiString;
  fText: TStringList;
  fPriorities: TLogMessagePriorities;
  fShowLogo: Boolean;

  function ReplaceSpecialChars(InputString: AnsiString): AnsiString;
protected
    {$WARNINGS OFF}
    {
      constructor TelLogger.Create @br
	  Type: Constructor @br
	  Parameters: None @br
      Description: Creates the logger class
    }
    constructor Create;
    {$WARNINGS ON}
    
  public
    {	
	  class function TelLogger.GetInstance @br
      Type: Function @br
	  Parameters: None @br
	  Description: Returns the instance of the logger class
	  
    }
    class function GetInstance: TelLogger;
	
    {
      destructor TelLogger.Destroy @br
      Type: Destructor @br
      Parameters: None @br
  Description: Destroy the logger class
    }
    destructor Destroy; Override;

    {
      procedure TelLogger.WriteLog @br
      Parameters: @br
	    @param Msg: AnsiString - The message to be written in the log file @br
	    @param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
      Description: Writes a user-defined text in the log file.
    }
    procedure WriteLog(const Msg: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;
    
    {	
	  procedure TelLogger.WriteLog @br
	  Parameters: @br
		@param Msg: AnsiString - The message to be written in the log file @br
		@param Category: Specify a category @br
		@param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
	  Description: Writes a user-defined text in the log file.
	}
    procedure WriteLog(const Msg: AnsiString; Category: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;
	
	{	
	  procedure TelLogger.WriteLog @br
	  Parameters: @br
		@param Msg: AnsiString - The message to be written in the log file @br
		@param Args: Array of constants @br
		@param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
	  Description: Writes a user-defined text in the log file.
	}
    procedure WriteLog(const Msg: AnsiString; Args: array of const; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;
    
    {	
	  procedure TelLogger.WriteLog @br
	  Parameters: @br
		@param Msg: AnsiString - The message to be written in the log file @br
		@param Args: Array of constants @br
		@param Category: Specify a category @br
		@param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
	  Description: Writes a user-defined text in the log file.
	}
    procedure WriteLog(const Msg: AnsiString; Args: array of const; Category: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;

    procedure WriteLog(Msg: TStringList; Category: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;

    procedure Dump();
  published
    property Filename: AnsiString read fFilename;
    property Priorities: TLogMessagePriorities read fPriorities write fPriorities;
    property ShowLogo: Boolean read fShowLogo write fShowLogo;
end;
{%endregion}

implementation

uses
  ElysionApplication;


procedure TelLogger.Dump();
var
  i: Integer;
  tmpStringList: TStringList;
begin
  tmpStringList := TStringList.Create;

  tmpStringList.Add('<html>');
  tmpStringList.Add('<head>');
  tmpStringList.Add('  <title>Elysion Library Log</title>');
  tmpStringList.Add('<style>');
  tmpStringList.Add('body { background:#fafafa; font-size:90%; color:#000; font-family:DejaVu Sans, Verdana, Arial; }');
  tmpStringList.Add('#log { border: 2px solid #dddddd; border-spacing: 4px; width: 99%; }');
  tmpStringList.Add('#log th { background: #c0c0c0; text-shadow: 0px 0px 4px #fff; }');
  tmpStringList.Add('#log tr:nth-child(even) { background: #e8e8e8; }');
  tmpStringList.Add('#log tr:nth-child(odd) { background: #d8d8d8; }');
  tmpStringList.Add('#logo { float:left; padding: 8px; }');
  tmpStringList.Add('header { padding-top: 20px; text-shadow: 1px 1px 2px #ccc; }');
  tmpStringList.Add('#title { font-size: 120%; padding-buttom: 8px; text-shadow: 1px 1px 4px #000; }');
  tmpStringList.Add('</style>');
  tmpStringList.Add('<!--[if lt IE 9]>');
  tmpStringList.Add('<script src = "http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>');
  tmpStringList.Add('<![endif]-->');
  tmpStringList.Add('</head>');
  tmpStringList.Add('<body>');
  tmpStringList.Add('<header>');

  if ShowLogo then
  begin
    tmpStringList.Add('<img id="logo" src="data:image/png;base64,');
    tmpStringList.Add('iVBORw0KGgoAAAANSUhEUgAAAEAAAAAqCAYAAAADBl3iAAAAAXNSR0IArs4c6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB9sEHgAAE3XVgf4AABLwSURBVGje1Zp5sGRXfd8/55y739vd7/XbZ+a9N/sMmtE6YwmxacFIxMbYwlsSQxEnxrGzVmyDBAIkEVyEsl2BGBI7dtkC7HJS');
    tmpStringList.Add('lXLKRWEQEhhKAQMSEoOW0SyaefP2eWuvd7/n5I9+A0nZiYURCE5VV3dXnb739/v+vvf7W/oIvgfLcRzyPP/W95+6665rrr/hupfv2rXrZiHVzUJwYHX18m/ee8877+clXuLFutDJHznJ4489ztt++ZduuuHEiRv9IHxFWeQvn5ic2GvbLlIKjMFoXYkkzXAdh7W1NdZWlx64+x3vuv+HGoBPfuqT/MSP/0TwkY9+5PHZ2emX');
    tmpStringList.Add('1Wp1kiwztu2IqqoQAoq8QNkWZVnhWIJKgzFQVZo07vLcmTMPvOfe997/QwnArbfdcvyXf+VfPBb4rptluXA9D9d1KYuCsFajKHKE0TiOS5ykAHiuRZqVYDRCCJCK1vYW5889f/977r33gR94AKIootfr8ea3/MIv3vH61/9Ro9Ewju0IbSDLMqIoIEsztDG4ro3n+eiqBCFJsxRLSpTlUJY5Qkhsy6Lb6+H7AaurqywtLtx3');
    tmpStringList.Add('zzvued8PJABHjh7hzHNneP8HfvPBa44ff2tZlcbxAmHKgjhJsBwfo0ss26LIMkZGmqyvrTE+MYGUikprLNuhKguyLMO2HdIsw7UVVVVhUBhjaLc2WZhfuO/tv/GO9/3AAPDBD/4H7r77Hu8//qcPPz49PXPMthX9foxtW9iOgy4zwrBGlsY4fg1LKTqdDo5jUZUaL4ygKvD8gLKqEGgEBoOiLEssJdFGk2U5juOgdUWn3eX0');
    tmpStringList.Add('6efue++73/2+lxyAf/wL/+iqV73qVY9PTk16QlqiLArKqiIIQqqqBANFWRAGPu12Bz/wicIIqRTdbhfXdbFtmzju43kenuchhCDPc7QegCGVhRCSLEsI/IA4SbBth63NLZ5//tx733n3O//99xUApQbU/NV/+a/e+vKbb3rQUsJogwjDEEsJiqJAG4Ft2UglKcsS17GpKo3ruoCk1+tiEIRhQJqlmKokCCL6/S71eo0wqpEX');
    tmpStringList.Add('JUpIEII8z0AIyqLEti2KPANpobWmvb3F8tLCe3/t3739uwZCvZBNxhje+ov/5IO3v/a23/Jc14RRJL5trEIqidYGU+VoFK7jYLQhL3LKsqLSGteWWEqgpCAMawipMLokiiI6nd4AZG2wLIuiKBACJAbLcciLCsuysBSURUVUqzE9PX3bP/jxH7tfSbn8zNPPPPH3BcB6IZuuufY6PvbHD949OTl1evee3X88MjyMVIpSa8os');
    tmpStringList.Add('Q+sSJS2UUmRJH9tqYCmFbTsYIwhDn06rjeu5xGlGt9fDsl083wMErheQpClhELK5sU5jaAhjBEJKXMtCSUESJ8wvLbG6epmV5WVz4eKFzWeffubtFy5cfPDQ4cOcO3v2+6MBUS0yJ0/cwF1vuot9+/cNIo9EVxVSQllpbNtBCkNVVWgESa/PnpkZ2q1tbNsi7sfU6hFCOWRJSpLE+GGEMRVSSMqioF6PePTRL7G0tMTqygrt');
    tmpStringList.Add('dhvHdUmTBMdxWFxcvOrMc2dOf99FsDkyYpSUpFnGtdddy113/STTs/uQAnzfJ0kyjCmp1Rp02y3q9RpaV3T7CUEYIRBARafdoT40jESTpzGWG4AxSCXpdnv8ycc+Tj+JwQijlBS9fp+1y2tYlmLfvn0sL6+cOPWNbzzx3QIg/36wCXzP4/y5c7zvgffzux/6EFtb26RpRprEGAapzwjBdruLkIqyKEiTGCEMSlpEjSF0VaEN');
    tmpStringList.Add('WE4IQFGkXJpf5Pf/y+/R7fUQQpIkibi8uvqhx776tZ/N8xxtBiW0MeZFyQLWd/yLQUcDQlBVFZ7rcubMWd7+a7/ODTdcz8/93JuYHj5AGse4nk+WZ2Rpgud7hEFIlhdkWR+MwfECdFmhqxKtK5599jSf/cxDO1RPyYucWq2GCcMnXc/1BKCkAMyLVgd8xwxQSiGlRAiBkgqDQezQ/+zZc7zrXffxO7/9O6yurlBWGqUsNAqj');
    tmpStringList.Add('YX19nbIsqUUhjmMjTYmpclw/5NN/+RkeeeRzFGXF+vo6tuNgWRaO46IsG4EY9AzGIIRACvHSMMAYjUGglEIbM6CiAcPgs+/7PHf6NO95zzc4efIkb/rZn2HX5BSGCqFsPN+n1W4TBgFZlhHHff78wU+wsrLM8tIKUinqtRpSCrQ2pEmfXrcHGCqtMYDWGmM0ADefvBplh8JUuTGm4suPneLW227nC3/1+e8VACCFGFRtQmBZ');
    tmpStringList.Add('FkYPomIAbTQw0IjVlWX+8Pd/j5mZWe688w4mpibpdTo0m8O0Wy3W19f4xMf/lHa7TbvVIssyarUa2hh0pbFtC6ksXNvBGPh20AUIKV975xsiy6/Xle36pirLIut3br19vKeLXvE9Y4AQg/wsACnlTjTMoChC7BhqqLQBIfE8n4WFBR588EFmpmd4zS2vxnEsvv7Ekzz80GcJguDb+WjnZUmJkIIyK7Fth1J3XpWk5ZeMufL4');
    tmpStringList.Add('G9ygPtrYffRGrzF6RDneiKmqPIu78/HWytPp1vLcLbfc0v/iF79oXlQATp6YevWFizlSGjRg9CBvI0DvOK6UBAyCgUhiBl7ZtsvcpUt89t6HOXDwIMZowjAc7BVX8rEYpEk5oLnWBmOMaTQm33bipHjb6nLfCCmEMYb6+PRVQ9OH9/i15rXSspsYU2RJ/5LthVMI8bl4ozoPxC9KKRy60eTs0aGPiKnhD6eruZFKCiUFUkqM');
    tmpStringList.Add('MQixI4pKYbQZsACB49o49qAnsCwLjKHT7SCEIAh8lFLkeY5SFo7t0O/3cBwH23bwfZ+qqgjDmlDKIk0NvX5P2JbFcLNJhu064dBtjh8es2x7Ks+SXVm/u9v2w9JovVHEndU9U2P9S5fmXhgDrtAZwHP829DG9Uctt6yqu4anwrd0CyXzVhettRBSYkm5k4wGpY02hqrUyJ00ZYxBCgVCUlUZAJXWCMQANATaMGAJAsd1sdTA');
    tmpStringList.Add('nEF1OaB7nsUkaU6epQP7hMQYbbTh1r1HrjEbG+ui6vXww5oYGZsYWrpw+mrHj74iXT8S0lJA+XcCEEU+vV7CxJh3vXG9d9Qmm2+onCLqrHRwbRfjWtRCm+lgllPmKWwGzY+QEtuSO84OqK53BFEIgVQSKQWO7eyo9gAytcN5XZVIIRBy4LSU8lvFqdjZEycZUgr6cYwUg3tVVSUm9x/lsYf/h0gKzeTeo8TtTeqeQPijfdex');
    tmpStringList.Add('qxdaKlgAE2POkYM3THx0Yy15rd30TFGUIu3ENIaHyeOcSbWLqfHddLqtQcSFgJ1crI1Ba4Ol1E5kDSAH0d4BQmtNVVVkWTZwfKfDvMICNFjK2om6wVISJeSOoA4YcyXtSimRykUIyZ7D1/LU//oUp858ldlDx5FHbzKmjK93qtXquScfvTh34flzfycAP/Wmqz5w6szmPZ3CNmnZpb/UFvXJCFk6WMrhdbfcQT+OaW23yLNB');
    tmpStringList.Add('m2oAtKE0FVIKhBikPyEElrIxRqPkwGAYZI00zXac5ltMKKsKYwz2/wmeGIBjACkHQmoMg6LL6EGbrBRJ6zIom/3X3UJZZDSbI3S7HXF5bY5OXZ44etPU/9x3yJ/3ZPXxT3/69PuBrF6v0el0/28RvP7E1AfWE707LUrhOBbCGPyhiDRO6W21OffseWq1GuPjY7iuy8hok7KsyNIUZUm0HtQFV0Sw0noQKSFxbAfLUlS6GrxX');
    tmpStringList.Add('Fb1eH9d1vzUNqsoSbTRKKdqdDrZtY9kOge9TlhVKSYQY/NlSqzdwbAffdxkfqSOLGPIeedLBlAU2JZcuPUV7e0lcvryOaTr1xZXWLVddNfbuV7/iwHWnTs2f09qs/PTP38jpZ5YGADz9zct/cPWhoUklq5NlEJqqKIUuNP1Wl3CoRlImLFxcZH7uEqMjQ+zatYtGo85Ic5g8HwxBxZVH4YoW7Dy/nj8Yj+dZjm1LMBDHGZ7n');
    tmpStringList.Add('4/keUkqqqkQIiWVZbG9toSwLx3EIwoC8KMBolJREUcTw0BB79kzh+wHDQ8OUaZfe9hplllKlXVprC5w9/zRuVFJVJVVViSAaoptkzG0mRw8cGP/nL/+R2Z9J+vHa+XPrpx9/5t2IyckpVldXuOnmmZ9Xnvpvy1llTClEv91lbPcovVZCliYkmxk6NkRhyLHjx9m3by8ryyuAYHlpifX1NYQciONA2ATDw8MEQUClDVJaaOWR');
    tmpStringList.Add('4xKEIb7rUGY98qSHKFMwmotzF/E9nyAMGR0dJc8zXNejVqvheR6u6xCEIXme0dpu0+l2sJXF8uoy3apDJ+6CNvhRg4kjDTqrMY3dEfFGiq40WIYiyc2k74jrjk2sP3d65Y2q1+sBsLTYfmZ2tvHI5HDwzzqlMGgEjqHsV4M6vpKMH27S2uqwvrXGmafPMjY+xszsXsIopNEYIs8zkjhBKoGuDEEY4NgOlYHa+F7CkT2M7XsZ');
    tmpStringList.Add('zYlZ6mO7sLwI26+TZxl5FmN0RVGWBIFPo1EnCEIaQ0M0R4aJwhDLtlhcXKC13aIoS3q9Ds+vX8TUKipKgkaI8gRhoFCui+N4bMxvEI372I5H2kqwlBGTB8fYP+kFSuk3/42Wav+Bycljx8b/6nJRHL14fs24kSMs20NYkLQSdAFlXuDWbUQKxJKjR4+yb98s7U6PfrfL8soKW5ubDA0NE9XrWOEoI7PHcKMhopGpna7SIou7');
    tmpStringList.Add('lFnM9uI5tpfOIYoefhgihWC4OUwtipCWRdLvs7m5SZplCAFZmrHcWcYelfhRRNrNCCzN+nafMKhTVinby21mr58l6ZQ4IVAaM9ZwRVRo9kz5dLrFo5fm2r/yNwA4dnw3zzy9xO2vm/5Rif8bfS3vXKtSY1JE0k8RUtLcVUfHmsvzm9ihomhX+PhcddVRDhw4xPraZeIkZnu7A9KiPnWYod0HcYOQxvgMXlQnT2I2VxewLJve');
    tmpStringList.Add('xhL91bPYVQ9LQT2KCOrDJHGf1ZUVkiRBCkmlK1a6y2y2NwmjECM1UihqkxGthS7hcI086yPiyhw4PCp276qRd2MjyvLU6OjI6TguvzG/0H7iob889XlAHzw0+v8eiY2NR6yv9ZgcG2ped3L0CwuJOm6FQvQ2K+xhRXexBZYk3S5oTrrkRmJ6hnKr4tg1x9mzew+tdmsw0W3uxYua7L/6Ri6vrRH3Onh+wPju/cw/9wSebZGu');
    tmpStringList.Add('P49VdnCsgaNra+v0ej0Uksuty3SLDsI3bC91GJ1u0t2KGZto0O8n1CaGcUNBtdoyxw+OC0uJb2aF+c///U+f+Atg9buaCc7sHWZ+bps73njczG1WeDVJe7OD0RohHNy6TRVrjDD0Wx2EFpBIHByOX3Oc2X2HiE2ACSc487VHSEvN+Mxh2hvLhJ7NwRtfD/EWKl6FvMP8xQv04xhjDNv9Fl3VQiDpbXeIwhrOkIXRDkWaUGQl');
    tmpStringList.Add('E7OTqG7HHJmuC2mrLzz59aV/880nF58y5gGEuO/FG4q+7o6Z98+1q3tzLEwOvivQtk0eF0jHYIwgT1McLNwxl86FBNPTuL7PiVfeztSB4yQi5NmvPkJ3Y5GJ6UMcOPmj+MqQbS8xf+YU/dYqaRqzuLqAM+5T5BkKSdD0yeISP3KJNxPsmoRKsGfIMYdmGmJsfOTzDz90+l9/7SsXnt1/cIwL59e/N1PhO15/dLGjy93tXNLv');
    tmpStringList.Add('ZBgpCOoOi08tcfTWQ2xcbJPHCY7r4YSS9mqXcquiMT7NroPHGdu1l1JDmSfUh5pYlkuZdthcOM/GynnifJuN9gbRSIDluJRJRn04ICkqTAb+qEd7YZODsw0OTQ8TJ+UnH/3C/K8vzG+cm907xqW59e94JPaCAZjdO8yluW1ec9v+106Mea/UyMPtVn6yFBzppIK85pj24paQrqK30UcIg+sEaJ0htYspPUYm91NrjKDICMI6');
    tmpStringList.Add('IFmeO0uvuExra4PmbJOslaJscIcCWktdPN9laHaYdG2bq/dNsHemxvJS5xMPf+b5uzc2tlYmp4ZZXdl+SQ9IOG9448FfSlLxb7sFhzdbCdZIgBSKhVPzTL1sis25LbyGi0mgt5UQ+DWiZkSpNHnewZIW3pBF2iuxXQsqCBo14qRLs+Yy6cP07iGynN/9o//6lXcBPdcNyLL4uzb+RTsjBHDs6l0zR46M/MNuJ/vJAvsVa602');
    tmpStringList.Add('/syoWTu3KaQNeb9iZM8ovU4HTUlUi0BY9La7NOshtioJPIehkQaWzvGFxhi92evlH/6LP3/mA0A50pxkc2v1B++Q1N+23vyW235sY+PSP+1U7k+vdFOTJ7EIh+vUmhHdSy1U02Vf02F2JCQIm5/r9tpfTtLuhXptrL211dn+s088+TQUGz8Up8T+tjUyWmdzo4MfurXpmfqnhl429erWRkKSZIwErjl5ZFL04/jjf/YnX//V');
    tmpStringList.Add('K/O7V77iTr705Yd++I7J/f/W6GiNjY0u08cnn/Rq0bUHxkNxYK+Xzl3Ib//UJ5/8a98LSdL+S3JMTn4/brKx0aXRCEhWenfefLS+XvP1xz764a+GN7/G/WvgJXMe4H8Dm+ltZTO6UaEAAAAASUVORK5CYII=');
    tmpStringList.Add('" alt="Elysion Logo" />');
  end;

  tmpStringList.Add('<span id="title"><a href="https://github.com/freezedev/elysion">Elysion Library</a> Log</span> <br />');
  tmpStringList.Add('<strong>CPU:</strong> ' + SYSTEM_CPU + ' <br /> <b>Operating system:</b> ' + SYSTEM_OS + '<br />');
  tmpStringList.Add('<strong>Version:</strong> ' + GetElysionVersion() + '<br /><strong>Language:</strong> ' + GetLanguage() + '<br />');
  tmpStringList.Add('<strong>Filename:</strong> ' + fFilename + '<br />');
  tmpStringList.Add('</header>');
  tmpStringList.Add('<br /></br />');
  tmpStringList.Add('<table id="log">');
  tmpStringList.Add('<tr>');
  tmpStringList.Add('  <th>Time</th>');
  tmpStringList.Add('  <th>Message type</th>');
  tmpStringList.Add('  <th>Category</th>');
  tmpStringList.Add('  <th>Message</th>');
  tmpStringList.Add('</tr>');

  tmpStringList.AddStrings(fText);

  tmpStringList.Add('</table>');
  tmpStringList.Add('</div>');
  tmpStringList.Add('</div>');
  tmpStringList.Add('</body>');
  tmpStringList.Add('</html>');

  // Save file
  try
    tmpStringList.SaveToFile(TelEnvironment.WorkingPath + fFilename + '.html');
  finally
    // TODO: Proper error handling ;)

  end;

  tmpStringList.Destroy;
end;

function TelLogger.ReplaceSpecialChars(InputString: AnsiString): AnsiString;
var
  TempString: AnsiString;
begin
  TempString := InputString;
	
  // Line break
  if (Pos('\n', TempString) <> 0) then
    TempString := StringReplace(TempString, '\n', '</br>', [rfReplaceAll])
  else begin
    TempString := StringReplace(TempString, '<', '&lt;', [rfReplaceAll]);
    TempString := StringReplace(TempString, '>', '&gt;', [rfReplaceAll]);
  end;
    
  // Replace special chars with HTML-aquivalent chars
  TempString := StringReplace(TempString, '"', '&quot;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '^', '&circ;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '~', '&tilde;', [rfReplaceAll]);
    
  TempString := StringReplace(TempString, '¡', '&iexcl;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¢', '&cent;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '^', '&circ;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '~', '&tilde;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '£', '&pound;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¤', '&curren;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¥', '&yen;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¦', '&brvbar;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '§', '&sect;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¨', '&uml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '©', '&copy;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ª', '&ordf;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '«', '&laquo;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¬', '&not;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '®', '&reg;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¯', '&macr;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '°', '&deg;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '±', '&plusmn;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '²', '&sup2;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '³', '&sup3;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '´', '&acute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'µ', '&micro;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¶', '&para;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '·', '&middot;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¸', '&cedil;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¹', '&sup1;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'º', '&ordm;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '»', '&raquo;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¼', '&frac14;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '½', '&frac12;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¾', '&frac34;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '¿', '&iquest;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'À', '&Agrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Á', '&Aacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Â', '&Acirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ã', '&Atilde;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ä', '&Auml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Å', '&Aring;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Æ', '&AElig;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ç', '&Ccedil;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'È', '&Egrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'É', '&Eacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ê', '&Ecirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ë', '&Euml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ì', '&Igrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Í', '&Iacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Î', '&Icirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ï', '&Iuml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ð', '&ETH;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ñ', '&Ntilde;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ò', '&Ograve;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ó', '&Oacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ô', '&Ocirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Õ', '&Otilde;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ö', '&Ouml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '×', '&times;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ø', '&Oslash;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ù', '&Ugrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ú', '&Uacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Û', '&Ucirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ü', '&Uuml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Ý', '&Yacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'Þ', '&THORN;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ß', '&szlig;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'à', '&agrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'á', '&aacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'â', '&acirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ã', '&atilde;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ä', '&auml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'å', '&aring;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'æ', '&aelig;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ç', '&ccedil;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'è', '&egrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'é', '&eacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ê', '&ecirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ë', '&euml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ì', '&igrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'í', '&iacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'î', '&icirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ï', '&iuml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ð', '&eth;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ñ', '&ntilde;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ò', '&ograve;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ó', '&oacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ô', '&ocirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'õ', '&otilde;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ö', '&ouml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, '÷', '&divide;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ø', '&oslash;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ù', '&ugrave;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ú', '&uacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'û', '&ucirc;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ü', '&uuml;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ý', '&yacute;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'þ', '&thorn;', [rfReplaceAll]);
  TempString := StringReplace(TempString, 'ÿ', '&yuml;', [rfReplaceAll]);
    
	
  Result := TempString;
end;

//
// TelLogger
//

constructor TelLogger.Create;
begin
  inherited;

  fFilename := ExtractFilename(ParamStr(0));

  // Write all logger messages by default
  fPriorities := [ltError, ltWarning, ltNote];

  ShowLogo := true;
	
  try
    fText := TStringList.Create;

    WriteLog('Start log', 'Initialization', ltNote);
  finally

  end;
end;

class function TelLogger.GetInstance: TelLogger;
begin
  if (Instance = nil) then Instance := TelLogger.Create;
  Result := Instance;
end;

destructor TelLogger.Destroy;
begin
  fText.Destroy;

  inherited;
end;

procedure TelLogger.WriteLog(const Msg: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
begin
  WriteLog(Msg, [], '', LogMessageType, FormatHTML);
end;

procedure TelLogger.WriteLog(const Msg: AnsiString; Category: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
begin
  WriteLog(Msg, [], Category, LogMessageType, FormatHTML);
end;

procedure TelLogger.WriteLog(const Msg: AnsiString; Args: array of const; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
begin
  WriteLog(Msg, Args, '', LogMessageType, FormatHTML);
end;

procedure TelLogger.WriteLog(const Msg: AnsiString; Args: array of const; Category: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
var
  FormatString, TempString: AnsiString;
begin
  if fPriorities = [] then fPriorities := fPriorities + [ltError];

  try
    if Msg = '' then FormatString := '' else FormatString := Format(Msg, Args);
    if FormatHTML then TempString := FormatString
      else TempString := ReplaceSpecialChars(FormatString);
  
    case LogMessageType of
      ltError:   if ltError in fPriorities then fText.Add('<tr><td><center><font size="-2">'+DateTimeToStr(Now)+'</font></center></td><td><center><font color="#FF0000"><b>Error</b></font></center></td><td><center><font size="-1"><b>'+Category+'</b></font></center></td><td><center><font size="-1" color="#FF0000">'+TempString+'</font></center></td></tr>');
      ltWarning: if ltWarning in fPriorities then fText.Add('<tr><td><center><font size="-2">'+DateTimeToStr(Now)+'</font></center></td><td><center><font size="-1" color="#BABA00"><b>Warning</b></font></center></td><td><center><font size="-1"><b>'+Category+'</b></font></center></td><td><center><font size="-1">'+TempString+'</font></center></td></tr>');
      ltNote:    if ltNote in fPriorities then fText.Add('<tr><td><center><font size="-2">'+DateTimeToStr(Now)+'</font></center></td><td><center><font size="-1">Note</font></center></td><td><center><font size="-1"><b>'+Category+'</b></font></center></td><td><center><font size="-1">'+TempString+'</font></center></td></tr>');
    end;
  finally
  
  end;
end;

procedure TelLogger.WriteLog(Msg: TStringList; Category: AnsiString; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
var
  i: Integer;
begin
  if ((Msg <> nil) and (Msg.Text <> '')) then
  begin
    for i := 0 to Msg.Count - 1 do
      Self.WriteLog(Msg[i], Category, LogMessageType, FormatHTML);
  end;
end;

end.
