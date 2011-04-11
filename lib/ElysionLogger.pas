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
  ElysionTypes,

  Classes,
  SysUtils;

type

{%region 'Logger priority types'}
// Logger message type
TLogMessageType = 
  (ltError,   //< Displays message as an error
   ltWarning, //< Displays message as a warning
   ltNote);   //< Displays message as a note

TLogMessagePriorities = set of TLogMessageType;
{%endregion}

{%region 'Logger class (Prototype)'}
{
  Class: TelLogger @br
  Group: Optional @br
  Description: Very simple logger class @br
  Singleton pattern -> See http://en.wikipedia.org/wiki/Singleton_pattern for more information 
}
TelLogger = class
private
  fFilename: String;
  fText: TStringList;
  fPriorities: TLogMessagePriorities;
	
  procedure HTMLWriteHead;
  procedure HTMLWriteTail;
  function ReplaceSpecialChars(InputString: String): String;
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
		@param Msg: String - The message to be written in the log file @br
		@param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
	  Description: Writes a user-defined text in the log file.
	}
    procedure WriteLog(const Msg: String; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;
    
    {	
	  procedure TelLogger.WriteLog @br
	  Parameters: @br
		@param Msg: String - The message to be written in the log file @br
		@param Category: Specify a category @br
		@param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
	  Description: Writes a user-defined text in the log file.
	}
    procedure WriteLog(const Msg: String; Category: String; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;
	
	{	
	  procedure TelLogger.WriteLog @br
	  Parameters: @br
		@param Msg: String - The message to be written in the log file @br
		@param Args: Array of constants @br
		@param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
	  Description: Writes a user-defined text in the log file.
	}
    procedure WriteLog(const Msg: String; Args: array of const; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;
    
    {	
	  procedure TelLogger.WriteLog @br
	  Parameters: @br
		@param Msg: String - The message to be written in the log file @br
		@param Args: Array of constants @br
		@param Category: Specify a category @br
		@param LogMessageType: TLogMessageType = ltError - The logger message type @seealso TLogType (Optional) @br
	  Description: Writes a user-defined text in the log file.
	}
    procedure WriteLog(const Msg: String; Args: array of const; Category: String; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false); Overload;
  
  published
    property Filename: String read fFilename;
    property Priorities: TLogMessagePriorities read fPriorities write fPriorities;
end;
{%endregion}

(**
  * 
  * @param None
  * @return True if TelLogger class has an instance, false if TelLogger is nil
  *
  *)
function isLoggerActive: Boolean;

implementation

var
  Logger: TelLogger;

function isLoggerActive: Boolean;
begin
  if (Logger = nil) then Result := false else Result := true;
end;

procedure TelLogger.HTMLWriteHead;
begin
  fText.Add('<html>');
  fText.Add('<head>');
  fText.Add('  <title>Elysion Library Log</title>');
  fText.Add('<style>');
  fText.Add('body { background:#fafafa; font-size:90%; color:#000; font-family:DejaVu Sans, Verdana, Arial; }');
  fText.Add('#log { border: 2px solid #dddddd; border-spacing: 4px; width: 99%; }');
  fText.Add('#log th { background: #c0c0c0; text-shadow: 0px 0px 4px #fff; }');
  fText.Add('#log tr:nth-child(even) { background: #e8e8e8; }');
  fText.Add('#log tr:nth-child(odd) { background: #d8d8d8; }');
  fText.Add('#logo { float:left; padding: 8px; }');
  fText.Add('header { padding-top: 20px; text-shadow: 1px 1px 2px #ccc; }');
  fText.Add('#title { font-size: 120%; padding-buttom: 8px; text-shadow: 1px 1px 4px #000; }');
  fText.Add('</style>');
  fText.Add('<!--[if lt IE 9]>');
  fText.Add('<script src = "http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>');
  fText.Add('<![endif]-->');
  fText.Add('</head>');
  fText.Add('<body>');
  fText.Add('<header>');
  fText.Add('<img id="logo" src="data:image/png;base64,');
  fText.Add('iVBORw0KGgoAAAANSUhEUgAAADwAAAA/CAYAAAC8aKvcAAAAAXNSR0IArs4c6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEQAACxEBf2RfkQAAAAd0SU1FB9sCGBM0JbUOUMUAAB1VSURBVGje1ZtZrG3ZVZ6/Medc3W5Of8+5Td1bdatvbExjbCJwYQggQQhSwgMhAQUSRYrykChSEImERPKQCKIEoYhgMApRBLzk');
  fText.Add('AYwIkAC2IgJEJjihcQw2Zbvq1q3bnHP2OWd3q5vNyMPa97psygZsbCnrZR9tnbX2HHPMMcY//n8s+AJdV68+8p8eu/nUR5948qn3Hh5d+XaAK1evfc7PvXRw8Of6f/l8G3r5ytV/bm32/Xv7B3rj8S+Svmv13u0/kuVy8Zuz2ck3LxeLi4NLB5yenL7h/fsH+39FkDejXBXhryN6DQRUASEh3yOiucT470/OzucH+/uczmZf');
  fText.Add('OIMvXTrk5OSYw8PD78rz6odG48nOU8++RSYHz7Ksa9aLY/Z297k4+RivffxDtF3zk7deefnvftIzJnu5luZHRPTvoQYxKCCvX6xuFq8AqppAhi/1e09PZv/60sE+J6ezz5/BB/sHnM5O2d3ZvVyNp7+QF+Vbbz7+NNeeeCt9zEiqdM2a5cU9ymoLMZayEO7d+qDee+1j0nft97x2+9V/c7B/8MNi+EciqCgixgwLldctWBR0');
  fText.Add('sG9j78bwwfakGBW+YXZy+qufVw9fuXr9p/K8+I4rV6/x5HNfQcsW49LhoxKTEvuG89NXKUfbYBzOCNZYUljyyku/q2end6VtVmoFSQoiimAQ8/rFykMf6wM/K6gmFEFVSbrZBOWnT09Pv/PgYJ/Tjbf/Qgy+cu36dziX/9T+3p4+9dxbRSbXsSKkpFhnyI0lzy1du+bO7ZcpR1uoOHJnsNYQolLkOcvzV3jt5Q+yXJzj25qo');
  fText.Add('CSMGQRExn3ymGcL4oXfZGJoUVUiDwZpUjw3+0ePTeQdgPxsDj46usF6vODy8/OLe/uHvb23tfPtzb/5SHnvuRQlmBwPkWQYi5C5jPM4Z5RkxdaxXF+TFCOssijKucqxYYlLyaocbN5+nyC1lluh6DzFgncVawRmz+RTEgDUGY9hshgxBbAQQREAEAcYJ+3dG1ej+eDL6/c/K4KIorhwcXvnxyXT7h24+8VTxlV/9zVAe0fRp');
  fText.Add('2GsRnBUSFmeHRapADJ75/ILt6YSUwIhBETJrETN4O0aY7l7myvUnmBRKTB6NPSIJaw3OCkVuMYCxDwwTDLIxWoZzKw/iHRFhmjnzrZ+Vhx+5cfP7JtOdXzw6uvqmL3v718iVG1/E2SrQhwRJwBoQwRiLEUGsGc6egEmRxfIMl5ckBGMdYoSUNlnXGKrSYQwgOTcee4a9/UNi7DEojkieG6rCISJYEcRsjB5+djB4E6tJFGcs');
  fText.Add('eWYlASGkP93gK1cfYbVccO36zW/aPzj8yP7epa95/ou+XK49+TbxlDRdoPMJVDbH1JBllswajAjjMsNaGBUZzirz8zNMVmKNxQz5BmstuXMYEZw1ZM5hrUNEuXJ0yOGVp5lOSnKXyJ2SO8UYwZrhlDw4u2KGkySblFbmlrIYckTnA+hn8PDhJk7Hk8kL+/tH93d2dv/Wk08/z5NveodovocAzgh9jJB4eCTFCkYMSYU8N+SZ');
  fText.Add('wzlHkTtEPRcXZxR5tXHJcAqcs4gRnDOMRwVF7sjzIRwu7Y6ISdnbf4Snnn6BcamMSkuKPUaUEJXMbkqXfqJ8TauMIrOs20Dn48Oq/WkNLsri8sHh1Z/a3tn/d48++pg8++avIt+6ToyGzA6eaH0kps3xNQZjh0U+CKDMGUZFTmYHT4fQc3Z6RlFVZJkj6hCHWZ4xLjMyZ8kyQ1Vk7E5LRlVGVHj+8T18TEwmFU89/TxHl69j');
  fText.Add('tMMKRN9vypeQUERgb1pijXC+7AlRHwKVN4xhC+7K9ce+f3f30i9cvXzlmaee+3IZX3qaJBlGICXFqxKSEpPgrB2OkTVkRrDW0idlVGRURUaZObanOeMqYzZf0TcLirIEMYTN/7nNhuTOMq4c46okJWV/u2R/p2JnWjEdl4zHBbtbBZPJlEcffwGXjbDGIyTWdYsVONyt6H3ibNEO9XkDTAYgtvHwlWs3WC3nXH3ksa/Zv3Tl');
  fText.Add('o1eOjl68+cTzsnPtLRJNBQopCT4kVGWAsQiZsYg1OGNxxhAUrAwejkkpMsu4yrl6aczFsmaxXFOYDucq8szReyXPhKrIhgycWabjkt1pybhyVGXBwU5FXmS88OQeiuPywZTJqMRaw8HhEU8+/SbWrceS2J0Yjs/WLNZ+qMXoBoxAer2HR6PJE5eOrv32wcH+P75x4yaXH/0SSdkuMQ1oRnVIAmIMgmDdJjsagzVms5OKMYbc');
  fText.Add('CWXhGBWWpk/ElDBGWKxa2q5nsbhgMhnR9hER4dLOiMmoJEVlZ1IyqTIQmIwLblyeMh2XXDvaosxKrl3dZTKdsrc/ZVzmZM7Q+cQjVx/lyaef5aWXj0mqpNiTYiSkh4jrIfqyzz7/xT+5s7v7H69evrx39eZbSMVl4gaYqg71DSOIGHQTczAYaq3dgAGDNZv4FaHMLavGM6oyMmMIIRJiIoaOtl5wuLvDeFzR9ZGtcU6R5fik');
  fText.Add('3Ly2RVVmlHnGuHK43HH1cML+9pj9y3tkW4+Sjw4wxS7VZEKVW/a3ctDE3bOOx24+w9buEcvlAh8SbduQYiKxQWAC9h1f+y3vufnU83TmgKgZyGDow3RnhlbFGIMxBucMmbNYIxjDUEKcxVghKextlYQ0JA8rQlU6fIhMRhnRtzT1gp3t6aZWb56ZWVKKbI1yDg+mTEc5l/ZGZJlle1JwdLiLmVxGqB6WIMThyh2EjovFmrbz');
  fText.Add('nJ63SFZyeOUpgu85Pb6DDx5Nw9qSggsxYIxjMhriMkbFJ4iqWBGMgDN2yKDWgDUDqjF2OAFGsEboQ2JcWPLMIKL4EClziw+RqnKMq5xX7/TUbeDu2ZqqKMhyy9a4ou0TVeHInaVpAlGVLFOefuyQ6aTEjDJUK0T0dfB/+HTVFpNJxe60wl8KnJ5D9JGirDBmgFmgD29zqmBEGOUZYqD3ivqIUYMYwWBAdAD6zhCjYDPzEA7W');
  fText.Add('nWdSFWyPczLnKIvhuyq3hDgYsjPKOVus2BplLM+Gth0imob/TWnw9O72iNmiJaTE3nbJvdMVxmZs6RBSb9jr2Cm7O2Pm8xXbvkQVnDFMxo4yt9RWCF4fbpHRpBgRjBWaLtJ6T1QlqRKjkgBjhLgJfrEDWLdWyJ0wHeWMSsveVsnlgxG5Hep0jErmhBADxxc1IUYgYiTiu5a+W9N3NfdPl0AiqtD5wKTMUBVCiBS5oSyE0DSb');
  fText.Add('TdJPsfZB+2jovdL1St1FuqCs60DdeGLQT/TNKC5pwofI2TpgROjDpm4JD8F65izWMny6AetmRmhDwlnDtMopCosmZbX2AExGlhTD0DTERN8HmqZnvV5hUFbLSOYMl/a3MZoTA5zNa8oypywtXZ+4c9KAGB5zOfnWHGTvU+wVNMxQIzz16A63C6HtAoulJ4RNf/ygb96cahdS2ngTeoaCZTYlp9gkJ2cFYfgbTXQ9BKuUeUbn');
  fText.Add('IyKG3nsulp7dSUYelbNVT98nxAzFXlGcKM16QdesMdrjk7JenSOxo5oe8Mp9uHZpG2stBktRQt15FnVNVZ+SjR3oAFrQISySX/Oxj844m68onLCzVRBCZDIaHDP0zJ/om12KQ0mOCZwFrFBkDrOBiMLQyIcUUBGqzA6lSoW2jxSZIYSA7yO5E+6fN7R9xFhDnkFK4AQyo5wtW5rVOarQtg1CpF0W9M2SJx4PXJvc4GzRcPlg');
  fText.Add('GxGlD5H5KlCcNuxOG6y5j2CJQWn7xHh3wunpgq1Jziuv9ZzHSNP01E0PCtuTHJJnvuzxg68eJK3BWGuEInewyb5RBUWIaQAbzgjTScG9i5ZMlcwZnAjzVYt1hqaPQ5dSWFQj1ljKLNL3PYt1z3zdsliu0Bjou4akgdoafN9iN13Ss888Tdv3jCvLat0xX3mCTxztjej6xMWqoywykhoW8zXjUcbxyYrtac7xrKb1CesyTi9a');
  fText.Add('7p81aIwUhcVYaDvFiQz11Bgz4OKUyIuCGBRrDZqGOCidYzLKsEapsmEzYlJqH1EMqQ9UpcMZQ9pk/twqq8ZDiqx7j/eBtusIviN2DYmEF0gxYI1wPJ2wvb3DC888ho9wsfKIE8rccHxeYxc9dR+ZjHJiFPqgXD+acLHyKLC3U+Ks4H0kpURKUDeBEBNiwCAbcARUuRvwZrKEEMnzYfFZZokJtsY5rU/0dcKHBGIoc0OIMKks');
  fText.Add('zlhCSoSQBo6qsLS9p+s8fYhoTJtmXYghEGNESYiVIf7nc07v3+fSwSG37u1ysJO4cbjFvXlgNvc0XcDHQNNFnLWECMt1z3LZsrtTMjtr6HygaT0xKjENyTJtYjd6JWnCGRkYhzwz+AhKInOW7VHOeJQPZUkM6zagCmXhqDJHFxJ5btAEMUZ8UHxUMvG8/33/hbt3bjPd3eGL3/6XGU+2KXOl10CR5/SdJTykVYeT4r3nbH7B');
  fText.Add('8ckJeweHjEcjLpYtV7crLhrltdM1hXUs28Ry7YlATANDYp3BGBnKWRK6Pg6YOumGzPtET+CSKiJK6SxFPiSlqsrZ2x6BKkVVMLtoKHM2GdugKKkZvDk8FMrC8r/+x/t46Y8McB2Ry8zuL/i1n/+v5MUFX/dN34Zv14zKgq4r8F1DiAmSIkCIib73zOYrTi/WuKoDsVhj2JnkhCQsmhavhnWTUJGhcUmReYzsbJcEH0EjnR+O');
  fText.Add('cXxoMCRVTUpyqkNPurs/IUal6wLTUUaZG0QMbd+T2URWOda1Z94MtTUlBjrGQkyJj790i49++BFEdgADtMACkRF99zK//qs/x5vf8lasMZRFQdtkw7HWhGIRMSQxhJg4X9aMtzpwBU1oECuMc6XMHX0TMaIYYzlb9qSRo+88XUqMyxwfM6Zjg6bBwzo4RDUhqrzdpaSEGFmvWgTLZJJRt57Ox6HUiZA5w/myQUVImrBiKTJo');
  fText.Add('Q2RUGYKP/Lf33GE0fguq403f0QLlBtC1rNczLhZ36UKNMQONG/qeEBPWgjEOa3MwGYjBh0jT9gglt48bDrYLyjxyabvi3llP0/dsVYa6DXRdpOkT1WWLiLI7KXiAL5KCRkTRfzabzT7gUkr0ISJtxFllsYoklU27JxhrqFd+0wEZikwIMeKcJU+RdROp646yegLVXURKsiwnxTUhgkiLyA6qe1xc3KXvIhoNmIQxBpKgDPTQ');
  fText.Add('ZDymKEqcdfiUMH2gLBLLNtFFZVw42k4ZVZaksG4DxgiTkcPHxMt3lxhRxmWB7yNRVTUhSfVHTmezHwBwAGVmGY8zztce9QMjuDsdupd5PcRSng2dk0ERqyzrjhiFzCbKTIEKKDAy4sqV6yyXx5yd1RsvF0BB28xgs+voAJiMeaARDfh7WlpcZhGE3kdmi4ZL2wXrLuJD4nzt2Sotj1/dIkRP66HtA5koyRoOd0dMxtXGs0mS');
  fText.Add('pn96Opv94AM0amKKZE5AFIOyPc65sjcipch83ZFZQ5VBJpEQPHUfaPpEYWGURUYuoLGm79cb0iBxcnLCer3eSJoDrBNJyCb2HwhiWTEwldYqMfUE71nVLX4xY+paNHqi9xyftxQuDYktJGaryO9+9BxHYKuyFJnQJWXdeubLmtI+gJLmvaensx+8tL//EH47awydj4zGhst7Faum53w5QMMyF1IMdD34qIgRCguFG8oImlhe');
  fText.Add('rFiu1jz3vOfjH7tMUqFuOkQ7RDxKi1AjcvaQKjLIwJJYJcWBc4rJs27OuDi3GLF4H9nbnTDeucTaO04uErmzZHmOT4mkwkfu1VzdCZRlQacR5xynix5jLobubHN4Tl6nFztrBuGp7TzztZK7oe1TIm2XiGoGdFUYCqe0jcer0q7WnC3W1E3LujkjH83JC6HvngF1qPQIc4QZyB32jwIxgtGBfXjgapsNxsekhNgynx/TtR2r');
  fText.Add('6TbnFxOm4xlHh3vEfIc2K+lCSZEVqDhCStw+7ymdZ1pZVn1kkjsWtSc+iJtPuZwqRB/wURkXAwho+7jBzzApoHCDLhTaSOwbTi7WLNcdfd8wX72K9wtUPdceO+djf2SBBBpB5qjephq9TIo5Rgw++g1vPcgg1oF1llw2vFPoWTcz6uaC3I2oqgn37t/j6uV9tvaOSG5EKCbYbITLc5JJrKOj9pHSOWoi49J8WjXYxZQw1lC5');
  fText.Add('xLpTNCrGGrZHgiGiGvB9omka5vOG+arBx56mvaBu7lOv55SVQUzipQ+9DXgOY5Qs8/TdLUQCTX2XslKQ9BCbF6Uj+kjuHCSl33RYKSnB98QINS2L5RzncpbLOVvTGXt7e0x397DlDkU1wZQjRkWFYmh8IKRBZVDl03lYiSGyaiPGCVsThwZP9D0q0Pc98/maxbqj7Tv6fsFicUpMS9pmTTlxrJc9xhTATWAX60aINKAdSg0c');
  fText.Add('Uq9eJcstYuUhV5acoQ+RFAauqu8CMaWBhxoYdLxGgu9pm4bVasF8cUF1fJ/DgwP2Di5hRnuYyS5FWbE1rqi7yLr1A9J4Iw8/yJh7E4sPQ1bUFAkhsFi0LJuOum3RGJkv7tLUZ/g4UC69D5gaQoyc3H4BkS1UHUIGGlFGiEyBPbruFWxmsQo+KHkpPHu/5vKLL3L5xg3++8/8NHecpbdgH84vDFJoSkqKnvU60DQ1ZVmxXMw5');
  fText.Add('Pj3h+rUrWH2UzF6iaWUQ5cxwj+obeHiQGBO+bwihp+sTdeupm56mD/TditXqmOAT6/qErEg4owSfsMbQ9YEULSk+gUgFDITc8FsWNEeZALtEPwcVssJydDb01BdZxm5Z8ejREZfv3Uc99MA9Ee5oIBQ5RjfKYFJiDKzXK9q2oe9buq7jRh94fOQoS4NqSQgPxiH+5OWMgMbIcrmi6RLthnvqo8f3K5rmmNXqApWI77uB07Kb');
  fText.Add('UhIjfReol88jso1qPmjCAimmjWCbIYyAXbpuxqQomSocdhE1Bt73Pl7W95Lv7KAhYLKMj+7t87Vf/3Xs7R/wgR//Mf4wRtpcNiMNkDSRNLJYBmLw3MsyEgWPP67s7GwznWwhn85gUHzwdG1i3fa0fY/vV/hQ0/Xn1OslwfeMpxmCo157ysohwobqyej7pxEzGijNzbjBMGljhp+QEpgiUhE18cK5Z/L8Cyxe+gjRBz7y6KN8');
  fText.Add('7TvfSTWd8Mc/+i6ePZtx69d+jXttyypEVppwakF0GHCJwxRP0kTdNpzNjplOx5ydTej7yFEKoJ/mSKsqvvfMly0hNPThlK6raZsWsR3WJXyvpAQhREQGJT3LHC5XYn2IyDZCgaqFzZjRA+ZzWKEFCpJO+crlCb9z7RovPvsMN158By//2I/x1K1bvPKzP4ttW1Lf45zDnZ6yBD4aI8+86U388Usfwhi74ZgFlykxDNM767rm');
  fText.Add('7r1XyXPLuLTcve83BMMbeDilRIiBtjuj685pmjkpBcQqXRMYT3N6E0lhyHpmE0tKIoRI310GLV7n3aE9GdLsoDYOllu+ioavt5Yvfe01Tn/xFzmPkRzwgC4W8NVfzdvf/jY+8nPv4eSll7gtQixKVssV1lliAJdBCoAanBvUQSTRtitu33kFTZHHrj9C0vQQ2n6SwSF0NPWcrj+h7Za0bYexQm6GmacQFETpfSTPLMEnYkzk');
  fText.Add('haVrI0ZyitGCrj7AuHaotUSMO8GkGrQFGvKy5nIKTNrIJMt4wTlCUXBS1xz7nhnCS7dv8weaiLdvcwLcAyzKa3fuUBSCmsHgoGDsRuwzlqIQUoSmW3P3/m2idvjOvWHiciCsVguWiwVZnsiyDfQzA9b1faBvIqNp/hCupTR412WW3Uv/m4vZITa7wBjDZHqfrrmOcM5k+ipN7RhNXqNrVgSN/PCzz/C2L38b3fvex5fduUNl');
  fText.Add('DM9XI6aTMc1izqsvv8Kva+JkOsHUDSGEzbBZhqDEsBniMBajCVcYshxULdYlmnbJ/fuBoDtv6GGTuYzjkw/TrJd0bY/3gawYhsCyYvBoMcpIMW2I7QElpTikzKI0bO/+MnnxG2TZHVz2MsZ+nK3d38Plt5lsvYzIBWLWpBT5usWSZ595lmvf/d1EYJUSZ03DTzz6GL90dJktZ8lU2do72IwcyidGD2Vg1q0bBmaGMgMhCJlT');
  fText.Add('+jYRY6DzNV1zokD2Jzycma3LXd/dsw5NEQlBaZZgnaGoMtBB+gxdIivcsAGVI4aIqtA3AU2J6fbvsbz4ACmVZMWHsCaiJlJMDH3bY4zH14kPH59w8pu/wZf8yq+QdJhCSSlx9YXn2dvf5+LWLbRt2dqabiggpSxLxGz4LzFk+SDCW+ew1hJCj/cb0a9IrOZK13oF91f/hPbmw2vrVz/Ov5pscSum9C0iqjEmCWHgg4JPxJDI');
  fText.Add('SjtUGh2+Q4Yux/tEXjqiT2S5ELxHTEfftVibBoI/M7jMcVT3zN/6Nr7sbW/nDw4OuPLhDw+qhxHq42P2JlPsBz/IbRF+6/h4KH3GkGUOzEBMVCOLcYYUEuOpw/uESERIpJh0fqbSe/1WFfnbaGzquvnkI33rlRkhzfziTH56ecZe26SPGRMwJhK9J/pI13jyzNI2fhhzcIOqD1CMsg3ohzw3GCekFBELbRuo156UIKXECuEb');
  fText.Add('3//btHdu88y9u5+Yk4zK3a/4Cn5ntSKl9LpUM1BMRfWgDxBsZgl9REXp2og1isuVxYUyv9B3Jyhnp7OfnZ2c9qdvMD78oI9idnbaIZzfenn25MW5vNj3oclyT15E8jLR1P1GQ/JkmaFv43DEu0i96FBN+KgYA0k34INhviP0AU3KmTO8+2ibX//DD/GBj3+YlkF4R2CrqPhL73wncTwmPZjVZOiXEaEoDOVo+F1NiSwDaxLL');
  fText.Add('hefkTvo/IfDs7HT294l0n2nQ7pPGltbrevhc1bcuzpp/ORqPfgvSd2YFGnokJd1IMBBDYrxVsFr2G7F6ECez3JFCJEWoxhnGDjNceZmx9pGJGtzWhONKKFcNSZUC+N2+4+5HPsK1269yS2FxsE/f95SlpcgtSXWYNOgDo4nQtUkXF/HMe/3u2ensH47Ho1ld19RNw5/Z4E+9Ls7rj53Nxv+iqvRmUekXu0x1NFZpmyG+izKn');
  fText.Add('bz2jaUGWWVLSh8b1jSfLLMYOGV+TElDsxLI+OcM2NbPC0kWlA1bPPUN5sM/Wq3e47T2rS7uo9kNWzobEJkZxFp2fB2mb9BMnJ/JiU8/+L0Bd13+mWdE/ddZyd6/i/t3Ze0jVu4yRb7IZh8YMakEKg8xqzCBOB5/I8kGQ6zs/DI+KoAlCH3GZwTpLMEqwlqUqJwIXlYPlnCIq9v4x93PLXekxRokx4dzAp60WnuUyvl+TfMnp');
  fText.Add('6ew/H+xVf6pHP+uJ+MOjfY7vz7j6yME7JhP9JZszMWT0vZLiMK+cVNnZG+F9pFl35EW2IfNBU6KoMkQMoffkZcb8rMFmhnKUD1M/IZGtPBdJ0UxwuSHLhYvTVus6XqDyjScnp+/f399n9hle5Pi8vNXy5FMHf9Pl+jPGoaHLpGkSxirbuxV2M/DSrHpiTGS5o2s9050KK4aL2Yrp3oi29lgnFKWjXvSbmhxRBd8nENX1Ioiq');
  fText.Add('/oOTk9m7/iKm9z/HVwD2zeNP8R+sk+8ajwaCvV5lWGcZTwuaOhD8IEjXTWRre1AEmnXP9l6FxmG8KXqlrgPOKVlmaNtI10Ri1HdrlO87PTs9+Yt6L8N+LjdfOhzra6/Oft5J9cvG8qXGydUsH7qUvhuURTEDwyGig64bEqNxRkyJECOhV7o+UhZCVsBy7una9FshyF87PZ29uxpVdfPnjNPPdJnP5eaT4+HlqqyQ3/7jD8/e');
  fText.Add('enGq3+a9XjgXNXcBJGBNJPpIljtcYemagBhDCorvEpDIXaLtgp7e70+7nm89Pp59pRH9APBZx+oX9M20m08efO94rD+AiIZoxbeGyVaBdZZ62SJmUByNgb4LWtdRUuKfnBzP/u3n+005+/l4qKP6n3de40e3tuWysfqWrIj43tA1AWOH8pViYF1Huib9x9DJN8xms/d+Id6B/LwYXDeN7l8arW+9PPs5TdWvTsbyZtV0Lcsj');
  fText.Add('MRhdr3qp63Qeet55ejp713hS1Z8K8v+/vR69OfS1128cfOezz+/rlav76fDo4G8M7ynuf8HX8/8Ac9jQjNPOf+oAAAAASUVORK5CYII=');
  fText.Add('" alt="Elysion Logo" />');
  fText.Add('<span id="title"><a href="http://www.elysion.freeze-dev.com">Elysion Library</a> Log</span> <br />');
  fText.Add('<strong>CPU:</strong> ' + IntToStr(SYS_BITS) + '-bit <br /> <b>Operating system:</b> ' + SYS_NAME + '<br />');
  fText.Add('<strong>Version:</strong> ' + IntToString(VER_MAJOR, true, 1) + '-' + IntToString(VER_MINOR, true, 1) + ' "' + VER_CODENAME + '" (' + VER_CODENAME_RANDOMQUOTE + ') <br /> <strong>Stable:</strong> '+BoolToString(VER_STABLE)+'<br />');
  fText.Add('<strong>Filename:</strong> ' + fFilename + '<br />');
  fText.Add('</header>');
  fText.Add('<br /></br />');
  fText.Add('<table id="log">');
  fText.Add('<tr>');
  fText.Add('  <th>Time</th>');
  fText.Add('  <th>Message type</th>');
  fText.Add('  <th>Category</th>');
  fText.Add('  <th>Message</th>');
  fText.Add('</tr>');
end;

procedure TelLogger.HTMLWriteTail;
begin
  fText.Add('</table>');
  fText.Add('</div>');
  fText.Add('</div>');
  fText.Add('</body>');
  fText.Add('</html>');
end;

function TelLogger.ReplaceSpecialChars(InputString: String): String;
var
  TempString: String;
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
	
  try
    fText := TStringList.Create;

    HTMLWriteHead;
    WriteLog('Start log', 'Initialization', ltNote);
  finally

  end;
end;

class function TelLogger.GetInstance: TelLogger;
begin
  if (Logger = nil) then Logger := TelLogger.Create;
  Result := Logger;
end;

destructor TelLogger.Destroy;
begin
  try
    WriteLog('End log', 'Finalization', ltNote);
    HTMLWriteTail;

    fText.SaveToFile(fFilename + '.html');
  finally
    inherited;
  end;
end;

procedure TelLogger.WriteLog(const Msg: String; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
begin
  WriteLog(Msg, [], '', LogMessageType, FormatHTML);
end;

procedure TelLogger.WriteLog(const Msg: String; Category: String; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
begin
  WriteLog(Msg, [], Category, LogMessageType, FormatHTML);
end;

procedure TelLogger.WriteLog(const Msg: String; Args: array of const; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
begin
  WriteLog(Msg, Args, '', LogMessageType, FormatHTML);
end;

procedure TelLogger.WriteLog(const Msg: String; Args: array of const; Category: String; LogMessageType: TLogMessageType = ltError; FormatHTML: Boolean = false);
var
  FormatString, TempString: String;
begin

  try
    if Msg = '' then FormatString := '' else FormatString := Format(Msg, Args);
    if FormatHTML then TempString := FormatString
      else TempString := ReplaceSpecialChars(FormatString);
  
    if fPriorities = [] then fPriorities := fPriorities + [ltError];
  
    case LogMessageType of
      ltError:   if ltError in fPriorities then fText.Add('<tr><td><center><font size="-2">'+DateTimeToStr(Now)+'</font></center></td><td><center><font color="#FF0000"><b>Error</b></font></center></td><td><center><font size="-1"><b>'+Category+'</b></font></center></td><td><center><font size="-1" color="#FF0000">'+TempString+'</font></center></td></tr>');
      ltWarning: if ltWarning in fPriorities then fText.Add('<tr><td><center><font size="-2">'+DateTimeToStr(Now)+'</font></center></td><td><center><font size="-1" color="#BABA00"><b>Warning</b></font></center></td><td><center><font size="-1"><b>'+Category+'</b></font></center></td><td><center><font size="-1">'+TempString+'</font></center></td></tr>');
      ltNote:    if ltNote in fPriorities then fText.Add('<tr><td><center><font size="-2">'+DateTimeToStr(Now)+'</font></center></td><td><center><font size="-1">Note</font></center></td><td><center><font size="-1"><b>'+Category+'</b></font></center></td><td><center><font size="-1">'+TempString+'</font></center></td></tr>');
    end;
  finally
  
  end;
end;

initialization
  ;

finalization
  if isLoggerActive then TelLogger.getInstance.Destroy;

end.
