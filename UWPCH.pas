unit UWPCH;

interface

uses
  Winapi.Windows, Winapi.Messages, IniFiles, System.SysUtils, System.Variants,
  System.Classes, System.IOUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Mask,
  JvExComCtrls, JvComCtrls, JvComponentBase, JvTrayIcon, JvTimer, JvToolEdit,
  JvExMask, JvAppHotKey, JvShellHook,
  XsuperObject;

const
      strErrorWallpaperchangeTitle = 'Ошибка при смене обоев';
      strErrorWallpaperchangeText = 'При смене обоев возникла ошибка';
      strErrorCloseFormText = 'Возникла ошибка при закрытии формы';
      strErrorCloseFormTitle = 'Ошибка при закрытии формы';
type
  TWallPaperSettings = class
  public
    pathSource: String;
    spanPause: Integer;
    constructor Create;
    class function GetDefaultSettingsINIFilename: string;
    class function GetDefaultSettingsJSONFilename: string;
    class function GetSettingsFolder: string;
    procedure LoadFromFile(AFileName: string = '');
    procedure LoadFromINIFile(AFileName: string = '');
    procedure SaveToFile(AFileName: string = '');
    procedure SaveToINIFile(AFileName: string = '');
  end;
  TFWallPaper = class(TForm)
    JvDirectoryEdit1: TJvDirectoryEdit;
    JvTimer1: TJvTimer;
    JvTrayIcon1: TJvTrayIcon;
    JvTrackBar1: TJvTrackBar;
    JvShellHook1: TJvShellHook;
    JvApplicationHotKey1: TJvApplicationHotKey;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure JvTimer1Timer(Sender: TObject);
    procedure JvApplicationHotKey1HotKey(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvDirectoryEdit1AfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
  private
    Settings: TWallPaperSettings;
    listoffiles:TStringList;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadFromSettings();
    procedure SaveToSettings();
    procedure getallf(path:string);
    function putwp: Boolean;
  public

  end;

var
  FWallPaper: TFWallPaper;

implementation

{$R *.dfm}

constructor TWallPaperSettings.Create;
begin
  pathSource:= GetSettingsFolder();
  spanPause:= 1;
end;

class function TWallPaperSettings.GetDefaultSettingsINIFilename: string;
begin
  Result := TPath.Combine(GetSettingsFolder(), 'settings.ini');
end;

class function TWallPaperSettings.GetDefaultSettingsJSONFilename: string;
begin
  Result := TPath.Combine(GetSettingsFolder(), 'settings.json');
end;

class function TWallPaperSettings.GetSettingsFolder: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure TWallPaperSettings.LoadFromFile(AFileName: string = '');
var Json: string;
begin
  if AFileName = '' then
    AFileName:= GetDefaultSettingsJSONFilename();
  if not FileExists(AFileName) then exit;
  Json:= TFile.ReadAllText(AFileName, TEncoding.UTF8);
  AssignFromJSON(Json);
end;

procedure TWallPaperSettings.LoadFromINIFile(AFileName: string = '');
var tif:TIniFile;
begin
  if AFileName = '' then
    AFileName:= GetDefaultSettingsINIFilename();
  if not FileExists(AFileName) then exit;
  tif:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'main.ini');
  try
    pathSource := tif.ReadString('main', 'pathtowp',
      TPath.Combine(ExtractFilePath(Application.ExeName), 'WP'));
    spanPause := tif.ReadInteger('main', 'timetowp', 1);
  finally
    tif.Free;
  end;
end;

procedure TWallPaperSettings.SaveToFile(AFileName: string = '');
var Json : string;
begin
  if AFileName = '' then
    AFileName:= GetDefaultSettingsJSONFilename();
  Json:= AsJSON(True);
  TFile.WriteAllText(AFileName, Json, TEncoding.UTF8);
end;

procedure TWallPaperSettings.SaveToINIFile(AFileName: string = '');
var tif:TIniFile;
begin
  if AFileName = '' then
    AFileName:= GetDefaultSettingsINIFilename();
  try
    tif:=TIniFile.Create(AFileName);
    tif.WriteString('main', 'pathtowp', pathSource);
    tif.WriteInteger('main', 'timetowp', spanPause);
  finally
    tif.Free;
  end;
end;

procedure TFWallPaper.LoadSettings;
begin
  if Settings = nil then Settings:= TWallPaperSettings.Create;

  if not FileExists(Settings.GetDefaultSettingsJSONFilename()) then
  begin
    ForceDirectories(Settings.GetSettingsFolder());
    Settings.SaveToFile();
  end;

  Settings.LoadFromFile();
  LoadFromSettings();
end;

procedure TFWallPaper.SaveSettings;
begin
  if Settings = nil then Settings:= TWallPaperSettings.Create;

  SaveToSettings;

  ForceDirectories(Settings.GetSettingsFolder());

  Settings.SaveToFile();
end;

procedure TFWallPaper.LoadFromSettings();
begin
  JvDirectoryEdit1.Text:= Settings.pathSource;
  JvTrackBar1.Position:= Settings.spanPause;
end;

procedure TFWallPaper.SaveToSettings();
begin
  Settings.pathSource:= JvDirectoryEdit1.Text;
  Settings.spanPause:= JvTrackBar1.Position;
end;

function TFWallPaper.putwp: Boolean;
var s:string;
    ch:integer;
begin
  result:=false;
  if listoffiles=nil then exit;
  if listoffiles.Count=0 then exit;

  ch:=Random(listoffiles.Count-1);
  s:=TPath.Combine(JvDirectoryEdit1.Directory, listoffiles[ch]);
  try
    //SPI_SETDESKWALLPAPER
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, @S[1], SPIF_UPDATEINIFILE OR SPIF_SENDWININICHANGE);
    result:=true;
  except
    on e : Exception do
    begin
      Application.MessageBox(Pchar(strErrorWallpaperchangeText+'. '+e.Message),
        strErrorWallpaperchangeTitle, MB_OK or MB_ICONERROR);
    end;
  end;
end;

procedure TFWallPaper.getallf(path:string);
var SR: TSearchRec;
begin
 if listoffiles=nil then listoffiles:=TStringList.Create;
 if FindFirst(Path + '*.jpg', faAnyFile, SR) = 0 then
   begin
     repeat
       if (SR.Attr <> faDirectory) then
       begin
         listoffiles.Add(SR.Name);
       end;
     until FindNext(SR) <> 0;
     FindClose(SR);
   end;
end;

procedure TFWallPaper.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;

  try
    if not Assigned(listoffiles) then FreeAndNil(listoffiles);
  except
    on e : Exception do
    begin
      Application.MessageBox(Pchar(strErrorCloseFormText+'. '+e.Message),
        strErrorCloseFormTitle, MB_OK or MB_ICONERROR);
    end;
  end;
end;

procedure TFWallPaper.FormCreate(Sender: TObject);
begin
  listoffiles:=TStringList.Create;
  LoadSettings;
end;

procedure TFWallPaper.FormShow(Sender: TObject);
begin
  JvDirectoryEdit1.Directory:=Settings.pathSource;
  JvDirectoryEdit1.Flat:=true;
  JvDirectoryEdit1.InitialDir:=Settings.pathSource;
  JvDirectoryEdit1.Text:=Settings.pathSource;
  JvDirectoryEdit1.ButtonFlat:=true;
  JvTrackBar1.Position:=Settings.spanPause;

  try
    getallf(JvDirectoryEdit1.Directory);
    if listoffiles.Count=0 then Application.Terminate;
    JvTimer1.Interval:=JvTrackBar1.Position*1000*60;
    JvTimer1.Enabled:=putwp;
  except
    Application.Terminate;
  end;
end;

procedure TFWallPaper.JvApplicationHotKey1HotKey(Sender: TObject);
begin
  JvTimer1.Enabled := putwp;
end;

procedure TFWallPaper.JvDirectoryEdit1AfterDialog(Sender: TObject; var AName: string;
  var AAction: Boolean);
begin
  if Assigned(listoffiles) then listoffiles.Clear;
  getallf(JvDirectoryEdit1.Directory);
  if listoffiles.Count=0 then Application.Terminate;
  JvTimer1.Enabled := putwp;
end;

procedure TFWallPaper.JvTimer1Timer(Sender: TObject);
begin
  JvTimer1.Enabled := putwp;
end;

end.
