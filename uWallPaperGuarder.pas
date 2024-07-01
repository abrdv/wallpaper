unit uWallPaperGuarder;

interface
uses Winapi.Windows, IniFiles, System.SysUtils, XsuperObject, System.Variants,
  System.Classes, System.IOUtils, uTimerThread, uLogit;
const
      strErrorWallpaperchangeTitle = 'Ошибка при смене обоев';
      strErrorWallpaperchangeText = 'При смене обоев возникла ошибка';
      strErrorCloseFormText = 'Возникла ошибка при закрытии формы';
      strErrorCloseFormTitle = 'Ошибка при закрытии формы';
      strErrorCreateFormText = 'Возникла ошибка при открытии формы';
      strErrorCreateFormTitle = 'Ошибка при открытии формы';
      strErrorChangeWPText = 'Ошибка изменения обоев';
      valueOfSecond = 1000;//1000
      valueOfMinute = 60;//60
      koefOfWaiting = 5;//5
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

  TWallPaperGuarder = class
    private
      FSettings: TWallPaperSettings;
      FListOfFiles:TStringList;
      FTChanger: TTimerThread;
      FTWaiter: TTimerThread;
      FReady: Boolean;

      procedure SetActive(value: Boolean);
      procedure TimerChangerEvent(Sender: TObject);
      procedure TimerWaiterEvent(Sender: TObject);
      procedure getallf(path:string);
      function putwp: Boolean;

      property ListOfFiles: TStringList read FListOfFiles;

    public
      constructor Create;
      destructor Destroy;
      procedure LoadSettings;
      procedure setUnworkable;
      procedure setWorkable;
      procedure SaveSettings(ApathSource: String; AspanPause: Integer);
      property Active: Boolean read FReady write SetActive;
      property Settings: TWallPaperSettings read FSettings;
      procedure Refresh;

  end;
implementation

  {TWallPaperSettings}

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
  tif:=TIniFile.Create(TPath.Combine(pathSource, 'main.ini'));
  try
    pathSource := tif.ReadString('main', 'pathtowp',
      TPath.Combine(pathSource, 'WP'));
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

  {TWallPaperGuarder}

constructor TWallPaperGuarder.Create;
begin
  try
    LoadSettings;

    Flistoffiles := TStringList.Create;

    FTChanger := TTimerThread.Create;
    FTChanger.Enabled := false;
    FTChanger.Interval := Settings.spanPause * valueOfSecond * valueOfMinute;
    FTChanger.OnTimerEvent := TimerChangerEvent;

    FTWaiter := TTimerThread.Create;
    FTWaiter.Enabled:=false;
    FTWaiter.Interval := Settings.spanPause * valueOfSecond * valueOfMinute;
    FTWaiter.OnTimerEvent:=TimerWaiterEvent;
  except
    on e: Exception do
      Logapp.writetolog(strErrorCreateFormText + '. ' + e.Message);
  end;
end;

destructor TWallPaperGuarder.Destroy;
begin
  try
    if Assigned(listoffiles) then FreeAndNil(listoffiles);
    if Assigned(Settings) then begin Settings.SaveToFile(); FreeAndNil(Settings); end;
    if Assigned(FTChanger) then begin FTChanger.Enabled:=false; FreeAndNil(FTChanger); end;
    if Assigned(FTWaiter) then begin FTWaiter.Enabled:=false; FreeAndNil(FTWaiter); end;
  except
    on e : Exception do
    begin
      Logapp.writetolog(strErrorCloseFormText+'. '+e.Message);
    end;
  end;
end;

procedure TWallPaperGuarder.LoadSettings;
begin
  if Settings = nil then FSettings := TWallPaperSettings.Create;

  if not FileExists(Settings.GetDefaultSettingsJSONFilename()) then
  begin
    ForceDirectories(Settings.GetSettingsFolder());
    Settings.SaveToFile();
  end;

  Settings.LoadFromFile();
end;

procedure TWallPaperGuarder.SaveSettings(ApathSource: String; AspanPause: Integer);
begin
  if Settings = nil then FSettings := TWallPaperSettings.Create;

  Settings.pathSource := ApathSource;
  Settings.spanPause := AspanPause;

  ForceDirectories(Settings.GetSettingsFolder());

  Settings.SaveToFile();
end;

function TWallPaperGuarder.putwp: Boolean;
var s:string;
    ch:integer;
begin
  result:=false;
  if ListOffiles=nil then exit;
  if ListOffiles.Count=0 then exit;

  ch:=Random(FListOffiles.Count-1);
  s:=TPath.Combine(Settings.pathSource, Flistoffiles[ch]);
  if TFile.Exists(s) then
  try
    //SPI_SETDESKWALLPAPER
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, @S[1], SPIF_UPDATEINIFILE OR SPIF_SENDWININICHANGE);
    result:=true;
  except
    on e : Exception do
    begin
      {
      Application.MessageBox(Pchar(strErrorWallpaperchangeText+'. '+e.Message),
        strErrorWallpaperchangeTitle, MB_OK or MB_ICONERROR);
      }
      logapp.writetolog(strErrorChangeWPText);
    end;
  end;
end;

procedure TWallPaperGuarder.SetActive(value: Boolean);
begin
  FReady := true;
end;

procedure TWallPaperGuarder.getallf(path:string);
var SR: TSearchRec;
begin
 if listoffiles=nil then Flistoffiles:=TStringList.Create;
 if FindFirst(Path + '*.jpg', faAnyFile, SR) = 0 then
   begin
     repeat
       if (SR.Attr <> faDirectory) then
       begin
         Flistoffiles.Add(SR.Name);
       end;
     until FindNext(SR) <> 0;
     FindClose(SR);
   end;
end;

procedure TWallPaperGuarder.Refresh;
begin
  if Settings=nil then exit;
  if listoffiles=nil then exit;
  getallf(Settings.pathSource);
  if listoffiles.Count=0 then begin setUnworkable; exit; end;
  setWorkable;
end;

procedure TWallPaperGuarder.TimerChangerEvent(Sender: TObject);
begin
  FTChanger.Enabled := putwp;
  FTWaiter.Enabled := not FTChanger.Enabled;
end;

procedure TWallPaperGuarder.TimerWaiterEvent(Sender: TObject);
begin
  FTWaiter.Enabled := false;
  getallf(Settings.pathSource);
  if listoffiles.Count=0 then begin setUnworkable; exit; end;
  setWorkable;
end;

procedure TWallPaperGuarder.setUnworkable;
begin
  FTChanger.Enabled := false;
  FTWaiter.Enabled := not FTChanger.Enabled;
end;

procedure TWallPaperGuarder.setWorkable;
begin
  FTChanger.Enabled := putwp;
  FTWaiter.Enabled := not FTChanger.Enabled;
  if not FTChanger.Enabled then setUnworkable;
end;

end.
