unit UWPCH;

interface

uses
  Winapi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Mask,
  JvExComCtrls, JvComCtrls, JvComponentBase, JvTrayIcon, JvTimer, JvToolEdit,
  JvExMask, JvAppHotKey, JvShellHook,
  uWallPaperGuarder, uLogit, System.Classes;


type
  TFWallPaper = class(TForm)
    JvDirectoryEdit1: TJvDirectoryEdit;
    JvTimer1: TJvTimer;
    JvTrayIcon1: TJvTrayIcon;
    JvTrackBar1: TJvTrackBar;
    JvShellHook1: TJvShellHook;
    JvApplicationHotKey1: TJvApplicationHotKey;
    JvTimer2: TJvTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure JvApplicationHotKey1HotKey(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvDirectoryEdit1AfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure JvTrackBar1Changed(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
      WallPaperGuarder: TWallPaperGuarder;
      procedure LoadFromSettings();
      procedure SaveToSettings();
  end;

var
  FWallPaper: TFWallPaper;

implementation

{$R *.dfm}

procedure TFWallPaper.LoadFromSettings();
begin
  JvDirectoryEdit1.Text:= WallPaperGuarder.Settings.pathSource;
  JvTrackBar1.Position:= WallPaperGuarder.Settings.spanPause;
end;

procedure TFWallPaper.SaveToSettings();
begin
  WallPaperGuarder.SaveSettings(JvDirectoryEdit1.Text, JvTrackBar1.Position);
end;

procedure TFWallPaper.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WallPaperGuarder.SaveSettings(JvDirectoryEdit1.Text, JvTrackBar1.Position);
end;

procedure TFWallPaper.FormCreate(Sender: TObject);
begin
  WallPaperGuarder := TWallPaperGuarder.Create;
  WallPaperGuarder.Refresh;
end;

procedure TFWallPaper.FormDestroy(Sender: TObject);
begin
  if WallPaperGuarder<>nil then WallPaperGuarder.Free;
end;

procedure TFWallPaper.FormShow(Sender: TObject);
begin
  if WallPaperGuarder<>nil then
  begin
    try
      JvDirectoryEdit1.Directory:=WallPaperGuarder.Settings.pathSource;
      JvDirectoryEdit1.Flat:=true;
      JvDirectoryEdit1.InitialDir:=WallPaperGuarder.Settings.pathSource;
      JvDirectoryEdit1.Text:=WallPaperGuarder.Settings.pathSource;
      JvDirectoryEdit1.ButtonFlat:=true;
      JvTrackBar1.Position:=WallPaperGuarder.Settings.spanPause;
      WallPaperGuarder.setWorkable;
    except
      WallPaperGuarder.setUnworkable;
    end;
  end else
  begin
    //ошибка создания объекта класса выходим
    Close;
  end;
end;

procedure TFWallPaper.JvApplicationHotKey1HotKey(Sender: TObject);
begin
  WallPaperGuarder.setWorkable;
end;

procedure TFWallPaper.JvDirectoryEdit1AfterDialog(Sender: TObject; var AName: string;
  var AAction: Boolean);
begin
  WallPaperGuarder.Settings.pathSource := JvDirectoryEdit1.Directory;
  WallPaperGuarder.setWorkable;
end;

procedure TFWallPaper.JvTrackBar1Changed(Sender: TObject);
begin
  WallPaperGuarder.Settings.spanPause := JvTrackBar1.Position;
  WallPaperGuarder.setWorkable;
end;

end.
