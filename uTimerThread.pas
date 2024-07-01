unit uTimerThread;
// https://stackoverflow.com/questions/12026951/using-vcl-ttimer-in-delphi-console-application
interface

uses
  Windows, Classes;

type
  TTimerThread = class(TThread)
  private
    FTimerEnabledFlag: THandle;
    FCancelFlag: THandle;
    FTimerProc: TNotifyEvent; // method to call
    FInterval: Cardinal;
    procedure SetEnabled(AEnable: Boolean);
    function GetEnabled: Boolean;
    procedure SetInterval(AInterval: Cardinal);
  protected
    procedure Execute; override;
  public
    constructor Create;
    Destructor Destroy; override;
    procedure Terminate;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval;
    // Note: OnTimerEvent is executed in TTimerThread thread
    property OnTimerEvent: TNotifyEvent read FTimerProc write FTimerProc;
  end;

implementation

constructor TTimerThread.Create;
begin
  FTimerEnabledFlag := CreateEvent(nil, True, False, nil);
  FCancelFlag := CreateEvent(nil, True, False, nil);
  FTimerProc := nil;
  FInterval := 1000;
  FreeOnTerminate := False; // Main thread controls for thread destruction
  inherited Create(False);
end;

destructor TTimerThread.Destroy; // Call TTimerThread.Free to cancel the thread
begin
  Terminate;
  if GetCurrentThreadID = MainThreadID then
  begin
    OutputDebugString('TTimerThread.Destroy :: MainThreadID (Waitfor)');
    Waitfor; // Synchronize
  end;
  CloseHandle(FCancelFlag);
  CloseHandle(FTimerEnabledFlag);
  OutputDebugString('TTimerThread.Destroy');
  inherited;
end;

procedure TTimerThread.Terminate;
begin
  inherited Terminate;
  Windows.ResetEvent(FTimerEnabledFlag); // Stop timer event
  Windows.SetEvent(FCancelFlag); // Set cancel flag
end;

procedure TTimerThread.SetEnabled(AEnable: Boolean);
begin
  if AEnable then
    Windows.SetEvent(FTimerEnabledFlag)
  else
    Windows.ResetEvent(FTimerEnabledFlag);
end;

function TTimerThread.GetEnabled: Boolean;
begin
  Result := WaitForSingleObject(FTimerEnabledFlag, 0) = WAIT_OBJECT_0 // Signaled
end;

procedure TTimerThread.SetInterval(AInterval: Cardinal);
begin
  FInterval := AInterval;
end;

procedure TTimerThread.Execute;
var
  WaitList: array[0..1] of THandle;
  WaitInterval, LastProcTime: Int64;
  Frequency, StartCount, StopCount: Int64; // minimal stop watch
begin
  QueryPerformanceFrequency(Frequency); // this will never return 0 on Windows XP or later
  WaitList[0] := FTimerEnabledFlag;
  WaitList[1] := FCancelFlag;
  LastProcTime := 0;
  while not Terminated do
  begin
    if (WaitForMultipleObjects(2, @WaitList[0], False, INFINITE) <> WAIT_OBJECT_0) then
      Break; // Terminate thread when FCancelFlag is signaled

    if Assigned(FTimerProc) then
    begin
      WaitInterval := FInterval - LastProcTime;
      if (WaitInterval < 0) then
        WaitInterval := 0;
      if WaitForSingleObject(FCancelFlag, WaitInterval) <> WAIT_TIMEOUT then
        Break;

      if Enabled then
      begin
        QueryPerformanceCounter(StartCount);
        if not Terminated then
          FTimerProc(Self);
        QueryPerformanceCounter(StopCount);
        // Interval adjusted for FTimerProc execution time
        LastProcTime := 1000 * (StopCount - StartCount) div Frequency; // ElapsedMilliSeconds
      end;
    end;
  end;
end;

end.
